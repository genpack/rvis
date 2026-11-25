function(el, x, data) {
  // Sends clicks on the items of an htmlwidget to shiny input data.input (default: <widget id>_item_click;
  // some widgets set their own <widget id>_click input).
  // Used by rvis::rvisClickEvents() through htmlwidgets::onRender(): it runs after the widget is drawn,
  // so the widget's own javascript is not copied or changed. data.handler says how the widget is read.
  var input = data.input || (el.id + '_item_click');
  // data.ids: ids reported instead of the ids found in the widget (like the keys of the nodes of rvis grviz graphs)
  var ids = data.ids || {};
  function key(id) { return ids[id] !== undefined ? ids[id] : id; }

  function send(payload) {
    payload.widget = el.id;
    el.__rvisLastClick = payload;
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue(input, payload, {priority: 'event'});
    }
    el.dispatchEvent(new CustomEvent('rvis:click', {detail: payload, bubbles: true}));
  }

  // Simple (JSON-friendly) copy of the d3 data of an element: values and the names of linked objects
  // (layout fields computed by d3, like positions and velocities, are left out)
  var skip = ['x', 'y', 'dx', 'dy', 'sy', 'ty', 'vx', 'vy', 'fx', 'fy', 'index', 'depth', 'sourceLinks', 'targetLinks'];
  function plain(d) {
    if (d === null || typeof d !== 'object') { return d; }
    var out = {};
    Object.keys(d).forEach(function(k) {
      var v = d[k];
      if (skip.indexOf(k) >= 0) { return; }
      if (v === null || ['string', 'number', 'boolean'].indexOf(typeof v) >= 0) { out[k] = v; }
      else if (typeof v === 'object' && !Array.isArray(v) && (v.name !== undefined || v.id !== undefined)) {
        out[k] = v.name !== undefined ? v.name : v.id;
      }
    });
    return out;
  }

  // One click listener per element: a widget drawn again (in shiny) replaces the previous listener
  function listen(handler) {
    if (el.__rvisClickHandler) { el.removeEventListener('click', el.__rvisClickHandler); }
    el.__rvisClickHandler = handler;
    el.addEventListener('click', handler);
  }

  // A click detected from pointer down and up at the same place. d3 charts with draggable items (like networkD3
  // sankey nodes) move the item in the page when the pointer goes down, and the browser then fires no click.
  // Listened in the capture phase, before the chart's own handlers can stop the events.
  function listenTap(handler) {
    if (el.__rvisDown) {
      el.removeEventListener('pointerdown', el.__rvisDown, true);
      el.removeEventListener('pointerup', el.__rvisUp, true);
    }
    var down = null;
    el.__rvisDown = function(ev) { down = {x: ev.clientX, y: ev.clientY, target: ev.target}; };
    el.__rvisUp = function(ev) {
      if (down && Math.abs(ev.clientX - down.x) < 5 && Math.abs(ev.clientY - down.y) < 5) { handler({target: down.target}); }
      down = null;
    };
    el.addEventListener('pointerdown', el.__rvisDown, true);
    el.addEventListener('pointerup', el.__rvisUp, true);
  }

  // The element matching selector at the clicked point: the clicked element or one of its parents, or an element
  // under a transparent layer drawn over the chart (like the event layers of c3 and billboarder)
  function hit(ev, selector) {
    var t = ev.target && ev.target.closest ? ev.target.closest(selector) : null;
    if (t && el.contains(t)) { return t; }
    if (ev.clientX === undefined) { return null; }
    if (document.elementsFromPoint) {
      var under = document.elementsFromPoint(ev.clientX, ev.clientY);
      for (var i = 0; i < under.length; i++) {
        if (el.contains(under[i]) && under[i].matches && under[i].matches(selector)) { return under[i]; }
      }
    }
    // shapes which ignore the pointer (css pointer-events: none, like billboarder bars) are not found above:
    // the point is tested against the geometry of each shape
    var shapes = el.querySelectorAll(selector);
    for (var j = shapes.length - 1; j >= 0; j--) {
      var s = shapes[j], b = s.getBoundingClientRect();
      if (ev.clientX < b.left || ev.clientX > b.right || ev.clientY < b.top || ev.clientY > b.bottom) { continue; }
      if (s.isPointInFill && s.getScreenCTM && s.ownerSVGElement) {
        var p = s.ownerSVGElement.createSVGPoint();
        p.x = ev.clientX; p.y = ev.clientY;
        p = p.matrixTransform(s.getScreenCTM().inverse());
        if (s.isPointInFill(p) || (s.isPointInStroke && s.isPointInStroke(p))) { return s; }
      } else { return s; }
    }
    return null;
  }

  // numbers given as text (by echarts4r and dygraphs) are sent as numbers
  function num(v) { return (typeof v === 'string' && v.trim() !== '' && isFinite(Number(v))) ? Number(v) : v; }

  // a time (milliseconds) as the chart shows it: 'YYYY-MM-DD' for whole days, else 'YYYY-MM-DD hh:mm:ss'
  function timeText(ms, utc) {
    var d = new Date(ms), pad = function(n) { return (n < 10 ? '0' : '') + n; };
    var p = utc ? [d.getUTCFullYear(), d.getUTCMonth() + 1, d.getUTCDate(), d.getUTCHours(), d.getUTCMinutes(), d.getUTCSeconds()]
                : [d.getFullYear(), d.getMonth() + 1, d.getDate(), d.getHours(), d.getMinutes(), d.getSeconds()];
    var day = p[0] + '-' + pad(p[1]) + '-' + pad(p[2]);
    return (p[3] === 0 && p[4] === 0 && p[5] === 0) ? day : day + ' ' + pad(p[3]) + ':' + pad(p[4]) + ':' + pad(p[5]);
  }

  function textOf(g) {
    return [].map.call(g.querySelectorAll('text'), function(t) { return t.textContent; }).join(' ').trim();
  }

  var handlers = {
    // Graphviz diagrams (DiagrammeR::grViz): nodes and edges are svg groups whose <title> is their id
    graphviz: function() {
      [].forEach.call(el.querySelectorAll('g.node, g.edge'), function(g) { g.style.cursor = 'pointer'; });
      listen(function(ev) {
        var g = ev.target.closest ? ev.target.closest('g.node, g.edge') : null;
        if (!g || !el.contains(g)) { return; }
        var title = g.querySelector('title');
        var id = title ? title.textContent : g.id;
        if (g.classList.contains('node')) {
          send({type: 'node', id: key(id), label: textOf(g) || id});
        } else {
          var ends = id.split(/->|--/);
          send({type: 'link', id: id, label: textOf(g), source: key(ends[0]), target: key(ends[1])});
        }
      });
    },

    // d3 charts which bind data to their elements (networkD3 sankey, force, tree networks, ...)
    d3data: function() {
      listenTap(function(ev) {
        var g = ev.target.closest ? ev.target.closest('.node, .link') : null;
        if (!g || !el.contains(g) || !g.__data__) { return; }
        var d = g.__data__;
        if (g.classList.contains('node')) {
          var p = plain(d);
          send({type: 'node', id: d.name !== undefined ? d.name : d.id, label: d.name, group: p.group, value: p.value, data: p});
        } else {
          var name = function(n) { return n && typeof n === 'object' ? (n.name !== undefined ? n.name : n.index) : n; };
          send({type: 'link', source: name(d.source), target: name(d.target), value: d.value, data: plain(d)});
        }
      });
    },

    // echarts4r: the chart's own click event
    echarts: function() {
      var chart = this.getChart();
      chart.off('click');
      chart.on('click', function(p) {
        var type = p.dataType === 'node' ? 'node' : (p.dataType === 'edge' ? 'link' : 'point');
        var payload = {type: type, id: p.name, label: p.name, series: p.seriesName, index: p.dataIndex, value: p.value};
        // cartesian charts give [x, y] (or [x, y, size]) pairs as value; echarts4r passes numbers as text
        if (Array.isArray(p.value)) { payload.x = num(p.value[0]); payload.y = num(p.value[1]); payload.value = num(p.value[1]); }
        else { payload.value = num(p.value); }
        if (type === 'link' && p.data) { payload.source = p.data.source; payload.target = p.data.target; }
        send(payload);
      });
    },

    // apexcharter: the chart's own click event (data point index -1: click outside the data points)
    apex: function() {
      var chart = this.getChart();
      if (!chart || !chart.addEventListener) { return; }
      if (el.__rvisApexHandler) { chart.removeEventListener('click', el.__rvisApexHandler); }
      el.__rvisApexHandler = function(e, ctx, cfg) {
        // the point index can be given as text, and the series index is not set in click events: it is read
        // from the svg group of the clicked series
        var i = cfg ? parseInt(cfg.dataPointIndex, 10) : NaN;
        if (isNaN(i) || i < 0) { return; }
        var g = cfg.globals, s = parseInt(cfg.seriesIndex, 10);
        if (isNaN(s) || s < 0) {
          var sEl = e && e.target && e.target.closest ? e.target.closest('.apexcharts-series') : null;
          s = sEl ? parseInt(sEl.getAttribute('data:realIndex'), 10) : 0;
          if (isNaN(s)) { s = 0; }
        }
        var pie = !Array.isArray(g.series[0]);
        var labels = (g.labels && g.labels.length) ? g.labels : g.categoryLabels;
        send({type: 'point', id: labels ? labels[i] : i, label: labels ? labels[i] : undefined,
              series: pie ? undefined : g.seriesNames[s], index: i,
              value: pie ? g.series[i] : (g.series[s] ? g.series[s][i] : undefined)});
      };
      chart.addEventListener('click', el.__rvisApexHandler);
    },

    // plotly: the plot's own click event
    plotly: function() {
      if (el.__rvisPlotlyHandler && el.removeListener) { el.removeListener('plotly_click', el.__rvisPlotlyHandler); }
      el.__rvisPlotlyHandler = function(d) {
        var p = d.points[0];
        send({type: 'point', id: p.id !== undefined ? p.id : p.pointNumber, label: p.label !== undefined ? p.label : p.text,
              series: p.data.name, index: p.pointNumber, x: p.x, y: p.y, value: p.value !== undefined ? p.value : p.y});
      };
      el.on('plotly_click', el.__rvisPlotlyHandler);
    },

    // highcharter: a click event added to the points of all series
    highcharts: function() {
      var chart = (window.Highcharts && Highcharts.charts || []).filter(function(c) {
        return c && (c.renderTo === el || el.contains(c.renderTo));
      })[0];
      if (!chart) { return; }
      chart.update({plotOptions: {series: {cursor: 'pointer', point: {events: {click: function() {
        // the point's own id only if given in the data (highcharts makes ids for all points)
        var own = this.options && this.options.id;
        send({type: 'point', id: own !== undefined ? own : (this.name !== undefined ? this.name : this.category),
              label: this.name !== undefined ? this.name : this.category, series: this.series.name, index: this.index,
              x: this.x, y: this.y, value: this.value !== undefined ? this.value : this.y});
      }}}}}}, true);
    },

    // any widget: clicks on the elements matching data.selector (also under a transparent event layer)
    selector: function() {
      listen(function(ev) {
        var t = hit(ev, data.selector);
        if (!t) { return; }
        var title = t.querySelector('title');
        var id = t.getAttribute('data-id') || (title ? title.textContent : null) || t.id || null;
        send({type: 'element', id: id, label: (t.textContent || '').trim(), data: plain(t.__data__)});
      });
    },

    // c3 and billboarder: shapes (bars, points, arcs) have d3 data, but clicks go to a transparent event layer
    // drawn over them, so the shape under the pointer is found with elementsFromPoint()
    c3: function() {
      var prefix = el.querySelector('.bb-shape, .bb-chart') ? 'bb' : 'c3';
      var inst = this;
      // category labels: billboarder charts give them (chart.categories()); for c3, the x axis tick labels are read
      var categories = function() {
        try { var ch = inst && inst.getChart ? inst.getChart() : null; if (ch && ch.categories) { return ch.categories(); } } catch (e) {}
        return null;
      };
      listen(function(ev) {
        var t = hit(ev, '.' + prefix + '-shape, .' + prefix + '-chart-arc path');
        if (!t || !t.__data__) { return; }
        var d = t.__data__;
        // c3 0.7 bars carry the data of their whole series: the point is picked by the index in their class name
        if (d.index === undefined && Array.isArray(d.values)) {
          var m = new RegExp('(?:^|\\s)' + prefix + '-shape-(\\d+)(?:\\s|$)').exec(t.getAttribute('class') || '');
          if (!m || !d.values[+m[1]]) { return; }
          d = d.values[+m[1]];
        }
        if (d.data && d.data.id !== undefined) {           // pie, donut and gauge arcs
          send({type: 'point', id: d.data.id, label: d.data.id, series: d.data.id, value: d.value});
          return;
        }
        var cats = categories(), category = d.x;
        if (cats && cats.length > d.index) { category = cats[d.index]; }
        else {
          var ticks = el.querySelectorAll('.' + prefix + '-axis-x .tick text');
          if (ticks.length > d.index && typeof d.x === 'number' && d.x === d.index) { category = ticks[d.index].textContent; }
        }
        send({type: 'point', id: category, label: category, series: d.id, index: d.index, x: category, y: d.value, value: d.value});
      });
    },

    // rAmCharts: the chart's own events (clickGraphItem for serial charts, clickSlice for pie and funnel charts)
    amcharts: function() {
      var tries = 0;
      (function attach() {
        var chart = ((window.AmCharts && AmCharts.charts) || []).filter(function(c) { return c && c.div && el.contains(c.div); })[0];
        if (!chart) { if (tries++ < 20) { setTimeout(attach, 100); } return; }
        if (chart.__rvisClick) { return; }
        chart.__rvisClick = true;
        chart.addListener('clickGraphItem', function(e) {
          var item = e.item;
          send({type: 'point', id: item.category, label: item.category, series: e.graph.title || e.graph.valueField,
                index: item.index, x: item.category, y: item.values.value, value: item.values.value});
        });
        chart.addListener('clickSlice', function(e) {
          var item = e.dataItem;
          send({type: 'point', id: item.title, label: item.title, index: item.index, value: item.value});
        });
      })();
    },

    // visNetwork: the network's own click event
    visnetwork: function() {
      var network = this.network;
      if (!network) { return; }
      network.off('click');
      network.on('click', function(p) {
        if (p.nodes && p.nodes.length > 0) {
          var node = network.body.data.nodes.get(p.nodes[0]) || {};
          send({type: 'node', id: p.nodes[0], label: node.label, group: node.group});
        } else if (p.edges && p.edges.length > 0) {
          var edge = network.body.data.edges.get(p.edges[0]) || {};
          send({type: 'link', id: p.edges[0], label: edge.label, source: edge.from, target: edge.to, value: edge.value});
        }
      });
    },

    // DT tables: clicks on cells (row number as in R, column name, value and the row name if shown)
    datatable: function() {
      var $ = window.jQuery;
      if (!$) { return; }
      $(el).off('click.rvis').on('click.rvis', 'tbody td', function() {
        var table = $(el).find('table.dataTable').DataTable();
        var cell = table.cell(this);
        var idx = cell.index();
        if (!idx) { return; }
        var header = $(table.column(idx.column).header()).text();
        var row = table.row(idx.row).data();
        send({type: 'cell', id: Array.isArray(row) ? row[0] : undefined, row: idx.row + 1, column: header,
              index: idx.column, value: cell.data()});
      });
    },

    // dygraphs: a click callback (added to the callback dygraphs may have set for its own shiny input)
    dygraphs: function() {
      var g = this.dygraph;
      if (!g) { return; }
      var previous = el.__rvisDygraphPrevious !== undefined ? el.__rvisDygraphPrevious : g.getOption('clickCallback');
      el.__rvisDygraphPrevious = previous || null;
      g.updateOptions({clickCallback: function(e, xval, points) {
        // the previous callback (dygraphs' own shiny input) runs after ours, and its failures are ignored
        var self = this, args = arguments;
        setTimeout(function() { if (previous) { try { previous.apply(self, args); } catch (err) {} } }, 0);
        // the series nearest to the pointer, among the points at the clicked x
        var y = g.eventToDomCoords(e)[1], best = null;
        (points || []).forEach(function(p) { if (!isNaN(p.canvasy) && (!best || Math.abs(p.canvasy - y) < Math.abs(best.canvasy - y))) { best = p; } });
        var date = x.format === 'date' || (x.attrs && x.attrs.axes && x.attrs.axes.x && x.attrs.axes.x.ticker && /date/i.test(String(x.attrs.axes.x.ticker)));
        var xv = date ? timeText(xval, g.getOption('labelsUTC')) : num(xval);
        send({type: 'point', id: xv, x: xv, series: best ? best.name : undefined, y: best ? num(best.yval) : undefined,
              value: best ? num(best.yval) : undefined, index: best ? best.idx : undefined});
      }}, true);
    },

    // leaflet: clicks on markers and shapes (layers added later, for example by leafletProxy, included)
    leaflet: function() {
      // in onRender code of leaflet widgets, 'this' is the leaflet map
      var map = (this && this.eachLayer) ? this : (this && this.getMap ? this.getMap() : null);
      if (!map) { var inst = HTMLWidgets.getInstance(el); map = inst && inst.getMap ? inst.getMap() : null; }
      if (!map) { return; }
      function hook(layer) {
        if (layer.__rvisClick || !layer.options || !(layer.getLatLng || layer.getLatLngs)) { return; }
        layer.__rvisClick = true;
        layer.on('click', function(e) {
          send({type: layer.getLatLng ? 'marker' : 'shape', id: layer.options.layerId, group: layer.options.group,
                lat: e.latlng.lat, lng: e.latlng.lng});
        });
      }
      map.eachLayer(hook);
      if (el.__rvisLayerAdd) { map.off('layeradd', el.__rvisLayerAdd); }
      el.__rvisLayerAdd = function(e) { hook(e.layer); };
      map.on('layeradd', el.__rvisLayerAdd);
    },

    // sunbursts and other d3 hierarchies (rvis coffeewheel): arcs whose d3 data have a name
    hierarchy: function() {
      listen(function(ev) {
        var t = hit(ev, 'path');
        if (!t || !t.__data__ || t.__data__.name === undefined) { return; }
        var d = t.__data__;
        send({type: 'node', id: d.name, label: d.name, value: d.value, depth: d.depth,
              parent: d.parent && d.parent.name !== undefined ? d.parent.name : undefined});
      });
    }
  };

  // a failure here must never stop the widget (or the other widgets of the page) from being shown
  var handler = handlers[data.handler];
  try {
    if (!handler) { throw new Error('unknown handler ' + data.handler); }
    handler.call(this);
  } catch (e) {
    if (window.console) { console.warn('rvis click events could not be added to widget ' + el.id + ': ' + e.message); }
  }
}
