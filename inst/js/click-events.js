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
        var num = function(v) { return (typeof v === 'string' && v.trim() !== '' && isFinite(Number(v))) ? Number(v) : v; };
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

    // any widget: clicks on the elements matching data.selector
    selector: function() {
      listen(function(ev) {
        var t = ev.target.closest ? ev.target.closest(data.selector) : null;
        if (!t || !el.contains(t)) { return; }
        var title = t.querySelector('title');
        var id = t.getAttribute('data-id') || (title ? title.textContent : null) || t.id || null;
        send({type: 'element', id: id, label: (t.textContent || '').trim(), data: plain(t.__data__)});
      });
    }
  };

  var handler = handlers[data.handler];
  if (!handler) { throw new Error('rvis click events: unknown handler ' + data.handler); }
  handler.call(this);
}
