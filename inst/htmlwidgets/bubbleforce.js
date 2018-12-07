HTMLWidgets.widget({

  name: "bubbleForceInfobox",

  type: "output",

  initialize: function(el, width, height) {

    d3.select(el).append("svg")
    .attr("width", width)
    .attr("height", height);

    return d3.layout.force();
  },

  resize: function(el, width, height, force) {

    d3.select(el).select("svg")
    .attr("width", width)
    .attr("height", height);

    force.size([width, height]).resume();
  },

  renderValue: function(el, x, force) {

    // get the width and height
    var width = el.offsetWidth;
    var height = el.offsetHeight;


    //Get options
    //var initInfobox = x.settings.initInfobox;
    var initInfobox = "Click on the node to see details!";

    // alias options
    // var width = 560;
    // var height = 500;
    var padding = 1.5, // separation between same-color nodes
    clusterPadding = 6, // separation between different-color nodes
    maxRadius = 12;

    var n = x.n, // total number of nodes
    m = x.m; // number of distinct clusters

    var color = d3.scale.category10()
    .domain(d3.range(m));

    var clusters = HTMLWidgets.dataframeToD3(x.clusters);

    var nodes = HTMLWidgets.dataframeToD3(x.data);

    // todo: if force is not given:...
    var force = d3.layout.force()
    .nodes(nodes)
    .size([width, height])
    .gravity(0.02)
    .charge(0)
    .on("tick", tick)
    .start();

    // select the svg element and remove existing children
    var svg = d3.select(el).select("svg");
    svg.selectAll("*").remove();

    // draw nodes
    var node = svg.selectAll("circle")
    .data(nodes)
    .enter().append("circle")
    .style("fill", function(d) {
      return color(d.cluster);
    })
    .call(force.drag);

    // intro transition
    node.transition()
    .duration(750)
    .delay(function(d, i) {
      return i * 5;
    })
    .attrTween("r", function(d) {
      var i = d3.interpolate(0, d.radius);
      return function(t) {
        return d.radius = i(t);
      };
    });


    // Add click event
    // d3.selectAll(".node,.link")
    d3.selectAll("circle")
    .on("click", function(d, i) {
      // var d = this.__data__;
      // todo: This is where d.htmlinfo is returned, better to return label of the node as shiny input
      console.log("Node ID: ", i);
    });

    function tick(e) {
      node
      .each(cluster(10 * e.alpha * e.alpha))
      .each(collide(0.5))
      .attr("cx", function(d) {
        return d.x;
      })
      .attr("cy", function(d) {
        return d.y;
      });
    }

    // Move d to be adjacent to the cluster node.
    function cluster(alpha) {
      return function(d) {
        var cluster = clusters[d.cluster];
        if (cluster === d) return;
        var x = d.x - cluster.x,
        y = d.y - cluster.y,
        l = Math.sqrt(x * x + y * y),
        r = d.radius + cluster.radius;
        if (l != r) {
          l = (l - r) / l * alpha;
          d.x -= x *= l;
          d.y -= y *= l;
          cluster.x += x;
          cluster.y += y;
        }
      };
    }

    // Resolves collisions between d and all other circles.
    function collide(alpha) {
      var quadtree = d3.geom.quadtree(nodes);
      return function(d) {
        var r = d.radius + maxRadius + Math.max(padding, clusterPadding),
        nx1 = d.x - r,
        nx2 = d.x + r,
        ny1 = d.y - r,
        ny2 = d.y + r;
        quadtree.visit(function(quad, x1, y1, x2, y2) {
          if (quad.point && (quad.point !== d)) {
            var x = d.x - quad.point.x,
            y = d.y - quad.point.y,
            l = Math.sqrt(x * x + y * y),
            r = d.radius + quad.point.radius + (d.cluster === quad.point.cluster ? padding : clusterPadding);
            if (l < r) {
              l = (l - r) / l * alpha;
              d.x -= x *= l;
              d.y -= y *= l;
              quad.point.x += x;
              quad.point.y += y;
            }
          }
          return x1 > nx2 || x2 < nx1 || y1 > ny2 || y2 < ny1;
        });
      };
    }
    // Legend
    // http://jsfiddle.net/Rom2BE/H2PkT/

      var legendData = {clusterName: x.clusters.clusterName}
    legendData = HTMLWidgets.dataframeToD3(legendData)
    // console.log("color.domain(): ",color.domain())
    // console.log("Colors: ",color)
    // console.log("Color 3: ",color(3))
    // console.log(legendData)

    var legend = svg.selectAll(".legend")
    .data(legendData)
    .enter().append("g")
    .attr("class", "legend")
    .attr("transform", function(d, i) {
      return "translate(0," + i * 20 + ")";
    });

    legend.append("rect")
    .attr("x", width - 18)
    .attr("width", 18)
    .attr("height", 18)
    .style("fill", function(d,i) {
      return color(i);
    });

    legend.append("text")
    .attr("x", width - 24)
    .attr("y", 9)
    .attr("dy", ".35em")
    .style("text-anchor", "end")
    .text(function(d) {
      return d.clusterName;
    });

    // Tooltip

    $('svg circle').tipsy({
      gravity: 'w',
      html: true,
      title: function() {
        var d = this.__data__;
        var hover = d.hover
        return hover;
      }
    });

  },
});
