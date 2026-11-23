HTMLWidgets.widget({
  name: 'grviz',
  type: 'output',

  initialize: function(el, width, height) {
    return {
      // TODO: add instance fields as required
    };
  },

  renderValue: function(el, x, instance) {
    //  Use this to sort of make our diagram responsive
    //  or at a minimum fit within the bounds set by htmlwidgets
    //  for the parent container
    function makeResponsive(el){
      var svg = el.getElementsByTagName("svg")[0];
      if (svg) {
        if (svg.width) {svg.removeAttribute("width")}
        if (svg.height) {svg.removeAttribute("height")}
        svg.style.width = "100%";
        svg.style.height = "100%";
      }
    }

    if (typeof x.diagram === "string" && x.diagram !== "") {
      var config = x.config || {};
      var vizOptions = {};
      var suppliedOptions = config.options || {};
      for (var option in suppliedOptions) {
        if (Object.prototype.hasOwnProperty.call(suppliedOptions, option)) {
          vizOptions[option] = suppliedOptions[option];
        }
      }
      vizOptions.format = "svg";
      vizOptions.engine = config.engine || vizOptions.engine || "dot";

      try {
        el.innerHTML = Viz(x.diagram, vizOptions);
        makeResponsive(el);

        // set up a container for tasks to perform after completion
        //  one example would be add callbacks for event handling
        //  styling
        var tasks = config.tasks;
        if (typeof tasks !== "undefined" && tasks !== null) {
          if ((typeof tasks.length === "undefined") ||
              (typeof tasks === "function")) {
            // handle a function not enclosed in array
            // should be able to remove once using jsonlite
            tasks = [tasks];
          }
          tasks.map(function(t){
            // for each tasks add it to the mermaid.tasks with el
            t.call(el);
          });
        }
      } catch(e){
        var p = document.createElement("pre");
        p.innerText = e;
        el.appendChild(p);
      }
    }

  },

  resize: function(el, width, height, instance) {
  }
});

