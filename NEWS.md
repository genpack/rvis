# rvis 5.16.0

* Click events for eight more plotters: rAmCharts, c3, billboarder, visNetwork, DT (clicks on cells),
  dygraphs, leaflet (markers and shapes, including layers added later) and rvis coffeewheel sunbursts.
  rAmCharts charts are converted to htmlwidgets by `rvisClickEvents()`.
* Clicks on shapes under a transparent event layer, or which ignore the pointer, are found (also in the
  selector mode). A failure of the click javascript is logged as a console warning and never stops a chart
  from being shown. Dates are reported as the chart shows them.

# rvis 5.15.0

* New function `rvisClickEvents()`: clicks on the items of a chart (nodes, links, bars, points, slices) set the
  shiny input `<output id>_item_click`, for Graphviz (DiagrammeR and rvis grviz), networkD3, echarts4r,
  apexcharter, plotly and highcharter charts, and for any widget with a CSS selector. The javascript is added
  with `htmlwidgets::onRender()`: the plotting packages are not changed.
* Dashboard outputs take the field `click.events: true`. New example dashboard `click-events`.
* `grviz` charts are the `grViz` widget of DiagrammeR: the copied widget javascript and libraries (2.1 MB)
  are removed. Clicks on grviz graph nodes report the node keys.

# rvis 5.14.0

* New plotter **apexcharter**: `bar`, `line`, `area`, `scatter`, `bubble`, `pie` (or donut), `tsline`,
  `tsarea`, `tsbar`, `heatmap` and `radar`.
* Plotter **echarts** (echarts4r): new chart types `line`, `area`, `tsline`, `tsarea`, `tsbar`, `scatter`,
  `bubble`, `pie`, `box`, `heatmap`, `calendar`, `gauge`, `sankey`, `treemap`, `sunburst` and `graph`
  (with node shapes, including ovals, and node sizes); standard settings (title, legend, tooltip, axis
  labels, stacking, labels, zoom, theme).
* Examples of the new charts in the chart guides; plotter roadmap updated.

# rvis 5.13.0

* Dashboard items and clothes of type `package::function` (like `shinyWidgets::pickerInput` or
  `shinydashboard::box`) call that function with the item fields as arguments, so any function of any
  package can be a dashboard item. Inputs, outputs and containers are found from the function arguments;
  outputs are rendered by the package's `renderFoo` function and inputs updated by its `updateFoo` function.
  New example dashboard `generic-types`.

# rvis 5.12.0

* Dashboards from YAML or JSON files or R lists: `rvisDashboard()`, `runDashboard()` and
  `rvisDashboardExample()`, with example dashboards: basic features, the apps of the Shiny gallery
  rebuilt from YAML, and two showcase dashboards.
* `DASHBOARD` code review: the user interface is built by calling shiny functions directly, the server code
  is parsed once per app; new item types and settings; many bug fixes.
* Five dashboard guides: getting started, layouts, item reference, gallery and Shiny packages.

# rvis 5.11.0

* Six chart guides with rendered examples of every chart type and plotter, each on its own R data set.
  Guide on the plotters covered and planned. Bug fixes found by the guides (xCharts, c3, highcharter,
  pivot and TFD3 charts).

# rvis 5.10.x

* 5.10.2: bundled jQuery (used by TFD3 tables) updated to 3.7.1.
* 5.10.1: charts which were created without error but could not be shown are fixed (TFD3, coffeewheel,
  d3plus, echarts word clouds, knitr printing of rCharts and googleVis charts).
* 5.10.0: code review release: bug fixes in many wrappers, faster configuration checks, clean-up.

The full history of earlier versions is in the header of file `R/rvis.R` and of each source file.
