# Interactive charts: clicks in shiny

In a shiny app or an rvis dashboard, a click on an item of a chart (a
node or link of a graph, a bar, a point, a slice, a table cell or a map
marker) can set a shiny input, so that other outputs react to it: show
the details of the clicked node, filter a table by the clicked bar, zoom
a map.

[`rvisClickEvents()`](https://genpack.github.io/rvis/reference/rvisClickEvents.md)
adds this to a chart. It adds a small javascript function to the
htmlwidget with
[`htmlwidgets::onRender()`](https://rdrr.io/pkg/htmlwidgets/man/onRender.html),
which runs after the plotting package has drawn the chart: the
javascript of the plotting package is not copied or changed, so new
versions of the package keep working.

## In a shiny app

Wrap the chart in
[`rvisClickEvents()`](https://genpack.github.io/rvis/reference/rvisClickEvents.md).
A click then sets `input$<output id>_item_click`:

``` r

library(shiny)
library(rvis)

ui <- fluidPage(
  echarts4r::echarts4rOutput("bars"),
  verbatimTextOutput("clicked"))

server <- function(input, output) {
  output$bars <- echarts4r::renderEcharts4r({
    deaths <- data.frame(age = rownames(VADeaths), VADeaths, check.names = FALSE)
    rvisClickEvents(rvisPlot("bar", "echarts", obj = deaths, x = "age", y = list("Rural Male", "Urban Male")))
  })
  output$clicked <- renderPrint(str(input$bars_item_click))
}

shinyApp(ui, server)
```

A click on the *70-74* bar of the *Rural Male* series gives:

    List of 9
     $ type  : chr "point"
     $ id    : chr "70-74"
     $ label : chr "70-74"
     $ series: chr "Rural Male"
     $ index : int 4
     $ value : num 66
     $ x     : chr "70-74"
     $ y     : num 66
     $ widget: chr "bars"

The input is set again on every click, also when the same item is
clicked twice. Give another input name with argument `inputId`.

## In a dashboard

Give an output item the field `click.events: true`: the chart returned
by its service sends its clicks to input `<item name>_item_click`, used
by the services of other items like any input. The example dashboard
[`click-events`](https://genpack.github.io/rvis/articles/dashboard-reference.html#clicks)
shows a graph and a bar chart and the items clicked on them:

``` yaml
# Clicks on chart items as shiny inputs.
# An output with 'click.events: true' sends clicks on the items of its chart (nodes, links, bars, points, ...)
# to input <output name>_item_click: a list with the type, id, label, series, value, ... of the clicked item.
# This works for Graphviz diagrams (rvis grviz and DiagrammeR), networkD3, echarts4r, apexcharter, plotly and
# highcharter charts; see ?rvisClickEvents for other widgets.
name: Click events
layout: page

objects:
  network:
    nodes: !expr data.frame(id = c('raw', 'clean', 'model', 'report'), label = c('Raw data', 'Cleaning', 'Model', 'Report'))
    links: !expr data.frame(source = c('raw', 'clean', 'model', 'clean'), target = c('clean', 'model', 'report', 'report'))

items:
  page:
    type: fluidPage
    title: Click a node, a link or a bar
    # a row of two columns (graph and bars), then a row with the clicked items
    layout: [[[graph], [bars]], [clicked]]

  graph:
    type: grvizOutput
    height: 350px
    weight: 6
    click.events: true
    service: |
      rvisPlot(type = 'graph', plotter = 'grviz', obj = session$userData$network,
               key = 'id', label = 'label', source = 'source', target = 'target')

  bars:
    type: echarts4r::echarts4rOutput
    height: 350px
    weight: 6
    click.events: true
    service: |
      deaths <- data.frame(age = rownames(VADeaths), VADeaths, check.names = FALSE)
      rvisPlot(type = 'bar', plotter = 'echarts', obj = deaths, x = 'age', y = list('Rural Male', 'Urban Male'))

  clicked:
    type: verbatimTextOutput
    service: |
      cat('Graph:\n'); str(input$graph_item_click)
      cat('\nBars:\n'); str(input$bars_item_click)
```

Run it with `runDashboard(rvisDashboardExample("click-events"))`.

## What a click gives

The value of the input is a list. Field `type` says what was clicked,
and the other fields are given as available:

| Field | Meaning |
|:---|:---|
| `type` | `"node"`, `"link"`, `"point"` (bars, points, slices), `"cell"` (tables), `"marker"` or `"shape"` (maps), `"element"` (selector mode) |
| `id` | id of the item: node key, category, row name, marker `layerId`, … |
| `label` | text shown for the item |
| `series` | series (or group) of the item |
| `index` | position of the item in its series |
| `value`, `x`, `y` | value and coordinates of the item |
| `source`, `target` | ends of a link |
| `row`, `column` | row number (as in R) and column name of a table cell |
| `lat`, `lng` | clicked point of a map |
| `group`, `depth`, `parent` | group of a node or marker, level and parent in a hierarchy |
| `data` | the d3 data of the clicked element (networkD3 and selector mode) |
| `widget` | id of the chart |

## Supported charts

| Plotter (package) | Clickable items | Notes |
|:---|:---|:---|
| `grviz` (DiagrammeR) and [`DiagrammeR::grViz()`](https://rich-iannone.github.io/DiagrammeR/reference/grViz.html) | nodes, edges | nodes of rvis grviz graphs are reported by their key |
| `networkD3` | nodes, links | sankey, force and tree networks |
| `echarts` (echarts4r) | all chart items | all chart types, including graphs and sankeys |
| `apexcharter` | bars, points, slices |  |
| `plotly` | points, bars |  |
| `highcharter` | points, bars, slices |  |
| `amCharts` (rAmCharts) | bars, points, slices | rAmCharts charts are converted to htmlwidgets by [`rvisClickEvents()`](https://genpack.github.io/rvis/reference/rvisClickEvents.md); [`rAmCharts::renderAmCharts()`](https://rdrr.io/pkg/rAmCharts/man/renderAmCharts.html) needs [`library(rAmCharts)`](https://datastorm-open.github.io/introduction_ramcharts/) |
| `c3`, `billboarder` | bars, points, slices | found under the transparent event layer of these charts |
| `visNetwork` | nodes, edges |  |
| `DT` | table cells | `row`, `column`, `value`, and the row name as `id` |
| `dygraphs` | points | the series nearest to the pointer; the date as the chart shows it |
| `leaflet` | markers, shapes | including layers added later, for example by `leafletProxy()` |
| `coffeewheel` (rvis) | sunburst arcs | `name`, `value`, `depth`, `parent` |

Some packages also set inputs of their own (like `input$<id>_click` of
DiagrammeR and billboarder, the `event_data()` of plotly or the
`_marker_click` inputs of leaflet). They keep working: rvis uses its own
input name, `<id>_item_click`, which has the same form for all charts.

## Any other widget: a CSS selector

For any other htmlwidget, give the CSS selector of its clickable
elements:

``` r

output$sun <- sunburstR::renderSunburst(
  rvisClickEvents(sunburstR::sunburst(sequences), selector = "path"))
```

The input then gets the id of the clicked element (its `data-id`
attribute, svg title or id), its text, and its d3 data in field `data`.
Elements under a transparent layer, or which ignore the pointer, are
found too.

## Good to know

- Charts whose packages bring different versions of the d3 javascript
  library cannot share a page: for example coffeewheel (d3 version 3)
  with networkD3, or c3 with billboarder. This is a limit of the
  packages, not of the click events.
- dygraphs selects a point when the pointer moves over the plot: a click
  without moving the pointer first gives no point.
- A failure of the click javascript is written as a warning in the
  browser console and never stops the chart, or the other charts of the
  page, from being shown.
