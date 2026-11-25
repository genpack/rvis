# Send clicks on a chart to shiny

Adds javascript to an htmlwidget so that a click on one of its items (a
node or link of a graph, a point, bar or slice of a chart) sets a shiny
input. The javascript is added with
[`htmlwidgets::onRender()`](https://rdrr.io/pkg/htmlwidgets/man/onRender.html)
and runs after the widget is drawn: the javascript of the plotting
package is not copied or changed, so later versions of the package keep
working.

## Usage

``` r
rvisClickEvents(widget, inputId = NULL, selector = NULL, ids = NULL)
```

## Arguments

- widget:

  An htmlwidget, for example a chart returned by
  [`rvisPlot()`](https://genpack.github.io/rvis/reference/rvisPlot.md)
  (rAmCharts charts, which are not htmlwidgets until they are plotted,
  are converted).

- inputId:

  Name of the shiny input. If `NULL`, the id of the output followed by
  `_item_click` (some widgets, like DiagrammeR and billboarder charts,
  set their own input `<outputId>_click`).

- selector:

  CSS selector of the clickable elements, for widgets which are not
  supported directly.

- ids:

  Optional named vector: ids reported instead of the ids found in the
  widget (names), for example the keys of graph nodes whose diagram uses
  internal node numbers. Charts made by rvis (like grviz graphs) keep
  these ids themselves.

## Value

The widget, with the click javascript added.

## Details

In shiny, the input is `input$<outputId>_item_click` (or
`input[[inputId]]` if `inputId` is given). Its value is a list with
field `type` (`"node"`, `"link"`, `"point"` or `"element"`) and, as
available, `id`, `label`, `series`, `index`, `value`, `x`, `y`, `source`
and `target` (links), `group` and `data`. It is set again on every
click, also when the same item is clicked twice.

Supported widgets: Graphviz diagrams
([`DiagrammeR::grViz()`](https://rich-iannone.github.io/DiagrammeR/reference/grViz.html)
and the `grviz` charts of rvis), networkD3 networks, echarts4r,
apexcharter, plotly, highcharter, rAmCharts, c3, billboarder,
visNetwork, dygraphs and leaflet charts, DT tables (clicks on cells:
type `"cell"`, with `row`, `column` and `value`) and rvis coffeewheel
sunbursts (type `"node"`, with `depth` and `parent`). Leaflet markers
and shapes (type `"marker"` or `"shape"`) report their `layerId` as
`id`, their `group` and the clicked `lat` and `lng`. For any other
widget, give a CSS `selector` of its clickable elements: the input then
gets the element's id (its `data-id` attribute, svg title or id), its
text and its d3 data, if any.

Outside shiny, each click also fires the javascript event `rvis:click`
on the widget element.

In a dashboard, give an output item the field `click.events: true` to
add this to the chart returned by its service.

## Examples

``` r
if (FALSE) { # \dontrun{
library(shiny)
ui = fluidPage(DiagrammeR::grVizOutput("g"), verbatimTextOutput("clicked"))
server = function(input, output){
  output$g = DiagrammeR::renderGrViz(
    rvisClickEvents(DiagrammeR::grViz("digraph {a -> b; b -> c}")))
  output$clicked = renderPrint(str(input$g_item_click))
}
shinyApp(ui, server)
} # }
```
