# clickevents.R ----------------------------------------------------------------

# Header
# Filename:       clickevents.R
# Description:    Sends clicks on the items of htmlwidgets (nodes, links, points, ...) to shiny inputs, by javascript
#                 added to the widget with htmlwidgets::onRender(). The javascript of the widget packages is not copied
#                 or changed, so new versions of these packages keep working.
# Author:         Nima Ramezani
# Email :         nima.ramezani@gmail.com
# Start Date:     24 November 2026
# Last Revision:  25 November 2026
# Version:        0.2.0

# Version History:

# Version   Date                Action
# ----------------------------------
# 0.1.0     24 November 2026    Initial issue: rvisClickEvents() for Graphviz (DiagrammeR), networkD3, echarts4r,
#                               apexcharter, plotly and highcharter widgets, and any widget with a CSS selector
# 0.2.0     25 November 2026    Handlers for rAmCharts, c3, billboarder, visNetwork, DT (cells), dygraphs, leaflet (markers and
#                               shapes) and rvis coffeewheel. rAmCharts charts (S4 objects) are converted to htmlwidgets.
#                               Clicks on shapes under a transparent event layer are found (selector mode too). The click
#                               javascript never stops a widget from being shown: failures are logged as console warnings.

#' @include visgen.R

# Widget classes (the first class of the htmlwidget object) and the javascript handler reading their clicks
click.handlers = c(grViz = 'graphviz', grviz = 'graphviz',
                   sankeyNetwork = 'd3data', forceNetwork = 'd3data', simpleNetwork = 'd3data',
                   diagonalNetwork = 'd3data', radialNetwork = 'd3data', dendroNetwork = 'd3data',
                   echarts4r = 'echarts', apexcharter = 'apex', plotly = 'plotly',
                   highchart = 'highcharts', highchart2 = 'highcharts', highchartzero = 'highcharts',
                   ramcharts_base = 'amcharts', c3 = 'c3', billboarder = 'c3', visNetwork = 'visnetwork',
                   datatables = 'datatable', dygraphs = 'dygraphs', leaflet = 'leaflet', coffeewheel = 'hierarchy')

# Javascript function passed to htmlwidgets::onRender() (read once from inst/js/click-events.js)
clickEventsJS = function(){
  if(is.null(.rvis.cache$click.js)){
    path = system.file('js', 'click-events.js', package = 'rvis')
    assert(nchar(path) > 0, "File js/click-events.js of package rvis not found!", err_src = 'rvisClickEvents')
    .rvis.cache$click.js <- paste(readLines(path, warn = FALSE), collapse = '\n')
  }
  .rvis.cache$click.js
}

#' Send clicks on a chart to shiny
#'
#' Adds javascript to an htmlwidget so that a click on one of its items (a node or link of a graph,
#' a point, bar or slice of a chart) sets a shiny input. The javascript is added with
#' \code{htmlwidgets::onRender()} and runs after the widget is drawn: the javascript of the plotting
#' package is not copied or changed, so later versions of the package keep working.
#'
#' In shiny, the input is \code{input$<outputId>_item_click} (or \code{input[[inputId]]} if \code{inputId}
#' is given). Its value is a list with field \code{type} (\code{"node"}, \code{"link"}, \code{"point"}
#' or \code{"element"}) and, as available, \code{id}, \code{label}, \code{series}, \code{index},
#' \code{value}, \code{x}, \code{y}, \code{source} and \code{target} (links), \code{group} and
#' \code{data}. It is set again on every click, also when the same item is clicked twice.
#'
#' Supported widgets: Graphviz diagrams (\code{DiagrammeR::grViz()} and the \code{grviz} charts of rvis),
#' networkD3 networks, echarts4r, apexcharter, plotly, highcharter, rAmCharts, c3, billboarder, visNetwork,
#' dygraphs and leaflet charts, DT tables (clicks on cells: type \code{"cell"}, with \code{row},
#' \code{column} and \code{value}) and rvis coffeewheel sunbursts (type \code{"node"}, with \code{depth}
#' and \code{parent}). Leaflet markers and shapes (type \code{"marker"} or \code{"shape"}) report their
#' \code{layerId} as \code{id}, their \code{group} and the clicked \code{lat} and \code{lng}.
#' For any other widget,
#' give a CSS \code{selector} of its clickable elements: the input then gets the element's id (its
#' \code{data-id} attribute, svg title or id), its text and its d3 data, if any.
#'
#' Outside shiny, each click also fires the javascript event \code{rvis:click} on the widget element.
#'
#' In a dashboard, give an output item the field \code{click.events: true} to add this to the chart
#' returned by its service.
#'
#' @param widget An htmlwidget, for example a chart returned by \code{\link{rvisPlot}()} (rAmCharts charts,
#'   which are not htmlwidgets until they are plotted, are converted).
#' @param inputId Name of the shiny input. If \code{NULL}, the id of the output followed by \code{_item_click}
#'   (some widgets, like DiagrammeR and billboarder charts, set their own input \code{<outputId>_click}).
#' @param selector CSS selector of the clickable elements, for widgets which are not supported directly.
#' @param ids Optional named vector: ids reported instead of the ids found in the widget (names), for example
#'   the keys of graph nodes whose diagram uses internal node numbers. Charts made by rvis (like grviz graphs)
#'   keep these ids themselves.
#' @return The widget, with the click javascript added.
#' @examples
#' \dontrun{
#' library(shiny)
#' ui = fluidPage(DiagrammeR::grVizOutput("g"), verbatimTextOutput("clicked"))
#' server = function(input, output){
#'   output$g = DiagrammeR::renderGrViz(
#'     rvisClickEvents(DiagrammeR::grViz("digraph {a -> b; b -> c}")))
#'   output$clicked = renderPrint(str(input$g_item_click))
#' }
#' shinyApp(ui, server)
#' }
#' @export
rvisClickEvents = function(widget, inputId = NULL, selector = NULL, ids = NULL){
  # rAmCharts charts are S4 objects until they are plotted: their htmlwidget is made here
  if(methods::is(widget, 'AmChart')){widget = rAmCharts::plot(widget)}
  verify(widget, 'htmlwidget', varname = 'widget', null_allowed = F, err_src = 'rvisClickEvents')
  verify(inputId, 'character', lengths = 1, varname = 'inputId', err_src = 'rvisClickEvents')
  verify(selector, 'character', lengths = 1, varname = 'selector', err_src = 'rvisClickEvents')
  if(is.null(selector)){
    handler = click.handlers[class(widget)[1]]
    if(is.na(handler)){
      stop("Clicks on widgets of class '", class(widget)[1], "' are not supported directly. ",
           "Give argument 'selector': a CSS selector of the elements to click.", call. = FALSE)
    }
  } else {handler = 'selector'}
  # ids kept on the widget by the function which made it (like the node keys of rvis grviz graphs):
  if(is.null(ids)){ids = attr(widget, 'rvis.click.ids')}
  if(!is.null(ids)){ids = as.list(stats::setNames(as.character(ids), names(ids)))}
  htmlwidgets::onRender(widget, clickEventsJS(), data = list(input = inputId, handler = unname(handler), selector = selector, ids = ids))
}
