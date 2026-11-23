# pivot.R ----------------------------------------------------------------


# Header
# Filename:       pivot.R
# Description:    Contains functions for plotting interactive pivot tables (js library pivottable) using package rpivotTable.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     17 October 2017
# Last Revision:  23 November 2026
# Version:        0.1.0
#

# Version History:

# Version   Date                Action
# ----------------------------------
# 0.0.1     17 October 2017     Initial issue
# 0.1.0     23 November 2026    The embedded copy of the rpivotTable widget could not be displayed (its javascript libraries were
#                               never added to rvis). pivot(), pivotOutput() and renderPivot() now use package rpivotTable.

# Reference of pivottable parameters: https://github.com/nicolaskruchten/pivottable/wiki/Parameters

# Creates an interactive pivot table of 'data' (data.frame or table). Arguments are passed to rpivotTable::rpivotTable():
# rows, cols:     columns shown as rows and columns of the pivot table
# aggregatorName: name of the aggregator, for example 'Count', 'Sum', 'Average'
# vals:           column(s) aggregated by the aggregator
# rendererName:   name of the renderer, for example 'Table', 'Heatmap', 'Bar Chart'
# sorter, exclusions, inclusions, ...: see ?rpivotTable::rpivotTable
pivot <- function(data, rows = NULL, cols = NULL, aggregatorName = NULL, vals = NULL, rendererName = NULL,
                  sorter = NULL, exclusions = NULL, inclusions = NULL, ..., width = NULL, height = NULL) {
  assert(requireNamespace("rpivotTable", quietly = TRUE), "Package rpivotTable is not installed!")
  args <- list(data = data, rows = rows, cols = cols, aggregatorName = aggregatorName, vals = vals,
               rendererName = rendererName, sorter = sorter, exclusions = exclusions, inclusions = inclusions, ...)
  # width and height are only passed when given, so that rpivotTable defaults apply otherwise
  if (!is.null(width)){args$width <- width}
  if (!is.null(height)){args$height <- height}
  do.call(rpivotTable::rpivotTable, args)
}

# Shiny output and render functions of pivot tables
pivotOutput <- function(outputId, width = '100%', height = '500px'){
  assert(requireNamespace("rpivotTable", quietly = TRUE), "Package rpivotTable is not installed!")
  rpivotTable::rpivotTableOutput(outputId, width = width, height = height)
}

renderPivot <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, pivotOutput, env, quoted = TRUE)
}

# rvisPlot(type = 'table', plotter = 'pivot', obj, ...): pivot table of obj
pivot.table = function(obj, ...){
  pivot(obj, ...)
}
