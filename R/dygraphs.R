
# dygraphs.R ----------------------------------------------------------------

# Header
# Filename:       dygraphs.R
# Description:    Contains functions for plotting various time series charts from dygraphs package using standrad inputs.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     29 December 2016
# Last Revision:  19 July 2017
# Version:        1.2.4
#

# Version History:

# Version   Date                Action
# ----------------------------------
# 1.0.0     29 December 2016    Initial issue transferred from niraPlotter.R
# 1.0.1     02 February 2017    Function dygraphs.tsline.timeBreak.plot() modified: inputs a data.frame() instead of a TIME.SERIES object.
# 1.1.0     02 February 2017    Function dygraphs.tsline.timeBreak.plot() removed and function dygraphs.line.plot() added
# 1.1.1     09 March 2017       Function dygraphs.line.plot() renamed to dygraphs.line()
# 1.2.0     28 March 2017       Fundamental changes: Calling prepare4Table() and using standard inputs
# 1.2.1     28 March 2017       dygraphs.defset added.
# 1.2.2     31 March 2017       Calls prepareAesthetics() for preparation of aesthetics before creating the table
# 1.2.3     19 July 2018        shape and color removed from A$aesthetics list
# 1.2.4     19 July 2018        subchart feature added configured by a set of properties in config
# 1.2.4     28 September 2018   Function() asISO8601Time() modified: Forces time to GMT

#' @include jscripts.R

dygraphs.combo.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    x = c("POSIXct", "Date", "character", "factor", "numeric", "integer"),
    y = 'numeric',
    y2 = 'numeric'),
  multiples  = c('y', 'y2'),
  essentials = c('x', 'y or y2')
)

dygraphs.tsline.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    x = c("POSIXct", "Date"),
    y = 'numeric',
    y2 = 'numeric'),
  multiples  = c('y', 'y2'),
  essentials = c('x', 'y or y2')
)

asISO8601Time <- function(x) {
  x %<>% lubridate::force_tz('GMT')
  if (!inherits(x, "POSIXct"))
    x <- as.POSIXct(x, tz = "GMT")
  format(x, format = "%04Y-%m-%dT%H:%M:%OS3Z", tz = 'GMT')
}

dychart = function (data, main = NULL, xlab = NULL, ylab = NULL, periodicity = NULL,
                    group = NULL, elementId = NULL, width = NULL, height = NULL)
{
  if (xts::xtsible(data)) {
    if (!xts::is.xts(data))
      data <- xts::as.xts(data)
    format <- "date"
  }
  else if (is.list(data) && is.numeric(data[[1]])) {
    if (is.null(names(data)))
      stop("For numeric values, 'data' must be a named list or data frame")
    format <- "numeric"
  }
  else {
    stop("Unsupported type passed to argument 'data'.")
  }
  if (format == "date") {
    if (is.null(periodicity)) {
      if (nrow(data) < 2) {
        periodicity <- defaultPeriodicity(data)
      }
      else {
        periodicity <- xts::periodicity(data)
      }
    }
    time <- time(data)
    data <- zoo::coredata(data)
    data <- unclass(as.data.frame(data))
    timeColumn <- list()
    timeColumn[[periodicity$label]] <- asISO8601Time(time)
    data <- append(timeColumn, data)
  }
  else {
    data <- as.list(data)
  }
  attrs <- list()
  attrs$title <- main
  attrs$xlabel <- xlab
  attrs$ylabel <- ylab
  attrs$labels <- names(data)
  attrs$legend <- "auto"
  attrs$retainDateWindow <- FALSE
  attrs$axes$x <- list()
  attrs$axes$x$pixelsPerLabel <- 60
  x <- list()
  x$attrs <- attrs
  x$scale <- if (format == "date")
    periodicity$scale
  else NULL
  x$group <- group
  x$annotations <- list()
  x$shadings <- list()
  x$events <- list()
  x$format <- format
  attr(x, "time") <- if (format == "date")
    time
  else NULL
  attr(x, "data") <- data
  attr(x, "autoSeries") <- 2
  names(data) <- NULL
  x$data <- data
  htmlwidgets::createWidget(name = "dygraphs", x = x, width = width,
                            height = height, htmlwidgets::sizingPolicy(viewer.padding = 10,
                                                                       browser.fill = TRUE), elementId = elementId)
}

dygraphs.applyConfig = function(d, config){
  if(config$subchart.enabled){
    d %<>% dyRangeSelector(dateWindow = c(config$xAxis.min, config$xAxis.max), height = config$subchart.height,
                           fillColor = '#' %++% config$subchart.background, strokeColor = '#' %++% config$subchart.color,
                           keepMouseZoom = config$subchart.zoom.enabled,
                           retainDateWindow =  FALSE)
  }
  return(d)
}

dygraphs.combo = function(obj, x = NULL, y = NULL, y2 = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(dygraphs), "Package dygraphs is not installed!", err_src = match.call()[[1]])
  config = dygraphs.combo.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
    verifyConfig(plotter = 'dygraphs') %>% verifyConfigDimProperties(dims = 'color')

  scatterMode = T

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, y2 = y2)
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config = config)

  if (!inherits(obj[, L$x], c('numeric', 'integer'))){
    dates = try(as.Date(obj[, L$x]), silent = T)
    if(inherits(dates,'Date')){
      obj %<>% distinct(.keep_all = T)
      rownames(obj) <- obj[, L$x]
    } else {
      getLabel <- 'function(d){
      var labels = [' %++% paste(paste0('"', obj[, L$x], '"'), collapse = ',') %++% '];
      if (Math.round(d) == d){return labels[d - 1]} else {return ""}
    }'
      obj[, L$x] <- sequence(nrow(obj))
      scatterMode = F
  }
}

  d = dychart(obj, main = config$title, ...)

  dygraphs.strokePattern = c(line.dot = 'dotted', line.dash = 'dashed', line.dash.dot = 'dotdash')

  for(i in L$y){
    d %<>% dySeries(i,
                    label         = i,
                    color         = config$color[[i]],
                    axis          = 'y',
                    stepPlot      = chif(is.null(config$shape[[i]]), NULL, config$shape[[i]] == 'line.step'),
                    stemPlot      = chif(is.null(config$shape[[i]]), NULL, config$shape[[i]] == 'bar.point'),
                    fillGraph     = chif(is.null(config$shape[[i]]), NULL, config$shape[[i]] == gndcd(2,1,162,170)),
                    drawPoints    = chif(is.null(config$shape[[i]]), NULL, config$shape[[i]] == 'line.point'),
                    pointSize     = config$size[[i]],
                    strokePattern = chif(config$shape[[i]] %in% names(dygraphs.strokePattern), dygraphs.strokePattern[config$shape[[i]]], NULL),
                    plotter       = chif(config$shape[[i]] == 'bar', dyPlotter[['bar']], NULL)
    )
  }
  # package has a bug! colors are not assigned to series names correctly!!
  for(i in L$y2){
    d %<>% dySeries(i,
                    label         = i,
                    color         = config$color[[i]],
                    axis          = 'y2',
                    stepPlot      = chif(is.null(config$shape[[i]]), NULL, config$shape[[i]] == 'line.step'),
                    stemPlot      = chif(is.null(config$shape[[i]]), NULL, config$shape[[i]] == 'bar.point'),
                    fillGraph     = chif(is.null(config$shape[[i]]), NULL, config$shape[[i]] == gndcd(2,1,162,170)),
                    drawPoints    = chif(is.null(config$shape[[i]]), NULL, config$shape[[i]] == 'line.point'),
                    pointSize     = config$size[[i]],
                    strokePattern = chif(config$shape[[i]] %in% names(dygraphs.strokePattern), dygraphs.strokePattern[config$shape[[i]]], NULL),
                    plotter       = chif(config$shape[[i]] == 'bar', dyPlotter[['bar']], NULL)
    )
  }
  if(!scatterMode){d %<>% dyAxis("x", valueFormatter=JS(getLabel), axisLabelFormatter = JS(getLabel))}
  return(d %>% dygraphs.applyConfig(config))
  }

dygraphs.tsline = function(obj, x = NULL, y = NULL, y2 = NULL, config = NULL, ...){
  config = dygraphs.tsline.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  config$shape = NULL
  dygraphs.combo(obj, x = x, y = y, y2 = y2, config = config, ...)
}

# OLD FUNCTIONS:

# dygraphs.line = function(obj, x = NULL, y = NULL, rownamesAsX = T, ...){
#   y = verify(y, 'character', domain = numerics(obj), default = numerics(obj), varname = 'y')
#   if (rownamesAsX){
#     lbl = rownames(obj)
#     obj = cbind(seq(lbl), obj[, y, drop = F])
#     names(obj)[1] <- x
#   } else {
#     lbl = obj[, x]
#     obj = obj[, y]
#     obj[, x] <- seq(lbl)
#   }
#
#   getLabel <- 'function(d){
#   var labels = [' %++% paste(paste0('"', lbl, '"'), collapse = ',') %++% '];
#   return labels[d - 1];
# }'
#
#   dygraph(obj, ...) %>% dyAxis("x", valueFormatter=JS(getLabel), axisLabelFormatter = JS(getLabel))
#   }
#
# dygraphs.tsline.settings = list(
#   title = NULL, xlab = NULL, ylab = NULL, width = NULL, height = NULL,
#   # legend = list(show  = "auto", width = 250, showZeroValues = TRUE, labelsDiv = NULL, labelsSeparateLines = FALSE, hideOnMouseOut = TRUE),
#   RangeSelector = list(dateWindow = NULL, height = 40, fillColor = " #A7B1C4", strokeColor = "#808FAB", keepMouseZoom = TRUE, retainDateWindow = FALSE),
#   Roller = list(showRoller = TRUE, rollPeriod = 1)
# )
#
# #' This function gets a dygraph plot and dygraph configuration as input and
# #' Returns the modified plot affected by given configuration
# dygraphs.tsline.apply.settings = function(plt, config){
#   if (!is.null(config$legend)){
#     plt = plt %>% dyLegend(show = config$legend$show, width = config$legend$width, showZeroValues = config$legend$showZeroValues, labelsDiv = config$legend$labelsDiv, labelsSeparateLines = config$legend$labelsSeparateLines, hideOnMouseOut = config$legend$hideOnMouseOut)
#   }
#   if (!is.null(config$RangeSelector)){
#     plt = plt %>% dyRangeSelector(dateWindow = config$RangeSelector$dateWindow, height = config$RangeSelector$height, fillColor = config$RangeSelector$fillColor, strokeColor = config$RangeSelector$strokeColor, keepMouseZoom = config$RangeSelector$keepMouseZoom, retainDateWindow = config$RangeSelector$retainDateWindow)
#   }
#   if (!is.null(config$Roller)){
#     plt = plt %>% dyRoller(showRoller = config$Roller$showRoller, rollPeriod = config$Roller$rollPeriod)
#   }
#
#   plt = plt %>% dyOptions(
#     stackedGraph = verify(config$stackedGraph, 'logical', default = F, varname = 'config$stackedGraph'),
#     fillGraph    = verify(config$fillGraph   , 'logical', default = F, varname = 'config$fillGraph'),
#     colors       = verify(config$colors, 'character', varname = 'config$colors')
#     # todo: write all the settings tuned by function dyOptions()
#   )
#   return(plt)
# }
#
# # Customized modifications for dygraphs plots:
# dygraph.Highlight.1 = function(x){
#   verify(x, 'dygraphs')
#   dyHighlight(x, highlightCircleSize = 5,
#               highlightSeriesBackgroundAlpha = 0.2,
#               hideOnMouseOut = TRUE)
# }
#



##########################################################################
# Java plotters:

dyPlotter = list(bar = dygraphs.shape.bar.js, multiBar = dygraphs.shape.multibar.js, candle = dygraphs.shape.candle.js)

