# plotly.R ----------------------------------------------------------------


# Header
# Filename:       plotly.R
# Description:    Contains functions for plotting various charts from package 'plotly' using standrad inputs.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     29 December 2016
# Last Revision:  27 June 2019
# Version:        1.3.7
#

# Version History:

# Version   Date               Action
# -----------------------------------
# 1.0.0     29 December 2016   Initial issue
# 1.0.1     28 February 2017   Function plotly.barChart() renamed to plotly.bar() and modified: Calls verifyPlotInputs() to verify inputs
# 1.1.0     03 March 2017      Function plotly.pie() added
# 1.1.1     09 March 2017      Function plotly.multi() modified: With new version of plotly, add_trace is replaced by various add_x functions like add_lines, add_markers, ...
# 1.1.2     24 March 2017      Function plotly.multi() modified: Argument color added. Specifies color of each trace
# 1.1.3     24 March 2017      Function plotly.multi() renamed to plotly.combo()
# 1.1.5     31 March 2017      Function plotly.combo() modified. Calls prepareAesthetics() from visgen.R
# 1.1.6     31 March 2017      plotly.combo.defset added
# 1.2.0     04 April 2017      plotly.scatter.defset added, Function plotly.scatter() added
# 1.2.3     13 April 2017      plotly.combo updated. Determines if the plot is horizontal
# 1.2.4     25 May 2017        Function plotly.applyConfig() modified: Some legend properties added to config
# 1.2.5     25 May 2017        require() is called with arguments quietly = TRUE & warn.conflicts = FALSE
# 1.2.6     14 February 2018   Function plotly.addSeries() modified: arguments 'symbol' and 'symbols' are set appropriately based on the values of 'shape' argument
# 1.2.7     21 May 2018        Function plotly.applyConfig() modified: config properties tickangle and layout margins applied.
# 1.2.8     29 May 2018        Function plotly.applyConfig() and plotly.prepareConfig() modified: config properties showtickLabel, zeroline and showgrid applied.
# 1.2.9     29 May 2018        Function plotly.pie() added
# 1.3.0     25 June 2018       Function prepareConfig() removed: All config verifications transferred to visgen
# 1.3.1     25 June 2018       plotly.box.molten added
# 1.3.2     30 June 2018       plotly.combo() modified: accepts class Date and POSIXct for x and y
# 1.3.3     30 June 2018       plotly.tsline() and plotly.tsbar() added.
# 1.3.4     02 July 2018       plotly.box.molten() renamed to plotly.box().
# 1.3.5     16 October 2018    plotly.box() modified: factors converted to character for computing nchar in order to adjust margin
# 1.3.6     18 October 2018    plotly.combo() modified: tooltip added
# 1.3.7     27 June 2019       plotly.pie() and plotly.box() modified: additional arguments passed to original function.



# To generate shiny input for event handling, read this:
# https://plot.ly/r/shiny-coupled-events/

valid.plotly.shapes = c('bar', 'line', 'point', 'line.point')


# symbols': '0', 'circle', 100', 'circle-open', '200', 'circle-dot', '300', 'circle-open-dot', '1', 'square', '101', 'square-open', '201', 'square-dot', '301', 'square-open-dot', '2', 'diamond', '102', 'diamond-open', '202', 'diamond-dot', '302', 'diamond-open-dot', '3', 'cross', '103', 'cross-open', '203', 'cross-dot', '303', 'cross-open-dot', '4', 'x', '104', 'x-open', '204', 'x-dot', '304', 'x-open-dot', '5', 'triangle-up', '105', 'triangle-up-open', '205', 'triangle-up-dot', '305', 'triangle-up-open-dot', '6', 'triangle-down', '106', 'triangle-down-open', '206', 'triangle-down-dot', '306', 'triangle-down-open-dot', '7', 'triangle-left', '107', 'triangle-left-open', '207', 'triangle-left-dot', '307', 'triangle-left-open-dot', '8', 'triangle-right', '108', 'triangle-right-open', '208', 'triangle-right-dot', '308', 'triangle-right-open-dot', '9', 'triangle-ne', '109', 'triangle-ne-open', '209', 'triangle-ne-dot', '309'),

plotly.symbol = c(bubble = 20, point = 20, circle = 20, square.hollow = 0, o = 1, circle.hollow = 1, bubble.hollow = 1, triangle.hollow = 2, plus = 3, x = 4, multiply = 4, rhombus.hollow = 5, curl.hollow = 6, square = 7, star = 8, rhombus.x = 9)
# plotly.symbol = c(bubble = 'circle', point = 'circle', circle = 'circle', o = 'o', circle.open = 'o')
plotly.mode  = c(line = 'lines', point = 'markers')
plotly.mode[names(plotly.symbol)] = 'markers'
plotly.mode['line.' %++% names(plotly.symbol)] = 'lines+markers'
plotly.symbol['line.' %++% names(plotly.symbol)] = plotly.symbol
valid.plotly.shapes = names(plotly.mode)

plotly.combo.defset = defset %<==>% list(
  # Valid classes for all dimensions
  dimclass   = list(
    x = c("character", "factor", "numeric", "integer", 'Date', 'POSIXct'),
    y = c("character", "factor", "numeric", "integer", 'Date', 'POSIXct'),
    color    = valid.classes,
    tooltip  = 'character',
    shape    = 'character'),
  multiples  = c('x', 'y', 'color', 'shape'),
  essentials = c('x', 'y')
)

plotly.box.defset = defset %<==>% list(
  dimclass   = list(
    x      = c("character", "factor", "numeric", "integer"),
    y      = c("character", "factor", "numeric", "integer"),
    group  = c('character', 'factor', 'integer', 'ordered')),
  multiples  = c(),
  essentials = c('x', 'y')
)

plotly.tsline.defset = defset %<==>% list(
  # Valid classes for all dimensions
  dimclass   = list(
    x = c("Date", "POSIXct", "numeric", "integer"),
    y = c("numeric", "integer", "Date", "POSIXct")),
  multiples  = c(),
  essentials = c('x', 'y')
)


plotly.scatter.defset = defset %<==>% list(
  # Valid classes for all dimensions
  dimclass   = list(
    # x = c('numeric', 'character'),
    x = valid.classes,
    y = c('numeric', 'character'),
    z = 'numeric',
    size = 'numeric',
    label = 'character',
    tooltip = 'character',
    color    = valid.classes,
    shape    = c( 'factor', 'character', 'integer')),
  multiples  = c('y', 'color', 'shape', 'size'),
  essentials = c('x', 'y'),
  colorize   = F,
  minSize    = 10,
  maxSize    = 1000,
  palette.shape = c('circle', 'square.hollow', 'triangle.hollow', 'plus', 'x', 'rhombus.hollow') # todo: should include all shapes and extended to domain size of the categorical field
)

plotly.pie.defset = defset %<==>% list(
  # Valid classes for all dimensions
  dimclass   = list(
    # x = c('numeric', 'character'),
    label = c('character', 'factor'),
    theta = c('numeric', 'integer')),
  essentials = c('label', 'theta'),
  xAxis.enabled = F,
  yAxis.enabled = F,
  xAxis.grid.enabled = F,
  yAxis.grid.enabled = F,
  xAxis.tick.label.enabled = F,
  yAxis.tick.label.enabled = F
)

plotly.addSeries = function(p, z = NULL, size = NULL, shape = NULL, name = NULL, label = NULL, tooltip = NULL, color = NULL, config = NULL, ...){
  if (is.empty(shape)){shape = 'line'}
  if (inherits(shape,c('integer', 'factor'))){
    mode   = gndcd(43, 39,31,53,130,121,144)
    symbol = shape
  } else {
    shape %<>% as.character
    mode   = chif(shape[1] == gndcd(142,39,110), NULL, chif(is.na(plotly.mode[shape[1]]),'markers', plotly.mode[shape[1]]))
    symbol = plotly.symbol[shape]
    symbol[is.na(symbol)] <- 20
    names(symbol) <- NULL
  }
  if (!is.empty(label) & !is.null(mode)){mode %<>% paste0('+text')}
  type = chif(shape[1] == 'bar', 'bar', chif(is.empty(z), 'scatter', 'scatter3d'))
  flg  = type == 'bar'
  if (!flg){
    flg = flg | mode == 'lines' | mode == 'lines+text'
  }
  if (flg){
    marker = NULL
    symbol = NULL
  } else {
    marker = config %>% plotly.makeMarker(name)
  }
  shapeknown = sum(!shape %in% names(plotly.symbol)) == 0
  p %>% add_trace(
    z       = chif(is.empty(z), NULL, z),
    size    = chif(is.empty(size), NULL, size),
    sizes   = chif(is.null(config$point.size.min) | is.null(config$point.size.max), NULL, c(config$point.size.min, config$point.size.max)),
    mode    = mode,
    symbol  = chif(shapeknown, symbol, NULL),
    symbols = chif(shapeknown, unique(plotly.symbol[shape]), plotly.symbol[config$palette.shape]),
    type    = type,
    name    = name,
    text    = chif(is.empty(label) | is.null(mode), chif(is.empty(tooltip), NULL, tooltip), label),
    color   = chif(is.empty(color), config$point.color, chif(inherits(color, c('factor', 'numeric', 'integer')), color, color %>% I)),
    colors  = chif(is.empty(color),NULL, chif(inherits(color, c('factor', 'numeric', 'integer')), config$palette.color, NULL)),
    hoveron = chif(type == 'scatter', 'fills+points', NULL),
    marker  = marker, hoverinfo = chif(is.empty(tooltip), NULL, 'text'), ...)
}


plotly.makeMarker = function(config, name = NULL){
  mkr = list(line = list())
  mkr$size       = config$point.size
  mkr$color      = config$point.color
  mkr$line$color = config$point.border.color
  mkr$line$width = config$point.border.width
  return(mkr)
}

# plotly.prepareConfig = function(config){
#   config$xAxis.enabled %<>% verify('logical', domain = c(T,F), default = T)
#   config$yAxis.enabled %<>% verify('logical', domain = c(T,F), default = T)
#   config$xAxis.label %<>% verify('character', lengths = 1, varname = "config$xAxis.label")
#   config$yAxis.label %<>% verify('character', lengths = 1, varname = "config$yAxis.label")
#   config$barMode %<>% verify('character', domain = c('group', 'stack', 'relative'), lengths = 1, default = 'group', varname = "config$barMode")
#   config$xAxis.grid.enabled %<>% verify('logical', domain = c(T,F), default = config$xAxis.enabled)
#   config$yAxis.grid.enabled %<>% verify('logical', domain = c(T,F), default = config$yAxis.enabled)
#   config$xAxis.tick.label.enabled %<>% verify('logical', domain = c(T,F), default = config$xAxis.enabled)
#   config$yAxis.tick.label.enabled %<>% verify('logical', domain = c(T,F), default = config$yAxis.enabled)
#
#   return(config)
# }

plotly.applyConfig = function(p, config){
  plotly::layout(p, title = config$title,
                 xaxis  = list(title = config$xAxis.label, tickangle = config$xAxis.tick.angle, showgrid = config$xAxis.grid.enabled, zeroline = config$xAxis.enabled, showticklabels = config$xAxis.tick.label.enabled),
                 yaxis  = list(title = config$yAxis.label, tickangle = config$yAxis.tick.angle, showgrid = config$yAxis.grid.enabled, zeroline = config$yAxis.enabled, showticklabels = config$yAxis.tick.label.enabled),
                 margin = list(b = config$xAxis.margin.bottom, r = config$yAxis.margin.right, l = config$yAxis.margin.left),
                 showlegend = config$legend.enabled,
                 legend = list(
                   font = list(
                     family = config$legend.font,
                     color  = config$legend.color,
                     size   = config$legend.size
                   ) %>% list.clean,
                   bgcolor = config$legend.background,
                   bordercolor = config$legend.border.color,
                   borderwidth = config$legend.border.width,
                   orientation = config$legend.orientation
                 ),
                 barmode = config$barMode)
}

plotly.scatter = function(obj, x, y, z = NULL, size = NULL, shape = NULL, color = NULL, label = NULL, tooltip = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  if(is.null(shape)){shape = 'point'}
  assert(require(plotly, quietly = T, warn.conflicts = F), "Package plotly is not installed!", err_src = match.call()[[1]])
  (plotly.scatter.defset %<==>% verify(config, 'list', default = list())) %>% verifyConfig(plotter = 'plotly') -> config

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, z = z, size = size, shape = shape, color = color, label = label, tooltip = tooltip, extend = c('y', 'shape', 'color'))
  L = a$labels
  A = a$aesthetics

  # Preparing Table:
  obj %<>% prepare4Plot(A, config = config)

  if(!is.null(L$color)){for (cl in L$color){obj[, cl] %<>% as.factor}}
  p <- plot_ly(x = obj[,L$x], ...)
  for (i in seq(L$y)){
    p %<>% plotly.addSeries(y = obj[,L$y[i]], z = obj[, L$z[i]], size = obj[ ,L$size[i]], shape = obj[, L$shape[i]], tooltip = obj[, L$tooltip[i]], name = L$y[i], label = obj[,L$label], color = obj[,L$color[i]], config = config)
  }

  if(is.null(config$xAxis.label)){config$xAxis.label = L$x}
  if(is.null(config$yAxis.label)){config$yAxis.label = L$y}
  # config %<>% plotly.prepareConfig

  p %>% plotly.applyConfig(config)
}

plotly.combo = function(obj, x = NULL, y = NULL, shape = NULL, color = NULL, config = NULL, tooltip = NULL, ...){
  # Verifications
  assert(require(plotly, quietly = T, warn.conflicts = F), "Package plotly is not installed!", err_src = match.call()[[1]])
  (plotly.combo.defset %<==>% verify(config, 'list', default = list())) %>% verifyConfig(plotter = 'plotly') -> config

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, color = color, shape = shape, tooltip = tooltip, extend = c('y', 'x', 'shape','color', 'tooltip'))
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config)

  hor = T
  for (i in L$x){hor = hor & inherits(obj[,i], c('numeric', 'integer'))}
  for (i in L$y){hor = hor & inherits(obj[,i], c('character', 'factor', 'Date', 'POSIXct'))}

  if (hor){
    if(inherits(obj[,L$y[1]], 'character')){obj[,L$y[1]] <- factor(obj[,L$y[1]], levels = obj[,L$y[1]])}

    g <- plot_ly(y = obj[,L$y[1]], orientation = 'h', ...)

    for (i in seq(L$x)){
      g %<>% plotly.addSeries(x = obj[, L$x[i]], shape = L$shape[i], name = L$x[i], color = obj[, L$color[i]], tooltip = obj[, L$tooltip[i]], config = config)
    }
  } else {
    if(inherits(obj[,L$x[1]], 'character')){obj[,L$x[1]] <- factor(obj[,L$x[1]], levels = obj[,L$x[1]])}
    g <- plot_ly(x = obj[,L$x[1]], ...)

    for (i in seq(L$y)){
      g %<>% plotly.addSeries(y = obj[, L$y[i]], shape = L$shape[i], name = L$y[i], color = obj[, L$color[i]], tooltip = obj[, L$tooltip[i]], config = config)
    }
  }

  g %>% plotly.applyConfig(config)
}

plotly.bar = function(..., config = NULL){
  plotly.combo(..., shape = 'bar', config = config)
}

plotly.pie = function(obj, label = NULL, theta = NULL, config = NULL, ...){
  assert(require(plotly, quietly = T, warn.conflicts = F), "Package plotly is not installed!", err_src = match.call()[[1]])
  config = plotly.pie.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>% verifyConfig(plotter = 'plotly')

  # Preparing Aesthetics:
  a = prepareAesthetics(label = label, theta = theta)
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config)

  plot_ly(labels = obj[, L$label], values = obj[, L$theta], type = 'pie', ...) %>% plotly.applyConfig(config)

}

plotly.box = function(obj, x = NULL, y = NULL, group = NULL, config = NULL, ...){
  # Verifications
  assert(require(plotly, quietly = T, warn.conflicts = F), "Package plotly is not installed!", err_src = match.call()[[1]])
  (plotly.box.defset %<==>% verify(config, 'list', default = list())) %>% verifyConfig(plotter = 'plotly') -> config

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, group = group)
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config)

  hor = T
  for (i in L$x){hor = hor & inherits(obj[,i], c('numeric', 'integer'))}
  for (i in L$y){hor = hor & inherits(obj[,i], c('character', 'factor', 'Date', 'POSIXct'))}

  if(!hor & is.null(config$xAxis.margin.bottom)){config$xAxis.margin.bottom = 5*(obj[, L$x] %>% as.character %>% nchar %>% max)}
  if( hor & is.null(config$yAxis.margin.left))  {config$yAxis.margin.left   = 10*(obj[, L$y] %>% as.character %>% nchar %>% max)}

  if(is.null(L$x)){xfrml = NULL} else {xfrml = as.formula('~`' %++% L$x %++% '`')}
  if(is.null(L$y)){yfrml = NULL} else {yfrml = as.formula('~`' %++% L$y %++% '`')}
  if(is.null(L$group)){clrfrml = NULL} else {clrfrml = as.formula('~`' %++% L$group %++% '`')}

  p  = plot_ly(obj, x = xfrml, y = yfrml, color = clrfrml, type = "box", ...) %>% plotly.applyConfig(config)
  if(!is.null(L$group)){p %<>% plotly::layout(boxmode = "group")}
  p
}


plotly.tsline = function(obj, x = NULL, y = NULL, config = NULL, ...){
  # Verifications
  assert(require(plotly, quietly = T, warn.conflicts = F), "Package plotly is not installed!", err_src = match.call()[[1]])
  (plotly.tsline.defset %<==>% verify(config, 'list', default = list())) -> config

  obj %>% plotly.combo(x = x, y = y, shape = 'line', config = config, ...)
}

plotly.tsbar = function(obj, x = NULL, y = NULL, config = NULL, ...){
  # Verifications
  assert(require(plotly, quietly = T, warn.conflicts = F), "Package plotly is not installed!", err_src = match.call()[[1]])
  (plotly.tsline.defset %<==>% verify(config, 'list', default = list())) -> config

  obj %>% plotly.combo(x = x, y = y, shape = 'bar', config = config, ...)
}
