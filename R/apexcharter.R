# apexcharter.R ----------------------------------------------------------------

# Header
# Filename:       apexcharter.R
# Description:    Contains functions for plotting various charts from package 'apexcharter' (ApexCharts.js) using standard inputs.
# Author:         Nima Ramezani
# Email :         nima.ramezani@gmail.com
# Start Date:     24 November 2026
# Last Revision:  24 November 2026
# Version:        0.1.0

# Version History:

# Version   Date                Action
# ----------------------------------
# 0.1.0     24 November 2026    Initial issue: bar, line, area, scatter, bubble, pie, tsline, tsarea, tsbar, heatmap and radar

#' @include visgen.R

apexcharter.bar.defset = defset %<==>% list(
  dimclass   = list(
    x     = c('character', 'factor', 'numeric', 'integer'),
    y     = c('numeric', 'integer', 'character', 'factor'),
    group = c('character', 'factor')),
  multiples  = c('x', 'y'),
  essentials = c('x', 'y')
)

apexcharter.line.defset = defset %<==>% list(
  dimclass   = list(
    x     = c('character', 'factor', 'numeric', 'integer'),
    y     = c('numeric', 'integer'),
    group = c('character', 'factor')),
  multiples  = 'y',
  essentials = c('x', 'y')
)

apexcharter.tsline.defset = defset %<==>% list(
  dimclass   = list(
    x     = c('Date', 'POSIXct'),
    y     = c('numeric', 'integer'),
    group = c('character', 'factor')),
  multiples  = 'y',
  essentials = c('x', 'y')
)

apexcharter.scatter.defset = defset %<==>% list(
  dimclass   = list(
    x     = c('numeric', 'integer'),
    y     = c('numeric', 'integer'),
    size  = c('numeric', 'integer'),
    group = c('character', 'factor')),
  multiples  = c(),
  essentials = c('x', 'y')
)

apexcharter.pie.defset = defset %<==>% list(
  dimclass   = list(
    label = c('character', 'factor'),
    theta = c('numeric', 'integer')),
  multiples  = c(),
  essentials = c('label', 'theta')
)

apexcharter.heatmap.defset = defset %<==>% list(
  dimclass   = list(
    x = c('character', 'factor', 'numeric', 'integer', 'Date'),
    y = c('character', 'factor'),
    z = c('numeric', 'integer')),
  multiples  = c(),
  essentials = c('x', 'y', 'z')
)

apexcharter.radar.defset = defset %<==>% list(
  dimclass   = list(
    x     = c('character', 'factor'),
    y     = c('numeric', 'integer'),
    group = c('character', 'factor')),
  multiples  = 'y',
  essentials = c('x', 'y')
)

# Stops if package apexcharter is not installed
apexcharterRequire = function(src){
  assert(requireNamespace('apexcharter', quietly = TRUE), "Package 'apexcharter' is not installed!", err_src = src)
}

# Merges config over the default settings of a chart and verifies it against the properties table
apexcharterConfig = function(defset, config){
  (defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))) %>%
    verifyConfig(plotter = 'apexcharter')
}

# Long table of the y series: one row per x value and series, with columns x, y and group.
# Series are either the y columns (group is the column name) or the values of the group column (a single y column).
apexcharterLong = function(obj, Lx, Ly, Lgroup = NULL){
  if(!is.null(Lgroup)){
    assert(length(Ly) == 1, 'You can define series by either grouping the values or selecting multiple columns!')
    return(data.frame(x = obj[[Lx]], y = obj[[Ly]], group = as.character(obj[[Lgroup]]), stringsAsFactors = FALSE))
  }
  data.frame(x = rep(obj[[Lx]], length(Ly)), y = unlist(obj[Ly], use.names = FALSE),
             group = rep(Ly, each = nrow(obj)), stringsAsFactors = FALSE)
}

# Calls apexcharter::apex() with a mapping of aesthetics (names) to columns of data (values)
apexcharterChart = function(data, mapping, type, ...){
  aesthetics = do.call(apexcharter::aes, lapply(mapping, as.name))
  apexcharter::apex(data, aesthetics, type = type, ...)
}

# Series colours: argument color (one colour per series) or config$color (a named list: series -> colour)
apexcharterColors = function(ax, series, color, config){
  clr = NULL
  if(!is.null(color)){clr = color %>% unlist %>% as.character %>% vect.extend(length(series))}
  else if(inherits(config$color, 'list') && length(config$color) > 0){
    clr = config$color[intersect(series, names(config$color))] %>% unlist %>% as.character
  }
  if(length(clr) == 0){return(ax)}
  apexcharter::ax_colors(ax, clr)
}

# Applies config properties (title, subtitle, axis labels, legend, data labels, stacking, theme) to the chart
apexcharterApplyConfig = function(ax, config){
  if(!is.null(config$title)){ax %<>% apexcharter::ax_title(text = config$title)}
  if(!is.null(config$subtitle)){ax %<>% apexcharter::ax_subtitle(text = config$subtitle)}
  if(!is.null(config$xAxis.label)){ax %<>% apexcharter::ax_xaxis(title = list(text = config$xAxis.label))}
  if(!is.null(config$yAxis.label)){ax %<>% apexcharter::ax_yaxis(title = list(text = config$yAxis.label))}
  ax %<>% apexcharter::ax_legend(show = config$legend.enabled, position = config$legend.position)
  if(!is.null(config$label.enabled)){ax %<>% apexcharter::ax_dataLabels(enabled = config$label.enabled)}
  if(isTRUE(config$stack.enabled)){ax %<>% apexcharter::ax_chart(stacked = TRUE)}
  if(!is.null(config$theme)){ax %<>% apexcharter::ax_theme(mode = config$theme)}
  if(!is.null(config$line.shape)){ax %<>% apexcharter::ax_stroke(curve = config$line.shape)}
  return(ax)
}

# Bar, line and area charts of one x column and several y series (columns, or the values of a group column)
apexcharterSeriesChart = function(obj, x, y, group, color, config, defset, type, src, ...){
  if (is.empty(obj)){return(NULL)}
  apexcharterRequire(src)
  config = apexcharterConfig(defset, config)

  a = prepareAesthetics(x = x, y = y, group = group)
  L = a$labels
  obj %<>% prepare4Plot(a$aesthetics, config = config)

  if(type == 'column' && isHorizontal(obj, L$x, L$y)){
    # categories on the vertical axis: a horizontal bar chart of the x columns
    type = 'bar'
    Lx = L$y; Ly = L$x
  } else {Lx = L$x; Ly = L$y}

  long    = apexcharterLong(obj, Lx, Ly, L$group)
  mapping = c(x = 'x', y = 'y')
  mapping[chif(type %in% c('column', 'bar'), 'fill', 'group')] = 'group'
  ax = apexcharterChart(long, mapping, type, ...)
  ax %>% apexcharterColors(unique(long$group), color, config) %>% apexcharterApplyConfig(config)
}

apexcharter.bar = function(obj, x = NULL, y = NULL, group = NULL, color = NULL, config = NULL, ...){
  apexcharterSeriesChart(obj, x, y, group, color, config, apexcharter.bar.defset, 'column', match.call()[[1]], ...)
}

apexcharter.line = function(obj, x = NULL, y = NULL, group = NULL, color = NULL, config = NULL, ...){
  apexcharterSeriesChart(obj, x, y, group, color, config, apexcharter.line.defset, 'line', match.call()[[1]], ...)
}

apexcharter.area = function(obj, x = NULL, y = NULL, group = NULL, color = NULL, config = NULL, ...){
  apexcharterSeriesChart(obj, x, y, group, color, config, apexcharter.line.defset, 'area', match.call()[[1]], ...)
}

apexcharter.tsline = function(obj, x = NULL, y = NULL, group = NULL, color = NULL, config = NULL, ...){
  apexcharterSeriesChart(obj, x, y, group, color, config, apexcharter.tsline.defset, 'line', match.call()[[1]], ...)
}

apexcharter.tsarea = function(obj, x = NULL, y = NULL, group = NULL, color = NULL, config = NULL, ...){
  apexcharterSeriesChart(obj, x, y, group, color, config, apexcharter.tsline.defset, 'area', match.call()[[1]], ...)
}

apexcharter.tsbar = function(obj, x = NULL, y = NULL, group = NULL, color = NULL, config = NULL, ...){
  apexcharterSeriesChart(obj, x, y, group, color, config, apexcharter.tsline.defset, 'column', match.call()[[1]], ...)
}

apexcharter.radar = function(obj, x = NULL, y = NULL, group = NULL, color = NULL, config = NULL, ...){
  apexcharterSeriesChart(obj, x, y, group, color, config, apexcharter.radar.defset, 'radar', match.call()[[1]], ...)
}

apexcharter.scatter = function(obj, x = NULL, y = NULL, group = NULL, color = NULL, config = NULL, ...){
  if (is.empty(obj)){return(NULL)}
  apexcharterRequire(match.call()[[1]])
  config = apexcharterConfig(apexcharter.scatter.defset, config)

  a = prepareAesthetics(x = x, y = y, group = group)
  L = a$labels
  obj %<>% prepare4Plot(a$aesthetics, config = config)

  data = data.frame(x = obj[[L$x]], y = obj[[L$y]], group = chif(is.null(L$group), L$y, as.character(obj[[L$group]])), stringsAsFactors = FALSE)
  apexcharterChart(data, c(x = 'x', y = 'y', group = 'group'), 'scatter', ...) %>%
    apexcharterColors(unique(data$group), color, config) %>% apexcharterApplyConfig(config)
}

apexcharter.bubble = function(obj, x = NULL, y = NULL, size = NULL, group = NULL, color = NULL, config = NULL, ...){
  if (is.empty(obj)){return(NULL)}
  apexcharterRequire(match.call()[[1]])
  config = apexcharterConfig(apexcharter.scatter.defset %<==>% list(essentials = c('x', 'y', 'size')), config)

  a = prepareAesthetics(x = x, y = y, size = size, group = group)
  L = a$labels
  obj %<>% prepare4Plot(a$aesthetics, config = config)

  data = data.frame(x = obj[[L$x]], y = obj[[L$y]], z = obj[[L$size]],
                    group = chif(is.null(L$group), L$y, as.character(obj[[L$group]])), stringsAsFactors = FALSE)
  apexcharterChart(data, c(x = 'x', y = 'y', z = 'z', group = 'group'), 'bubble', ...) %>%
    apexcharterColors(unique(data$group), color, config) %>% apexcharterApplyConfig(config)
}

apexcharter.pie = function(obj, label = NULL, theta = NULL, color = NULL, config = NULL, ...){
  if (is.empty(obj)){return(NULL)}
  apexcharterRequire(match.call()[[1]])
  config = apexcharterConfig(apexcharter.pie.defset, config)

  a = prepareAesthetics(label = label, theta = theta)
  L = a$labels
  if ('rownames' %in% L$label){obj %<>% rownames2Column('rownames') %>% as.data.frame}
  obj %<>% prepare4Plot(a$aesthetics, config = config)

  data = data.frame(x = as.character(obj[[L$label]]), y = obj[[L$theta]], stringsAsFactors = FALSE)
  type = chif(isTRUE(config$pie.donut), 'donut', 'pie')
  apexcharterChart(data, c(x = 'x', y = 'y'), type, ...) %>%
    apexcharterColors(data$x, color, config) %>% apexcharterApplyConfig(config)
}

apexcharter.heatmap = function(obj, x = NULL, y = NULL, z = NULL, config = NULL, ...){
  if (is.empty(obj)){return(NULL)}
  apexcharterRequire(match.call()[[1]])
  config = apexcharterConfig(apexcharter.heatmap.defset, config)

  a = prepareAesthetics(x = x, y = y, z = z)
  L = a$labels
  obj %<>% prepare4Plot(a$aesthetics, config = config)

  data = data.frame(x = obj[[L$x]], y = as.character(obj[[L$y]]), z = obj[[L$z]], stringsAsFactors = FALSE)
  ax = apexcharterChart(data, c(x = 'x', y = 'y', fill = 'z'), 'heatmap', ...)
  if(!is.null(config$color.max)){
    ax %<>% apexcharter::ax_colors(config$color.max)
  }
  ax %>% apexcharterApplyConfig(config)
}
