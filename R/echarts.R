# echarts.R ----------------------------------------------------------------



# Header
# Filename:       echarts.R
# Description:    Contains functions for plotting various charts from js package 'echarts' using standrad inputs.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     19 June 2018
# Last Revision:  24 November 2026
# Version:        0.2.0
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 0.1.0     19 June 2018       Initial issue with bar, funnel, wordCloud and gauge
# 0.1.1     23 November 2026   Bug fix: internal echarts4r helpers (.rm_axis, .set_y_axis, .set_x_axis) are not exported;
#                              they are now fetched from the echarts4r namespace (funnel and wordCloud always failed).
#                              Data access works with recent echarts4r versions which keep data as a list (all charts failed).
# 0.1.2     23 November 2026   ecloud() delegates to echarts4r::e_cloud_() which adds the echarts-wordcloud plugin (word clouds were empty).
#                              Word size range taken from config properties size.min/size.max (it read non-existent point.size.min/max,
#                              so the plugin got no size range and failed).
# 0.1.3     23 November 2026   Config properties size.min and size.max are numbers (were recorded as character) with defaults 12 and 60.
# 0.2.0     24 November 2026   New chart types: line, area, tsline, tsarea, tsbar, scatter, bubble, pie, box, heatmap, calendar,
#                              gauge, sankey, treemap, sunburst and graph. echarts.bar() accepts a group column and applies the
#                              standard config properties (title, subtitle, legend, tooltip, axis labels, labels, stacking, zoom,
#                              theme) through echartsApplyConfig().

#' @include visgen.R

# Internal (non-exported) helper of package echarts4r
echarts.internal = function(name){utils::getFromNamespace(name, 'echarts4r')}

echarts.bar.defset = defset %<==>% list(
  dimclass   = list(
    x       = c('character', 'factor'),
    y       = c('numeric', 'integer'),
    group   = c('character', 'factor')),
  multiples  = 'y',
  essentials = c('x', 'y')
)

echarts.funnel.defset = defset %<==>% list(
  dimclass   = list(
    label    = 'character',
    size     = 'numeric'),
  multiples  = c(),
  essentials = c('label', 'size')
)

echarts.wordCloud.defset = defset %<==>% list(
  dimclass   = list(
    label    = 'character',
    size     = 'numeric',
    color    = c('character', 'numeric', 'integer', 'factor')),
  multiples  = c(),
  essentials = c('label', 'size')
)

# Data of an echarts object (recent versions of echarts4r keep a list of data frames, one per group)
echarts.data = function(e){
  d = e$x$data
  if (!is.data.frame(d)){d = d[[1]]}
  return(d)
}

echarts.build_data <- function(e, ...){
  echarts.data(e) %>%
    dplyr::select(...) %>%
    unname(.) -> data

  apply(data, 1, function(x){
    list(value = unlist(x, use.names = FALSE))
  })

}
echarts.add_bind <- function(e, l, bind, col = "name"){
  echarts.data(e) %>%
    dplyr::select(bind) %>% unname() %>% unlist() -> bind

  for(i in 1:length(l)){
    l[[i]][[col]] <- bind[i]
  }
  l
}

echarts.get_data <- function(e, serie){
  echarts.data(e) %>%
    dplyr::select(serie) %>%
    unname() %>%
    .[[1]]
}


ebar <- function(e, serie, bind = NULL, name = NULL, legend = TRUE, y.index = 0, x.index = 0, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)

  if(missing(serie))
    stop("must pass serie", call. = FALSE)

  if(is.null(name)) # defaults to column name
    name <- serie

  if(y.index != 0)
    e <- echarts.internal(".set_y_axis")(e, serie, y.index)

  if(x.index != 0)
    e <- echarts.internal(".set_x_axis")(e, x.index)

  # build JSON data
  echarts.build_data(e, e$x$mapping$x, serie) -> vector

  if(!is.null(bind))
    vector <- echarts.add_bind(e, vector, bind)

  serie <- list(
    name = name,
    type = "bar",
    data = vector,
    yAxisIndex = y.index,
    xAxisIndex = x.index,
    ...
  )

  if(isTRUE(legend))
    e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))

  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}

efunnel <- function(e, values, labels, name = NULL, legend = TRUE, rm.x = TRUE, rm.y = TRUE, ...){

  if(missing(values) || missing(labels))
    stop("missing values or labels", call. = FALSE)

  # remove axis
  e <- echarts.internal(".rm_axis")(e, rm.x, "x")
  e <- echarts.internal(".rm_axis")(e, rm.y, "y")

  # build JSON data
  funnel <- echarts.build_data(e, values)

  funnel <- echarts.add_bind(e, funnel, labels)

  serie <- list(
    name = name,
    type = "funnel",
    data = funnel,
    ...
  )

  # addlegend

  if(isTRUE(legend)){
    legend <- echarts.get_data(e, labels) %>% as.character()
    e$x$opts$legend$data <- append(e$x$opts$legend$data, legend)
  }

  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e

}

# Word cloud series. Uses echarts4r::e_cloud_(), which also adds the echarts-wordcloud plugin needed to draw it
# (the series was empty without the plugin).
ecloud <- function(e, word, freq, color = NULL, rm.x = TRUE, rm.y = TRUE, ...){
  echarts4r::e_cloud_(e, word = word, freq = freq, color = color, rm_x = rm.x, rm_y = rm.y, ...)
}

egauge <- function(e, theta, label, config = NULL, ...){
  if(missing(theta) || missing(label))
    stop("missing label, or theta", call. = FALSE)

  if(!inherits(theta, "numeric"))
    stop("must pass numeric or interger", call. = FALSE)

  # remove axis
  rm_axis = echarts.internal(".rm_axis")
  e = rm_axis(rm_axis(e, T, "x"), T, "y")

  e$x$opts$series <- list(
    list(
      type = "gauge",
      data = list(list(value = theta, name = label)),
      ...
    )
  )
  e
}

echarts.bar = function(obj, x = NULL, y = NULL, group = NULL, config = NULL, ...){
  echartsSeriesChart(obj, x, y, group, config, echarts.bar.defset, 'e_bar_', match.call()[[1]], ...)
}

echarts.funnel = function(obj, label = NULL, size = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(echarts4r), "Package echarts4r is not installed!", err_src = match.call()[[1]])

  config = echarts.funnel.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  # Preparing Aesthetics:
  a = prepareAesthetics(label = label, size = size)
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config)
  # todo: apply grioup_by with  config$aggrigator.function, not required if NULL
  # todo: check for column names with space
  obj %>% e_charts() %>% efunnel(L$size, L$label, ...)
}

# Font size range of word clouds from config properties size.min and size.max (defaults 12 and 60 if missing)
echarts.sizeRange = function(config){
  rng = suppressWarnings(as.numeric(c(config$size.min, config$size.max)))
  if (length(rng) != 2 || anyNA(rng)){rng = c(12, 60)}
  return(rng)
}

echarts.wordCloud = function(obj, label = NULL, size = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(echarts4r), "Package echarts4r is not installed!", err_src = match.call()[[1]])

  config = echarts.wordCloud.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
    verifyConfig(plotter = 'echarts')

  # Preparing Aesthetics:
  a = prepareAesthetics(label = label, size = size, color = color)
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config)

  obj %>% e_charts() %>% ecloud(L$label, L$size, L$color, shape = config$cloud.shape, sizeRange = echarts.sizeRange(config))
}

# More chart types (version 0.2.0) -----------------------------------------------------------------------------------

echarts.line.defset = defset %<==>% list(
  dimclass   = list(
    x     = c('character', 'factor', 'numeric', 'integer'),
    y     = c('numeric', 'integer'),
    group = c('character', 'factor')),
  multiples  = 'y',
  essentials = c('x', 'y')
)

echarts.tsline.defset = echarts.line.defset %<==>% list(
  dimclass   = list(x = c('Date', 'POSIXct')))

echarts.scatter.defset = defset %<==>% list(
  dimclass   = list(
    x     = c('numeric', 'integer'),
    y     = c('numeric', 'integer'),
    size  = c('numeric', 'integer'),
    group = c('character', 'factor')),
  multiples  = c(),
  essentials = c('x', 'y')
)

echarts.pie.defset = defset %<==>% list(
  dimclass   = list(
    label = c('character', 'factor'),
    theta = c('numeric', 'integer')),
  multiples  = c(),
  essentials = c('label', 'theta')
)

echarts.box.defset = defset %<==>% list(
  dimclass   = list(y = c('numeric', 'integer')),
  multiples  = 'y',
  essentials = 'y'
)

echarts.heatmap.defset = defset %<==>% list(
  dimclass   = list(
    x = c('character', 'factor', 'numeric', 'integer'),
    y = c('character', 'factor', 'numeric', 'integer'),
    z = c('numeric', 'integer')),
  multiples  = c(),
  essentials = c('x', 'y', 'z')
)

echarts.calendar.defset = defset %<==>% list(
  dimclass   = list(
    t     = 'Date',
    color = c('numeric', 'integer')),
  multiples  = c(),
  essentials = c('t', 'color'),
  colorize   = F
)

echarts.sankey.defset = defset %<==>% list(
  dimclass   = list(
    source    = c('character', 'factor'),
    target    = c('character', 'factor'),
    linkWidth = c('numeric', 'integer')),
  multiples  = c(),
  essentials = c('source', 'target', 'linkWidth')
)

echarts.treemap.defset = defset %<==>% list(
  dimclass   = list(
    label = c('character', 'factor'),
    size  = c('numeric', 'integer'),
    group = c('character', 'factor')),
  multiples  = c(),
  essentials = c('label', 'size')
)

echarts.graph.defset = defset %<==>% list(
  dimclass   = list(
    key       = c('character', 'factor', 'integer', 'numeric'),
    label     = c('character', 'factor'),
    size      = c('numeric', 'integer'),
    group     = c('character', 'factor'),
    source    = c('character', 'factor', 'integer', 'numeric'),
    target    = c('character', 'factor', 'integer', 'numeric'),
    linkWidth = c('numeric', 'integer')),
  multiples  = c(),
  essentials = c('source', 'target')
)

# Stops if package echarts4r is not installed
echartsRequire = function(src){
  assert(requireNamespace('echarts4r', quietly = TRUE), "Package 'echarts4r' is not installed!", err_src = src)
}

# Merges config over the default settings of a chart and verifies it against the properties table
echartsConfig = function(defset, config){
  (defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))) %>%
    verifyConfig(plotter = 'echarts')
}

# Applies config properties (title, subtitle, legend, tooltip, axis labels, data labels, zoom, theme) to the chart.
# axes: FALSE for charts without x and y axes (pie, gauge, sankey, ...), whose tooltip is shown per item.
echartsApplyConfig = function(e, config, axes = TRUE){
  if(!is.null(config$title) || !is.null(config$subtitle)){
    e %<>% echarts4r::e_title(text = chif(is.null(config$title), '', config$title), subtext = config$subtitle)
  }
  legend = list(e, show = config$legend.enabled)
  if(!is.null(config$legend.position)){
    legend[[config$legend.position]] = 0
    if(config$legend.position %in% c('left', 'right')){legend$orient = 'vertical'}
  }
  e = do.call(echarts4r::e_legend, legend)
  if(isTRUE(config$tooltip.enabled)){e %<>% echarts4r::e_tooltip(trigger = chif(axes, 'axis', 'item'))}
  if(axes && (!is.null(config$xAxis.label) || !is.null(config$yAxis.label))){
    e %<>% echarts4r::e_axis_labels(x = chif(is.null(config$xAxis.label), '', config$xAxis.label),
                                    y = chif(is.null(config$yAxis.label), '', config$yAxis.label))
  }
  if(isTRUE(config$label.enabled)){e %<>% echarts4r::e_labels(show = TRUE)}
  if(axes && isTRUE(config$zoom.enabled)){e %<>% echarts4r::e_datazoom()}
  if(!is.null(config$theme)){e %<>% echarts4r::e_theme(config$theme)}
  return(e)
}

# Groups a table by a column, giving one series per value (echarts4r draws a series for each group)
echartsGroup = function(obj, Lgroup){
  if(is.null(Lgroup)){return(obj)}
  dplyr::group_by(obj, dplyr::across(dplyr::all_of(Lgroup)))
}

# Bar, line and area charts of one x column and several y series (columns, or the values of a group column).
# serie: name of the echarts4r function adding a series, like 'e_line_'.
echartsSeriesChart = function(obj, x, y, group, config, defset, serie, src, ...){
  if (is.empty(obj)){return(NULL)}
  echartsRequire(src)
  config = echartsConfig(defset, config)

  a = prepareAesthetics(x = x, y = y, group = group)
  L = a$labels
  obj %<>% prepare4Plot(a$aesthetics, config = config)
  if(!is.null(L$group)){
    assert(length(L$y) == 1, 'You can define series by either grouping the values or selecting multiple columns!')
  }

  add = getExportedValue('echarts4r', serie)
  e   = obj %>% echartsGroup(L$group) %>% echarts4r::e_charts_(L$x)
  for (i in L$y){
    e = chif(isTRUE(config$stack.enabled), add(e, i, stack = 'stack', ...), add(e, i, ...))
  }
  e %>% echartsApplyConfig(config)
}

echarts.line = function(obj, x = NULL, y = NULL, group = NULL, config = NULL, ...){
  echartsSeriesChart(obj, x, y, group, config, echarts.line.defset, 'e_line_', match.call()[[1]], ...)
}

echarts.area = function(obj, x = NULL, y = NULL, group = NULL, config = NULL, ...){
  echartsSeriesChart(obj, x, y, group, config, echarts.line.defset, 'e_area_', match.call()[[1]], ...)
}

echarts.tsline = function(obj, x = NULL, y = NULL, group = NULL, config = NULL, ...){
  echartsSeriesChart(obj, x, y, group, config, echarts.tsline.defset, 'e_line_', match.call()[[1]], ...)
}

echarts.tsarea = function(obj, x = NULL, y = NULL, group = NULL, config = NULL, ...){
  echartsSeriesChart(obj, x, y, group, config, echarts.tsline.defset, 'e_area_', match.call()[[1]], ...)
}

echarts.tsbar = function(obj, x = NULL, y = NULL, group = NULL, config = NULL, ...){
  echartsSeriesChart(obj, x, y, group, config, echarts.tsline.defset, 'e_bar_', match.call()[[1]], ...)
}

echarts.scatter = function(obj, x = NULL, y = NULL, size = NULL, group = NULL, config = NULL, ...){
  if (is.empty(obj)){return(NULL)}
  echartsRequire(match.call()[[1]])
  config = echartsConfig(echarts.scatter.defset, config)

  a = prepareAesthetics(x = x, y = y, size = size, group = group)
  L = a$labels
  obj %<>% prepare4Plot(a$aesthetics, config = config)

  e = obj %>% echartsGroup(L$group) %>% echarts4r::e_charts_(L$x)
  e = chif(is.null(L$size), echarts4r::e_scatter_(e, L$y, ...), echarts4r::e_scatter_(e, L$y, size = L$size, ...))
  e %>% echartsApplyConfig(config)
}

echarts.bubble = function(obj, x = NULL, y = NULL, size = NULL, group = NULL, config = NULL, ...){
  assert(!is.null(size), "Bubble charts need a size column!", err_src = match.call()[[1]])
  echarts.scatter(obj, x = x, y = y, size = size, group = group, config = config, ...)
}

echarts.pie = function(obj, label = NULL, theta = NULL, config = NULL, ...){
  if (is.empty(obj)){return(NULL)}
  echartsRequire(match.call()[[1]])
  config = echartsConfig(echarts.pie.defset, config)

  a = prepareAesthetics(label = label, theta = theta)
  L = a$labels
  if ('rownames' %in% L$label){obj %<>% rownames2Column('rownames') %>% as.data.frame}
  obj %<>% prepare4Plot(a$aesthetics, config = config)
  obj[[L$label]] %<>% as.character

  e = echarts4r::e_charts_(obj, L$label)
  e = chif(isTRUE(config$pie.donut), echarts4r::e_pie_(e, L$theta, radius = c('50%', '70%'), ...), echarts4r::e_pie_(e, L$theta, ...))
  e %>% echartsApplyConfig(config, axes = FALSE)
}

echarts.box = function(obj, y = NULL, config = NULL, ...){
  if (is.empty(obj)){return(NULL)}
  echartsRequire(match.call()[[1]])
  config = echartsConfig(echarts.box.defset, config)

  a = prepareAesthetics(y = y)
  L = a$labels
  obj %<>% prepare4Plot(a$aesthetics, config = config)

  e = echarts4r::e_charts(obj)
  for (i in L$y){e = echarts4r::e_boxplot_(e, i, ...)}
  e %>% echartsApplyConfig(config, axes = FALSE)
}

echarts.heatmap = function(obj, x = NULL, y = NULL, z = NULL, config = NULL, ...){
  if (is.empty(obj)){return(NULL)}
  echartsRequire(match.call()[[1]])
  config = echartsConfig(echarts.heatmap.defset, config)

  a = prepareAesthetics(x = x, y = y, z = z)
  L = a$labels
  obj %<>% prepare4Plot(a$aesthetics, config = config)

  echarts4r::e_charts_(obj, L$x) %>%
    echarts4r::e_heatmap_(L$y, L$z, ...) %>%
    echarts4r::e_visual_map_(L$z) %>%
    echartsApplyConfig(config, axes = FALSE)
}

echarts.calendar = function(obj, t = NULL, color = NULL, config = NULL, ...){
  if (is.empty(obj)){return(NULL)}
  echartsRequire(match.call()[[1]])
  config = echartsConfig(echarts.calendar.defset, config)

  a = prepareAesthetics(t = t, color = color)
  L = a$labels
  obj %<>% prepare4Plot(a$aesthetics, config = config)
  rng = range(obj[[L$t]], na.rm = TRUE) %>% format('%Y-%m-%d')

  echarts4r::e_charts_(obj, L$t) %>%
    echarts4r::e_calendar(range = rng) %>%
    echarts4r::e_heatmap_(L$color, coord_system = 'calendar', ...) %>%
    echarts4r::e_visual_map_(L$color) %>%
    echartsApplyConfig(config, axes = FALSE)
}

# A gauge of a single value: a number, or a column of a table aggregated by config$aggregator (default mean)
echarts.gauge = function(obj = NULL, theta = NULL, config = NULL, ...){
  echartsRequire(match.call()[[1]])
  config = echartsConfig(defset, config)
  if (inherits(obj, 'data.frame')){
    a = prepareAesthetics(theta = theta)
    aggregator = chif(is.null(config$aggregator), mean, match.fun(config$aggregator))
    value = aggregator(obj[, a$labels$theta[1]])
    name  = a$labels$theta[1]
  } else {
    value = chif(is.null(obj), theta, obj)
    name  = chif(is.null(config$title), '', config$title)
  }
  value %<>% verify(c('numeric', 'integer'), lengths = 1, varname = 'value')
  echarts4r::e_charts() %>% echarts4r::e_gauge_(value, name, ...) %>% echartsApplyConfig(config, axes = FALSE)
}

# Sankey diagram of a table of links (source, target and width columns)
echarts.sankey = function(obj, source = NULL, target = NULL, linkWidth = NULL, config = NULL, ...){
  if (is.empty(obj)){return(NULL)}
  echartsRequire(match.call()[[1]])
  config = echartsConfig(echarts.sankey.defset, config)

  a = prepareAesthetics(source = source, target = target, linkWidth = linkWidth)
  L = a$labels
  obj %<>% prepare4Plot(a$aesthetics, config = config)
  obj[[L$source]] %<>% as.character
  obj[[L$target]] %<>% as.character

  echarts4r::e_charts(obj) %>%
    echarts4r::e_sankey_(L$source, L$target, L$linkWidth, ...) %>%
    echartsApplyConfig(config, axes = FALSE)
}

# Hierarchical data of treemaps and sunbursts: items (label and size) with an optional parent (group)
echartsTree = function(obj, L){
  if(is.null(L$group)){
    return(data.frame(name = as.character(obj[[L$label]]), value = obj[[L$size]], stringsAsFactors = FALSE))
  }
  groups = unique(as.character(obj[[L$group]]))
  tree = data.frame(name = groups, value = vapply(groups, function(g) sum(obj[[L$size]][obj[[L$group]] == g], na.rm = TRUE), numeric(1)),
                    stringsAsFactors = FALSE)
  tree$children = lapply(groups, function(g){
    sub = obj[obj[[L$group]] == g, , drop = FALSE]
    data.frame(name = as.character(sub[[L$label]]), value = sub[[L$size]], stringsAsFactors = FALSE)
  })
  tree
}

echartsHierarchy = function(obj, label, size, group, config, chart, src, ...){
  if (is.empty(obj)){return(NULL)}
  echartsRequire(src)
  config = echartsConfig(echarts.treemap.defset, config)

  a = prepareAesthetics(label = label, size = size, group = group)
  L = a$labels
  obj %<>% prepare4Plot(a$aesthetics, config = config)

  add = getExportedValue('echarts4r', chart)
  echartsTree(obj, L) %>% echarts4r::e_charts() %>% add(...) %>% echartsApplyConfig(config, axes = FALSE)
}

echarts.treemap = function(obj, label = NULL, size = NULL, group = NULL, config = NULL, ...){
  echartsHierarchy(obj, label, size, group, config, 'e_treemap', match.call()[[1]], ...)
}

echarts.sunburst = function(obj, label = NULL, size = NULL, group = NULL, config = NULL, ...){
  echartsHierarchy(obj, label, size, group, config, 'e_sunburst', match.call()[[1]], ...)
}

# Network graph of obj = list(nodes = <data.frame>, links = <data.frame>), like visNetwork.graph().
# Nodes have a key (default: label), size and group (category); links have source, target and width.
echarts.graph = function(obj, key = NULL, label = NULL, size = NULL, group = NULL,
                         source = NULL, target = NULL, linkWidth = NULL, config = NULL, ...){
  obj %>% verify('list', names_include = c('nodes', 'links'), varname = 'obj', null_allowed = F, err_src = 'echarts.graph')
  obj$nodes %>% verify('data.frame', varname = 'obj$nodes', null_allowed = F, err_src = 'echarts.graph')
  obj$links %>% verify('data.frame', varname = 'obj$links', null_allowed = F, err_src = 'echarts.graph')
  echartsRequire(match.call()[[1]])
  config = echartsConfig(echarts.graph.defset, config)

  if(is.null(key)){key = label}
  assert(!is.null(key), "Nodes need a key or label column!", err_src = 'echarts.graph')
  an = prepareAesthetics(key = key, label = label, size = size, group = group)
  al = prepareAesthetics(source = source, target = target, linkWidth = linkWidth)
  Ln = an$labels; Ll = al$labels
  nodes = obj$nodes %>% prepare4Plot(an$aesthetics, config = config)
  links = obj$links %>% prepare4Plot(al$aesthetics, config = config)

  labels = chif(is.null(Ln$label), NULL, as.character(nodes[[Ln$label]]))
  nodes = data.frame(name     = as.character(nodes[[Ln$key]]),
                     value    = chif(is.null(Ln$size), 1, nodes[[Ln$size]]),
                     size     = chif(is.null(Ln$size), 15, 10 + 30*vect.normalize(nodes[[Ln$size]])),
                     category = chif(is.null(Ln$group), 'nodes', as.character(nodes[[Ln$group]])),
                     stringsAsFactors = FALSE)
  keys  = nodes$name
  edges = data.frame(source = as.character(links[[Ll$source]]), target = as.character(links[[Ll$target]]),
                     value  = chif(is.null(Ll$linkWidth), 1, links[[Ll$linkWidth]]), stringsAsFactors = FALSE)
  # echarts identifies (and shows) nodes by name: node labels replace the keys if they are unique
  if(!is.null(labels) && !identical(labels, keys)){
    if(!anyDuplicated(labels[!duplicated(keys)])){
      nodes$name   = labels
      edges$source = labels[match(edges$source, keys)]
      edges$target = labels[match(edges$target, keys)]
    }
  }
  nodes = nodes[!duplicated(keys), , drop = FALSE]
  if(is.null(Ln$group)){config$legend.enabled = FALSE}

  # e_graph_nodes() and e_graph_edges() take column names unquoted:
  # echarts' default force settings pack the nodes too tightly to see the links; arguments in ... replace these:
  args = list(layout = config$graph.layout, roam = TRUE, label = list(show = TRUE, position = 'right'),
              force = list(repulsion = 300, edgeLength = 80), lineStyle = list(width = 2, curveness = 0.1))
  extra = list(...)
  args[names(extra)] = extra
  e = do.call(echarts4r::e_graph, c(list(echarts4r::e_charts()), args))
  e = do.call(echarts4r::e_graph_nodes, list(e, nodes, as.name('name'), as.name('value'), as.name('size'), as.name('category'),
                                              legend = !is.null(Ln$group)))
  e = do.call(echarts4r::e_graph_edges, list(e, edges, as.name('source'), as.name('target'), as.name('value')))
  e %>% echartsApplyConfig(config, axes = FALSE)
}
