# d3plus.R ----------------------------------------------------------------


# Header
# Filename:       d3plus.R
# Description:    Contains functions for plotting various charts from package 'd3plus' using standrad inputs.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     28 March 2017
# Last Revision:  25 July 2018
# Version:        1.1.6
#
# Version   Date               Action
# -----------------------------------
# 1.0.0     26 October 2016    Initial issue by adding d3plus.defset
# 1.1.0     13 May 2017        Function d3plus.bubble() modified: Accommodating modifications in visgen
# 1.1.1     13 May 2017        Function d3plus.bubble.moten() added
# 1.1.2     13 May 2017        Function d3plus.tree() added
# 1.1.5     01 July 2018       Functions d3plus.bar.molten(), d3plus.tsline.molten() and d3plus.tsbar.molten() added using new htmlwidget: D3plusR
# 1.1.6     25 July 2018       Function d3plus.bar.molten() converted to d3plus.bar() plots both wide and molten

d3plus.scatter.defset = defset %<==>% list(
  # Valid classes for all dimensions
  dimclass  = list(x = 'numeric',
                   y = 'numeric',
                   size = 'numeric',
                   label = 'character')
)

d3plus.bubble.defset = defset %<==>% list(
  # Valid classes for all dimensions
  dimclass  = list(
    size = 'numeric',
    label = 'character'),
  multiples  = c('size', 'label'),
  essentials = c('size', 'label')
)

d3plus.bubble.molten.defset = defset %<==>% list(
  # Valid classes for all dimensions
  dimclass  = list(
    size  = 'numeric',
    label = 'character',
    group = c('factor', 'character', 'integer')),
  multiples  = c(),
  essentials = c('size', 'label')
)

d3plus.bar.defset = defset %<==>% list(
  dimclass  = list(
    x     = c('character', 'factor', 'numeric', 'integer'),
    y     = c('numeric', 'integr', 'character', 'factor'),
    group = 'factor'),
  multiples  = c('x', 'y'),
  essentials = c('x', 'y')
)

d3plus.bar.molten.defset = defset %<==>% list(
  dimclass  = list(
    x     = 'character',
    y     = 'numeric',
    group = 'factor'),
  multiples  = c(),
  essentials = c('x', 'y', 'group'),
  shape = 'bar'
)
d3plus.line.molten.defset = d3plus.bar.molten.defset %<==>% list(shape = 'line')
d3plus.area.molten.defset = d3plus.bar.molten.defset %<==>% list(shape = 'area')

d3plus.tree.defset = defset %<==>% list(
  # Valid classes for all dimensions
  dimclass  = list(
    label = 'character',
    size  = 'numeric',
    color = valid.classes),
  multiples  = c(),
  essentials = c('size', 'label'),
  colorize = F
)

d3plus.tsline.molten.defset = defset %<==>% list(
  dimclass  = list(
    x     = 'Date',
    y     = 'numeric',
    group = 'factor'),
  multiples  = c(),
  essentials = c('x', 'y', 'group'),
  shape = 'line'
)

d3plus.tsline.defset = defset %<==>% list(
  dimclass  = list(
    x     = 'Date',
    y     = 'numeric',
    group = 'factor'),
  multiples  = 'y',
  essentials = c('x', 'y'),
  shape = 'line'
)

d3plus.tsbar.molten.defset  = d3plus.tsline.molten.defset %<==>% list(shape = 'bar')
d3plus.tsarea.molten.defset = d3plus.tsline.molten.defset %<==>% list(shape = 'area')

d3plus.treemap.defset = defset %<==>% list(
  dimclass  = list(
    label = 'character',
    size  = 'numeric',
    color = c('character', 'numeric')),
  multiples  = 'label',
  essentials = c('label', 'size')
)


d3plus.shape = c(bar = 'bar', line = 'line', area = 'stacked')
# Check this function sholud not be working!!!
d3plus.scatter = function(obj, x = NULL, y = NULL, size = NULL, label = NULL, config = NULL, ...){

  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(d3plus), "Package d3plus is not installed!", err_src = match.call()[[1]])
  config = d3plus.scatter.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  # Preparing Aesthetics:
  x %<>% nameList('x')
  y %<>% nameList('y')
  size %<>% nameList('size')
  label %<>% nameList('label')

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, size = size, label = label, config = config)
  L = a$labels
  A = a$aesthetics

  # xL         = names(x)
  # yL         = names(y)
  # sizeL      = names(size)

  obj %<>% prepare4Plot(A, config = config)

  d3plus('scatter', obj, ...)
}

d3plus.bubble = function(obj, size = NULL, label = NULL, config = NULL, ...){
  # Verifications:
  assert(require(d3plus), "Package d3plus is not installed!", err_src = match.call()[[1]])
  config = d3plus.bubble.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  # Preparing Aesthetics:
  a = prepareAesthetics(size = size, label = label, extend = c('size', 'label'))
  L = a$labels
  A = a$aesthetics

  # Function d3plus with type 'bubbles' uses melted table for multiple series of bubbles
  # This is what they called as Grouped bubbles. The series name comes in a column named as group between label and size columns
  obj %<>% prepare4Plot(A, config = config) %>%
    melt(id.vars = unique(L$label))

  names(L$label) <- L$size
  obj = cbind(name = obj[, L$label[obj$variable]] %>% as.matrix %>% diag, obj[, c('variable', 'value')]) %>% na.omit

  d3plus('bubbles', obj, ...)
}

d3plus.bubble.molten = function(obj, size = NULL, label = NULL, group = NULL, config = NULL, ...){
  # Verifications:
  assert(require(d3plus), "Package d3plus is not installed!", err_src = match.call()[[1]])
  config = d3plus.bubble.molten.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  if(is.null(group)){group = 1}
  # Preparing Aesthetics:
  a = prepareAesthetics(label = label, group = group, size = size)
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config = config)

  d3plus('bubbles', obj, ...)
}

d3plus.tree = function(obj, label = NULL, size = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  assert(require(d3plus), "Package d3plus is not installed!", err_src = match.call()[[1]])
  config = d3plus.tree.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  # Preparing Aesthetics:
  a = prepareAesthetics(label = label, size = size, color = color)
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config = config)

  d3plus('tree', obj, ...)
}

d3plus.bar = function(obj, x = NULL, y = NULL, group = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(D3plusR), "Package D3plusR is not installed!", err_src = match.call()[[1]])
  config = d3plus.bar.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
    verifyConfig(plotter = 'd3plus')

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, group = group, color = color)
  L = a$labels
  A = a$aesthetics %>% list.remove('color')

  obj %<>% prepare4Plot(A, config = config)
  hor = isHorizontal(obj, L$x, L$y)
  Sx  = chif(hor, T, NULL) ; Sy = chif(hor, NULL, T)
  Ly  = chif(hor, L$x, L$y); Lx = chif(hor, L$y, L$x)
  if(!is.null(L$group)){
    clrlist = getColorList(obj[, L$group] %>% as.character %>% unique, L$color, config)
    assert(length(L$x) == 1 & length(L$y) == 1, 'You can define series by either grouping the values or selecting multiple columns!')
    d3 = d3plus(data = obj, id = L$group, type = 'bar') %>%
      d3plusX(value = L$x, scale = chif(hor, 'linear', 'discrete'), stacked = Sx) %>%
      d3plusY(value = L$y, scale = chif(hor, 'discrete', 'linear'), stacked = Sy)
    Lgroup = L$group
  } else {
    if(length(Ly) > 1){
      d3 = d3plus(data = obj %>% reshape2::melt(id.vars = Lx, measure.vars = Ly), id = 'variable', type = 'bar') %>%
        d3plusX(value = chif(hor, 'value', L$x), scale = chif(hor, 'linear', 'discrete'), stacked = Sx) %>%
        d3plusY(value = chif(hor, L$y, 'value'), scale = chif(hor, 'discrete', 'linear'), stacked = Sy)
      Lgroup = 'variable'
    } else {
      d3 = d3plus(data = obj, type = 'bar') %>%
        d3plusX(value = L$x, scale = chif(hor, 'linear', 'discrete'), stacked = Sx) %>%
        d3plusY(value = L$y, scale = chif(hor, 'discrete', 'linear'), stacked = Sy)
      Lgroup = Ly
    }
    clrlist = getColorList(Ly, L$color, config)
  }
  attributes = clrlist %>%
    as.data.frame %>% t %>% rownames2Column(Lgroup) %>% as.data.frame %>% list
  names(attributes) <- Lgroup

  d3 %>% d3plusAttrs(value = attributes) %>% d3plusColor(value = "V1") %>%
    d3plus.applyConfig(config)
}

d3plus.applyConfig = function(chart, config){
  chart %>% d3plusLegend(value = config$legend.enabled, size = config$legend.size, data = config$legend.tooltip.enabled) %>%
    d3plusTooltip(value = config$tooltip) %>% d3plusTitle(config$title)
}

# to be removed
d3plus.bar.molten = function(obj, x = NULL, y = NULL, group = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(D3plusR), "Package D3plusR is not installed!", err_src = match.call()[[1]])
  config = d3plus.bar.molten.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
    verifyConfig(plotter = 'd3plus')

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, group = group)
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config = config)

  attributes = getColorList(obj[, L$group] %>% unique, L$color, config) %>%
    as.data.frame %>% t %>% rownames2Column(L$group) %>% as.data.frame %>% list
  names(attributes) <- L$group

  d3plus(data = obj, id = L$group, type = 'bar') %>%
    d3plusX(value = L$x) %>%
    d3plusY(value = L$y) %>%
    d3plusAttrs(value = attributes) %>%
    d3plusColor(value = "V1") %>%
    d3plus.applyConfig(config)
}

d3plus.line.molten = function(obj, x = NULL, y = NULL, group = NULL, config = NULL, ...){
  config = d3plus.line.molten.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  obj %>% d3plus.bar.molten(x = NULL, y = NULL, group = NULL, config = NULL, ...)
}

d3plus.area.molten = function(obj, x = NULL, y = NULL, group = NULL, config = NULL, ...){
  config = d3plus.area.molten.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  obj %>% d3plus.bar.molten(x = NULL, y = NULL, group = NULL, config = NULL, ...)
}

d3plus.tsline = function(obj, x = NULL, y = NULL, group = NULL, config = NULL, ...){
  config = d3plus.tsline.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
    verifyConfig(plotter = 'd3plus')

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, group = group)
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config = config)

  if(length(L$y) > 1){
    obj %>% reshape2::melt(id.vars = L$x, measure.vars = L$y) %>%
      d3plus.tsline.molten(x = L$x, y = 'value', group = 'variable', config = config, ...)
  } else {
    obj %>% d3plus.tsline.molten(x = L$x, y = L$y, config = config, ...)
  }

}

d3plus.tsline.molten = function(obj, x = NULL, y = NULL, group = NULL, config = NULL, ...){
  assert(require(D3plusR), "Package D3plusR is not installed!", err_src = match.call()[[1]])
  config = d3plus.tsline.molten.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  L = list(x = x, y = y, group = group)
  obj[, L$x]  %<>% format("%Y/%m/%d")

  date_filter = obj[, L$x]
  if(!is.null(config$xAxis.min)){date_filter = date_filter %^% obj[, L$x][obj[, L$x] >= config$xAxis.min %>% verify('Date') %>% format("%Y/%m/%d")]}
  if(!is.null(config$xAxis.max)){date_filter = date_filter %^% obj[, L$x][obj[, L$x] <= config$xAxis.max %>% verify('Date') %>% format("%Y/%m/%d")]}

  labelFormat = config$label.format %>% unlist

  d3plus(data = obj, id = L$group,
         type         = d3plus.shape[config$shape] %>% unname,
         percent_var  = names(labelFormat)[which(labelFormat == 'percentage')],
         currency     = config$currency,
         currency_var = names(labelFormat)[which(labelFormat == 'currency')],
         height = config$height,
         width  = config$width) %>%
    d3plusX(value = L$x, grid = config$xAxis.grid.enabled) %>%
    d3plusY(value = L$y, grid = config$yAxis.grid.enabled) %>%
    d3plusTime(value = L$x, solo = date_filter ) %>%
    d3plusTooltip(value = config$tooltip) %>%
    d3plusTitle(config$title)
}

d3plus.tsbar.molten = function(obj, x = NULL, y = NULL, group = NULL, config = NULL, ...){
  config = d3plus.tsbar.molten.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  obj %>% d3plus.tsline.molten(x = x, y = y, group = group, config = config, ...)
}

d3plus.tsarea.molten = function(obj, x = NULL, y = NULL, group = NULL, config = NULL, ...){
  config = d3plus.tsarea.molten.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  obj %>% d3plus.tsline.molten(x = x, y = y, group = group, config = config, ...)
}

d3plus.treemap = function(obj, label = NULL, size = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(D3plusR), "Package D3plusR is not installed!", err_src = match.call()[[1]])
  config = d3plus.treemap.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
    verifyConfig(plotter = 'd3plus')

  # Preparing Aesthetics:
  a = prepareAesthetics(label = label, size = size, color = color)
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config = config)

  d3p = d3plus(data = obj,
               type   = "tree_map",
               id     = L$label,
               width  = config$width,
               height = config$height) %>%
    d3plusSize(value = L$size) %>%
    d3plusLegend(value = config$legend.enabled, order = list(sort = "desc", value = "size"))
  if(!is.null(L$color)){d3p %<>% d3plusColor(L$color)}

  d3p %>% d3plusDepth(0) %>%
    d3plusLabels(value = TRUE, valign = "top")
}
