
# c3.R ----------------------------------------------------------------

# Header
# Filename:       c3.R
# Description:    Contains functions for plotting various charts from package 'c3' using standrad inputs.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     14 April 2017
# Last Revision:  17 September 2018
# Version:        0.1.0
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 0.0.1     14 April 2017      Initial issue
# 0.0.2     05 March 2017      Function c3.combo() modified: extends vector shape to the count of series, function c3.bar() added
# 0.0.3     19 June 2018       Function c3.applyConfig() modified: Does not apply xAxis.label and yAxis labels if NULL
# 0.0.4     19 June 2018       Function c3.gauge() added
# 0.0.5     19 July 2018       Function c3.tsline() added
# 0.0.6     24 July 2018       Function c3.bar() is re-written: needed to call c3_bar because combo does not accept argument 'rotated'
# 0.0.9     24 July 2018       Functions c3.line() and c3.area() and c3.tsarea() added
# 0.1.0     17 September 2018  Functions c3.tsarea() modified

c3.combo.defset = defset %<==>% list(
  dimclass   = list(
    x       = c('character', 'Date'),
    y       = 'numeric',
    shape   = 'character'),
  multiples  = c('y', 'shape'),
  essentials = 'y'
)

# don't use integer for c3!
c3.bar.defset = defset %<==>% list(
  dimclass   = list(
    x     = c('numeric', 'character', 'factor'),
    y     = c('numeric', 'character', 'factor'),
    group = c('factor', 'character')),
  multiples  = c('x', 'y'),
  essentials = c('y')
)

c3.tsline.defset = defset %<==>% list(
  dimclass   = list(
    x       = 'Date', # POSIXct does not work. Convert to character
    y       = 'numeric',
    shape   = 'character'),
  multiples  = c('y', 'shape'),
  essentials = c('x', 'y')
)

c3.pie.defset = defset %<==>% list(
  dimclass   = list(
    label   = 'character',
    theta   = 'numeric'),
  multiples  = c(),
  essentials = c('label', 'theta')
)


c3.scatter.molten.defset = defset %<==>% list(
  dimclass   = list(
    x       = 'numeric',
    y       = 'numeric',
    group   = valid.nominal.classes),
  multiples = c(),
  essentials = c('x', 'y')
)

c3.applyConfig = function(chart, config){
  if(!is.null(config$xAxis.label)){chart %<>% xAxis(label = config$xAxis.label)}
  if(!is.null(config$yAxis.label)){chart %<>% yAxis(label = config$yAxis.label)}
  return(chart)
}

c3.applyColors <- function(ct, colorvect) {
  ct$x$color$pattern <- colorvect
  # You can change all other properties here
  # reference: https://c3js.org/reference.html
  return(ct)
}

c3.combo = function(obj, x = NULL, y = NULL, shape = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(c3), "Package c3 is not installed!", err_src = match.call()[[1]])

  config = c3.combo.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
    verifyConfig(plotter = 'c3')

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, shape = shape, extend = c('y', 'shape'))
  L = a$labels
  A = a$aesthetics %>% list.remove('shape')

  obj %<>% prepare4Plot(A, config)

  # assert(length(L$y) > 1, "THE MOST STUPID PACKAGE I HAVE SEEN!!")

  if(obj[, L$x] %>% duplicated %>% sum > 0 & !is.null(config$aggregator)){
    # if(inherits(config$aggregator, 'function')){config$aggregator = as.character(substitute(config$aggregator))}
    config$aggregator %>% verify('character')
    obj %<>% dplyr::group_by_(L$x)
    scr = "obj %>% dplyr::summarise("
    N   = length(L$y)
    for (i in sequence(N)){
      scr %<>% paste0("`", L$y[i], "` = ", config$aggregator, "(`", L$y[i], "`)", chif(i < N, ", ", ")"))
    }
    obj = parse(text = scr) %>% eval %>% as.data.frame
  }

  if (!is.null(L$x)){
    if (inherits(obj[,L$x], 'Date')){ct = obj %>% c3(x = L$x)} else {
      ct = obj %>% c3 %>% xAxis(type = 'category', categories = obj[, L$x] %>% as.character)
    }
  } else {ct = obj %>% c3}
  if(!is.null(L$shape)){names(L$shape) <- L$y}
  if(is.null(L$shape)){
    if(!is.null(config$shape)){
      L$shape = config$shape
    } else {L$shape = 'line'}
  }

  L$shape %<>% vect.extend(length(L$y))
  if(is.empty(config$stack.groups)){config$stack.groups = L$y}
  options(warn = -1)
  ct %<>% c3_mixedGeom(type = most.common(L$shape), types = L$shape %>% as.list, stacked = chif(config$stack.enabled, config$stack.groups, NULL))
  options(warn = 1)

  return(ct %>% c3.applyConfig(config))
}

c3.tsline = function(obj, x = NULL, y = NULL, config = NULL, ...){
  config = c3.tsline.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  obj %>% c3.combo(x = x, y = y, shape = 'line', config = config, ...)
}

c3.tsarea = function(obj, x = NULL, y = NULL, config = NULL, ...){
  config = c3.tsline.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
    verifyConfig(plotter = 'c3')

  obj %>% c3.combo(x = x, y = y, shape = 'area', config = config, ...)
}

c3.area = function(obj, x = NULL, y = NULL, config = NULL, ...){
  config = c3.bar.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
    verifyConfig(plotter = 'c3')

  obj %>% c3.combo(x = x, y = y, shape = 'area', config = config, ...)
}

c3.line = function(obj, x = NULL, y = NULL, config = NULL, ...){
  config = c3.bar.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
    verifyConfig(plotter = 'c3')

  obj %>% c3.combo(x = x, y = y, shape = 'line', config = config, ...)
}

c3.scatter.molten = function(obj, x = NULL, y = NULL, shape = NULL, group = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(c3), "Package c3 is not installed!", err_src = match.call()[[1]])

  config = c3.scatter.molten.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, shape = shape, group = group)
  L = a$labels
  A = a$aesthetics %>% list.remove('shape')

  obj %<>% prepare4Plot(A, config)

  ct = obj %>% c3(x = L$x, y = L$y, group = L$group)

  if(is.null(L$shape)){L$shape = 'point'}
  if(L$shape == 'point'){ct %<>% c3_scatter} else if (L$shape == 'bar') {ct %<>% c3_bar}
  return(ct %>% c3.applyConfig(config))
}

c3.bar = function(obj, x = NULL, y = NULL, color = NULL, config = NULL){
  # Verifications:
  if (is.empty(obj)){return(NULL)}

  assert(require('c3'), "Package 'c3' is not installed!", err_src = match.call()[[1]])
  (c3.bar.defset %<==>% config) %>%
    verifyConfig(plotter = 'c3') %>%
    verifyConfigDimProperties(dims = 'color') -> config

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, color = color)
  L = a$labels
  A = a$aesthetics %>% list.remove('color')

  obj %<>% prepare4Plot(A, config = config)

  hor = isHorizontal(obj, L$x, L$y)
  Ly = chif(hor, L$x, L$y); Lx = chif(hor, L$y, L$x)

  getColorVect(Ly, L$color, config)

  obj[, c(Lx, Ly)] %>% c3 %<>% c3_bar(stacked = config$stack.enabled, rotated = hor, bar_width = 0.6, zerobased = TRUE) %>%
    xAxis(type = 'category', categories = obj[, Lx] %>% as.character) %>%
    c3.applyConfig(config) %>% c3.applyColors(clrvect)
}

c3.pie = function(obj, theta = NULL, label = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(c3), "Package c3 is not installed!", err_src = match.call()[[1]])

  config = c3.pie.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
    verifyConfig(plotter = 'c3')

  # Preparing Aesthetics:
  a = prepareAesthetics(label = label, theta = theta)
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config)

  df = t(obj[, L$theta]) %>% as.data.frame
  colnames(df) <-   obj[, L$label]
  df %>% c3 %>% c3_donut(title = config$title)
}

c3.gauge = function(theta, config = NULL, ...){
  theta %>% verify('numeric') %>% data.frame %>% c3 %>% c3_gauge(...)
}

