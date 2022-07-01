# amCharts.R ----------------------------------------------------------------
# Header
# Filename:       amcharts.R
# Description:    Contains functions for plotting various charts from package amCharts using standrad inputs.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     28 November 2016
# Last Revision:  24 July 2018
# Version:        1.1.9
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 1.0.0     28 November 2016   Initial issue
# 1.1.0     03 April 2017      Fundamental changes with standard format: Calls prepare4Plot & prepareAusthetics from visgen.R
# 1.1.1     03 April 2017      amCharts.defset
# 1.1.2     19 April 2017      amCharts.tsline() added
# 1.1.3     20 June 2018       All config verifications tranferred to visgen. Function amCharts.prepareConfig() removed. Function verifyConfig() is called in each plot
# 1.1.4     20 June 2018       amCharts.gauge() added
# 1.1.5     30 June 2018       amCharts.gauge() modified.
# 1.1.6     30 June 2018       amCharts.funnel() added.
# 1.1.7     30 June 2018       amCharts.box() added.
# 1.1.8     30 June 2018       amCharts.bullet() added.
# 1.1.9     24 July 2018       amCharts.tsline() added.

# General settings for all the plots
defset = list(
  palette= list(
    # color = c("#FB1108", "#FA7806","#FBE426","#FCFB8F", "#F3F5E7", "#C7E4EA","#ABD6E6","#9AD2E1"),
    color = c("purple", "blue","cyan","green", "yellow", "orange","red","black" , 'white'),
    shape = c('circle', 'x', 'o', 'plus', 'square.hollow', 'rhombus.hollow')
  ),

  withRowNames = F,
  colorize     = T
)

symbols = c('point', 'circle', 'bubble', 'square', 'rhombus', 'diamond', 'delta', 'right', 'left')
ltypes  = c('line', 'dashLine')
valid.amCharts.shapes = c('line', paste(ltypes[1], symbols, sep = '.'), 'dashLine', paste(ltypes[2], symbols, sep = '.'))

amCharts.bullet = character()
amCharts.bullet[valid.amCharts.shapes] = c(NA, rep('bubble', 3), 'square', rep('diamond',2), 'triangleUp', 'triangleRight', 'triangleLeft') %>% rep(2)

amCharts.linetype = numeric()
amCharts.linetype[valid.amCharts.shapes] = c(rep(0, 10), rep(1, 10))

amCharts.bar.defset = defset %<==>% list(
  # Valid classes for all dimensions
  dimclass   = list(
    x = c("character", "factor", "numeric", "integer"),
    y = c("character", "factor", "numeric", "integer")),
  multiples  = c('x', 'y'),
  essentials = c('x', 'y'),
  xLabelsRotation = 0
)

amCharts.pie.defset = defset %<==>% list(
  # Valid classes for all dimensions
  dimclass   = list(
    theta = c("numeric", "integer"),
    label = c("character", "factor")),
  multiples  = c(),
  essentials = c('theta', 'label')
)

amCharts.tsline.defset = defset %<==>% list(
  dimclass   = list(
    x = c('POSIXct', 'POSIXlt'),
    y = 'numeric',
    high = 'numeric',
    low  = 'numeric',
    shape = 'character',
    color = 'character'),
  multiples  = c('y', 'shape', 'color', 'high', 'low'),
  essentials = c('x', 'y')
)

amCharts.gauge.defset = defset %<==>% list(
  # Valid classes for all dimensions
  dimclass   = list(
    theta = c('numeric', 'integer')),
  multiples  = c(),
  essentials = 'theta',
  aggregator = mean,
  theta.min = 0,
  theta.max = 100,
  thetaAxis.tick.step = 20
  # todo: set in settings table
)

amCharts.bullet.defset = defset %<==>% list(
  # Valid classes for all dimensions
  dimclass   = list(
    x = c('numeric', 'integer'),
    y = c('numeric', 'integer')
  ),
  multiples  = c(),
  essentials = 'x or y',
  aggregator = mean,
  x.min = 0,
  x.max = 100,
  xAxis.tick.step = 20
  # todo: set in settings table
)

amCharts.funnel.defset = defset %<==>% list(
  # Valid classes for all dimensions
  dimclass   = list(
    label = c('character', 'factor'),
    y     = c('numeric', 'integer')),
  multiples  = c(),
  essentials = c('y', 'label'),
  direction = 'up.down'
  # todo: add to settings
)

amCharts.box.defset = defset %<==>% list(
  dimclass   = list(
    x = c("character", "factor", "numeric", "integer"),
    y = c("character", "factor", "numeric", "integer")),
  multiples  = c(),
  essentials = c('x', 'y')
)

amCharts.applyConfig = function(obj, config){
  amOptions(obj, theme = chif(is.null(config$theme), 'none', config$theme),
            legend = config$legend.enabled, legendPosition = config$legend.position, legendAlign = "left",
            creditsPosition = 'top-right', main = config$title, mainColor = config$title.color,
            mainSize = 15, zoom = T, scrollbar = config$yAxis.scrollbar.enabled, scrollbarHeight = config$yAxis.scrollbar.width,
            valuescrollbar = config$xAxis.scrollbar.enabled, valuescrollbarHeight = config$yAxis.scrollbar.width,
            labelRotation = config$xAxis.label.rotation)
}

amCharts.bar = function(obj, x = NULL, y = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(rAmCharts), "Package rAmCharts is not installed!", err_src = match.call()[[1]])
  config = amCharts.bar.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
    verifyConfig(plotter = 'amCharts')

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, color = color)
  L = a$labels
  A = a$aesthetics %>% list.remove('color')

  # Preparing Table:
  obj %<>% prepare4Plot(A, config = config)

  hor     = isHorizontal(obj, L$x, L$y)
  Ly      = chif(hor, L$x, L$y); Lx = chif(hor, L$y, L$x)
  clrvect = getColorVect(Ly, L$color, config)

  amBarplot(data = obj, x = Lx, y = Ly, horiz = hor,
            stack_type    = chif(is.null(config$barMode), NULL, chif(config$barMode == gndcd(37,31,60,154,82), NULL, 'regular')),
            groups_color  = clrvect, ...) %>% amCharts.applyConfig(config)
}

amCharts.pie = function(obj, label = NULL, theta = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(rAmCharts), "Package rAmCharts is not installed!", err_src = match.call()[[1]])
  config = amCharts.pie.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
    verifyConfig(plotter = 'amCharts')


  # Preparing Aesthetics:
  a = prepareAesthetics(label = label, theta = theta)
  L = a$labels
  A = a$aesthetics

  # Preparing Table:
  obj %<>% prepare4Plot(A, config = config)

  names(obj) <- c('label', 'value')
  amPie(data = obj, ...)
}

amCharts.tsline = function(obj, x = NULL, y = NULL, shape = NULL, size = NULL, color = NULL, high = NULL, low = NULL, config = NULL, ...){
  assert(require(rAmCharts), "Package rAmCharts is not installed!", err_src = match.call()[[1]])
  config = amCharts.tsline.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, shape = shape, size = size, color = color, high = high, low = low, extend = c('shape', 'size', 'color'))
  L = a$labels
  A = a$aesthetics %>% list.remove('shape', 'size', 'color')

  # Preparing Table:
  obj %<>% prepare4Plot(A, config = config)

  NSeries = length(L$y)
  if(is.null(config$point.size)){bulsize = a$aesthetics[[gndcd(144,118,66,130)]] %>% unlist} else {bulsize = config$point.size %>% vect.extend(NSeries)}
  if(is.null(config$line.width)){linewid = a$aesthetics[[gndcd(144,118,66,130)]] %>% unlist} else {linewid = config$line.width %>% vect.extend(NSeries)}
  if(is.null(linewid)){linewid = 1}
  if(is.null(L$shape)){
    bul = NULL
    ltp = NULL
  } else {
    bul = amCharts.bullet[L$shape]
    ltp = amCharts.linetype[L$shape]
    bul[is.na(bul)] <- 'round'
    ltp[is.na(ltp)] <- 0
  }

  if(is.null(L$high) | is.null(L$low)){dat = L$y} else {
    dat = list()
    for (i in seq(L$y)){
      if(is.empty(L$high[i]) | is.empty(L$low[i])){
        dat[[i]] = L$y[i]
      } else {
        dat[[i]] = c(L$low[i], L$y[i], L$high[i])
      }
    }
  }

  amTimeSeries(obj, L$x, dat, bullet = bul, linetype = ltp, linewidth = linewid, bulletSize = bulsize,
               color  = chif(is.null(L$color), c("#2E2EFE", "#31B404", "#FF4000", "#AEB404"), L$color),
               legend = chif(is.null(config[['legend']]), T, config[['legend']]),
               main   = chif(is.null(config[['title']]), '', config[['title']]),
               ylab   = chif(is.null(config$yAxis.label), '', config$yAxis.label), ...)
}

amCharts.gauge = function(obj = data.frame(), theta = NULL, config = NULL, ...){
  # Verifications:
  assert(require(rAmCharts), "Package rAmCharts is not installed!", err_src = match.call()[[1]])
  config = amCharts.gauge.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
    verifyConfig(plotter = 'amCharts')

  # Preparing Aesthetics:
  a = prepareAesthetics(theta = theta)
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config = config)

  if(is.empty(obj)){return(NULL)}

  df = data.frame(min = numeric(), max = numeric(), color = character())
  for(e in config$thetaAxis.zone){df %<>% rbind(e %>% list.extract(c('min', 'max', 'color')) %>% as.data.frame)}
  colnames(df) <- c('start', 'end', 'color')

  config$aggregator %>% do.call(list(obj[, L$theta])) %>%
    amAngularGauge(start = config$theta.min, end = config$theta.max,
                   step  = config$thetaAxis.tick.step,
                   bands = df)
}

amCharts.funnel = function(obj, y = NULL, label = NULL, config = NULL, ...){
  # Verifications:
  assert(require(rAmCharts), "Package rAmCharts is not installed!", err_src = match.call()[[1]])
  config = amCharts.funnel.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
    verifyConfig(plotter = 'amCharts')

  label %<>% renameSeries('description')
  y     %<>% renameSeries('value')

  # Preparing Aesthetics:
  a = prepareAesthetics(y = y, label = label)
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config = config)

  if(is.empty(obj)){return(NULL)}

  obj %>% amFunnel(inverse = config$direction == 'down.up', ...)
}


amCharts.box = function(obj, x = NULL, y = NULL, config = NULL, ...){
  # Verifications:
  assert(require(rAmCharts), "Package rAmCharts is not installed!", err_src = match.call()[[1]])
  config = amCharts.box.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
    verifyConfig(plotter = 'amCharts')

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y)
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config = config)

  hor = T
  for (i in L$x){hor = hor & inherits(obj[,i], c('numeric', 'integer'))}
  for (i in L$y){hor = hor & inherits(obj[,i], c('character', 'factor'))}

  if(hor){frml = as.formula(L$x %++% '~' %++% L$y)} else {frml = as.formula(L$y %++% '~' %++% L$x)}
  amBoxplot(frml, data = obj, horiz = hor, ...)
}


amCharts.bullet = function(obj = data.frame(), x = NULL, y = NULL, config = NULL, ...){
  # Verifications:
  assert(require(rAmCharts), "Package rAmCharts is not installed!", err_src = match.call()[[1]])
  config = amCharts.bullet.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
    verifyConfig(plotter = 'amCharts')

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y)
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config = config)

  if(is.empty(obj)){return(NULL)}

  hor = is.null(L$y)

  assert(hor | is.null(x), 'For bullet chart, only one of dimensions x or y should be selected. Both of them cannot have values!')

  config$aggregator %>% do.call(list(obj[, chif(hor, L$x, L$y)])) %>%
    amBullet(min   = chif(hor, config$x.min, config$y.min),
             max   = chif(hor, config$x.max, config$y.max),
             steps = chif(hor, config$xAxis.step.enabled, config$yAxis.step.enabled),
             horiz = hor)
}

















# old functions:

#' amCharts.gauge.settings <- list(colorder = c('red', 'yellow', "#00CC00", 'purple', 'aqua', 'blue', 'yellow', 'magenta', 'cyan', 'black', 'grey', 'orange'))
#'
#'
#' amCharts.gauge.old = function(theta, legend = list(), config = NULL, ...){
#'   assert(require(rAmCharts), "Package rAmCharts is not installed!", err_src = match.call()[[1]])
#'
#'   legend$min  = verify(legend$min , 'numeric', varname = 'legend$min', default = 0.00)
#'   legend$max  = verify(legend$max , 'numeric', varname = 'legend$max', domain = c(legend$min, Inf), default = 180.00)
#'   legend$percentage  = verify(legend$percentage , 'logical', varname = 'legend$percentage', domain = c(T, F), default = F)
#'
#'   if (legend$percentage){
#'     legend$levels  = verify(legend$levels , 'numeric', varname = 'legend$levels', domain = c(0.0, 100.0), default = c(0.0, 30.0, 70.0, 100.0))
#'   } else {
#'     legend$levels  = verify(legend$levels , 'numeric', varname = 'legend$levels', default = c(legend$min, legend$min + 0.3*(legend$max - legend$min), legend$min + 0.7*(legend$max - legend$min), legend$max))
#'   }
#'
#'   legend$levels = sort(legend$levels)
#'   nn     = length(legend$levels)
#'   assert(nn > 1, "Argument 'legend$levels' needs at least two elements!", err_src = match.call()[[1]])
#'
#'   L  = legend$levels[nn] - legend$levels[1]
#'
#'   if (is.null(config)){config = amCharts.gauge.settings}
#'
#'   if (legend$percentage){
#'     bands = data.frame(start = legend$levels[- nn], end = legend$levels[- 1], color = config$colorder[sequence(nn-1)], stringsAsFactors = FALSE)
#'     ag    = amAngularGauge(x = round(100*(theta - legend$min)/(legend$max - legend$min), 2), start = 0, end = 100, step = 20, bands = bands, text = "%", ...)
#'     ag    = amCharts.gauge.apply.settings(ag, config)
#'   } else {
#'     bands = data.frame(start = legend$levels[- nn], end = legend$levels[-1], color = config$colorder[sequence(nn-1)], stringsAsFactors = FALSE)
#'     ag    = amAngularGauge(x = round(theta, 2), start = legend$levels[1], end = legend$levels[nn], step = L/5, bands = bands, ...)
#'     ag    = amCharts.gauge.apply.settings(ag, config)
#'     # %>% setProperties(fontSize = 10, adjustSize = T) #%>% amOptions(theme = 'dark', mainSize = 2, scrollbarHeight = 2)
#'   }
#'   return(ag)
#'
#' }
#'
#' amCharts.gauge.apply.settings = function(plt, config){
#'   props2bSet = c('fontSize', 'adjustSize')
#'   eval(parse(text = paste0('plt %>% setProperties(', list2Script(config, fields = props2bSet, fields_remove = 'colorder'), ')')))
#'   eval(parse(text = paste0('plt %>% amOptions(', list2Script(config, fields_remove = c('colorder', props2bSet)), ')')))
#'   return(plt)
#' }




