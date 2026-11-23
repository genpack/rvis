# amCharts.R ----------------------------------------------------------------
# Header
# Filename:       amcharts.R
# Description:    Contains functions for plotting various charts from package amCharts using standrad inputs.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     28 November 2016
# Last Revision:  23 November 2026
# Version:        1.2.0
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
# 1.2.0     23 November 2026   Bug fix: shape lookup vector renamed from amCharts.bullet to amCharts.bullet.shape. It was overwritten
#                              by function amCharts.bullet(), so amCharts.tsline() failed whenever 'shape' was given.
#                              Duplicate definition of 'defset' removed (single definition in visgen.R). Old commented code removed.

#' @include visgen.R

symbols = c('point', 'circle', 'bubble', 'square', 'rhombus', 'diamond', 'delta', 'right', 'left')
ltypes  = c('line', 'dashLine')
valid.amCharts.shapes = c('line', paste(ltypes[1], symbols, sep = '.'), 'dashLine', paste(ltypes[2], symbols, sep = '.'))

# amCharts bullet type and line type associated with each rvis shape:
amCharts.bullet.shape = character()
amCharts.bullet.shape[valid.amCharts.shapes] = c(NA, rep('bubble', 3), 'square', rep('diamond',2), 'triangleUp', 'triangleRight', 'triangleLeft') %>% rep(2)

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
)

amCharts.funnel.defset = defset %<==>% list(
  # Valid classes for all dimensions
  dimclass   = list(
    label = c('character', 'factor'),
    y     = c('numeric', 'integer')),
  multiples  = c(),
  essentials = c('y', 'label'),
  direction = 'up.down'
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
    bul = amCharts.bullet.shape[L$shape]
    ltp = amCharts.linetype[L$shape]
    bul[is.na(bul)] <- 'round'
    ltp[is.na(ltp)] <- 0
  }

  # A series with both 'low' and 'high' is passed as c(low, y, high) to show the range:
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

  # Gauge bands come from config$thetaAxis.zone: a list of lists with elements min, max and color
  zones <- config$thetaAxis.zone
  if (length(zones)) {
    bands <- lapply(zones, function(zone) {
      data.frame(start = zone$min, end = zone$max, color = zone$color,
                 stringsAsFactors = FALSE)
    })
    df <- do.call(rbind, bands)
  } else {
    df <- data.frame(start = numeric(), end = numeric(), color = character())
  }

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

  hor <- isHorizontal(obj, L$x, L$y)

  if (hor) {
    formula <- stats::reformulate(
      termlabels = paste0("`", L$y, "`"),
      response = paste0("`", L$x, "`")
    )
  } else {
    formula <- stats::reformulate(
      termlabels = paste0("`", L$x, "`"),
      response = paste0("`", L$y, "`")
    )
  }
  amBoxplot(formula, data = obj, horiz = hor, ...)
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
