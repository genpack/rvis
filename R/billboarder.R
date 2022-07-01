# billboarder.R ----------------------------------------------------------------

# Header
# Filename:       billboarder.R
# Description:    Contains functions for plotting various billboarder charts from package billboarder using standrad inputs.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     05 March 2018
# Last Revision:  17 September 2018
# Version:        0.1.1
#

# Version History:

# Version   Date                Action
# ----------------------------------
# 0.0.1     05 March 2018       Initial issue
# 0.0.2     25 June 2018        All config validations transferred to visgen
# 0.0.3     25 June 2018        Function applyConfig() modified: more config properties supported
# 0.0.4     25 June 2018        Function applyColors() added: applies property config$point.color
# 0.0.5     25 June 2018        Function applyColors() added: applies property config$point.color if argument color is NULL
# 0.0.6     25 June 2018        pie chart added
# 0.0.7     25 June 2018        tsline chart added
# 0.0.8     18 July 2018        Config property point.color renamed to color
# 0.0.9     24 July 2018        billboarder.bar.molten() embedded in added function billboarder.bar()
# 0.1.0     24 July 2018        billboarder.bb_opt() removed: applyConfig() and applyColor() now directly modifies object property and data lists
# 0.1.1     17 Spetember 2018   billboarder.tsarea() added

billboarder.bar.defset = defset %<==>% list(
  # Valid classes for all dimensions
  dimclass   = list(
    x     = c('character', 'factor', 'numeric', 'integer'),
    y     = c('numeric','integer', 'character', 'factor'),
    group = c('factor', 'character')),
  multiples  = c('x', 'y'),
  essentials = c('x', 'y')
)

# valid.yAxis.label.positions = c('top', 'middle', 'bottom')
# valid.yAxis.label.positions %<>% c(
#   paste('inner', valid.yAxis.label.positions, sep = '-'),
#   paste('outer', valid.yAxis.label.positions, sep = '-'), 'inner', 'outer')
#
# valid.xAxis.label.positions = c('left', 'right')
# valid.xAxis.label.positions %<>% c(
#   paste('inner', valid.xAxis.label.positions, sep = '-'),
#   paste('outer', valid.xAxis.label.positions, sep = '-'), 'inner', 'outer')
# valid.legend.positions = c('right', 'bottom', 'top-right', 'top-left', 'bottom-right', 'bottom-left')



billboarder.scatter.defset = defset %<==>% list(
  # Valid classes for all dimensions
  dimclass   = list(
    x     = c('numeric','integer'),
    y     = c('numeric','integer'),
    group = c('factor', 'character')),
  multiples  = c(),
  essentials = c('x', 'y')
)

billboarder.pie.defset = defset %<==>% list(
  # Valid classes for all dimensions
  dimclass   = list(
    label = 'character',
    theta = c('numeric','integer')),
  multiples  = c(),
  essentials = c('label', 'theta')
)

billboarder.tsline.defset = defset %<==>% list(
  # Valid classes for all dimensions
  dimclass   = list(
    x = c('Date', 'POSIXct'),
    y = c('numeric','integer')),
  multiples  = c('y'),
  essentials = c('x', 'y'),
  subchart.enabled = TRUE
)



billboarder.applyColors <- function(bb, colorlist, opacity = 1) {
  bb$x$bb_opts[["billboarderspecials"]][["opacity"]] <- opacity
  bb$x$bb_opts[["data"]] <- bb$x$bb_opts[["data"]] %<==>% list(colors = colorlist)
  return(bb)
}

billboarder.applyConfig = function(bb, config){
  nin = config$legend.position %in% c('buttom', 'right')
  if(!is.null(config$point.label.enabled)){
    bb %<>% bb_data(labels = config$point.label.enabled)
  }

  # Apply xAxis and yAxis label formats:
  if     (!is.null(config$xAxis.tick.label.format)){xAxTckFrmt = config$xAxis.tick.label.format}
  else if(!is.null(config$xAxis.tick.label.suffix)){xAxTckFrmt = billboarder.format.suffix.js(config$xAxis.tick.label.suffix)}
  else {xAxTckFrmt = NULL}

  if     (!is.null(config$yAxis.tick.label.format)){yAxTckFrmt = config$yAxis.tick.label.format}
  else if(!is.null(config$yAxis.tick.label.suffix)){yAxTckFrmt = billboarder.format.suffix.js(config$yAxis.tick.label.suffix)}
  else {yAxTckFrmt = NULL}

  bb$x$bb_opts[["grid"]] <- bb$x$bb_opts[["grid"]] %<==>% list(
    y = list(show = config$yAxis.grid.enabled),
    x = list(show = config$xAxis.grid.enabled)
  )

  bb$x$bb_opts[["axis"]] <- bb$x$bb_opts[["axis"]] %<==>% list(
    y = list(tick  = list(format = yAxTckFrmt, fit = FALSE),
             label = list(text = config$yAxis.label, position = config$yAxis.label.position)),
    x = list(tick  = list(format = xAxTckFrmt, fit = FALSE),
             label = list(text = config$xAxis.label, position = config$xAxis.label.position))
  )

  bb$x$bb_opts[["legend"]] <- bb$x$bb_opts[["legend"]] %<==>% list(
    show     = config$legend.enabled,
    position = chif(nin, config$legend.position, 'inset'),
    inset    = chif(nin, NULL, list(anchor = config$legend.position))
  )

  bb$x$bb_opts[["title"]] <- bb$x$bb_opts[["title"]] %<==>% list(
    text     = config$title,
    position = config$title.position
    # todo: padding doesn't work! check why?
    # padding  = list(top = config$title.padding.top, right = config$title.padding.right, left = config$title.padding.left, buttom = config$title.padding.buttom)
  ) %>% list.clean

  bb$x$bb_opts[["caption"]] <- bb$x$bb_opts[["caption"]] %<==>% list(
    text = config$subtitle
  )

  if(config$stack.enabled){
    bb$x$bb_opts[["data"]][["groups"]] <- config$stack.groups
  }

  bb$x$bb_opts[["point"]][["r"]] <- config$size

  bb$x$bb_opts[["subchart"]] <- bb$x$bb_opts[["subchart"]] %<==>% list(
    show = config$subchart.enabled,
    size = list(height = config$subchart.height)
  )

  return(bb)
}

billboarder.bar = function(obj, x = NULL, y = NULL, group = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}

  assert(require('billboarder'), "Package 'billboarder' is not installed!", err_src = match.call()[[1]])
  (billboarder.bar.defset %<==>% config) %>%
    verifyConfig(plotter = 'billboarder') %>%
    verifyConfigDimProperties(dims = 'color') -> config

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, group = group, color = color)
  L = a$labels
  A = a$aesthetics %>% list.remove('color')

  obj %<>% prepare4Plot(A, config = config)

  hor = T
  for (i in L$x){hor = hor & inherits(obj[,i], c('numeric', 'integer'))}
  for (i in L$y){hor = hor & inherits(obj[,i], c('character', 'factor'))}

  # apply series colors:
  Ly      = chif(hor, L$x, L$y); Lx = chif(hor, L$y, L$x)
  clrlist = getColorList(Ly, L$color, config)

  if(!is.null(L$group)){
    assert(length(L$x) == 1 & length(L$y) == 1, 'You can define series by either grouping the values or selecting multiple columns!')
    bb = billboarder(data = obj) %>%
      bb_aes_string(x = Lx, y = Ly, group = L$group) %>% bb_barchart(rotate = hor, ...)
  } else {
    if(length(Ly) > 1){
      bb = billboarder() %>% bb_barchart(data = obj[, c(Lx, Ly)], color = L$color, stacked = config$stack.enabled, rotate = hor, ...)
    } else {
      bb = billboarder(data = obj) %>%
        bb_aes_string(x = Lx, y = Ly) %>% bb_barchart(rotate = hor, color = L$color[1], ...)
    }
  }
  bb %>% billboarder.applyColors(clrlist) %>% billboarder.applyConfig(config)
}

billboarder.scatter = function(obj, x = NULL, y = NULL, group = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}

  assert(require('billboarder'), "Package 'billboarder' is not installed!", err_src = match.call()[[1]])
  config = billboarder.scatter.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, group = group)
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config = config)

  billboarder() %>%
    bb_scatterplot(data = obj, x = L$x, y = L$y, group = L$group) %>% billboarder.applyConfig(config)

}

billboarder.pie = function(obj, label = NULL, theta = NULL, config = NULL, ...){
  assert(require('billboarder'), "Package 'billboarder' is not installed!", err_src = match.call()[[1]])
  config = billboarder.pie.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>% verifyConfig(plotter = 'billboarder')

  # Preparing Aesthetics:
  a = prepareAesthetics(label = label, theta = theta)
  L = a$labels
  A = a$aesthetics

  if ('rownames' %in% L$label){obj %<>% rownames2Column('rownames') %>% as.data.frame}

  obj %<>% prepare4Plot(A, config = config)

  # Verifications:
  # if (is.empty(obj)){return(NULL)}

  billboarder() %>%
    bb_piechart(data = obj) %>%
    bb_labs(title   = config$title,
            caption = config$subtitle)
}

billboarder.tsLineChart = function(obj, x = NULL, y = NULL, color = NULL, shape = 'spline', config = NULL, ...){

  assert(require('billboarder'), "Package 'billboarder' is not installed!", err_src = match.call()[[1]])
  config = billboarder.tsline.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
    verifyConfig(plotter = 'billboarder') %>%
    verifyConfigDimProperties(dims = 'color')

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, color = color)
  L = a$labels
  A = a$aesthetics %>% list.remove('color')

  if ('rownames' %in% L$y){obj %<>% rownames2Column('rownames') %>% as.data.frame}

  if(is.null(config$stack.groups)){config$stack.groups <- list(as.list(L$y))}

  obj %<>% prepare4Plot(A, config = config)

  # apply series colors:
  if(!is.null(L$color)){clrlist = L$color %>% vect.extend(length(L$y)) %>% as.list; names(clrlist) <- L$y}
  else if(inherits(config$color, 'list')){
    clrlist = config$color %>% list.extract(L$y)
  }
  else {clrlist = list()}

  billboarder() %>% bb_linechart(data = obj, type = shape) %>%
    billboarder.applyColors(clrlist) %>%
    billboarder.applyConfig(config)
}

billboarder.tsline = function(obj, x = NULL, y = NULL, color = NULL, config = NULL, ...){
  obj %>% billboarder.tsLineChart(x = x, y = y, color = color, config = config, ...)
}

billboarder.tsarea = function(obj, x = NULL, y = NULL, color = NULL, config = NULL, ...){
  obj %>% billboarder.tsLineChart(x = x, y = y, color = color, shape = 'area', config = config, ...)
}

