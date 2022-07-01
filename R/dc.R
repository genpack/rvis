
# dc.R ----------------------------------------------------------------



# Header
# Filename:       dc.R
# Description:    Contains functions for plotting various charts from package 'dc' using standrad inputs.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     28 July 2017
# Last Revision:  28 July 2018
# Version:        0.0.1
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 0.0.1     28 July 2018      Initial issue

dc.bar.defset = defset %<==>% list(
  dimclass   = list(
    x       = c('character'),
    y       = 'numeric'),
  multiples  = c(),
  essentials = 'x'
)

dc.line.defset = defset %<==>% list(
  dimclass   = list(
    x       = c('numeric', 'integer', 'character'),
    y       = 'numeric',
    group   = c('character', 'factor', 'integer')),
  multiples  = c(),
  essentials = 'x'
)

dc.prepareConfig = function(config){
  config %<==>% list(
    yAxis.elastic  = T,
    yAxis.padding  = '5%',
    yAxis.offset   = 0,
    xAxis.elastic  = T,
    offset         = 0,
    legend.item.height = 13,
    legend.item.width  = 70
  )
}

dc.bar = function(obj, x = NULL, y = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(dcr), "Package dcr is not installed!", err_src = match.call()[[1]])

  config = dc.bar.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
    verifyConfig(plotter = 'dcr')

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y)
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config)

  if(ncol(obj) == 1){obj$key = as.integer(sequence(nrow(obj)))}

  obj %>% dcr::dcr(type = 'column', x = x, y = y, config = config %>% dc.prepareConfig)
}

dc.scatter = function(obj, x = NULL, y = NULL, group = NULL, shape = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(dcr), "Package dcr is not installed!", err_src = match.call()[[1]])

  config = dc.line.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
    verifyConfig(plotter = 'dcr')

  shape %<>% verify('character', lengths = 1, domain = c('line', 'point'), default = 'point')

  if(is.null(group)){group = 'Sery'}
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, group = group)
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config)

  if(is.null(config$xAxis.min)){config$xAxis.min = min(obj[, L$x])}
  if(is.null(config$xAxis.max)){config$xAxis.max = max(obj[, L$x])}

  if(shape == 'line'){type = 'scatterLine'} else {type = 'scatterPoint'}

  obj %>% dcr::dcr(type = type, x = L$x, y = L$y, group = L$group, config = config %>% dc.prepareConfig)

}

