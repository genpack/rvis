# highcharts.R ----------------------------------------------------------------


# Header
# Filename:       highcharts.R
# Description:    Contains functions for plotting various charts from package highcharts plus and rCharts hPlots using standrad inputs.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     12 May 2017
# Last Revision:  12 May 2017
# Version:        1.0.0
#

# Version History:

# Version   Date                Action
# ----------------------------------
# 1.0.0     12 May 2017         Initial issue separated from rCharts.R

highcharts.scatter.molten.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    x     = c('numeric', 'character'),
    y     = c('numeric', 'character'),
    size  = 'numeric',
    group = 'factor',
    shape = 'character'),
  multiples  = c('group', 'x', 'y'),
  essentials = c('x', 'y'),
  horizontal = F
)


# Use with molten table:
# valid shapes are: square, circle, bar, line
# Note: size only affect circles
# Note: config$horizontal is affective only if there is at least one bar among shapes.
highcharts.scatter.molten = function(obj, x = NULL, y = NULL, size = NULL, shape = NULL, group = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(rCharts), "Package rCharts is not installed!", err_src = match.call()[[1]])
  config = rCharts.highcharts.scatter.molten.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, size = size, shape = shape, group = group)
  L = a$labels
  A = a$aesthetics %>% list.remove('shape') # Only x, y, group, size will be columns of the table

  obj %<>% prepare4Plot(A, config = config)

  translateShape = c(square = 'scatter', bubble = 'bubble', point = 'scatter', circle = 'bubble', bar   = ifelse(config$horizontal, 'bar', 'column'), line  = 'line')
  # warn if bar is not among shapes, horizontal does not work!

  L$shape = translateShape[L$shape]
  if(is.empty(L$shape)){L$shape = 'scatter'}
  L$shape[is.na(L$shape)] == 'bubble'

  hPlot(x = L$x, y = L$y, data = obj,
        type = L$shape, group = L$group, size = L$size)
}


