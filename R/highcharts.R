# highcharts.R ----------------------------------------------------------------


# Header
# Filename:       highcharts.R
# Description:    Contains functions for plotting various charts from package highcharts plus and rCharts hPlots using standrad inputs.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     12 May 2017
# Last Revision:  23 November 2026
# Version:        1.0.1
#

# Version History:

# Version   Date                Action
# ----------------------------------
# 1.0.0     12 May 2017         Initial issue separated from rCharts.R
# 1.0.1     23 November 2026    Bug fixes: wrong defset name (rCharts.highcharts.scatter.molten.defset) corrected; unknown shapes
#                               are now set to 'bubble' (a comparison '==' was used instead of an assignment).
#                               Shape translation moved to shared function rCharts.hPlotType().

#' @include visgen.R

highcharts.scatter.molten.defset = defset %<==>% list(
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

# Translates rvis shapes into rCharts hPlot chart types.
# Missing shapes give 'scatter', unknown shapes give 'bubble'.
# horizontal: if TRUE, shape 'bar' is translated to horizontal bars, otherwise to columns.
rCharts.hPlotType = function(shape, horizontal = F){
  translateShape = c(square = 'scatter', bubble = 'bubble', point = 'scatter', circle = 'bubble', bar = ifelse(isTRUE(horizontal), 'bar', 'column'), line  = 'line')
  if(is.empty(shape)){return('scatter')}
  type = translateShape[shape]
  type[is.na(type)] <- 'bubble'
  return(unname(type))
}

# Use with molten table:
# valid shapes are: square, circle, bar, line
# Note: size only affect circles
# Note: config$horizontal is affective only if there is at least one bar among shapes.
highcharts.scatter.molten = function(obj, x = NULL, y = NULL, size = NULL, shape = NULL, group = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(rCharts), "Package rCharts is not installed!", err_src = match.call()[[1]])
  config = highcharts.scatter.molten.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, size = size, shape = shape, group = group)
  L = a$labels
  A = a$aesthetics %>% list.remove('shape') # Only x, y, group, size will be columns of the table

  obj %<>% prepare4Plot(A, config = config)

  hPlot(x = L$x, y = L$y, data = obj,
        type = rCharts.hPlotType(L$shape, config$horizontal), group = L$group, size = L$size)
}
