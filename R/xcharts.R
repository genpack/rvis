# xcharts.R ----------------------------------------------------------------

# Header
# Filename:       xcharts.R
# Description:    Contains functions for plotting various xCharts from rCharts package using standrad inputs.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     12 May 2017
# Last Revision:  12 May 2017
# Version:        1.0.0
#

# Version History:

# Version   Date                Action
# ----------------------------------
# 1.0.0     12 Mary 2017       Initial issue separated from rCharts.R

xCharts.area.molten.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    x     = c('character', 'factor'),
    y     = c('numeric','integer'),
    group = 'factor'),
  multiples  = 'group',
  essentials = c('x', 'y')
)

xCharts.area.molten = function(obj, x = NULL, y = NULL, group = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(rCharts), "Package rCharts is not installed!", err_src = match.call()[[1]])
  config = xCharts.area.molten.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, group = group)
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config = config)

  xPlot(as.formula(paste(L$y, '~', L$x)), group = L$group, data = obj, type = "line-dotted")
}




