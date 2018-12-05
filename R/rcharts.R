# rcharts.R ----------------------------------------------------------------


# Header
# Filename:       rcharts.R
# Description:    Contains functions for plotting various charts from rCharts package using standrad inputs.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     31 March 2017
# Last Revision:  12 May 2017
# Version:        1.1.3
#

# Version History:

# Version   Date                Action
# ----------------------------------
# 1.0.0     31 March 2017       Initial issue
# 1.0.1     31 March 2017       rCharts.scatter.defset added
# 1.0.2     14 April 2017       rCharts.scatter modified
# 1.1.0     12 May 2017         dimple charts separated and transferred to dimple.R
# 1.1.1     12 May 2017         nvd3 charts separated and transferred to nvd3.R
# 1.1.2     12 May 2017         xCharts charts separated and transferred to xCharts.R
# 1.1.3     12 May 2017         highcharts charts separated and transferred to highcharts.R


# Incomplete:
rCharts.scatter = function(obj, x = NULL, y = NULL, size = NULL, shape = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(rCharts), "Package rCharts is not installed!", err_src = match.call()[[1]])
  config = rCharts.scatter.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, size = size, shape = shape, color = color)
  L = a$labels
  A = a$aesthetics %>% list.remove('shape') # Only x, y, color, size will be columns of the table

  obj %<>% prepare4Plot(A, config = config)

  rcharts.type = c(square = 'scatter', bubble = 'bubble', point = 'scatter', circle = 'bubble', bar   = ifelse(config$horizontal, 'bar', 'column'), line  = 'line')
  # warn if bar is not among shapes, horizontal does not work!

  L$shape = translateShape[L$shape]
  if(is.empty(L$shape)){L$shape = 'scatter'}
  L$shape[is.na(L$shape)] == 'bubble'

  hPlot(x = L$x, y = L$y, data = obj,
        type = L$shape, group = L$group, size = L$size)

}

