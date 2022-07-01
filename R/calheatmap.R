# Header
# Filename:       calheatmap.R
# Description:    Contains functions for plotting calendar charts from js package 'calheatmap' using standrad inputs.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     30 June 2018
# Last Revision:  30 June 2018
# Version:        0.0.1
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 0.0.1     30 June 2018       Initial issue

calheatmap.calendar.defset = defset %<==>% list(
  dimclass   = list(
    t        = 'Date',
    color    = 'numeric'),
  multiples  = c(),
  essentials = 'y'
)

# look at here for configurations:
# http://cal-heatmap.com/
# todo: provide config settings

calheatmap.calendar = function(obj, t = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(rChartsCalmap), "Package rChartsCalmap is not installed!", err_src = match.call()[[1]])

  config = calheatmap.calendar.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
    verifyConfig(plotter = 'calheatmap')

  # Preparing Aesthetics:
  a = prepareAesthetics(t = t, color = color)
  L = a$labels
  A = a$aesthetics

  config$colorize = F

  obj %<>% prepare4Plot(A, config)

  calheatmap(x = L$t, y = L$color,
             data = obj,
             domain = 'month',
             start = min(obj[, L$t], na.rm = T),
             ...
  )
}



