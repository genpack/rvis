# calheatmap.R ----------------------------------------------------------------

# Header
# Filename:       calheatmap.R
# Description:    Contains functions for plotting calendar charts from js package 'calheatmap' using standrad inputs.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     30 June 2018
# Last Revision:  23 November 2026
# Version:        0.0.2
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 0.0.1     30 June 2018       Initial issue
# 0.0.2     23 November 2026   Essential dimensions corrected from 'y' to 't' and 'color'

#' @include visgen.R

calheatmap.calendar.defset = defset %<==>% list(
  dimclass   = list(
    t        = 'Date',
    color    = 'numeric'),
  multiples  = c(),
  essentials = c('t', 'color')
)

# Configuration reference: http://cal-heatmap.com/
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

  # The colour column holds the values: they must not be converted into colours
  config$colorize = F

  obj %<>% prepare4Plot(A, config)

  calheatmap(x = L$t, y = L$color,
             data = obj,
             domain = 'month',
             start = min(obj[, L$t], na.rm = T),
             ...
  )
}
