# bpexploder.R ----------------------------------------------------------------

# Header
# Filename:       bpexploder.R
# Description:    Contains functions for plotting interactive box plots from bpexploder javascript package using standrad inputs.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     23 May 2018
# Last Revision:  18 July 2018
# Version:        0.0.2
#

# Version History:

# Version   Date                Action
# ----------------------------------
# 0.0.1     23 May 2018         Initial issue
# 0.0.2     18 July 2018        bpexploder.box.molten() renamed to bpexploder.box: Changed argument group to x

# this is a molten chart:
bpexploder.box.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    y = c('numeric','integer'),
    x = c('factor', 'character', 'integer')),
  multiples  = c(),
  essentials = c( 'y', 'x'),
  palette = list(color = NULL)
)

bpexploder.box = function(obj, y = NULL, group = NULL, config = NULL, ...){
  # Verifications:
  # if (is.empty(obj)){return(NULL)}
  assert(require(bpexploder), "Package bpexploder is not installed!", err_src = match.call()[[1]])
  config = bpexploder.box.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  config$feedAdditionalColumns %<>% c(config$point.tooltip %>% names)

  # Preparing Aesthetics:
  a = prepareAesthetics(y = y, x = x)
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config = config)


  bpexploder(data = obj,
             settings = list(
               groupVar = L$x,
               levels   = levels(obj[, L$x]),
               yVar     = L$y,
               tipText = config$point.tooltip %>% verify('list', names_domain = names(obj), default = list()),
               relativeWidth = config$point.size %>% verify('numeric', domain = c(0,1), default = 0.75))
  )
}



