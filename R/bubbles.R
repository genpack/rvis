# bubbles.R ----------------------------------------------------------------


# Header
# Filename:       bubbles.R
# Description:    Contains functions for plotting various bubble charts from package 'bubbles' using standrad inputs.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     18 October 2016
# Last Revision:  31 March 2017
# Version:        1.1.2
#
# Version   Date               Action
# -----------------------------------
# 1.0.0     18 October 2016    Initial issue
# 1.0.1     14 March 2017      Modifications in bubbles.bubble(): Calls verifyPlotInputs() to change the table
# 1.0.2     21 March 2017      Modifications in bubbles.bubble(): Color Preparations eliminated. Will be done by verifyplotInputs() from visgen.R
# 1.1.0     27 March 2017      Major modifications in bubbles.bubble():
#                              Uses function prepare4Plot()
#                              all non-aesthetic arguments comes from config now.
# 1.1.2     31 March 2017      Calls prepareAusthetics() from visgen.R

# Shiny Inputs:
# chartID_click

# Default settings for package bubbles:
bubbles.defset = defset %>% list.edit(
  # Percentage threshold for showing the label in the bubble
  point.label.threshold = 0.0,
  # Valid classes for all dimensions
  dimclass = list(size  = valid.numeric.classes,
                  color = valid.classes,
                  label = valid.nominal.classes,
                  tooltip = 'character',
                  labelColor = valid.classes)
)

bubbles.defset$palette %<>% list.edit(
  labelColor = c("#FB1108", "#FA7806","#FBE426","#FCFB8F", "#F3F5E7", "#C7E4EA","#ABD6E6","#9AD2E1")
)


bubbles.bubble <- function(obj, size = NULL, color = NULL, label = NULL, labelColor = NULL, tooltip = NULL, config = bubbles.defset, ...){

  if (is.empty(obj)){return(NULL)}

  # Verifications:
  assert(require(bubbles), "Package bubbles is not installed!", err_src = match.call()[[1]])
  config = bubbles.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  # Preparing Aesthetics:
  a = prepareAesthetics(size = size, color = color, label = label, labelColor = labelColor, tooltip = tooltip)
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config = config)

  if ((!is.null(L$label)) & (!is.null(L$size))){
    if (!is.null(config$point.label.threshold)){
      obj[, L$label] %<>% as.character
      obj[which(vect.normalise(obj[, L$size]) < config$point.label.threshold), L$label] <- ''
    }
  }

  bubbles(value = obj[, L$size], label = obj[, L$label],
          color     = chif(is.null(color)     , "#EEEEEE", obj[, L$color]) ,
          key       = rownames(obj),
          tooltip   = chif(is.null(tooltip)   , "", obj[, L$tooltip]),
          textColor = chif(is.null(labelColor), "#333333", obj[, L$labelColor]), ...)
}



