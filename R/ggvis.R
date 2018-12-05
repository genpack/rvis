
# ggvis.R ----------------------------------------------------------------

# Header
# Filename:       ggvis.R
# Description:    Contains functions for plotting various charts from package 'gvis' using standrad inputs.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     14 April 2017
# Last Revision:  14 April 2017
# Version:        0.0.1
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 0.0.1     14 April 2017      Initial issue


ggvis.shape = c('+' = 'cross', plus = 'cross')


ggvis.scatter.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    x       = 'numeric',
    y       = 'numeric',
    size    = 'numeric',
    color   = valid.classes,
    shape   = 'character'),
  multiples  = c(),
  essentials = c('x', 'y'),
  colorize   = F
)

ggvis.histogram.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    x       = 'numeric'),
  multiples  = c(),
  essentials = c('x')
)

ggvis.scatter = function(obj, x = NULL, y = NULL, size = NULL, color = NULL, shape = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(ggvis), "Package ggvis is not installed!", err_src = match.call()[[1]])

  config = ggvis.scatter.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, size = size, shape = shape, color = color, extend = c('y', 'x', 'size', 'shape', 'color'))
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config)

  # if(is.null(L$shape)){L$shape = 'point'}
  # L$shape = translateShape[L$shape]
  # names(L$shape) <- NULL
  symbol = L$shape


  scr = paste("obj %>% ggvis(x = ~", L$x, ", y = ~", L$y,
              chif(is.null(config$point.color),
                   chif(is.null(L$color), "",
                        chif(inherits(obj[, L$color], 'character'),
                             ", fill := '" %++% obj[1, L$color] %++% "'",
                             ", fill = ~" %++% L$color)),
                   ", fill  := '" %++% config$point.color %++% "'"),
              chif(is.null(config$point.size ),
                   chif(is.null(L$size ), "" ,
                        ", size = ~" %++% L$size),
                   ", size  :=  " %++% config$point.size),
              chif(is.null(config$point.shape),
                   chif(is.null(L$shape), "" ,
                        chif(inherits(obj[, L$shape], 'character'),
                             ", shape := '" %++% ggvis.shape[obj[1, L$shape]]  %++% "'",
                             ", shape = ~" %++% L$shape)),
                   ", size  :=  '" %++% ggvis.shape[config$point.shape]  %++% "'"),
              chif(is.null(config$point.border.color), "" , ", stroke := '" %++% config$point.border.color %++% "'"),
              chif(is.null(config$point.opacity), "" , ", opacity := " %++% config$point.opacity), ", ...)",
              chif(is.null(config$point.tooltip), "" , "%>% add_tooltip(" %++% config$point.tooltip %++% ")"),
              "%>% layer_points")

  parse(text = scr) %>% eval
}

ggvis.histogram = function(obj, x = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(ggvis), "Package ggvis is not installed!", err_src = match.call()[[1]])

  config = ggvis.histogram.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x)
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config)

  g   = obj %>% ggvis(as.formula('~' %++% L$x))
  scr = paste("layer_histograms(g ",
              chif(is.null(config$hist.bar.width), "", ", width = " %++% config$hist.bar.width),
              chif(is.null(config$hist.bar.center), "", ", center = " %++% config$hist.bar.center), ")")
  parse(text = scr) %>% eval

}

