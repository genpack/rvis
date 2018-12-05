# rbokeh.R ----------------------------------------------------------------



# Header
# Filename:       rbokeh.R
# Description:    Contains functions for plotting various charts from package 'rbokeh' using standrad inputs.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     24 April 2017
# Last Revision:  24 April 2017
# Version:        0.0.1
#

# Version History:

# Version   Date               Action
# -----------------------------------
# 0.0.1     24 April 2017      Initial issue


rbokeh.scatter.defset = defset %>% list.edit(
  dimclass   = list(
    x       = 'numeric',
    y       = 'numeric',
    color   = valid.classes,
    shape   = 'character'),
  multiples  = c('y', 'shape', 'color'),
  essentials = c('x', 'y'),
  colorize = F
)

rbokeh.scatter = function(obj, x = NULL, y = NULL, color = NULL, shape = NULL, config = NULL, ...){
  # todo: add size, fix tooltip and other properties
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(rbokeh), "Package rbokeh is not installed!", err_src = match.call()[[1]])

  config = rbokeh.scatter.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  if(is.null(shape)){shape = 'point'}
  # Preparing Aesthetics:
  # a = prepareAesthetics(x = x, y = y, color = color, shape = shape, extend = c('y', 'color', 'shape'))
  a = prepareAesthetics(x = x, y = y, color = color, shape = shape)
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config)

  scr = gndcd(33,148,76,154,1,94) %++% "(data = obj, title = config$title, xlab = config$xAxis.label, ylab = config$yAxis.label, ...)"
  for (i in seq(L$y)){
    funstr = chif(L$shape[i] == 'line', "ly_lines(ly_points(")
    if (is.empty(funstr)){funstr = "ly_points("}
    if(is.empty(L$shape[i])){glph = NULL}
    else if ((L$shape[i] %in% names(obj)) & (obj[,L$shape[i]] %>% unique %>% length > 1))
    {glph = L$shape[i]} else {glph = NULL}
    scr %<>% paste("%>%", funstr, "x =", L$x, ",", "y =", L$y[i], ", data = obj",
                   chif(is.empty(L$color[i]), "", ", color = " %++% L$color[i]),
                   chif(is.empty(glph), "", ", glyph = " %++% glph),
                   # chif(is.empty(config$tooltip), "", ", hover = list(" %++% paste(config$tooltip, collapse = ',')  %++% ")"),
                   ")")
  }

  parse(text = scr) %>% eval
}
