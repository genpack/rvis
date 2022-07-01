# morrisjs.R ----------------------------------------------------------------



# Header
# Filename:       morrisjs.R
# Description:    Contains functions for plotting various charts from package 'morrisjs' using standrad inputs.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     23 April 2017
# Last Revision:  26 May 2017
# Version:        0.0.4
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 0.0.1     23 April 2017      Initial issue
# 0.0.2     26 May 2017        package is imported silently
# 0.0.3     26 May 2017        argument 'obj' can be ignored or given an empty data.frame()
# 0.0.4     26 May 2017        Function morrisjs.options() modified: corrects package bug for single color



# http://morrisjs.github.io/morris.js/lines.html

morrisjs.tsline.defset = defset %<==>% list(
  dimclass   = list(
    x       = 'Date',
    y       = 'numeric',
    color   = valid.classes),
  multiples  = 'y',
  essentials = c('x', 'y'),
  smooth = F
)

morrisjs.pie.defset = defset %<==>% list(
  dimclass   = list(
    theta    = 'numeric',
    color    = valid.classes,
    label    = 'character'),
  multiples  = c(),
  essentials = c('theta', 'label'),
  resize     = TRUE
)

morrisjs.options = function(config, color = NULL){
  # Should add more arguments
  opt = list(resize = config$resize)
  # if(is.null(color)){color = config$color}
  if(!is.null(color)){if(length(color) == 2){color = c(color, 'black')}}  # STUPID PACKAGE !!!!!!!!!!!!!!! When length of color is 1, it draws everything in black !!!!
  if (!color %>% is.empty){
    opt$colors = color
    opt$lineColors = color
    opt$barColors = color
  }
  if (!is.null(config$tooltip)){
    opt$formatter = JS(config$tooltip)
    opt$hoverCallback = JS(config$tooltip)
  }
  opt$smooth = config$smooth
  opt$ymin = config$yAxis.min
  opt$ymax = config$yAxis.max
  opt$preUnits = config$yAxis.tick.label.prefix
  opt$postUnits = config$yAxis.tick.label.suffix
  # opt$lineColors = config$color

  opt %<>% list.clean
  if(opt %>% is.empty){return (NULL)} else {return(opt)}
}

morrisjs.tsline = function(obj, x = NULL, y = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(morrisjs, quietly = T, warn.conflicts = F), "Package morrisjs is not installed!", err_src = match.call()[[1]])

  config = morrisjs.tsline.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, color = color)
  L = a$labels
  A = a$aesthetics %>% list.remove(gndcd(18,60,171,143,110))

  obj %<>% prepare4Plot(A, config)

  mx = max(obj[, L$y], na.rm = T); mn = min(obj[, L$y], na.rm = T); rndmlt = 10^((mx - mn) %>% log(10) %>% as.integer)

  if(is.null(config$yAxis.min)){config$yAxis.min <- mn %>% roundto.multiple(rndmlt, adjust = 'bottom')}
  if(is.null(config$yAxis.max)){config$yAxis.max <- mx %>% roundto.multiple(rndmlt, adjust = 'top')}

  morrisjs(obj) %>% mjsLine(option = config %>% morrisjs.options(color = L$color))
}

morrisjs.tsbar = function(obj, x = NULL, y = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  assert(require(morrisjs, quietly = T, warn.conflicts = F), "Package morrisjs is not installed!", err_src = match.call()[[1]])

  config = morrisjs.tsline.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, color = color)
  L = a$labels
  A = a$aesthetics %>% list.remove('color')

  obj %<>% prepare4Plot(A, config)

  morrisjs(obj) %>% mjsBar(option = config %>% morrisjs.options(color = L$color))
}


morrisjs.pie = function(obj = data.frame(), theta = NULL, label = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  assert(require(morrisjs, quietly = T, warn.conflicts = F), "Package morrisjs is not installed!", err_src = match.call()[[1]])
  if(is.empty(label) | is.empty(theta)){return(NULL)}
  config = morrisjs.pie.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  # Preparing Aesthetics:
  a = prepareAesthetics(theta = theta, label = label, color = color)
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config)

  if(is.empty(obj)){return(NULL)}

  list(obj[,L$label], obj[,L$theta]) %>%  morrisjs(...) %>% mjsDonut(options = morrisjs.options(config, obj[, L$color]))

}

