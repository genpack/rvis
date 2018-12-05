
# coffeewheel.R ----------------------------------------------------------------



# Header
# Filename:       coffeewheel.R
# Description:    Contains functions for plotting pie charts in form of zoomable sunbursts using CoffeeWheel package from D3.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     14 April 2017
# Last Revision:  29 August 2017
# Version:        0.0.3
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 0.0.1     14 April 2017      Initial issue
# 0.0.2     20 July 2017       widget embedded in the package
# 0.0.3     29 August 2017     config$tree.function added to apply user's desired function to nibe tree elements: leafs and branches

#' @include visgen.R

coffeewheel.sunburst.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    theta   = 'numeric',
    label   = c('character', 'factor'),
    color   = valid.classes),
  multiples  = 'label',
  essentials = c('theta', 'label'),
  aggregator.function.string = 'sum',
  labelTheta = T
)

tree2cwTree = function(tr, attributes = c('colour', 'value')){
  cwt = list()
  for (e in tr){
    if(inherits(e, gndcd(160,152,6,80))){
      nwitm = list(name = e$tree_name)
      for (attr in attributes){nwitm[[attr]] = e[[attr]]}
      nwitm$children = tree2cwTree(e, attributes)
      cwt %<>% list.add(nwitm)
    } else if (inherits(e, 'list')){
      nwitm = list(name = e$leaf_name)
      for (attr in attributes){nwitm[[attr]] = e[[attr]]}
      cwt %<>% list.add(nwitm)
    }
  }
  return(cwt)
}

coffeewheel.prepareConfig = function(config){
  config$title  %<>% verify('character', default = '', varname = 'config$title')
  config$width  %<>% verify(c('integer', 'numeric'), default = 1200, varname = 'config$width') %>% as.integer
  config$height %<>% verify(c('integer', 'numeric'), default = 800, varname = 'config$height') %>% as.integer

  return(config)
}

coffeewheel <- function(treeData, width=400, height=400, main="", partitionAttribute="value") {
  x <- list(
    treeData = treeData,
    main = main,
    partitionAttribute = partitionAttribute
  );

  # create widget
  htmlwidgets::createWidget(
    name = 'coffeewheel',
    x,
    width = width,
    height = height,
    package = 'viser'
  );
}

## Widget output function for use in Shiny
coffeewheelOutput <- function(outputId, width= 400, height= 400) {
  shinyWidgetOutput(outputId, 'coffeewheel', width, height, package = 'viser');
}


## Widget render function for use in Shiny
rendercoffeewheel <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, coffeewheelOutput, env, quoted = TRUE);
}

coffeewheel.sunburst = function(obj, theta = NULL, label = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  # assert(require(coffeewheel), "Package highcharter is not installed!", err_src = match.call()[[1]])

  config = coffeewheel.sunburst.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  if (is.null(color)){
    color = list(colour = theta[[1]])
  } else {
    names(color) = 'colour'
    color %<>% as.list
  }


  # Preparing Aesthetics:
  a = prepareAesthetics(theta = theta, label = label, color = color)
  L = a$labels
  A = a$aesthetics

  obj       %<>% prepare4Plot(A, config)
  config    %<>% coffeewheel.prepareConfig
  cwt = obj %>% df2tree(id_cols = L$label, var_cols = c(L$theta, L$color), func = c(config$aggregator.function.string, 'color.mean'), name = 'ROOT')
  if(!is.null(config$tree.function)){cwt %<>% treeApply(func = config$tree.function)}

  cwt %>% tree2cwTree(attributes = c(L$theta, L$color)) %>%
    coffeewheel(width = config$width, height = config$height, main = config$title, partitionAttribute = L$theta)
}

