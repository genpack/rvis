# bubbleCloud.R ----------------------------------------------------------------

# Header
# Filename:       bubblecloud.R
# Description:    Contains functions for plotting various bubble charts from js package 'bubbleForce' using standrad inputs.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     21 June 2018
# Last Revision:  21 June 2018
# Version:        0.1.0
#
# Version   Date               Action
# -----------------------------------
# 0.1.0     21 June 2018       Initial issue with function bubbleCloud.bubble.molten()

# Default settings for package bubbleCloud:
bubbleCloud.bubble.molten.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass  = list(
    size    = valid.numeric.classes,
    group   = valid.classes,
    tooltip = 'character'),
  multiples = c(),
  essentials = c('size')
)

bubbleCloud.bubble.molten <- function(obj, size = NULL, group = NULL, tooltip = NULL, config = bubbleCloud.bubble.molten.defset, ...){
  if (is.empty(obj)){return(NULL)}

  # Verifications:
  assert(require(bubbleCloud), "Package bubbleCloud is not installed!", err_src = match.call()[[1]])
  config = bubbleCloud.bubble.molten.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  # Preparing Aesthetics:
  a = prepareAesthetics(size = size, group = group, tooltip = tooltip)
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config = config)

  if (is.null(obj$hover)){obj$hover <- ''}

  obj$x <- runif(nrow(obj),1,400)
  obj$y <- runif(nrow(obj),1,400)

  if (is.null(obj$radius)){
    obj$radius <- 5
  }

  if (is.null(obj$group)){
    obj$clusterName <- ""
    obj$cluster <- 0
  } else{
    obj$cluster <- as.numeric(factor(obj$group)) - 1
    obj$clusterName <- obj$group
  }

  clusters <- ddply(obj, .(cluster), function(r){
    r <- r[with(r, order(radius)),]
    head(r,1)
  })

  n <- nrow(obj)
  m <- nrow(clusters)

  x = list(
    n = n,
    m = m,
    data = obj,
    clusters = clusters,
    settings = config
  )

  # create widget
  htmlwidgets::createWidget(
    name = "bubbleforce",
    x,
    width = width,
    height = height,
    package = 'viser',
    sizingPolicy = htmlwidgets::sizingPolicy(
      viewer.padding = 0,
      browser.fill = TRUE
    )
  )
}


bubbleCloudOutput <- function(outputId, width = '100%', height = '500px'){
  shinyWidgetOutput(outputId, 'bubbleforce', width, height, package = 'viser')
}

renderBubbleCloud <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, bubbleCloudOutput, env, quoted = TRUE)
}
