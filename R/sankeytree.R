# sankeytree.R ----------------------------------------------------------------



# Header
# Filename:       sankeytree.R
# Description:    Contains functions for plotting interactive sankey tree charts in using package sankeyTreeR.
# Author:         Nima Ramezani
# Email :         nima.ramezani@gmail.com
# Start Date:     11 September 2018
# Last Revision:  23 November 2026
# Version:        0.0.3
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 0.0.1     11 September 2018  Initial issue
# 0.0.2     04 September 2021  Some config properties passed to the plotter
# 0.0.3     23 November 2026   dplyr verbs namespaced (failed when dplyr was not attached). Commented code removed.

#' @include visgen.R

sankeytree.tree.defset = defset %<==>% list(
  # Valid classes for all dimensions
  dimclass   = list(
    size   = 'numeric',
    label  = c('character', 'factor'),
    color   = valid.classes),
  multiples  = 'label',
  essentials = c('size', 'label'),
  aggregator = 'sum'
)

# Placeholder for sankeytree specific config verifications
sankeytree.prepareConfig = function(config){

  return(config)
}

sankeytree.tree = function(obj, label = NULL, size = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(sankeytreeR), "Package sankeytreeR is not installed!", err_src = match.call()[[1]])

  config = sankeytree.tree.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))


  # Preparing Aesthetics:
  a = prepareAesthetics(label = label, size = size, color = color)
  L = a$labels
  A = a$aesthetics

  obj       %<>% prepare4Plot(A, config)
  config    %<>% sankeytree.prepareConfig

  obj %>% treemap::treemap(index = L$label, vSize = c(L$size), vColor = L$color, fun.aggregate = config$aggregator, draw = F) %>%
  {.$tm} %>% dplyr::rename(size = vSize) %>%
  {.[, c(L$label, 'size', 'color')]} %>% d3r::d3_nest(value_cols=c("size", "color"), root = "ROOT", json = F) %>%
    dplyr::mutate(size = sum(obj[, L$size])) %>%
    d3r::d3_json(strip = T) %>%
    sankeytreeR::sankeytree(maxLabelLength = config$node.label.size.max, nodeHeight = config$node.height, tooltip = list("size"), ...)
}
