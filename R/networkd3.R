# networkd3.R ----------------------------------------------------------------


# Header
# Filename:       networkd3.R
# Description:    Contains functions for plotting dynamic and interactive network graphs, flowcharts and digarams using package 'networkD3'.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     28 November 2016
# Last Revision:  27 June 2018
# Version:        1.1.2
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 1.0.0     28 November 2016   Initial issue
# 1.1.0     13 May 2017        Fundamental changes, using standard aesthetics
# 1.1.1     27 June 2018       Dimension 'key' added
# 1.1.2     27 June 2018       More config properties added

networkD3.sankey.defset = defset %>% list.edit(
  dimclass = list(
    key          = c('character', 'factor', 'integer'),
    label        = c('character', 'factor'),
    group        = c('character', 'factor'),
    size         = 'numeric',
    color        = valid.classes,
    borderColor  = valid.classes,
    linkColor    = valid.classes,
    tooltip      = 'character',
    source       = c('character', 'factor', 'integer'),
    target       = c('character', 'factor', 'integer'),
    linkLength   = 'numeric',
    linkWidth    = 'numeric',
    linkLabel    = 'character',
    linkLabelColor = valid.classes,
    linkLabelSize = 'numeric'
  ),
  multiples  = c(),
  essentials = c('key', 'source', 'target'),
  link.tooltip.suffix = ''
)

networkD3.sankey = function(obj, key = NULL, label = NULL, source = NULL, target = NULL, linkWidth = NULL, config = NULL){

  obj %>% verify('list', lengths = 2, names_identical = c('nodes', 'links'), varname = 'obj', null_allowed = F, err_src = 'visNetwork.graph')
  obj$nodes %>% verify('data.frame', varname = 'obj$nodes', null_allowed = F, err_src = 'networkD3.sankey')
  obj$links %>% verify('data.frame', varname = 'obj$links', null_allowed = F, err_src = 'networkD3.sankey')

  assert(require(networkD3), "Package networkD3 is not installed!", err_src = match.call()[[1]])

  config = networkD3.sankey.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
    verifyConfig(plotter = 'networkD3')

  # Preparing Aesthetics:
  a = prepareAesthetics(key = key, label  = label, source = source, target = target, linkWidth = linkWidth)
  L = a$labels
  A = a$aesthetics

  obj$nodes %<>% prepare4Plot(A %>% list.extract('key', 'label'), config) %>% distinct_(L$key, .keep_all = T)
  obj$links %<>% prepare4Plot(A %>% list.extract('source', 'target', 'linkWidth'), config)

  assert(obj$links[, L$source] %<% obj$nodes[, L$key], "Value in column '" %++% L$source %++% "' must be a seubset of node IDs'", err_src = 'network.sankey')
  assert(obj$links[, L$target] %<% obj$nodes[, L$key], "Value in column '" %++% L$target %++% "' must be a seubset of node IDs'", err_src = 'network.sankey')

  if(!inherits(obj$nodes[, L$key], 'integer')){
    nodemap = sequence(nrow(obj$nodes)) - 1
    names(nodemap) <- obj$nodes[, L$key]

    obj$links[, L$source] = nodemap[obj$links[, L$source]] %>% unname
    obj$links[, L$target] = nodemap[obj$links[, L$target]] %>% unname
    obj$nodes[, L$key]    = nodemap[obj$nodes[, L$key]] %>% unname
  }

  sankeyNetwork(Links  = obj$links, Nodes = obj$nodes, Source = L$source,
                Target = L$target, Value = L$linkWidth, NodeID = L$label,
                units  = config$link.tooltip.suffix, fontSize = 0.5*config$node.label.size, nodeWidth = config$node.width)
}


