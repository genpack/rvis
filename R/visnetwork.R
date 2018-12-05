# visnetwork.R ----------------------------------------------------------------

# Header
# Filename:       visnetwork.R
# Description:    Contains functions for plotting dynamic and interactive network graphs, flowcharts and digarams using visNetwork.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     09 November 2016
# Last Revision:  27 June 2018
# Version:        1.1.5
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 1.0.0     09 November 2016   Initial issue
# 1.0.1     10 February 2017   Functions exported
# 1.1.0     19 April 2017      Fundamental changes: Using prepareAesthetics() from visgen.R using standard dim inputs.
# 1.1.1     20 April 2017      Link length, color and some other features added. Old functions deleted.
# 1.1.2     10 May 2017        The named character vector 'visNetwork.shape' added to translate shapes to visNetwork language
# 1.1.3     11 May 2017        Config properties: 'layout' and 'direction' added. Global variable 'visNetwork.shape' modified: added more shapes
# 1.1.4     27 June 2018       Config properties: more config properties added.
# 1.1.5     27 June 2018       Dimension 'key' added to specify id of nodes


visNetwork.graph.defset = defset %>% list.edit(
  dimclass   = list(
    key     = c('character', 'factor', 'integer'),
    label   = c('character', 'factor'),
    group   = c('character', 'factor'),
    shape   = 'character',
    size    = 'numeric',
    color    = valid.classes,
    borderColor    = valid.classes,
    linkColor    = valid.classes,
    tooltip = 'character',
    source  = c('character', 'factor', 'integer'),
    target  = c('character', 'factor', 'integer'),
    linkLength  = 'numeric',
    linkWidth  = 'numeric',
    linkLabel  = 'character',
    linkLabelColor = valid.classes,
    linkLabelSize = 'numeric'
  ),
  multiples  = c(),
  essentials = c('key', 'source', 'target'),
  palette = list(color           = c('white', 'navy'),
                 linkColor       = c('gray',  'black'),
                 labelColor      = c('black', 'black'),
                 linkLabelColor  = c('black', 'black'),
                 borderColor     = c('black', 'black')),

  essentials = c('key', 'source', 'target')
)

#
# ellipse, circle, database, box/rectangle, text are fixed size, label appears inside the node. Their size don not change by property 'config$node.size' or column 'size'
# image, circularImage, diamond/rhombus, bubble/dot, star, triangle/delta, triangleDown/curl, square and icon: size is determined by column 'size' or 'config$node.size'. column values outweight config value!

visNetwork.shape = c(square = 'square', delta = 'triangle', curl = 'triangleDown', icon = 'icon', triangleDown = 'triangleDown', point = 'dot',
                     triangle = 'triangle', box = 'box', rectangle = 'box', dot = 'dot', bubble = 'dot', circle = gndcd(88,4,1,18,199,94), bubble = 'circle',
                     star = 'star', ellipse = 'ellipse', cylinder = "database", diamond = 'diamond', rhombus = 'diamond',
                     image = 'image', circularImage = 'circularImage', database = 'database', text = 'text')

valid.visNetwork.shapes = visNetwork.shape %>% names

visNetwork.direction = c(up.down = 'UD', left.right = 'LR', right.left = 'RL', down.up = 'DU')
valid.visNetwork.directions = names(visNetwork.direction)

visNetwork.applyConfig = function(net, config){
  node.shadow = list(enabled = config$node.shadow.enabled, size = config$node.shadow.size) %>% list.clean
  node.scale  = list(min = config$node.size.min, max = config$node.size.max) %>% list.clean
  node.color  = list(background = config$node.color, border = config$node.border.color, highlight = config$node.highlight.color) %>% list.clean
  node.label  = list(color = config$node.label.color, size = config$node.label.size, face = config$node.label.font, background = config$node.label.background) %>% list.clean # todo: more features to add

  net %<>% visNodes(shape = chif(is.null(config$node.shape), NULL, visNetwork.shape[config$node.shape] %>% unname),
                    value = config$node.size,
                    physics = config$node.physics.enabled,
                    title = config$node.tooltip,
                    label = config$node.label,
                    font  = chif(is.empty(node.label), NULL, node.label),
                    color = chif(is.empty(node.color), NULL, node.color),
                    shadow  = chif(is.empty(node.shadow), NULL, node.shadow),
                    scaling = chif(is.empty(node.scale), NULL, node.scale))

  link.color  = list(background = config$link.color, highlight = config$link.highlight.color, hover = config$link.hover.color, inherit = config$link.color.inherit, opacity = config$link.color.opacity) %>% list.clean
  link.label  = list(color = config$link.label.color, size = config$link.label.size, face = config$link.label.font, background = config$link.label.background) %>% list.clean # todo: more features to add
  link.scale  = list(min = config$link.width.min, max = config$link.width.max) %>% list.clean

  net %<>% visEdges(title  = config[['link.tooltip']],
                    value  = config[['link.size']],
                    label  = config[['link.label']],
                    length = config[['link.length']],
                    width  = config$link.width,
                    dashes = config$link.dashed, # todo: make compatible plotly and dygraphs: use config$link.shape and use dash-line or dot-line, ...
                    hidden = config$link.hidden,
                    hoverWidth          = config$link.hover.width,
                    physics = config$link.physics.enabled,
                    selectionWidth      = config$link.selection.width,
                    selfReferenceSize   = config$link.loop.size,
                    labelHighlightBold  = config$link.label.highlight.bold,
                    smooth  = config[['link.smooth']],
                    font    = chif(is.empty(link.label), NULL, link.label),
                    color   = chif(is.empty(link.color), NULL, link.color),
                    scaling = chif(is.empty(link.scale), NULL, link.scale))

  net %<>% visLayout(hierarchical = config$layout == 'hierarchical')
  # todo: add improvedLayout
  if(config$layout == 'hierarchical'){
    dir  = visNetwork.direction[config$direction] %>% unname
    net %<>% visHierarchicalLayout(
      sortMethod = 'directed', edgeMinimization = F, blockShifting = T, parentCentralization = T,
      direction  = chif(is.empty(dir), 'TD', dir))
  }
  # todo: define config properties for hierarchical ...
  # todo: fixed, physics, node.label.font, node image, node broken image, icon, shape properties
  return(net)
}

visNetwork.graph = function(obj,
                            key = NULL, label  = NULL, shape  = NULL, size = NULL, color = NULL, borderColor = NULL, tooltip = NULL,
                            source = NULL, target = NULL, linkColor = NULL, linkLength = NULL, linkWidth = NULL,
                            linkLabel = NULL, linkLabelColor = NULL, linkLabelSize = NULL,
                            config = NULL, ...){
  # Verifications:
  obj       %>% verify('list', lengths = 2, names_identical = c(gndcd(27,143,150,130,98), gndcd(55,4,158,13,144)), varname = 'obj', null_allowed = F, err_src = 'visNetwork.graph')
  obj$nodes %>% verify('data.frame', varname = 'obj$nodes', null_allowed = F, err_src = 'visNetwork.graph')
  obj$links %>% verify('data.frame', varname = 'obj$links', null_allowed = F, err_src = 'visNetwork.graph')

  assert(require(visNetwork), "Package visNetwork is not installed!", err_src = match.call()[[1]])
  config = visNetwork.graph.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
    verifyConfig(plotter = 'visNetwork')

  key %<>% renameSeries('id')
  label %<>% renameSeries('label')
  shape %<>% renameSeries('shape')
  size %<>% renameSeries(gndcd(100,11,64,19,130))
  color %<>% renameSeries('color.background')
  borderColor %<>% renameSeries('color.border')
  tooltip %<>% renameSeries(gndcd(25,118,196,55,29))
  source %<>% renameSeries(gndcd(33,110,12,200))
  target %<>% renameSeries(gndcd(136,12))
  linkLength %<>% renameSeries(gndcd(64,162,14,59,136,86))
  linkWidth %<>% renameSeries('value')
  linkColor %<>% renameSeries('color')
  linkLabel %<>% renameSeries(gndcd(199,2,142,9,171))
  linkLabelColor %<>% renameSeries('font.color')
  linkLabelSize %<>% renameSeries('font.size')

  # Preparing Aesthetics:
  a = prepareAesthetics(key = key, label  = label, shape   = shape, size = size, color = color, borderColor = borderColor, tooltip = tooltip,
                        source = source, target = target, linkColor = linkColor, linkLength = linkLength, linkWidth = linkWidth,
                        linkLabel = linkLabel, linkLabelColor = linkLabelColor, linkLabelSize = linkLabelSize)
  L = a$labels
  A = a$aesthetics

  if(is.null(L$key)){
    L$key = 'key'
    A$key = list(key = 'key')
    nodeids = rownames(obj$nodes)
    if(is.null(nodeids)){
      if(is.null(obj$nodes[, L$label])){
        stop('node IDs are not specified')
      } else {
        obj$nodes$key <- obj$nodes[, L$label]
      }
    } else {obj$nodes$key = nodeids}
  }

  obj$nodes %<>% prepare4Plot(A %>% list.extract('key', 'label', 'shape', 'size', 'tooltip', 'color', 'borderColor'), config) %>% distinct(id, .keep_all = T)
  obj$links %<>% prepare4Plot(A %>% list.extract('source', 'target', 'linkColor', 'linkLength', 'linkWidth',
                                                 'linkLabel', 'linkLabelColor', 'linkLabelSize'), config)

  # if (is.empty(obj$links)){cat("Table 'links' is empty! Nothing to plot."); return(NULL)}

  assert(obj$links[, L$source] %<% obj$nodes$id, "Value in column '" %++% L$source %++% "' must be a seubset of node IDs'", err_src = 'visNetwork.graph')
  assert(obj$links[, L$target] %<% obj$nodes$id, "Value in column '" %++% L$target %++% "' must be a seubset of node IDs'", err_src = 'visNetwork.graph')

  if(!is.null(config$node.size.min) & !is.null(config$node.size.min) & !is.null(L$size)){
    obj$nodes$value %<>% vect.map(min = config$node.size.min, max = config$node.size.max)
  }

  # if(!is.null(config$link.width.min) & !is.null(config$link.width.max) & !is.null(L$linkWidth)){
  #   obj$links$value %<>% vect.map(min = config$link.width.min, max = config$link.width.max)
  # }

  if(!is.null(config$link.length.min) & !is.null(config$link.length.max) & !is.null(L$linkLength)){
    obj$links$length %<>% vect.map(min = config$link.length.min, max = config$link.length.max)
  }

  obj$links$arrows = 'to'
  obj$nodes$shadow = config$node.shadow.enabled %>% verify('logical', domain = c(T,F), default = T)
  # todo: add dimensions and in coonfig when requried also other dimensions like hidden(visible), image, mass, borderWidth, borderWidthSelected, labelHighlightBold...
  if(!is.null(obj$nodes$shape)){obj$nodes$shape <- visNetwork.shape[obj$nodes$shape] %>% unname}

  if (is.null(L$labelColor) & !is.null(L$color)){
    obj$nodes$font.color <- obj$nodes$color.background %>%
      niragen::contrastColors()
  }

  visNetwork(obj$nodes, obj$links, ...)  %>% visNetwork.applyConfig(config)
}







## OLD FUNCTIONS:



#sizeColumn
#sizeLegend

'Old'
#' @export
visNetwork.graphChart = function(nodes, links, size = NULL, sizeLegend = NULL, shape = NULL, shapeLegend = NULL, color = NULL, colorLegend = NULL, label = NULL, linkColor = NULL, linkLength = NULL, linkLengthLegend = NULL, linkSource = NULL, linkTarget = NULL, linkWidth = NULL, ...){
  # Currently only valid.visNetwork.linkTypes are supported, todo: generate values for categorical fields
  # todo: add linkType to arguments, valid.linkTypes = c('continuous', 'dashed', ...)
  # linkType   = verify(linkType, 'character', lengths = 1, domain = valid.visNetwork.linkTypes, default = 'continuous', varname = 'linkType')
  # shadow for nodes

  return(v)
}


