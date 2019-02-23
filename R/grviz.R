# grvis.R ----------------------------------------------------------------

# Header
# Filename:       grviz.R
# Description:    Contains functions for plotting dynamic and interactive network graphs, flowcharts and digarams using diagrammeR package.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     28 April 2018
# Last Revision:  05 November 2018
# Version:        0.1.7
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 0.1.0     28 April 2018      Initial issue
# 0.1.1     18 October 2018    function grviz.graph() modified
# 0.1.2     24 October 2018    function grviz.graph() modified: bug rectified when empty nodes and links tables are passed
# 0.1.4     05 November 2018   functions grviz.createNodeDF() & .createEdgeDF() added.
# 0.1.6     05 November 2018   functions grviz.createGraph() & .generateDot() added.
# 0.1.7     05 November 2018   functions replaceInSpec() added.


grviz.graph.defset = defset %>% list.edit(
  dimclass   = list(
    key            = c('character', 'factor', 'integer'),
    label          = c('character', 'factor'),
    group          = c('character', 'factor'),
    shape          = 'character',
    size           = 'numeric',
    color          = valid.classes,
    borderColor    = valid.classes,
    linkColor      = valid.classes,
    tooltip        = 'character',
    source         = c('character', 'factor', 'integer'),
    target         = c('character', 'factor', 'integer'),
    labelColor     = valid.classes,
    linkTooltip    = 'character',
    linkLength     = 'numeric',
    linkWidth      = 'numeric',
    linkLabel      = 'character',
    linkLabelColor = valid.classes,
    linkLabelSize  = 'numeric'
  ),
  multiples  = c(),
  palette = list(color           = c('white', 'navy'),
                 linkColor       = c('gray',  'black'),
                 labelColor      = c('black', 'black'),
                 linkLabelColor  = c('black', 'black'),
                 borderColor     = c('black', 'black')),

  essentials = c('key', 'source', 'target')
)


replaceInSpec <- function(spec){
  # Directive for marking subscripted text in a label or tooltip '@_'
  if (grepl("@_", spec)) {spec <- gsub('(label|tooltip)[ ]*=[ ]*\'(.*?)@_\\{(.*?)\\}(.*?)\'', '\\1 = <\\2<FONT POINT-SIZE=\'8\'><SUB>\\3</SUB></FONT>\\4>', spec, perl = TRUE)}
  # Directive for marking superscripted text in a label or tooltip '@_'
  if (grepl("@\\^", spec)) {spec <- gsub('(label|tooltip)[ ]*=[ ]*\'(.*?)@\\^\\{(.*?)\\}(.*?)\'', '\\1 = <\\2<FONT POINT-SIZE=\'8\'><SUP>\\3</SUP></FONT>\\4>', spec, perl = TRUE)}
  # Make a second pass to add subscripts as inline HTML
  while (grepl('(label|tooltip)[ ]*=[ ]*<(.*?)@_\\{(.+?)\\}(.*?)>', spec)) {
    spec <- gsub('(label|tooltip)[ ]*=[ ]*<(.*?)@_\\{(.*?)\\}(.*?)>','\\1 = <\\2<FONT POINT-SIZE=\'8\'><SUB>\\3</SUB></FONT>\\4>', spec, perl = TRUE)
  }
  # Make a second pass to add superscripts as inline HTML
  while (grepl('(label|tooltip)[ ]*=[ ]*<(.*?)@\\^\\{(.+?)\\}(.*?)>', spec)) {
    spec <- gsub('(label|tooltip)[ ]*=[ ]*<(.*?)@\\^\\{(.*?)\\}(.*?)>', '\\1 = <\\2<FONT POINT-SIZE=\'8\'><SUP>\\3</SUP></FONT>\\4>', spec, perl = TRUE)
  }
  # Directive for substitution of arbitrary specification text '@@'
  if (grepl("@@", spec)) {
    # Extract the spec into several pieces: first being the body, subsequent pieces belonging the replacement references
    spec_body <- unlist(strsplit(x = spec, "\\n\\s*\\[1\\]:"))[1]
    spec_references <- paste0("[1]:", unlist(strsplit(x = spec, "\\n\\s*\\[1\\]:"))[2])
    # Split the references into a vector of R statements
    split_references <- gsub("\\[[0-9]+\\]:[ ]?", "", unlist(strsplit(x = spec_references, "\\n")))
    # Evaluate the expressions and save into a list object
    for (i in 1:length(split_references)) {
      if (i == 1) {eval_expressions <- list()}
      eval_expressions <- c(eval_expressions, list(eval(parse(text = split_references[i]))))
    }
    # Make replacements to the spec body for each replacement that has no hyphen
    for (i in 1:length(split_references)) {
      while (grepl(paste0("@@", i, "([^-0-9])"), spec_body)) {
        spec_body <- gsub(paste0("'@@", i, "'"), paste0("'", eval_expressions[[i]][1], "'"), spec_body)
      }
    }
    # If the replacement has a hyphen, then obtain the digit(s) immediately following and return the value from that index
    for (i in 1:length(split_references)) {
      while (grepl(paste0("@@", i, "-", "[0-9]+"), spec_body)) {
        the_index <- as.numeric(gsub("^([0-9]+)(.*)", "\\1", strsplit(spec_body, paste0("@@", i, "-"))[[1]][2]))
        if (the_index > length(eval_expressions[[i]])) {
          spec_body <- gsub(paste0("@@", i, "-", the_index, "([^0-9])"), paste0(eval_expressions[[i]][length(eval_expressions[[i]])], "\\1"), spec_body)
        } else {
          spec_body <- gsub(paste0("@@", i, "-", the_index, "([^0-9])"), paste0(eval_expressions[[i]][the_index], "\\1"), spec_body)
        }
      }
    }
    # Return the updated spec with replacements evaluated
    return(spec_body)
  }
  if (grepl("@@", spec) == FALSE) {return(spec)}
}

grviz.direction = c(up.down = 'TD', left.right = 'LR', right.left = 'RL', down.up = 'DT')

grviz.applyConfig = function(net, config){
  # shdw = list(enabled = chif(is.null(config$node.shadow.enabled), T, config$node.shadow.enabled),
  #             size    = chif(is.null(config$node.shadow.size), 10, config$node.shadow.size))
  # scl  = list(min = config$node.size.min, max = config$node.size.max)

  dir  = grviz.direction[config$direction %>% verify('character', domain = valid.visNetwork.directions, default = 'up.down')] %>% unname
  lot  = config$layout %>% verify('character', domain = c('random', 'hierarchical', 'neato', 'twopi', 'circo'), default = 'dot')

  if(lot == 'hierarchical'){lot = 'dot'}
  if(lot == 'random'){lot = 'neato'}

  net %>%
    grviz.addAttributes(attr = "rankdir", value = dir, attr_type = "graph") %>%
    grviz.addAttributes(attr = "layout" , value = lot, attr_type = "graph")

  # net %>% visNodes(shape = config$node.shape,
  #                  size  = config$node.size,
  #                  value = config$node.size,
  #                  title = config$node.tooltip,
  #                  label = config$node.label,
  #                  color = list(background = config$node.color,
  #                               border     = config$node.border.color,
  #                               highlight  = config$node.highlight.color),
  #                  shadow  = shdw,
  #                  scaling = chif(is.empty(scl), NULL, scl))
  # if (!is.null(sizeLegend)){v %<>% visNodes(scaling = sizeLegend)}
  # todo: fixed, physics, node.label.font, node image, node broken image, icon, shape properties
}

grviz.addActions = function(graph_log, version_id, function_used, time_modified, duration, nodes, edges, d_n = 0, d_e = 0)
{
  if (inherits(time_modified, "POSIXct") == FALSE) {
    stop("The `time_modified` value must inherit from POSIXct.",
         call. = FALSE)
  }
  graph_log_line <- data.frame(version_id = as.integer(version_id),
                               function_used = as.character(function_used), time_modified = time_modified,
                               duration = as.numeric(duration), nodes = as.integer(nodes),
                               edges = as.integer(edges), d_n = as.integer(d_n), d_e = as.integer(d_e),
                               stringsAsFactors = FALSE)
  dplyr::bind_rows(graph_log, graph_log_line)
}

grviz.addAttributes = function(graph, attr, value, attr_type){
  time_function_start <- Sys.time()
  fcn_name <- getCallingFunctionName()
  assert(graph %>% is.grviz.graph, "Given 'graph' object is not valid")
  value.x <- value.y <- NULL
  if (length(value) == 1) {
    if (inherits(value, "logical") & value %in% c(TRUE, FALSE)) {
      value <- tolower(as.character(value))
    }
  }
  global_attrs_to_add <- dplyr::tibble(attr = as.character(attr), value = as.character(value), attr_type = as.character(attr_type)) %>%
    as.data.frame(stringsAsFactors = FALSE)
  graph$global_attrs %<>%
    dplyr::full_join(global_attrs_to_add, by = c("attr", "attr_type")) %>%
    dplyr::transmute(attr, attr_type, value = dplyr::coalesce(value.y, value.x)) %>%
    dplyr::select(attr, value, attr_type)

  graph$graph_log %<>%
    grviz.addActions(version_id = nrow(graph$graph_log) + 1, function_used = fcn_name,
                     time_modified = time_function_start, duration = graphFunctionDuration(time_function_start),
                     nodes = nrow(graph$nodes_df), edges = nrow(graph$edges_df))
  graph
}

# Function that gets the calling function as a formatted character string todo: transfer to gener
#' @importFrom stringr str_replace_all
getCallingFcn <- function() {
  calling_fcn <- deparse(sys.call(-1))
  stringr::str_replace_all(calling_fcn, pattern = "([a-z0-9_]*)(.*)", replacement = "\\1")
}

# Function to add log line for a graph `action`
#' @importFrom dplyr bind_rows
addAction2Log <- function(graph_log, version_id, function_used, time_modified, duration, nodes, edges, d_n = 0, d_e = 0){
  if (inherits(time_modified, "POSIXct") == FALSE) {stop("The `time_modified` value must inherit from POSIXct.", call. = FALSE)}

  # Create a log line
  graph_log_line <- data.frame(version_id = as.integer(version_id), function_used = as.character(function_used), time_modified = time_modified,
                               duration = as.numeric(duration), nodes = as.integer(nodes), edges = as.integer(edges), d_n = as.integer(d_n), d_e = as.integer(d_e),
                               stringsAsFactors = FALSE)

  # Append the log line to `graph_log`
  dplyr::bind_rows(graph_log, graph_log_line)
}

# Function to get the time difference from the start of the function
graphFunctionDuration <- function(start_time) {
  end_time <- Sys.time()
  time_diff_s <- (end_time - start_time)[[1]]
  return(time_diff_s)
}

grviz.createNodeDF <- function(n, type = NULL, label = NULL, ...){
  fcn_name <- getCallingFcn()
  gener::assert(inherits(n, "numeric") | inherits(n, "integer"), "The value supplied to `n` must be numeric", fcn_name)
  gener::assert(length(n) <= 1, "The value supplied to `n` must be a single numeric value", fcn_name)

  if (is.null(type)) {type <- rep(as.character(NA), n)}
  if (!is.null(type)) {
    if (length(type) == 1) {type <- rep(type, n)}
    if (length(type) > 1 & length(type) < n) {type <- c(type, rep(as.character(NA), (n - length(type))))}
    if (length(type) > n) {type <- type[1:n]}
  }

  extras <- list(...)
  if (length(extras) > 0) {
    for (i in 1:length(extras)) {
      if (length(extras[[i]]) == 1) {extras[[i]] <- rep(extras[[i]], n)}
      if (length(extras[[i]]) > 1 & length(extras[[i]]) < n) {extras[[i]] <- c(extras[[i]], rep("", (n - length(extras[[i]]))))}
      if (length(extras[[i]]) > n) {extras[[i]] <- extras[[i]][1:n]}
    }
    extras <- as.data.frame(extras, stringsAsFactors = FALSE)
  }
  if (is.null(label)) {
    label <- rep(as.character(NA), n)
  } else if (inherits(label, "numeric") | inherits(label, "character")) {
    label <- as.character(label)
  } else if (inherits(label, "logical") & length(label) == 1) {
    if (label == TRUE) {label <- as.character(1:n)} else {label <- rep(as.character(NA), n)}
  }
  if (inherits(extras, "data.frame")) {
    nodes_df <- dplyr::bind_cols(data.frame(id = 1:n, type = type, label = label, stringsAsFactors = FALSE), extras)
  } else {
    nodes_df <- data.frame(id = 1:n, type = type, label = label, stringsAsFactors = FALSE)
  }

  nodes_df
}

grviz.createEdgeDF <- function(from, to, rel = NULL, ...) {
  stopifnot(length(from) == length(to))
  n <- length(from)
  from <- as.integer(from)
  to <- as.integer(to)
  if (is.null(rel)) {
    rel <- rep(as.character(NA), length(from))
  } else {
    rel <- as.character(rel)
  }
  if (!is.null(rel)) {
    if (length(rel) == 1) {
      rel <- rep(rel, length(from))
    }

    # Expand vectors with `length` > `1` and
    # `length` < `length(from)`
    if (length(rel) > 1 &
        length(rel) < length(from)) {
      rel <-
        c(rel,
          rep(as.character(NA),
              (length(from) - length(rel))))
    }

    # Trim vectors with number of values exceeding
    # the number of edges
    if (length(rel) > length(from)) {
      rel <- rel[1:length(from)]
    }
  }

  # Collect extra vectors of data as `extras`
  extras <- list(...)

  if (length(extras) > 0) {
    for (i in 1:length(extras)) {
      if (length(extras[[i]]) == 1) {
        extras[[i]] <- rep(extras[[i]], length(from))
      }

      if (length(extras[[i]]) > 1 &
          length(extras[[i]]) < length(from)) {
        extras[[i]] <- c(extras[[i]], rep(as.character(NA), (length(from) - length(extras[[i]]))))
      }

      if (length(extras[[i]]) > length(from)) {
        extras[[i]] <- extras[[i]][1:length(from)]
      }
    }

    # Create a data frame from the `extras` list
    extras <-
      as.data.frame(
        extras, stringsAsFactors = FALSE)
  }

  if (inherits(extras, "data.frame")) {
    edges_df <- dplyr::bind_cols(data.frame(id = sequence(n), from = from, to = to, rel = rel, stringsAsFactors = FALSE), extras)
  } else {
    edges_df <- data.frame(id = sequence(n), from = from, to = to, rel = rel, stringsAsFactors = FALSE)
  }
  edges_df
}

grviz.graph = function(obj,
                       key = NULL, label  = NULL, shape  = NULL, size = NULL, color = NULL, borderColor = NULL, tooltip = NULL, labelColor = NULL,
                       source = NULL, target = NULL, linkColor = NULL, linkTooltip = NULL, linkWidth = NULL,
                       linkLabel = NULL, linkLabelColor = NULL, linkLabelSize = NULL,
                       config = NULL, ...){

  # label %<>% renameSeries('label')
  # shape %<>% renameSeries('shape')
  # size %<>% renameSeries(gndcd(100,11,64,19,130))
  # color %<>% renameSeries('fillcolor')
  # borderColor %<>% renameSeries('color')
  # labelColor %<>% renameSeries('fontcolor')
  # labelSize %<>% renameSeries('fontsize')
  #
  # tooltip %<>% renameSeries('tooltip')
  # source %<>% renameSeries(gndcd(33,110,12,200))
  # target %<>% renameSeries(gndcd(136,12))
  # linkWidth %<>% renameSeries('penwidth')
  # linkColor %<>% renameSeries('color')
  # linkLabel %<>% renameSeries('label')
  # linkTooltip %<>% renameSeries('tooltip')
  # linkLabelColor %<>% renameSeries('fontcolor')
  # linkLabelSize %<>% renameSeries('fontsize')

  # Verifications:
  obj %>% verify('list', lengths = 2, names_identical = c(gndcd(27,143,150,130,98), gndcd(55,4,158,13,144)), varname = 'obj', null_allowed = F, err_src = 'grviz.graph')
  obj$nodes %>% verify('data.frame', varname = 'obj$nodes', null_allowed = F, err_src = 'grviz.graph')
  obj$links %>% verify('data.frame', varname = 'obj$links', null_allowed = F, err_src = 'grviz.graph')

  # nodeCharIDs = rownames(obj$nodes)
  # assert(!is.null(nodeCharIDs), "obj$nodes has not rownames! Rownames serve as node IDs.", err_src = 'grviz.graph')
  #
  # nodeIDs = nodeCharIDs %>% as.factor %>% as.integer
  # names(nodeIDs) <- nodeCharIDs

  config = grviz.graph.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
    verifyConfig(plotter = 'grviz')

  # Check http://rich-iannone.github.io/DiagrammeR/ndfs_edfs.html for all node and edge attributes

  # Preparing Aesthetics:
  a = prepareAesthetics(key = key, label  = label, shape   = shape, size = size, color = color, borderColor = borderColor, tooltip = tooltip,
                        source = source, target = target, linkColor = linkColor, linkWidth = linkWidth, labelColor = labelColor,
                        linkLabel = linkLabel, linkLabelColor = linkLabelColor, linkLabelSize = linkLabelSize, linkTooltip = linkTooltip)
  L = a$labels
  A = a$aesthetics

  obj$nodes %<>% prepare4Plot(A %>% list.extract('key', 'label', 'shape', 'size', 'tooltip', 'color', 'borderColor', 'borderWidth', 'labelColor'), config) %>% distinct_(L$key, .keep_all = T)
  obj$links %<>% prepare4Plot(A %>% list.extract('source', 'target', 'linkColor', 'linkLength', 'linkWidth', 'linkLabel', 'linkLabelColor', 'linkLabelSize', 'linkTooltip'), config)

  # if (is.empty(obj$nodes)){obj$nodes <- data.frame(id = nodeIDs)} else {obj$nodes$id = nodeIDs}

  gener::assert(obj$links[, L$source] %<% obj$nodes[, L$key], "Value in column '" %++% L$source %++% "' must be a seubset of node IDs'", err_src = 'grviz.graph')
  gener::assert(obj$links[, L$target] %<% obj$nodes[, L$key], "Value in column '" %++% L$target %++% "' must be a seubset of node IDs'", err_src = 'grviz.graph')

  if(!inherits(obj$nodes[, L$key], 'integer')){
    nodemap = sequence(nrow(obj$nodes))
    names(nodemap) <- obj$nodes[, L$key]

    obj$links[, L$source] = nodemap[obj$links[, L$source]] %>% unname
    obj$links[, L$target] = nodemap[obj$links[, L$target]] %>% unname
    obj$nodes[, L$key]    = nodemap[obj$nodes[, L$key]] %>% unname
  }

  if (is.null(L$labelColor) & !is.null(L$color)){
    obj$nodes[, L$labelColor] = contrastColors(obj$nodes[, L$color])
  }

  if(!is.null(config$node.size.min) & !is.null(config$node.size.max) & !is.null(L$size)){
    obj$nodes[, L$size] %<>% vect.map(min = config$node.size.min, max = config$node.size.max)

  }

  if(!is.null(config$link.width.min) & !is.null(config$link.width.max) & !is.null(L$linkWidth)){
    obj$links[, L$linkWidth] %<>% vect.map(min = config$link.width.min, max = config$link.width.max)
  }

  if(!is.null(config$link.length.min) & !is.null(config$link.length.max) & !is.null(L$linkLength)){
    obj$links[, L$linkLength] %<>% vect.map(min = config$link.length.min, max = config$link.length.max)
  }

  if(is.empty(obj$nodes)){
    nodes_df = data.frame(n = integer(), label = character(), shape = character(), fillcolor = character(),fontcolor = character(), color = character(), width = numeric(), height = numeric(),
                          tooltip   = character(), penwidth  = numeric(), fixedsize = logical(), style = character(), fontsize  = numeric(), fontname  = character(),
                          stringsAsFactors = F)}
  else {
    grviz.createNodeDF(
      n         = nrow(obj$nodes),
      label     = obj$nodes[, L$label],
      shape     = obj$nodes[, L$shape], # todo: translate shapes
      fillcolor = obj$nodes[, L$color],
      fontcolor = obj$nodes[, L$labelColor],
      color     = obj$nodes[, L$borderColor],
      tooltip   = obj$nodes[, L$tooltip],
      penwidth  = obj$nodes[, L$borderWidth],
      fixedsize = F, # get from config, also fontname
      style     = "rounded,filled") -> nodes_df # todo: get from config

    config$node.size.ratio %<>% verify('numeric', domain = c(0,1), default = 0.7)

    if(is.null(config$node.width) & !is.null(config$node.size)){config$node.width  <- config$node.size}
    if(is.null(config$node.height) & !is.null(config$node.size)){config$node.height <- config$node.size.ratio*config$node.size}

    if(!is.null(L$size)){nodes_df$width    = obj$nodes[, L$size]} else {nodes_df$width <- config$node.width}
    if(!is.null(L$size)){nodes_df$height   = config$node.size.ratio*obj$nodes[, L$size]} else {nodes_df$height <- config$node.height}
    if(!is.null(L$size)){nodes_df$fontsize = (config$node.label.size/mean(obj$nodes[, L$size]))*obj$nodes[, L$size]} else {nodes_df$fontsize <- config$node.label.size}

    if(is.null(nodes_df$fontsize)){
      nodes_df$fontsize = config$node.label.size
    }
    if(is.null(nodes_df$fillcolor)){
      nodes_df$fillcolor = config$node.color
    }
    if(is.null(nodes_df$shape)){
      nodes_df$shape = config$node.shape
    }
    # todo: add sides, peripheries fontcolor, ... as dimensions

    nodes_df$distortion = config$node.distortion
    nodes_df$fixedsize  = config$node.fixedSize
    nodes_df$fontcolor  = config$node.label.color
    nodes_df$fontname   = config$node.label.font
    nodes_df$peripheries = config$node.border.peripheries # todo: line, and dblLines to be defined? convert to an integer here
    ns = names(nodes_df)
    if(!'shape' %in% ns & !is.null(config$node.shape)){nodes_df$shape = config$node.shape}

    nodes_df$shadow = T
  }

  if(is.empty(obj$links)){
    edges_df = data.frame(from = integer(), to = integer(), label = character(), penWidth = numeric(), color = character(), fontname = character(),
                          stringsAsFactors = F)}
  else {
    grviz.createEdgeDF(
      from      = obj$links[, L$source],
      to        = obj$links[, L$target],
      color     = obj$links[, L$linkColor],
      label     = obj$links[, L$linkLabel],
      penwidth  = obj$links[, L$linkWidth],
      fontsize  = obj$links[, L$linkLabelSize],
      tooltip   = obj$links[, L$linkTooltip]) -> edges_df

    if(is.null(edges_df$fontsize)){
      edges_df$fontsize = config$link.label.size
    }

    edges_df$fontcolor = config$link.label.color
    edges_df$fontname  = config$link.label.font
    edges_df$arrowhead = config$link.arrow.head
    edges_df$arrowsize = config$link.arrow.size
    edges_df$arrowtail = config$link.arrow.tail
    edges_df$dir       = config$link.direction
    edges_df$headport  = NULL
    edges_df$tailport  = NULL

    edges_df$rel = 'leading_to'

    # todo: add more dimensions inarguments and when requried, also add additional dimensions in config, like hidden(visible), image, mass, borderWidth, borderWidthSelected, labelHighlightBold...
    # obj$nodes = obj$nodes[order(obj$nodes$id), ]

    # column headport and tailport can take these values: n, ne, e, se, s, sw, w, and nw
    # look at: https://www.rdocumentation.org/packages/DiagrammeR/versions/1.0.0/topics/edge_aes
    # https://www.graphviz.org/doc/info/attrs.html#h:undir_note
  }

  grviz.createGraph(nodes_df, edges_df, ...) %>% grviz.applyConfig(config) %>%
    grviz.renderGraph(layout = config$layout %>% verify('character', default = 'dot'),
                      title  = config$title %>% verify('character', lengths == 1),
                      shinyInput.click = config$shinyInput.click %>% verify('character', lengths = 1, varname = 'config$shinyInput.click'))
}


# todo: translate shapes to viser standard
grviz.shape = c(square = 'square', delta = 'triangle', icon = 'icon', triangle = 'triangle', rectangle = 'rectangle', box = 'rectangle', dot = 'point', point = 'point', egg = 'egg', oval = 'oval', text = 'plaintext', bubble = 'circle', circle = gndcd(88,4,1,18,199,94),  star = 'star', ellipse = 'ellipse', cylinder = "database", diamond = 'diamond', rhombus = 'diamond')
valid.grviz.shapes = grviz.shape %>% names


#sizeColumn
#sizeLegend


#######################################################################################################################

# Function to check whether a graph object is valid
is.graph.valid <- function(graph) {
  # Check for all component names to be present
  if (!all(c("graph_info", "nodes_df", "edges_df", "global_attrs", "directed", "last_node", "last_edge", "node_selection", "edge_selection", "cache", "graph_log") %in% names(graph))){
    return(FALSE)
  }
  # Check for specific graph classes
  if (any(
    !inherits(graph$graph_info, "data.frame"),
    !inherits(graph$nodes_df, "data.frame"),
    !inherits(graph$edges_df, "data.frame"),
    !inherits(graph$global_attrs, "data.frame"),
    !inherits(graph$global_attrs$attr, "character"),
    !inherits(graph$global_attrs$value, "character"),
    !inherits(graph$global_attrs$attr_type, "character"),
    !inherits(graph$directed, "logical"),
    !inherits(graph$node_selection, "data.frame"),
    !inherits(graph$edge_selection, "data.frame"),
    !inherits(graph$cache, "list"),
    !inherits(graph$graph_log, "data.frame"))) {

    return(FALSE)
  }
  return(TRUE)
}

is.grviz.graph = function(graph){
  if (!all(c("graph_info", "nodes_df", "edges_df", "global_attrs",
             "directed", "last_node", "last_edge", "node_selection",
             "edge_selection", "cache", "graph_log") %in% names(graph))) {
    return(FALSE)
  }
  if (any(inherits(graph$graph_info, "data.frame") == FALSE,
          inherits(graph$nodes_df, "data.frame") == FALSE, inherits(graph$edges_df,
                                                                    "data.frame") == FALSE, inherits(graph$global_attrs,
                                                                                                     "data.frame") == FALSE, inherits(graph$global_attrs$attr,
                                                                                                                                      "character") == FALSE, inherits(graph$global_attrs$value,
                                                                                                                                                                      "character") == FALSE, inherits(graph$global_attrs$attr_type,
                                                                                                                                                                                                      "character") == FALSE, inherits(graph$directed, "logical") ==
          FALSE, inherits(graph$node_selection, "data.frame") ==
          FALSE, inherits(graph$edge_selection, "data.frame") ==
          FALSE, inherits(graph$cache, "list") == FALSE, inherits(graph$graph_log,
                                                                  "data.frame") == FALSE)) {
    return(FALSE)
  }
  return(TRUE)
}

grviz.createGraph <- function(nodes_df = NULL, edges_df = NULL, directed = TRUE, graph_name = NULL, attr_theme = "default", write_backups = FALSE) {
  fcn_name   <- getCallingFcn()
  graph_time <- Sys.time()
  graph_tz   <- Sys.timezone()
  graph_id   <- replicate(8, sample(c(LETTERS, letters, 0:9), 1)) %>% paste(collapse = "")
  graph_info <- data.frame(
    graph_id = as.character(graph_id),
    graph_name = as.character(paste0("graph_", graph_id)),
    graph_time = graph_time,
    graph_tz = graph_tz,
    write_backups = as.logical(write_backups),
    stringsAsFactors = FALSE)

  # Insert a user-defined `graph_name` if supplied
  if (!is.null(graph_name)) {graph_info[1, 2] <- as.character(graph_name)}
  global_attrs <- data.frame(
    attr = as.character(NA),
    value = as.character(NA),
    attr_type = as.character(NA),
    stringsAsFactors = FALSE)[-1, ]

  if (inherits(attr_theme, "character")) {
    if (attr_theme == "default") {
      global_attrs <- data.frame(
        attr      = c("layout", "outputorder", "fontname", "fontsize", "shape", "fixedsize", "width", "style",
                      "fillcolor", "color", "fontcolor", "bgcolor", "fontname", "fontsize", "len", "color", "arrowsize"),
        value     = c("neato", "edgesfirst", "Helvetica", "10", "circle", "true", "0.5", "filled", "aliceblue", "gray70", "gray50",
                      "white", "Helvetica", "8", "1.5", "gray80", "0.5"),
        attr_type = c(rep("graph", 2), rep("node", 9), "graph", rep("edge", 5)),
        stringsAsFactors = FALSE)
    } else {gener::assert(F, "The value for `attr_theme` doesn't refer to any available theme", fcn_name)}
  } else if (is.null(attr_theme)) {
    global_attrs <- data.frame(
      attr  = as.character(NA),
      value = as.character(NA),
      attr_type = as.character(NA),
      stringsAsFactors = FALSE)[-1, ]
  }

  ndf <- data.frame(id = as.integer(NA), type = as.character(NA), label = as.character(NA), stringsAsFactors = FALSE)[-1, ]
  edf  <- data.frame(id = as.integer(NA), from = as.integer(NA), to = as.integer(NA), rel = as.character(NA), stringsAsFactors = FALSE)[-1, ]
  nsdf <- dplyr::tibble(node = as.integer(NA))[-1, ] %>% as.data.frame(stringsAsFactors = FALSE)
  esdf <- dplyr::tibble(edge = as.integer(NA), from = as.integer(NA), to = as.integer(NA))[-1, ] %>% as.data.frame(stringsAsFactors = FALSE)

  graph_actions <-  data.frame(action_index = as.integer(NA), action_name = as.character(NA), stringsAsFactors = FALSE)[-1, ]

  graph_log     <-  data.frame(version_id = as.integer(NA), function_used = as.character(NA), time_modified = graph_time,      duration = as.numeric(NA),
                               nodes = as.integer(NA), edges = as.integer(NA), d_n = as.integer(NA), d_e = as.integer(NA), stringsAsFactors = FALSE)[-1, ]

  cache <- list()

  graph <- list(
    graph_info = graph_info, nodes_df = ndf, edges_df = edf, global_attrs = global_attrs,
    directed = ifelse(directed, TRUE, FALSE), last_node = 0, last_edge = 0, node_selection = nsdf,
    edge_selection = esdf, cache = cache, graph_actions = graph_actions, graph_log = graph_log)

  attr(graph, "class") <- "dgr_graph"

  if (all(c(is.null(nodes_df), is.null(edges_df)))) {
    graph_log <- addAction2Log(graph_log = graph_log, version_id = 1,
                               function_used = fcn_name, time_modified = graph_time,
                               duration = graphFunctionDuration(graph_time), nodes = nrow(graph$nodes_df),
                               edges = nrow(graph$edges_df), d_n = nrow(graph$nodes_df), d_e = nrow(graph$edges_df))
  } else if (!is.null(nodes_df) & is.null(edges_df)) {
    if (inherits(nodes_df, "tbl_df")) {nodes_df <- nodes_df %>% as.data.frame(stringsAsFactors = FALSE)}

    for (i in 2:3) {nodes_df[, i] <- as.character(nodes_df[, i])}

    graph$nodes_df <- dplyr::bind_rows(graph$nodes_df, nodes_df)

    graph$last_node <- nrow(nodes_df)

    graph_log <- addAction2Log(
      graph_log = graph_log,
      version_id = 1,
      function_used = fcn_name,
      time_modified = graph_time,
      duration = graphFunctionDuration(graph_time),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df),
      d_n = nrow(graph$nodes_df),
      d_e = nrow(graph$edges_df))

  } else if (!is.null(nodes_df) & !is.null(edges_df)) {

    if (inherits(nodes_df, "tbl_df")) {nodes_df <- nodes_df %>% as.data.frame(stringsAsFactors = FALSE)}
    if (inherits(edges_df, "tbl_df")) {edges_df <- edges_df %>% as.data.frame(stringsAsFactors = FALSE)}
    for (i in 2:3) {nodes_df[, i] <- as.character(nodes_df[, i])}

    graph$nodes_df <- dplyr::bind_rows(graph$nodes_df, nodes_df)

    graph$last_node <- nrow(nodes_df)

    if (inherits(edges_df, "data.frame")) {
      if (ncol(edges_df) > 2) {edges_df$rel <- as.character(edges_df$rel)}
    }

    graph$edges_df <- dplyr::bind_rows(graph$edges_df, edges_df)

    graph$last_edge <- nrow(edges_df)

    graph_log <- addAction2Log(
      graph_log = graph_log,
      version_id = 1,
      function_used = fcn_name,
      time_modified = graph_time,
      duration = graphFunctionDuration(graph_time),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df),
      d_n = nrow(graph$nodes_df),
      d_e = nrow(graph$edges_df))
  }

  # Add the `graph_log` df to the graph object
  graph$graph_log <- graph_log

  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }

  # If neither an ndf nor both ndf & edf provided,
  # return the initialized graph with no nodes or edges
  return(graph)
}

grviz.generateDot <- function(graph) {
  fcn_name <- getCallingFcn()
  is.graph.valid(graph) %>% gener::assert("The graph object is not valid", fcn_name)
  attr_type <- attr <- value <- string <- NULL
  # Extract objects from the graph objecct
  nodes_df <- graph$nodes_df
  edges_df <- graph$edges_df
  directed <- graph$directed
  global_attrs <- graph$global_attrs

  if ("graph" %in% global_attrs$attr_type) {
    graph_attrs <- global_attrs %>%
      dplyr::filter(attr_type == "graph") %>%
      dplyr::mutate(string = paste0(attr, " = '", value, "'"))
    graph_attrs <- graph_attrs %>% dplyr::pull(string)
  } else {graph_attrs <- NA}

  if ("node" %in% global_attrs$attr_type) {
    node_attrs <- global_attrs %>%
      dplyr::filter(attr_type == "node") %>%
      dplyr::mutate(string = paste0(attr, " = '", value, "'"))
    node_attrs <- node_attrs %>% dplyr::pull(string)

    for (i in 1:nrow(global_attrs %>% dplyr::filter(attr_type == "node"))) {
      node_attr_to_set <- (global_attrs %>% dplyr::filter(attr_type == "node"))[i, 1]
      if (node_attr_to_set %in% colnames(nodes_df)) {
        col_num <- which(colnames(nodes_df) == node_attr_to_set)
        nodes_df[which(is.na(nodes_df[, col_num])), col_num] <-
          (global_attrs %>% dplyr::filter(attr_type == "node"))[i, 2]
      }
    }
  } else {node_attrs <- NA}

  if ("edge" %in% global_attrs$attr_type) {
    edge_attrs <- global_attrs %>%
      dplyr::filter(attr_type == "edge") %>%
      dplyr::mutate(string = paste0(attr, " = '", value, "'"))

    edge_attrs <- edge_attrs %>% dplyr::pull(string)

    for (i in 1:nrow(global_attrs %>% dplyr::filter(attr_type == "edge"))) {
      edge_attr_to_set <- (global_attrs %>% dplyr::filter(attr_type == "edge"))[i, 1]
      if (edge_attr_to_set %in% colnames(edges_df)) {
        col_num <- which(colnames(edges_df) == edge_attr_to_set)
        edges_df[which(is.na(edges_df[, col_num])), col_num] <-
          (global_attrs %>% dplyr::filter(attr_type == "edge"))[i, 2]
      }
    }
  } else {edge_attrs <- NA}

  # Replace NA values with empty strings in `nodes_df`
  if (!is.null(nodes_df)) {
    if (ncol(nodes_df) >= 4) {
      for (i in 4:ncol(nodes_df)) {
        nodes_df[, i] <-  as.character(nodes_df[, i])
        nodes_df[, i] <-  ifelse(is.na(nodes_df[, i]), "", nodes_df[, i])
      }
    }
  }

  # Replace NA values with empty strings in `edges_df`
  if (!is.null(edges_df)) {
    if (ncol(edges_df) >= 4) {
      for (i in 4:ncol(edges_df)) {
        edges_df[, i] <-  ifelse(is.na(edges_df[, i]), "", edges_df[, i])
        edges_df[, i] <-  as.character(edges_df[, i])
      }
    }
  }

  # If `equation` column in `nodes_df`, ensure the right amount of escaping is present
  if ("equation" %in% colnames(nodes_df)) {
    equation_col <- which(colnames(nodes_df) == "equation")
    for (i in 1:nrow(nodes_df)) {
      if (grepl("^\\$.*\\$$", nodes_df[i, equation_col])) {
        nodes_df[i, equation_col] <- str_replace_all(
          nodes_df[i, equation_col], "\\\\", "\\\\\\\\")
      } else {nodes_df[i, equation_col] <- ""}
    }
  }

  # If `display` column in `nodes_df`, modify label column for this render
  if ("display" %in% colnames(nodes_df)) {
    display_col <- which(colnames(nodes_df) == "display")
    label_col   <- which(colnames(nodes_df) == "label")
    for (i in 1:nrow(nodes_df)) {
      if (nodes_df[i, display_col] != "") {
        nodes_df[i, label_col] <- nodes_df[i, which(colnames(nodes_df) == nodes_df[i, display_col])]
      } else {nodes_df[i, label_col] <- ""}
    }
  }

  # If `display` column in `edges_df`, modify label column for this render
  if ("display" %in% colnames(edges_df)) {
    display_col <- which(colnames(edges_df) == "display")
    if (!("label" %in% colnames(edges_df))) {
      edges_df <- edges_df %>% mutate(label = as.character(NA))
    }

    label_col <- which(colnames(edges_df) == "label")

    for (i in 1:nrow(edges_df)) {
      if (!is.na(edges_df[i, display_col]) ) {
        if (edges_df[i, display_col] != "") {

          edges_df[i, label_col] <-
            edges_df[
              i, which(colnames(edges_df) == edges_df[i, display_col])]
        }
      } else {
        edges_df[i, label_col] <- ""
      }
    }
  }

  graph_attributes <- c("layout", "bgcolor", "rankdir", "overlap", "outputorder", "fixedsize", "mindist", "nodesep", "ranksep", "stylesheet")
  node_attributes  <- c("shape", "style", "penwidth", "color", "fillcolor", "fontname", "fontsize", "fontcolor", "height", "width", "group", "tooltip", "xlabel",
                        "URL", "distortion", "sides", "skew", "peripheries", "gradientangle", "label", "fixedsize", "labelloc", "margin", "orientation", "pos")
  edge_attributes  <- c("style", "penwidth", "color", "arrowsize", "arrowhead", "arrowtail", "fontname", "fontsize", "fontcolor", "len", "tooltip", "URL", "label",
                        "labelfontname", "labelfontsize", "labelfontcolor", "labeltooltip", "labelURL", "edgetooltip", "edgeURL", "headtooltip", "headURL", "headclip",
                        "headlabel", "headport", "tailtooltip", "tailURL", "tailclip",  "taillabel", "tailport", "dir", "decorate")

  if (nrow(nodes_df) == 0 & nrow(edges_df) == 0) {
    # Create DOT code with nothing in graph
    dot_code <- paste0(ifelse(directed, "digraph", "graph"), " {\n", "\n}")
  } else {
    # Create the DOT attributes block
    # Create the default attributes statement
    # for graph attributes
    if (!(any(is.na(graph_attrs)))) {
      graph_attr_stmt <- paste0("graph [", paste(graph_attrs, collapse = ",\n       "), "]\n")
    } else {graph_attr_stmt <- ""}

    # Create the default attributes statement
    # for node attributes
    if (!(any(is.na(node_attrs)))) {
      node_attr_stmt <-
        paste0("node [", paste(node_attrs,
                               collapse = ",\n      "),
               "]\n")
    } else {
      node_attr_stmt <- ""
    }

    # Create the default attributes statement
    # for edge attributes
    if (!(any(is.na(edge_attrs)))) {
      edge_attr_stmt <-
        paste0("edge [", paste(edge_attrs,
                               collapse = ",\n     "),
               "]\n")
    } else {
      edge_attr_stmt <- ""
    }

    # Combine default attributes into a single block
    combined_attr_stmts <-
      paste(
        graph_attr_stmt,
        node_attr_stmt,
        edge_attr_stmt, sep = "\n")

    #
    # Create the DOT node block
    #

    if (nrow(nodes_df) > 0) {

      # Determine whether positional (x,y)
      # data is included
      column_with_x <-
        which(colnames(nodes_df) %in% "x")[1]

      column_with_y <-
        which(colnames(nodes_df) %in% "y")[1]

      if (!is.na(column_with_x) & !is.na(column_with_y)) {

        pos <-
          data.frame(
            "pos" =
              paste0(
                nodes_df[, column_with_x],
                ",",
                nodes_df[, column_with_y],
                "!"))

        nodes_df$pos <- pos$pos
      }

      # Determine whether column 'alpha' exists
      if (any(grepl("$alpha^", colnames(nodes_df)))) {
        column_with_alpha_assigned <-
          grep("$alpha^", colnames(nodes_df))
      } else {
        column_with_alpha_assigned <- NA
      }

      if (!is.na(column_with_alpha_assigned)) {

        # Determine the number of color attributes in
        # the node data frame
        number_of_col_attr <-
          length(which(colnames(nodes_df) %in%
                         c("color", "fillcolor",
                           "fontcolor")))

        # If the number of color attrs in df is 1,
        # rename referencing alpha column
        if (number_of_col_attr == 1) {

          name_of_col_attr <-
            colnames(nodes_df)[
              which(colnames(nodes_df) %in%
                      c("color", "fillcolor",
                        "fontcolor"))]

          colnames(nodes_df)[column_with_alpha_assigned] <-
            paste0("alpha:", name_of_col_attr)
        }
      }

      # Determine whether column 'alpha' with
      # color attr exists
      if (any(grepl("alpha:.*", colnames(nodes_df)))) {

        alpha_column_no <- grep("alpha:.*", colnames(nodes_df))

        color_attr_column_name <-
          unlist(strsplit(colnames(nodes_df)[
            (which(grepl("alpha:.*", colnames(nodes_df))))
            ], ":"))[-1]

        color_attr_column_no <-
          which(colnames(nodes_df) %in% color_attr_column_name)

        # Append alpha value only if referenced
        # column is for color
        if (any(c("color", "fillcolor", "fontcolor") %in%
                colnames(nodes_df)[color_attr_column_no])) {

          # Append alpha for color values that are
          # X11 color names
          if (all(grepl("[a-z]*",
                        as.character(nodes_df[, color_attr_column_no]))) &
              all(as.character(nodes_df[, color_attr_column_no]) %in%
                  x11_hex()[, 1])) {

            for (i in 1:nrow(nodes_df)) {
              nodes_df[i, color_attr_column_no] <-
                paste0(x11_hex()[
                  which(x11_hex()[, 1] %in%
                          as.character(nodes_df[i, color_attr_column_no])), 2],
                  formatC(round(as.numeric(nodes_df[i, alpha_column_no]), 0),
                          flag = "0", width = 2))
            }
          }

          # Append alpha for color values that
          # are hex color values
          if (all(grepl("#[0-9a-fA-F]{6}$",
                        as.character(nodes_df[, color_attr_column_no])))) {

            for (i in 1:nrow(nodes_df)) {
              nodes_df[, color_attr_column_no] <-
                as.character(nodes_df[, color_attr_column_no])

              nodes_df[i, color_attr_column_no] <-
                paste0(nodes_df[i, color_attr_column_no],
                       round(as.numeric(nodes_df[i, alpha_column_no]), 0))
            }
          }
        }
      }

      # Determine which other columns correspond
      # to node attribute values
      other_columns_with_node_attributes <-
        which(colnames(nodes_df) %in% node_attributes)

      # Construct the 'node_block' character object
      for (i in 1:nrow(nodes_df)) {
        if (i == 1) {
          node_block <- vector(mode = "character", length = 0)
        }

        if (length(other_columns_with_node_attributes) > 0) {

          for (j in other_columns_with_node_attributes) {

            if (j == other_columns_with_node_attributes[1]) {
              attr_string <- vector(mode = "character", length = 0)
            }

            # Create the node attributes for labels
            # and tooltips when provided
            if (all(colnames(nodes_df)[j] %in%
                    c("label", "tooltip"),
                    is.na(nodes_df[i, j]))) {
              attribute <- NULL
            } else if (all(colnames(nodes_df)[j] %in%
                           c("label", "tooltip"),
                           !is.na(nodes_df[i, j]))) {
              attribute <-
                paste0(colnames(nodes_df)[j],
                       " = ", "'", nodes_df[i, j], "'")
            } else if (all(!(colnames(nodes_df)[j] %in%
                             c("label", "tooltip")),
                           is.na(nodes_df[i, j]))) {
              attribute <- NULL
            } else if (all(!(colnames(nodes_df)[j] %in%
                             c("label", "tooltip")),
                           !is.na(nodes_df[i, j]))) {
              attribute <-
                paste0(colnames(nodes_df)[j],
                       " = ", "'", nodes_df[i, j], "'")
            }
            attr_string <- c(attr_string, attribute)
          }

          if (j == other_columns_with_node_attributes[
            length(other_columns_with_node_attributes)]) {
            attr_string <- paste(attr_string, collapse = ", ")
          }
        }

        # Generate a line of node objects when an
        # attribute string exists
        if (exists("attr_string")) {
          line <- paste0("  '", nodes_df[i, 1], "'",
                         " [", attr_string, "] ")
        }

        # Generate a line of node objects when an
        # attribute string doesn't exist
        if (!exists("attr_string")) {
          line <-
            paste0("  '",
                   nodes_df[i, 1],
                   "'")
        }
        node_block <- c(node_block, line)
      }

      if ("rank" %in% colnames(nodes_df)) {
        node_block <-
          c(node_block,
            tapply(node_block,
                   nodes_df$rank, FUN = function(x) {
                     if(length(x) > 1) {
                       x <- paste0('subgraph{rank = same\n',
                                   paste0(x, collapse = '\n'),
                                   '}\n')
                     }
                     return(x)
                   }))
      }

      else if ('cluster' %in% colnames(nodes_df)) {
        clustered_node_block <- character(0)
        clusters <- split(node_block, nodes_df$cluster)
        for (i in seq_along(clusters)) {
          if (names(clusters)[[i]] == "") {
            # nodes not in clusters
            cluster_block <- clusters[[i]]
          } else {
            cluster_block <- paste0("subgraph cluster", i, "{\nlabel='",
                                    names(clusters)[[i]], "'\n",
                                    paste0(clusters[[i]], collapse="\n"), "}\n")
          }
          clustered_node_block <- c(clustered_node_block, cluster_block)
        }

        node_block <- clustered_node_block

        # cleanup variables
        rm(clustered_node_block, clusters, cluster_block)
      }

      # Construct the `node_block` character object
      node_block <- paste(node_block, collapse = "\n")

      # Remove the `attr_string` object if it exists
      if (exists("attr_string")) {
        rm(attr_string)
      }

      # Remove the `attribute` object if it exists
      if (exists("attribute")) {
        rm(attribute)
      }
    }

    #
    # Create the DOT edge block
    #

    if (nrow(edges_df) > 0) {

      # Determine whether `from` or `to` columns are
      # in `edges_df`
      from_to_columns <-
        ifelse(any(c("from", "to") %in%
                     colnames(edges_df)), TRUE, FALSE)

      # Determine which columns in `edges_df`
      # contain edge attributes
      other_columns_with_edge_attributes <-
        which(colnames(edges_df) %in% edge_attributes)

      # Determine whether the complementary set of
      # columns is present
      if (from_to_columns) {
        both_from_to_columns <-
          all(c(any(c("from") %in%
                      colnames(edges_df))),
              any(c("to") %in%
                    colnames(edges_df)))
      }

      # If the complementary set of columns is present,
      # determine the positions
      if (exists("both_from_to_columns")) {
        if (both_from_to_columns) {
          from_column <-
            which(colnames(edges_df) %in% c("from"))[1]
          to_column <-
            which(colnames(edges_df) %in% c("to"))[1]
        }
      }

      # Construct the `edge_block` character object
      if (exists("from_column") &
          exists("to_column")) {

        if (length(from_column) == 1 &
            length(from_column) == 1) {

          for (i in 1:nrow(edges_df)) {

            if (i == 1) {
              edge_block <-
                vector(mode = "character", length = 0)
            }

            if (length(other_columns_with_edge_attributes) > 0) {

              for (j in other_columns_with_edge_attributes) {

                if (j == other_columns_with_edge_attributes[1]) {
                  attr_string <- vector(mode = "character", length = 0)
                }

                # Create the edge attributes for labels
                # and tooltips when provided
                if (all(colnames(edges_df)[j] %in%
                        c("edgetooltip", "headtooltip",
                          "label", "labeltooltip",
                          "taillabel", "tailtooltip",
                          "tooltip"),
                        is.na(edges_df[i, j]))) {
                  attribute <- NULL
                } else if (all(colnames(edges_df)[j] %in%
                               c("edgetooltip", "headtooltip",
                                 "label", "labeltooltip",
                                 "taillabel", "tailtooltip",
                                 "tooltip"),
                               edges_df[i, j] != '')) {
                  attribute <-
                    paste0(colnames(edges_df)[j],
                           " = ", "'", edges_df[i, j],
                           "'")
                } else if (all(!(colnames(edges_df)[j] %in%
                                 c("edgetooltip", "headtooltip",
                                   "label", "labeltooltip",
                                   "taillabel", "tailtooltip",
                                   "tooltip")),
                               is.na(edges_df[i, j]))) {

                  attribute <- NULL
                } else if (all(!(colnames(edges_df)[j] %in%
                                 c("edgetooltip", "headtooltip",
                                   "label", "labeltooltip",
                                   "taillabel", "tailtooltip",
                                   "tooltip")),
                               edges_df[i, j] != '')) {
                  attribute <-
                    paste0(colnames(edges_df)[j],
                           " = ", "'", edges_df[i, j], "'")
                }
                attr_string <- c(attr_string, attribute)
              }

              if (j == other_columns_with_edge_attributes[
                length(other_columns_with_edge_attributes)]) {
                attr_string <- paste(attr_string, collapse = ", ")
              }
            }

            # Generate a line of edge objects when an
            # attribute string exists
            if (exists("attr_string")) {
              line <-
                paste0("'", edges_df[i, from_column], "'",
                       ifelse(directed, "->", "--"),
                       "'", edges_df[i, to_column], "'",
                       paste0(" [", attr_string, "] "))
            }

            # Generate a line of edge objects when an
            # attribute string doesn't exist
            if (!exists("attr_string")) {
              line <-
                paste0("  ",
                       "'", edges_df[i, from_column], "'",
                       ifelse(directed, "->", "--"),
                       "'", edges_df[i, to_column], "'",
                       " ")
            }
            edge_block <- c(edge_block, line)
          }
        }
      }

      # Construct the `edge_block` character object
      if (exists("edge_block")) {
        edge_block <- paste(edge_block, collapse = "\n")
      }
    }

    # Create the graph code from the chosen attributes,
    # and the nodes and edges blocks
    if (exists("combined_attr_stmts")) {
      if (exists("edge_block") & exists("node_block")) {
        combined_block <-
          paste(combined_attr_stmts,
                node_block, edge_block,
                sep = "\n")
      }
      if (!exists("edge_block") & exists("node_block")) {
        combined_block <-
          paste(combined_attr_stmts,
                node_block,
                sep = "\n")
      }
    }
    if (!exists("combined_attr_stmts")) {
      if (exists("edge_block")) {
        combined_block <- paste(node_block, edge_block,
                                sep = "\n")
      }
      if (!exists("edge_block")) {
        combined_block <- node_block
      }
    }

    # Create DOT code
    dot_code <-
      paste0(ifelse(directed, "digraph", "graph"),
             " {\n", "\n", combined_block, "\n}")

    # Remove empty node or edge attribute statements
    dot_code <- gsub(" \\[\\] ", "", dot_code)
  }

  dot_code
}

grviz.renderGraph = function (graph, layout = NULL, title = NULL, width = NULL, height = NULL, shinyInput.click = NULL)
{
  if(nrow(graph$nodes_df) == 0){dotstr = ""} else {
    fcn_name <- getCallingFunctionName()
    is.grviz.graph(graph) %>% gener::assert("The graph object is not valid", fcn_name)
    V1 <- V2 <- x <- y <- attr_type <- value_x <- NULL
    value <- hex <- fillcolor <- new_fillcolor <- NULL
    if (!is.null(title)) {
      graph %<>% grviz.addAttributes("label", title, "graph")
      graph %<>% grviz.addAttributes("labelloc", "t", "graph")
      graph %<>% grviz.addAttributes("labeljust", "c", "graph")
      graph %<>% grviz.addAttributes("fontname", "Helvetica", "graph")
      graph %<>% grviz.addAttributes("fontcolor", "gray30", "graph")
    }
    if (nrow(graph$nodes_df) > 0){
      if (!("fillcolor" %in% colnames(graph$nodes_df))) {
        if ("fillcolor" %in% graph$global_attrs$attr) {
          graph$nodes_df$fillcolor <- graph$global_attrs %>%
            dplyr::filter(attr == "fillcolor" & attr_type == "node") %>% dplyr::select(value) %>% purrr::flatten_chr()
        }
        else {
          graph$nodes_df$fillcolor <- "white"
        }
      }
    }
    if (nrow(graph$nodes_df) > 0) {
      if ("fillcolor" %in% colnames(graph$nodes_df)) {
        if ("fillcolor" %in% graph$global_attrs$attr) {
          graph$nodes_df$fillcolor[which(is.na(graph$nodes_df$fillcolor))] <- graph$global_attrs[which(graph$global_attrs$attr == "fillcolor"), 2]
        }
      }
    }
    if (!("fontcolor" %in% colnames(graph$nodes_df)) & "fillcolor" %in% colnames(graph$nodes_df)) {
      graph$nodes_df$fontcolor <- graph$nodes_df$fillcolor %>%
        dplyr::as_data_frame() %>% dplyr::mutate(value_x = contrastColors(color = value)) %>%
        dplyr::pull(value_x)
    }
    if (!is.null(layout)) {
      if (layout %in% c("circle", "tree", "kk", "fr", "nicely")) {
        graph %<>% grviz.addAttributes(attr = "layout", value = "neato", attr_type = "graph")
        if ("x" %in% colnames(graph$nodes_df)) {
          graph$nodes_df <- graph$nodes_df %>% dplyr::select(-x)
        }
        if ("y" %in% colnames(graph$nodes_df)) {
          graph$nodes_df <- graph$nodes_df %>% dplyr::select(-y)
        }
        if (layout == "circle") {
          coords <- graph %>% to_igraph() %>% igraph::layout_in_circle() %>%
            dplyr::as_tibble() %>%
            dplyr::rename(x = V1, y = V2) %>% dplyr::mutate(x = x * 1.25) %>% dplyr::mutate(y = y * 1.25)
        }
        if (layout == "tree") {
          coords <- (graph %>% to_igraph() %>% igraph::layout_with_sugiyama())[[2]] %>%
            dplyr::as_tibble() %>% dplyr::rename(x = V1, y = V2)
        }
        if (layout == "kk") {
          coords <- graph %>% to_igraph() %>% igraph::layout_with_kk() %>%
            dplyr::as_tibble() %>% dplyr::rename(x = V1, y = V2)
        }
        if (layout == "fr") {
          coords <- graph %>% to_igraph() %>% igraph::layout_with_fr() %>%
            dplyr::as_tibble() %>% dplyr::rename(x = V1, y = V2)
        }
        if (layout == "nicely") {
          coords <- graph %>% to_igraph() %>% igraph::layout_nicely() %>%
            dplyr::as_tibble() %>% dplyr::rename(x = V1, y = V2)
        }
        graph$nodes_df <- graph$nodes_df %>% dplyr::bind_cols(coords)
      }
    }
    dotstr = graph %>% grviz.generateDot}

  dotstr %>% grviz(width = width, height = height, shinyInput.click = shinyInput.click)
}

# task.js = JS('
#   $(document).on("click", "a", function(e){ console.log($(this).text())} )
# ')
#
#
# task.js = JS('
#   $(document).on("click", "a", function(e){ console.log($(this))} )
# ')
#
# task.js2 = JS('
#   $(document).on("click", function(e){e.preventDefault();
#   Shiny.onInputChange("nicolas", Object.getOwnPropertyNames($(this).context.activeElement.childNodes[1].childNodes[1].))})
# ')
#
# task.js2 = JS('
#   $(document).on("click", "a", function(e){e.preventDefault();
#   Shiny.onInputChange("nicolas", Math.random() + Object.getOwnPropertyNames($(this).context.childNodes[1].parentNode))})
# ')

grviz.task.js = JS('
                   $(document).on("click", "a", function(e){e.preventDefault();
                   Shiny.onInputChange("nicolas", Math.random() + " --> " + $(this)[0].childNodes[3].firstChild.data)})
                   ')


grviz = function(diagram = "", engine = "dot", allow_subst = TRUE,
                 options = NULL, width = NULL, height = NULL, shinyInput.click = NULL)
{
  if(!is.null(shinyInput.click)){
    # shinyInput.click %<>% verify('character', varname = 'shinyInput.click') Already verified!
    task.js = JS('$(document).on("click", "a", function(e){e.preventDefault();
                 Shiny.onInputChange("' %++% shinyInput.click %++% '", Math.random() + " --> " + $(this)[0].childNodes[3].firstChild.data)})')
} else {task.js = NULL}
  if (inherits(diagram, "connection") || file.exists(diagram)) {
    diagram <- readLines(diagram, encoding = "UTF-8", warn = FALSE)
    diagram <- paste0(diagram, collapse = "\n")
  }
  else {
    if (length(diagram) > 1) {diagram <- paste0(diagram, collapse = "\n")}
  }
  if (allow_subst == TRUE) {diagram <- replaceInSpec(diagram)}
  diagram <- gsub(x = diagram, "'", "\"")
  x <- list(diagram = diagram, config = list(engine = engine, options = options, tasks = task.js))
  # x <- list(diagram = diagram, config = list(engine = engine, options = options))
  viewer.suppress <- rstudioapi::isAvailable() && !rstudioapi::isAvailable("0.99.120")
  htmlwidgets::createWidget(name = "grviz", x = x, width = width,
                            height = height, package = "viser", htmlwidgets::sizingPolicy(viewer.suppress = viewer.suppress))
  }

grvizOutput <- function(outputId, width = '100%', height = '400px') {
  shinyWidgetOutput(outputId = outputId, 'grviz', width, height, package = 'viser')
}

renderGrviz <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) expr <- substitute(expr)
  shinyRenderWidget(expr, grvizOutput, env, quoted = TRUE)
}


