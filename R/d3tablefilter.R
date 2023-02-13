# d3tablefilter.R ----------------------------------------------------------------


# Header
# Filename:       d3tablefilter.R
# Description:    Contains functions for plotting table charts from D3TableFilter package using standrad inputs.
# Author:         Nicolas Berta
# Email :         nima.ramezani@gmail.com
# Start Date:     26 April 2017
# Last Revision:  05 June 2018
# Version:        0.0.1
#

# Version History:

# Version   Date                Action
# ----------------------------------
# 0.0.1     02 February 2023    Initial issue

#' @include visgen.R
#' @include tfd3.R

D3TableFilter.table.defset = TFD3.table.defset

D3TableFilter.addColumnTypes = TFD3.addColumnTypes

D3TableFilter.tableprops = TFD3.tableprops

D3TableFilter.config.verify = TFD3.config.verify

D3TableFilter.footData = TFD3.footData

D3TableFilter.rowStyles = TFD3.rowStyles

D3TableFilter.edit = TFD3.edit

D3TableFilter.lastEdits.empty = TFD3.lastEdits.empty

D3TableFilter.initialFilters = TFD3.initialFilters

D3TableFilter.applyFilterstr = TFD3.applyFilterstr

D3TableFilter.filteredRows = function(obj, config){
  ff = obj %>% nrow %>% sequence
  for(i in names(config$column.filter)){
    if (i == 'rownames'){
      ff = ff %^% (rownames(obj) %>% D3TableFilter.applyFilterstr(config$column.filter[[i]]))
    } else {
      ff = ff %^% (obj[, i] %>% D3TableFilter.applyFilterstr(config$column.filter[[i]]))
    }
  }
  return(ff)
}

D3TableFilter.colNames = TFD3.colNames

D3TableFilter.bgColScales = function(obj, config){
  bgcs = list()
  nms  = c(chif(config$withRowNames,'rownames',NULL), colnames(obj))
  for (cc in names(config$column.color)){
    w = which(nms == cc) - 1
    if(config$column.color[[cc]] %>% unique %>% length == 1){
      scr = D3TableFilter.color.single.js(config$column.color[[cc]] %>% unique)
    } else if(config$column.color.auto[[cc]]){
      scr = paste('auto', config$column.color[[cc]] %>% paste(collapse = ':'), sep = ':')
    } else if(inherits(obj[, cc], valid.numeric.classes)){
      scr = D3TableFilter.color.numeric.js(domain = obj[, cc], range = config$column.color[[cc]])
    } else if (inherits(obj[, cc], valid.nominal.classes)){
      scr = D3TableFilter.color.nominal.js(domain = obj[, cc], range = config$column.color[[cc]])
    } else {scr = ''}
    if(!is.empty(scr)){for (i in w){bgcs[[paste0('col_', i)]] <- scr}}
  }
  return(bgcs)
}

D3TableFilter.table = function(obj, label = NULL, color = NULL, shape = NULL, config = NULL, ...){
  if((nrow(obj) == 0) & (ncol(obj) == 0)){return(NULL)}
  if (is.null(label)){label = as.list(names(obj))}
  # Verifications:
  # assert(require(D3TableFilter), "Package D3TableFilter is not installed!", err_src = match.call()[[1]])
  config = D3TableFilter.table.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
    D3TableFilter.config.verify

  config$column.title %<>% verify('list', names_domain = c(chif(config$withRowNames,'rownames',NULL), colnames(obj)), varname = 'config$column.title')
  # config$column.type  %<>% verify('list', names_domain = c(chif(config$withRowNames,'rownames',NULL), colnames(obj)), domain = c(config$dimclass$label, 'prettyDate', 'prettyTime', 'prettyTimeDate'), default = obj %>% apply(2,class) %>% as.list, varname = 'config$column.type')

  # Preparing Aesthetics:
  # Preparing Aesthetics:
  a = prepareAesthetics(label = label, color = color, shape = shape, extend = c('label','color', 'shape'))
  L = a$labels
  A = a$aesthetics %>% list.remove(gndcd(112,86,2,135,162))

  obj %<>% prepare4Plot(A, config)
  if (!inherits(obj, 'data.frame')){return(NULL)}
  if (ncol(obj) == 0){return(NULL)}

  names(obj) <- names(obj) %>% make.unique('.1')

  # Specify background color from argument 'color':
  bgColScales = list()
  for(i in seq(L$color)){
    if(!is.empty(color[[i]])){
      if(L$color[i] %in% L$label){L$color[i] %<>% paste('1', sep = '.')}
      lin = paste0('col_', i)# list item name
      if (obj[, L$color[i]] %>% unique %>% length == 1){
        bgColScales[[lin]] = D3TableFilter.color.single.js(obj[1, L$color[i]])
      } else if (obj[, L$color[i]] %>% length == nrow(obj)){
        if(inherits(obj[,L$label[i]], valid.numeric.classes)){
          bgColScales[[lin]] = D3TableFilter.color.numeric.js(domain = obj[, L$label[i]], range = obj[, L$color[i]])
        } else {
          bgColScales[[lin]] = D3TableFilter.color.nominal.js(domain = obj[, L$label[i]], range = obj[, L$color[i]])
        }
      }
    }
  }

  if(is.null(L$color)){bgColScales = D3TableFilter.bgColScales(obj, config)}

  if(is.null(L$shape)){
    if(!is.null(config$column.shape)){
      L$shape = rep('', length(L$label))
      for (i in names(config$column.shape)){
        w = which(L$label == i)
        L$shape[w] = config$column.shape[[i]]
      }
    }
  }
  # turn cell values into scaled SVG graphics from argument 'shape':
  cellFunctions = list()
  for(i in seq(L$shape)){
    shp = L$shape[i]
    if(!is.empty(shp)){
      lin = paste0('col_', i)# list item name
      if      (shp == 'bar'){cellFunctions[[lin]] = D3TableFilter.shape.bar.js()}
      else if (shp %in% c('bubble', 'circle', 'point', 'dot')){cellFunctions[[lin]] = D3TableFilter.shape.bubble.js()}
    }
  }

  footCellFunctions = list()
  nms = c('rownames', colnames(obj))
  for (col in names(config$column.footer.font)){
    wch = which(nms == col) - 1
    for (cn in wch){
      lin = paste0('col_', cn)# list item name
      footCellFunctions[[lin]] = D3TableFilter.font.js(
        side   = config$column.footer.font[[col]]$adjust,
        format = config$column.footer.font[[col]]$format,
        weight = config$column.footer.font[[col]]$weight)
    }
  }

  wcb = which(L$shape == 'checkBox')
  wrb = which(L$shape == 'radioButtons')

  obj[, L$label] %>% D3TableFilter::d3tf(
    colNames     = D3TableFilter.colNames(config) %>% unname,
    bgColScales  = bgColScales,
    cellFunctions = cellFunctions,
    footCellFunctions = footCellFunctions,
    showRowNames = config$withRowNames,
    enableTf     = config$column.filter.enabled,
    filterInput  = config$column.filter.enabled,
    edit         = L$label %>% D3TableFilter.edit(config),
    checkBoxes   = chif(is.empty(wcb), NULL, 'col_' %++% wcb),
    radioButtons = chif(is.empty(wcb), NULL, 'col_' %++% wrb),
    initialFilters = D3TableFilter.initialFilters(L$label, config),
    footData = D3TableFilter.footData(obj[, L$label], config),
    tableStyle = config$table.style,
    selectableRows = config$selection.mode,
    selectableRowsClass = config$selection.color,
    rowStyles = D3TableFilter.rowStyles(obj[, L$label], config),
    tableProps = config %>% D3TableFilter.tableprops,
    extensions = config$extensions,
    height = config$height,
    width  = config$width,
    ...)
}

