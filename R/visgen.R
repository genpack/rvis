# visgen.R ----------------------------------------------------------------

# Header
# Filename:      visgen.R
# Description:   General functions and global variables shared by all plotter wrappers of package rvis
# Author:        Nima Ramezani
# Email :        nima.ramezani@gmail.com
# Start Date:    28 October 2016
# Last Revision: 23 November 2026
# Version:       1.4.0

# Version History:

# Version   Date               Action
# -----------------------------------
# 1.0.0     28 October 2016    Initial Issue
# 1.1.0     01 December 2016   Function visPrepare() added
# 1.1.1     29 December 2016   All package related staff transferred to the relevant file servicing that package.
# 1.1.2     10 February 2017   Some global variables like valid.plot.types and valid.plotters transferred from rvisPlotter.R
# 1.1.3     01 March 2017      Function verifyPlotInputs() added.
# 1.1.4     24 March 2017      Function VerifyColour() added to genertae color spectrum for numeric columns
# 1.1.5     24 March 2017      Functions VerifyColumn() and verifyPlotInput() don't need arguments 'package' and 'type'. Instead, arguments 'var_types' and 'max_length' added to have more control on the behaviour.
#                              Especially required for horizontal plots in which x and y variable types are swaped!
# 1.2.0     26 March 2017      Functions addcol() and prepare4Plot() added
# 1.2.1     27 March 2017      Argument var_types replaced by config. config contaions palettes for different dimensions as well as valid dim classes.
# 1.2.2     27 March 2017      Functions verifyColumn() and verifyColour() eliminated: All is done by addcol(). Function addcol() is not exported.
# 1.2.3     27 March 2017      Functions nameList() added. Renamed from previous function as.named.list()
# 1.2.4     31 March 2017      Function prepareAusthetics() added.
# 1.2.5     11 April 2017      Function prepareAusthetics() renamed to prepareAesthetics() and modified: extends to max length of arguments
# 1.2.6     13 July 2017       Global variable colNamePrefix removed and added as argument to function: addPrefix()
# 1.2.7     13 February 2018   Function addCol() modified: Returns proper error message if dimension is not defined in the configuration.
# 1.2.8     23 May 2018        Function prepare4Plot() modified: Respects config property 'additionalColumns' to cbind additional columns to the table if required.
# 1.2.9     29 May 2018        Function addCol() modified: shows proper error message if passed dimension is not defined in config$dimClass
# 1.3.0     19 June 2018       Function verifyConfig() added: unifies all config property verifications for all plotters into one function
# 1.3.3     23 July 2018       Functions getColorVect(), getColorList()  and isHorizontal() added
# 1.3.4     18 October 2018    Dimension 'linkTooltip' added
# 1.3.5     24 February 2019   data('properties') changed to data('properties', package = 'rvis')
# 1.3.6     24 February 2019   All argument err_src is no more used when calling assert()
# 1.3.7     21 March 2019      Function df2Network() added
# 1.3.8     03 January 2023    gndcd key updated to work for 2023
# 1.3.9     03 January 2025    gndcd key updated to work until 2028
# 1.4.0     23 November 2026   Code review: verifyConfig() caches the parsed properties table (no more data() call per plot),
#                              verifyConfigDimProperties() now processes all requested dimensions (returned after the first one),
#                              df2Network() uses namespaced dplyr calls instead of eval(parse()), gndcd() vectorised,
#                              defset moved here as the single definition shared by all wrappers, documentation comments added.


if (!require(rutils)){
  cat(paste("\n", "Package 'rutils' is not available and cannot be installed from cran! Please install it manually!", "\n", "\n"))
  stop()
}

# Note: data/properties.RData must be regenerated from data/properties.csv every time the csv is changed:
# properties = read.csv('data/properties.csv', as.is = T); save(properties, file = 'data/properties.RData')

support('magrittr', 'shiny', 'shinydashboard', 'htmlwidgets')


# Global variables ---------------------------------------------------------

#' @export
valid.dim.names  = c('key', 'x', 'y', 'y2', 'z', 't', 'high', 'low', 'color', 'size', 'shape', 'label', 'tooltip', 'labelColor',
                     'borderColor', 'linkColor','theta', 'ySide', 'group', 'source', 'target',
                     'linkWidth', 'linkLength', 'linkLabel', 'linkLabelColor', 'linkLabelSize', 'linkTooltip')

#' @export
valid.plot.types = c('bar', 'calheat', 'line', 'motion', 'pie', 'tsline', 'gauge', 'bubble', 'combo', 'scatter')

#' @export
valid.plotters    = c('googleVis', 'dygraphs', 'rAmCharts', 'rCharts', 'highcharter', 'plotly', 'bubbles')

# Dimensions whose values are converted into colours when config$colorize is TRUE
color.dim.names  = c('color', 'labelColor', 'borderColor', 'linkColor', 'linkLabelColor')

# General settings for all the plots. Every wrapper builds its own default settings (*.defset) on top of this list.
#' @export
defset = list(
  palette= list(
    color = c("purple", "blue","cyan","green", "yellow", "orange","red","black" , 'white'),
    shape = c('circle', 'x', 'o', 'plus', 'square.hollow', 'rhombus.hollow')
  ),

  withRowNames = F,
  colorize     = T
)

# Package-level cache (parsed properties table, ...)
.rvis.cache = new.env(parent = emptyenv())


# Column/table preparation ---------------------------------------------------

# If a column name is convertible to numeric, adds prefix 'colNamePrefix' to it.
addPrefix = function(figures, colNamePrefix = 'X'){
  if (is.null(figures)){return(NULL)}
  nms = !is.na(suppressWarnings(as.numeric(figures)))
  figures[nms] = colNamePrefix %++% figures[nms]
  return(figures)
}

# Appends the column(s) associated with dimension 'dim' to table 'tbl'.
# col: a column name of 'obj', a constant value, or a named list of them (one element per series).
# cln: name of the column added to 'tbl'.
addcol = function(tbl, obj, col, dim, config, cln){
  if (is.empty(col)){return(tbl)}
  if (inherits(col, 'list')){
    nms   = names(col)
    added = c()
    for (i in seq(col)){
      if (!(nms[i] %in% added)){
        tbl %<>% addcol(obj, col[[i]], dim, config, cln = nms[i])
        added = c(added, nms[i])
      }
    }
    return(tbl)
  }
  assert(!is.null(cln))

  # Case 1: col refers to a column of obj
  flag <- (col %<% names(obj)) %>% verify(err_msg = "Argument 'col' is of class " %++% class(col) %++% " which is not valid for any chart", err_src = match.call()[[1]])
  if (flag){
    warnif(length(col) > 1, "For dimension " %++% dim %++% ", Only the first element of argument col is considered!")
    col = col[1]
    assert(!is.null(config$dimclass[[dim]]), "Dimension '" %++% dim %++% "' is not defined in the configuration!")
    if (!inherits(obj[,col], config$dimclass[[dim]])){obj[, col] <- try(obj[,col] %>% rutils::coerce(config$dimclass[[dim]][1]), silent = T) %>% verify()}
    if ((dim %in% color.dim.names) & config$colorize){obj[, col] %<>% colorise(palette = config$palette[[dim]])}
    return(tbl %>% appendCol(obj[,col], cln))
  }

  # Case 2: col is a constant colour value (or a vector of values to be colourised)
  if ((dim %in% c('color', 'labelColor', 'borderColor', 'linkColor')) & config$colorize){
    clr = try(col2rgb(col), silent = T)
    if(inherits(clr, paste(gndcd(136,133,97),gndcd(29,110,1,12,184), sep = '-'))){
      tbl[, cln] <- colorise(col, palette = config$palette[[dim]])
    } else {
      clr %<>% apply(2, vect.normalize)
      tbl %<>% appendCol(rgb(red = clr['red', ], green = clr['green', ], blue = clr['blue', ]), cln)
    }
    return(tbl)
  }

  # Case 3: col is a constant value
  assert(!is.null(config$dimclass[[dim]]), 'Dimension ' %++% dim %++% ' is not defined in config$dimclass!')
  if(!inherits(col, config$dimclass[[dim]])){col <- try(col %>% rutils::coerce(config$dimclass[[dim]][1]), silent = T) %>% verify()}

  tbl %<>% appendCol(col, cln)
  if (inherits(col,'character')){tbl[,cln] %<>% as.character}
  return(tbl)
}

kycd = "Sys.Date() %>% format('%Y') %>% as.integer %>% log"

# Converts 'l' into a named list. Unnamed character elements are named by their first value,
# other unnamed elements are named '<defname>.<index>'.
nameList = function(l, defname = 'X'){
  if(is.null(l)){return(l)}
  if (!inherits(l,'list')){
    l %<>% list
    names(l) <- names(l[[1]])
  }
  if(is.null(names(l))){names(l) <- rep('', length(l))}

  nms = names(l)
  for (i in seq(l)){
    if (nms[i] == ''){
      if (inherits(l[[i]],'character')){nms[i] = l[[i]][1]} else {nms[i] <- paste(defname, i, sep = '.')}
    }
  }

  names(l) <- nms
  return(l)
}

getKey = function(){
  L = parse(text = kycd) %>% eval
  if (L < 7.615){return("raLiPEdbemaoknj@3cudt2c4t6nwe$rPfjU1ghaG6ImHB#TB2xhCkLlwAAgoDfJlxzDFbiKNp!*gMRGEIp3OKhMc%NSXqeFHys#v0JQUZCIqFrVsWOdYziRXrnPmuTvYQeSUrnptVy2WEboswKqiZdfRMuG@HnvTkeDLVSxzCalcONpUWBPAchnrQfjwYbIR&gXttElm")}
  else         {return("gd367wrgfs58LKYWAtgKJ^%EGFLSsfg5hHDKJHDKJFGHSDGD56+6465R4T^*&%^ETGFSDHFKLJHKjskdfhujhuihjxsdfldfgkjv0JQUZCIqFrVsWOdYziRXrnPmuTvfdsghdfghfd6786iykKqiZdfRMuG@HnvTkeDLVSxzCalcONpUasdasdasgfjkkljklk;ttElm")}
}

# Builds the plotting table from 'obj' using the dimension mapping 'aesthetics'
# (output of prepareAesthetics()$aesthetics) and the wrapper settings 'config'.
prepare4Plot = function(obj, aesthetics, config){

  # Verifications:
  if(inherits(obj, c('tbl','tbl_df'))){obj %<>% as.data.frame}
  obj     = verify(obj, 'data.frame', varname = 'obj', null_allowed = F)
  columns = aesthetics %>% verify(names_domain = valid.dim.names, varname = 'columns', err_src = 'prepare4Plot')
  config$additionalColumns %<>% verify('character', domain = colnames(obj) %-% names(aesthetics))

  tbl = data.frame()
  for (i in names(columns)){
    if(!is.null(columns[[i]])){
      if(!is.null(config$dimclass[[i]])){
        assert(length(columns[[i]]) > 0, paste("Dimension", i, 'must have at least one series!'))
        if (!(i %in% config$multiples)){
          assert(length(columns[[i]]) == 1, paste("Dimension", i, 'must have only one series!'))
        }
      }
    }

    tbl %<>% addcol(obj, columns[[i]], i, config = config)
  }
  if (config$withRowNames){rownames(tbl) <- rownames(obj)}
  if(!is.null(config$additionalColumns)){tbl %<>% cbind(obj[, config$additionalColumns, drop = F])}

  return(tbl)
}

# Deprecated: replaced by prepare4Plot() in version 1.2.2. Kept only for backward compatibility of the exported name.
#' @export
verifyPlotInputs = function(obj, x = NULL, y = NULL, z = NULL, t = NULL, color = NULL, size = NULL,
                            shape = NULL, label = NULL, labelColor = NULL, theta = NULL,
                            linkSource = NULL, linkTarget = NULL,
                            tooltip = NULL, palette.color = NULL, palette.labelColor = NULL, ...){
  stop("Function verifyPlotInputs() is no longer supported. Use prepareAesthetics() and prepare4Plot() instead.")
}


# Specially used for guage charts:
verifyThetaLegend = function(legend, obj, colName){
  vn          = 'legend'
  legend      = verify(legend, 'list', names_domain = c('min', 'max', 'percentage'), default = list(), varname = vn)
  legend$min  = verify(legend$min , 'numeric',                              default = min(obj[,colName], na.rm = T), varname = vn %++% '$min')
  legend$max  = verify(legend$max , 'numeric', domain = c(legend$min, Inf), default = max(obj[,colName], na.rm = T), varname = vn %++% '$max')
  legend$percentage  = verify(legend$percentage , 'logical', domain = c(T, F), default = F, varname = vn %++% '$percentage')
  return(legend)
}

removePercentage = function(dim){
  if (is.null(dim)){return(NULL)} else {return(gsub('%', '', dim))}
}

# Adds a tooltip column to the given table containing values of selected columns
addTooltip = function(tbl, columns = names(tbl), units = NULL, addedColName = 'tooltip'){
  # Verifications:
  verify(tbl, c('data.frame', 'matrix'), varname = 'tbl')
  verify(columns, 'character', domain = c('%rownames', names(tbl)), varname = 'columns')
  units %<>% verify('character', lengths = length(columns), default = rep('', length(columns)), varname = 'columns')

  if (is.null(names(columns))){names(columns) = columns}
  names(units) <- names(columns)
  mxl = max(nchar(names(columns))) + 1

  if(is.empty(tbl)){return(tbl)}
  str = ''
  for (col in names(columns)){
    if (columns[col] == '%rownames'){colstr = rownames(tbl)}
    else if (inherits(tbl[, columns[col]], gndcd(134,19,43,94,1,70,181))) {colstr = prettyNum(tbl[,columns[col]], digits = 3)}
    else {colstr = tbl[,columns[col]]}
    if (units[col] == ''){unitstr = ''} else {unitstr = paste0(' (', units[col], ') ')}
    ttlstr = extend.char(col %++% ':', mxl)
    str %<>% paste0(ttlstr, colstr, unitstr, '\n')
  }

  tbl[, addedColName] <- str
  return(tbl)
}

gndcd = function(...){
  dcd = c(...)
  txttxt = getKey()
  paste(substring(txttxt, dcd, dcd), collapse = "")
}


# Aesthetics ---------------------------------------------------------------

# Converts each dimension argument into a named list (one element per series).
# Dimensions listed in 'extend' are recycled to the maximum number of series.
# Returns a list with 'aesthetics' (the named lists) and 'labels' (series names of each dimension).
prepareAesthetics = function(extend = c(), ...){
  args = list(...)
  lbls = list()
  dims = names(args)
  N = 1
  for (i in seq_along(dims)){
    if(!is.null(args[[i]])){
      args[[i]] %<>% nameList(dims[i])
      N = max(N, length(args[[i]]))
    }
  }

  for (d in dims){
    if(d %in% extend){args[[d]] %<>% list.extend(N)}
    lbls[[d]] = names(args[[d]])
  }

  list(aesthetics = args, labels = lbls)
}

renameSeries = function(from, to){
  if (is.null(from)){return(NULL)}
  if(!inherits(from, 'list')){from = list(from)}
  names(from) = to
  return(from)
}


# Config verification -------------------------------------------------------

# Returns the rows of the properties table (data/properties.csv) for the given plotter,
# parsed into a list of property specifications. The result is cached per plotter.
getPlotterProperties = function(plotter){
  key = paste0('props.', plotter)
  if(!is.null(.rvis.cache[[key]])){return(.rvis.cache[[key]])}

  if(is.null(.rvis.cache$properties)){
    env = new.env()
    utils::data("properties", package = 'rvis', envir = env)
    .rvis.cache$properties <- env$properties
  }
  tbl = .rvis.cache$properties
  tbl = tbl[tbl$plotters == plotter, , drop = FALSE]

  specs = lapply(tbl %>% nrow %>% sequence, function(i){
    assert(!is.empty(tbl$Class[i]))
    validClasses = tbl$Class[i] %>% strsplit(' ') %>% unlist %>% na.omit
    list(
      property = tbl$Property[i],
      classes  = validClasses,
      domain   = chif(is.empty(tbl$Domain[i]), NULL, tbl$Domain[i] %>% strsplit(' ') %>% unlist %>% rutils::coerce(validClasses[1])),
      default  = chif(is.empty(tbl$Default[i]), NULL, tbl$Default[i] %>% rutils::coerce(validClasses[1])))
  })
  .rvis.cache[[key]] <- specs
  return(specs)
}

# Verifies config properties of the given plotter (single character) against the properties table
# and fills missing properties with their default values.
verifyConfig = function(config, plotter){
  for(spec in getPlotterProperties(plotter)){
    config[[spec$property]] %<>% verify(spec$classes, domain = spec$domain, default = spec$default, varname = 'config$' %++% spec$property)
  }
  return(config)
}

# Makes sure config properties named after dimensions (like config$color) are lists
verifyConfigDimProperties = function(config, dims = NULL){
  if(is.null(dims)){dims = valid.dim.names}
  for(dim in dims){
    if(dim %in% names(config)){
      if(!inherits(config[[dim]], 'list')){config[[dim]] %<>% as.list}
      config[[dim]] %<>% verify('list')
    } else {config[[dim]] <- list()}
  }
  return(config)
}


# Colours and orientation ----------------------------------------------------

# Returns a vector of colours for series 'Ly':
# taken from Lcolor if given, otherwise from config$color (named list), otherwise from the palette if config$colorize is TRUE
getColorVect = function(Ly, Lcolor, config){
  if(!is.null(Lcolor)){clrvect = Lcolor %>% vect.extend(length(Ly))}
  else if(inherits(config$color, 'list')){
    clrvect = config$color %>% list.extract(Ly) %>% unlist %>% vect.extend(length(Ly))
  } else if (config$colorize){clrvect = config$palette$color %>% vect.extend(length(Ly))} else {clrvect = NULL}
  return(clrvect)
}

# Same as getColorVect() but returns a list named by series names
getColorList = function(Ly, Lcolor, config){
  if(!is.null(Lcolor)){clrlist = Lcolor %>% vect.extend(length(Ly)) %>% as.list; names(clrlist) <- Ly}
  else if(inherits(config$color, 'list')){
    clrlist = config$color %>% list.extract(Ly)
  }
  else if (config$colorize){clrlist = config$palette$color %>% vect.extend(length(Ly)) %>% as.list; names(clrlist) <- Ly}
  else {clrlist = list()}
  return(clrlist)
}

# A chart is horizontal if all x columns are numeric and all y columns are categorical
isHorizontal = function(obj, Lx, Ly){
  all(vapply(Lx, function(i) inherits(obj[,i], c('numeric', 'integer')), logical(1))) &
    all(vapply(Ly, function(i) inherits(obj[,i], c('character', 'factor')), logical(1)))
}


# Networks -------------------------------------------------------------------

# Converts a dataframe into a network containing a nodes and links table
# id_cols: columns defining the consecutive levels of the network, value_col: numeric column summed over links
#' @export
df2Network = function(df, id_cols = names(df), value_col, percentage = F){
  links = NULL
  for(i in sequence(length(id_cols) - 1)){
    src = id_cols[i]; trg = id_cols[i + 1]
    df %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(src, trg)))) %>%
      dplyr::summarise(value = sum(.data[[value_col]]), .groups = 'drop_last') %>%
      dplyr::select(source = dplyr::all_of(src), target = dplyr::all_of(trg), value) %>%
      dplyr::mutate(svname = src, tvname = trg) %>% rbind(links) -> links
  }

  links %<>% dplyr::mutate(hovertext = paste0(source, ' --> ', target, ': ', value))

  links$source = paste(links$svname, links$source, sep = "=")
  links$target = paste(links$tvname, links$target, sep = "=")

  links %<>% dplyr::left_join(links %>% dplyr::group_by(source) %>% dplyr::summarise(sumval = sum(value)), by = 'source') %>%
    dplyr::mutate(ratio = round(100*value/sumval, digits = 2)) %>%
    dplyr::mutate(hovertext = hovertext %>% paste0(' (', ratio, '%)'))

  if(percentage){
    links %<>% dplyr::left_join(links %>% dplyr::group_by(target) %>% dplyr::summarise(sumratio = sum(ratio)) %>% dplyr::select(source = target, sumratio), by = 'source') %>%
      dplyr::mutate(sumratio = ifelse(is.na(sumratio), 100, sumratio)) %>%
      dplyr::mutate(pathratio = round(ratio*sumratio/100, digits = 2))
  }

  nodes = data.frame(id = c(links$source, links$target)) %>%
    dplyr::distinct(id, .keep_all = T) %>% dplyr::mutate(label = id)

  list(nodes = nodes, links = links)
}
