
# dt.R ----------------------------------------------------------------


# Header
# Filename:       dt.R
# Description:    Contains functions for plotting various table charts from DT package using standrad inputs.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     25 December 2016
# Last Revision:  23 November 2026
# Version:        1.1.7
#

# Version History:

# Version   Date                Action
# ----------------------------------
# 1.0.0     25 December 2016    Initial issue transferred from niraPlotter.R
# 1.1.0     27 March 2017       Fundamental changes with standard arguments. Calls function prepare4Plot to generate considerations for special dimensions
# 1.1.1     28 March 2017       DT.defset added
# 1.1.4     23 February 2018    Functions DT.applyConfig2Obj() and DT.applyConfig() and DT.options() added. Currently only take care of property 'links'
# 1.1.5     28 February 2018    config property 'paging.length' added.
# 1.1.5     28 February 2018    config property 'autoWidth' added.
# 1.1.7     23 November 2026    Duplicated code generating link columns moved to function DT.addLinks(), used by DT.applyConfig2Obj() and DT.Table()

#' @include visgen.R

# Default settings for package DT:
DT.defset = defset %<==>% list(
  # Valid classes for all dimensions
  dimclass  = list(label = valid.classes),
  multiples = 'label',
  withRowNames  = T
)

# Extra Arguments for function DT.table:
# Legend for setting argument class:
# display:	    Short-hand for the stripe, hover, row-border and order-column classes.
# cell-border:  Border around all four sides of each cell
# compact:      Reduce the amount of white-space the default styling for the DataTable uses, increasing the information density on screen (since 1.10.1)
# hover:        Row highlighting on mouse over
# nowrap:       Disable wrapping of content in the table, so all text in the cells is on a single line (since 1.10.1)
# order-column: Highlight the column that the table data is currently ordered on
# row-border:   Border around only the top an bottom of each each (i.e. for the rows).
# stripe:       Row striping
#
# You can use multiple options together. For example: style = 'cell-border stripe'. Note cell-border and row-border are mutually exclusive and cannot be used together.


# New function for plotting DT tables
DT.table = function(obj, label = NULL, config = NULL, sessionobj = NULL, ...){
  if (is.null(label)){label = as.list(names(obj))}
  # Verifications:
  assert(require(DT), "Package DT is not installed!", err_src = match.call()[[1]])
  config = DT.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  # Preparing Aesthetics:
  label %<>% nameList(gndcd(171,39,69,162,199))

  obj %<>% prepare4Plot(list(label = label), config = config) %>% DT.applyConfig2Obj(config)

  DT::datatable(obj, escape = F, options = config %>% DT.options(obj, sessionobj), ...) %>% DT.applyConfig(config)
}

# Adds html link (<a>) columns to the table.
# links: a list of link definitions, each a list with elements:
#   column.name (column to be added/replaced), class.name, href, inputs (named vector: data attribute names -> source columns), icon and text
DT.addLinks = function(obj, links){
  verify(links, 'list')
  for (link in links){
    nms  = names(link)

    scr = '<a '
    if ('class.name' %in% nms){scr = paste0(scr, 'class="', link$class.name, '" ')}
    if ('href'       %in% nms){scr = paste0(scr, 'href="', link$href, '" ')}
    if ('inputs'     %in% nms){
      for (j in names(link$inputs)){
        scr = paste0(scr, ' data-', j, "=\"", obj[,link$inputs[j]], "\"")
      }
    }
    scr = paste0(scr, '>')
    if ('icon' %in% nms){scr = paste0(scr, '<i class="', link$icon, '"></i>')}
    if ('text' %in% nms){scr = paste0(scr, link$text)}
    scr = paste0(scr, '</a>')

    if(nrow(obj) == length(scr)){obj[, link$column.name] = scr}
  }
  return(obj)
}

# This function changes the data.frame according to the config and returns the modified data.frame
# input argument obj must be the data.frame which is going to be plotted.
# This function currently only takes care of property 'links' in the config
# If any other properties impact the data.frame to be plotted, you should add it here
DT.applyConfig2Obj = function(obj, config){
  if(!is.null(config$links)){obj %<>% DT.addLinks(config$links)}
  return(obj)
}


# This function applies config to the DT object
DT.applyConfig = function(obj, config){
  return(obj)
}

# This function modifies the options argument that needs to be passed to the plotter function
# in order to apply  some of the config properties to the plot.
# todo: Create appropriate config parameters to add these options https://datatables.net/reference/option/
# Currently only respects property 'links'
DT.options = function(config, df, sessionobj = NULL){
  opt = config %>% list.extract('autoWidth')
  if(!is.null(config$links) & !is.null(sessionobj)){
    action   <- DT::dataTableAjax(sessionobj, df)
    opt$ajax %<>% verify('list', default = list()) %<==>% list(url = action)
  }

  opt$pageLength = config$paging.length %>% verify(c('integer', 'numeric'), lengths = 1, domain = c(1,Inf), default = 10)
  return(opt)
}

#' Legacy DT table functions
#'
#' Older functions for DT tables, kept for existing code. For new code, use
#' \code{rvisPlot("table", "DT", ...)} (see the \href{../articles/tables.html}{tables guide}).
#'
#' \code{DT.Table()} makes a DT table of \code{x} without escaping html. A list \code{options$header}
#' gives CSS properties of the header row. \code{links} adds action links to cells (they need the shiny
#' \code{session}). \code{DT.Table.Classify.bold()} makes the rows bold whose value in column
#' \code{classColumn} is one of \code{boldValues}.
#'
#' @param x A data frame (\code{DT.Table()}) or a DT table (\code{DT.Table.Classify.bold()}).
#' @param links Optional list of links added to the cells.
#' @param session Shiny session, needed with \code{links}.
#' @param options List of DataTables options, passed to \code{DT::datatable()}.
#' @param rownames Whether to show the row names.
#' @param classColumn Name of the column deciding which rows are bold.
#' @param boldValues Values of \code{classColumn} whose rows are bold.
#' @return A DT table (htmlwidget).
#' @examples
#' \dontrun{
#' tbl <- DT.Table(head(mtcars), options = list(header = list(`background-color` = "#ddd")))
#' DT.Table.Classify.bold(DT.Table(data.frame(a = 1:3, big = c(FALSE, TRUE, FALSE))), "big")
#' }
#' @name DT.Table
#' @export
DT.Table = function(x, links = NULL, session = NULL, options = list(), rownames = T){
  TBL = x
  opt = options

  if (!is.null(options$header)){
    scr = paste0("$(this.api().table().header()).css(", list2Json(options$header, quotation = T), ");")
    opt$initComplete = JS("function(settings, json) {", scr, "}")
    opt$header = NULL
  }

  if (!is.null(links)){
    TBL %<>% DT.addLinks(links)
    action   <- DT::dataTableAjax(session, TBL)
    opt$ajax <- list(url = action)
  }
  return(DT::datatable(TBL, escape = FALSE, options = opt, rownames = rownames))
}

#' @rdname DT.Table
#' @export
DT.Table.Classify.bold = function(x, classColumn, boldValues = 'TRUE'){
  assert(inherits(x, 'datatables'), "Class verification failed! Argument 'x' must be of class 'datatables'!")
  lvls = unique(x$x$data[,classColumn])
  vals = rep('normal', length(lvls))
  vals[lvls %in% boldValues] = 'bold'
  DT::formatStyle(x, classColumn, target = 'row', fontWeight = DT::styleEqual(levels = lvls, values = vals))
}
