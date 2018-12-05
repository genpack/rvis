# tfd3.R ----------------------------------------------------------------


# Header
# Filename:       tfd3.R
# Description:    Contains functions for plotting table charts from TFD3 package using standrad inputs.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     26 April 2017
# Last Revision:  05 June 2018
# Version:        0.0.9
#

# Version History:

# Version   Date                Action
# ----------------------------------
# 0.0.1     26 April 2017       Initial issue
# 0.0.2     14 June 2017        bug in TFD3.bgColScales() rectified.
# 0.0.3     14 June 2017        TFD3.config.verify modified: If config$column.color.auto is missing, fills with default value (TRUE)
# 0.0.4     31 August 2017      Function TFD3.table() modified: Argument extensions applied to TFD3
# 0.0.5     12 September 2017   Function TFD3.tableprops() modified: col_10 extended to col_100
# 0.0.6     01 November 2017    Function TFD3.tableprops() modified: Arguments related to filter pop-up added
# 0.0.7     15 January 2017     Function TFD3.config.verify() modified: property 'sort' added to checks
# 0.0.8     28 February 2018    config property paging_length renamed to paging.length same in DT
# 0.0.9     05 June 2018        column labels unnamed before being passed to argument 'colNames'

#' @include visgen.R

# Default settings for package DT:
TFD3.table.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass  = list(
    label = valid.classes,
    color = valid.classes),
  multiples = c('label', 'color'),
  withRowNames  = T,
  column.filter.enabled = TRUE
)

TFD3.table.molten.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass  = list(
    label = valid.classes,
    color = valid.classes,
    shape = 'character',
    group = valid.nominal.classes),
  multiples = c('label', 'color', 'shape'),
  withRowNames  = F,
  column.filter.enabled = FALSE
)

TFD3.addColumnTypes = function(config, obj){
  TFD3.column_type = c(numeric = 'Number', character = 'String', Date = 'Date')
  types = apply(obj, 2, class)
  names(types) = NULL
  types = TFD3.column_type['type']
  types[is.na(types)] <- 'None'
  config$sort_config %<>% add.list(sort_types = types)
  return(config)
}

# Check here for complete reference:
# http://tablefilter.free.fr/doc.php

TFD3.tableprops = function(config){
  tblprp = config %>% list.extract('fixed_headers', 'tbody_height', 'filters_cell_tag', 'col_width', 'popup_filters', 'popup_filters_image', 'popup_filters_image_active',
                                   'popup_filters_image_html', 'popup_div_css_class', 'on_before_popup_filter_open', 'on_after_popup_filter_open', 'on_before_popup_filter_close', 'on_after_popup_filter_close',
                                   'inf_div_css_class', 'left_div_css_class', 'right_div_css_class', 'middle_div_css_class', 'paging',
                                   'flts_row_css_class', 'flt_css_class', 'flt_small_css_class', 'flt_multi_css_class', 'single_flt_css_class',
                                   'highlight_css_class', 'paging_slc_css_class', 'even_row_css_class', 'odd_row_css_class', 'btn_css_class', 'btn_reset_css_class',
                                   'input_watermark_css_class', 'active_columns_css_class', 'nb_pages_css_class', 'paging_btn_css_class',
                                   'on_keyup', 'on_keyup_delay',
                                   'grid', 'search_type', 'refresh_filters', 'rows_always_visible',
                                   'col_operation', 'exact_match', 'custom_cell_data',
                                   'btn', 'btn_text','btn_reset', 'btn_reset_text', 'btn_reset_html', 'btn_reset_target_id', 'btn_next_page_text', 'btn_prev_page_text', 'btn_last_page_text', 'btn_first_page_text',
                                   'btn_next_page_html', 'btn_prev_page_html', 'btn_last_page_html', 'btn_first_page_html',
                                   'page_text', 'of_text', 'showHide_cols_at_start',
                                   'sort', 'sort_select', 'sort_num_asc', 'sort_num_desc',
                                   'slc_filling_method', 'multiple_slc_tooltip',
                                   'rows_counter', 'rows_counter_text', 'col_number_format', 'sort_config', 'rows_always_visible',
                                   paste('col', 0:100, sep = '_'), 'rows_always_visible',
                                   'sort_config', 'msg_sort', 'on_sort_loaded')
  tblprp$paging_length = config$paging.length %>% verify(c('integer', 'numeric'), lengths = 1, domain = c(1,Inf), default = 10)
  return(tblprp)
}

TFD3.config.verify = function(config){
  config$sort                  %<>% verify('logical', domain = c(T,F), lengths = 1, default = T, varname = "config$sort")
  config$withRowNames          %<>% verify('logical', domain = c(T,F), lengths = 1, default = T, varname = "config$withRowNames")
  config$column.filter.enabled %<>% verify('logical', domain = c(T,F), lengths = 1, default = T, varname = "config$column.filter.enabled")
  config$selection.mode        %<>% verify('character', domain = c('single', 'multi'), lengths = 1, varname = 'config$selection.mode')
  config$selection.color       %<>% verify('character', domain = c('active', 'success', 'info', 'warning', 'danger'), default = 'info', lengths = 1, varname = 'config$selection.color')
  #config$footer.font.weight    %<>% verify('character', domain = c('bold'), lengths = 1, varname = 'config$footer.font.weight')
  #config$footer.font.adjust    %<>% verify('character', domain = c('left', 'right', 'center'), lengths = 1, varname = 'config$footer.font.adjust')
  #config$footer.font.format    %<>% verify('character', domain = 1:9 %++% '.f', lengths = 1, varname = 'config$footer.font.format')
  if (!is.null(config$column.color)){
    if (is.null(config$column.color.auto)){config$column.color.auto = list()}
    nms = names(config$column.color.auto)
    for(i in names(config$column.color)){if(!(i %in% nms)){config$column.color.auto[[i]] = T}}
  }
  # and many more ...
  return(config)
}

# Converts config$column.footer list to a data.frame 2b passed as argument 'footData' to function 'TFD3()'
TFD3.footData = function(obj, config){
  out = data.frame()
  rws = obj %>% TFD3.filteredRows(config)
  for (col in names(config$column.footer)){
    nms = c(chif(config$withRowNames,'rownames',NULL), colnames(obj))
    if   (config$column.footer[[col]] %>% inherits('function')){val = obj[rws, col] %>% list %>% sapply(config$column.footer[[col]])}
    else {val = config$column.footer[[col]] %>% as.character}
    if(col == 'rownames'){col = 'Rownames'}
    if(!is.empty(val)){out[1, col] = val}
  }
  return(chif(out %>% is.empty, NULL, out))
}

TFD3.rowStyles = function(obj, config){
  if(is.null(config$row.color) & is.null(config$selection.mode)){return(NULL)}

  if(!is.null(config$row.color)){
    out = config$row.color %>% verify('character', domain = c('', 'active', 'success', 'info', 'warning', 'danger'), varname = 'config$row.color') %>% vect.extend(nrow(obj))
  } else {out = rep('', nrow(obj))}

  if(!is.null(config$selection.mode)){
    out[config$selected] = 'info'
  }
  return(out)
}

# Generates column.editable from config to be given to argument 'edit' when TFD3() is called
TFD3.edit = function(colnames, config){
  if(is.empty(config$column.editable)){return(FALSE)}
  enacols = config$column.editable %>% verify('list', default = list()) %>%
    unlist %>% coerce('logical') %>% which %>% names %>% intersect(c(chif(config$withRowNames,'rownames',NULL), colnames))

  nms = c(chif(config$withRowNames,'rownames',NULL), colnames %>% verify('character', default = character(), varname = 'colnames')) %>% unique
  out = character()
  for(i in enacols){
    w = which(nms == i) - 1
    for (j in w){out %<>% c('col_' %++% w)}
  }
  return(out)
}

TFD3.lastEdits.empty <- data.frame(Row = c("", ""), Column = (c("", "")), Value = (c("", "")), stringsAsFactors = FALSE);
rownames(TFD3.lastEdits.empty) <- c("Fail", "Success");


TFD3.initialFilters = function(colnames, config){
  nms = c(chif(config$withRowNames,'rownames',NULL), colnames %>% verify('character', default = character(), varname = 'colnames')) %>% unique
  out = list()
  for(i in names(config$column.filter) %>% verify('character', domain = nms, default = character(), varname = 'names(config$column.filter)')){
    w = which (nms == i)
    for (j in w){out[['col_' %++% (w - 1)]] = config$column.filter[[i]]}
  }
  return(out)
}

TFD3.applyFilterstr = function(v, fltstr){
  # todo: currently it can only work with four very simple filterstrs, "<, <=, >=, >" does not support "=" and combined conditions with and , or, not, ...
  if(v %>% inherits('character')){return(fltstr %>% tolower %>% grep(v %>% tolower))}
  parse(text = paste('v', fltstr)) %>% eval %>% which
}

TFD3.filteredRows = function(obj, config){
  ff = obj %>% nrow %>% sequence
  for(i in names(config$column.filter)){
    if (i == 'rownames'){
      ff = ff %^% (rownames(obj) %>% TFD3.applyFilterstr(config$column.filter[[i]]))
    } else {
      ff = ff %^% (obj[, i] %>% TFD3.applyFilterstr(config$column.filter[[i]]))
    }
  }
  return(ff)
}

TFD3.colNames = function(config){
  if(is.null(config$column.title)){return(NULL)}
  cn = character()
  for (cc in names(config$column.title)){
    if(cc == 'rownames'){cn['Rownames'] <- config$column.title[[cc]]} else {cn[cc] <- config$column.title[[cc]]}
  }
  return(cn)
}

TFD3.bgColScales = function(obj, config){
  bgcs = list()
  nms  = c(chif(config$withRowNames,'rownames',NULL), colnames(obj))
  for (cc in names(config$column.color)){
    w = which(nms == cc) - 1
    # config$column.color.auto[cc] %<>% verify('list', domain = c(T,F), lengths = 1, default = F, varname = "config$column.color.auto['" %++% cc %++% "']")
    if(config$column.color[[cc]] %>% unique %>% length == 1){
      scr = TFD3.color.single.js(config$column.color[[cc]] %>% unique)
    } else if(config$column.color.auto[[cc]]){
      scr = paste('auto', config$column.color[[cc]] %>% paste(collapse = ':'), sep = ':')
    } else if(inherits(obj[, cc], valid.numeric.classes)){
      scr = TFD3.color.numeric.js(domain = obj[, cc], range = config$column.color[[cc]])
    } else if (inherits(obj[, cc], valid.nominal.classes)){
      scr = TFD3.color.nominal.js(domain = obj[, cc], range = config$column.color[[cc]])
    } else {scr = ''}
    if(!is.empty(scr)){for (i in w){bgcs[[paste0('col_', i)]] <- scr}}
  }
  return(bgcs)
}

## TFD3 Generate a HTML table widget with advanced filtering, sorting and
## colouring.
##
## R interface to Max Guglielmi's \href{http://tablefilter.free.fr/ }{HTML Table
## Filter Generator} JavaScript library. Provides advanced filtering and
## sorting. Columns can be formatted using D3 functions.
##
## @section Configuration: The TFD3 widget can be highly customized.
##   See the website of the JavaScript library
##   \href{http://tablefilter.free.fr/}{HTML Table Filter Generator} for
##   details. Configuration is passed as a list of key value pairs to the
##   JavaScript engine.
##
## @section Extensions: Some of the TableFilter functions are beeing provided as
##   extensions, in particular \itemize{ \item ColsVisibility: Visibility of
##   columns can be adjusted by configuration or interactively \item
##   ColumnsResizer: Interactive resizing of column width \item
##   FiltersRowVisibility: Interactively show or hide the filter row. } To
##   activate these extensions simply define them as a character vector in the
##   extensions parameter, e.g. \code{extensions = c("ColsVisibility",
##   "ColumnsResizer", "FiltersRowVisibility")}. This takes care of enabling and
##   basic configuration of the extensions. For further customization use the
##   tableProps parameter.
##
## @section Editing: The whole table (\code{edit = TRUE}) or selected columns
##   (\code{edit = c("col_1", "col_3")}) can set to be editable. An editable
##   table provides an input element named like the corresponding output element
##   + "_edit". Here each (debounced) edit event in a table cell is visible as a
##   list of row (\code{row}), column (\code{col}) and new value (\code{val}).
##   See examples/interaction for a Shiny app demonstrating this feature.
##
## @section Colouring: Table columns can be colored based on their cells value
##   using D3.js colour scales. Table background and foreground (text) can be
##   coloured independently. Colour definitions are passed to the JavaScript
##   engine as D3 scale functions. This allows for a variety of scales for
##   different purposes. See
##   \href{https://github.com/mbostock/d3/wiki/Scales}{D3 scale documentation}
##   and examples below for details. As a shortcut a linear scale over the full
##   value range of a column can be defined as \code{col_n =
##   "auto:startcolour:endcolour"} (n is the column number, starting with 0).
##   For better mapping from numeric values to perceived intensity a HCL colour
##   interpolation is used.
##
## @section Row selection: If \code{selectableRows} is set to \code{"single"} or
##   to \code{"multi"}, the widget provides a shiny input named outputid +
##   "_select". On (\code{ctrl-}) mouse click the input delivers an array of 1
##   based row coordinates. Selected rows are highligthed using the "info"
##   Bootstrap class. \code{setRowClass} can be used to set or to unset this
##   class from the server.
##
## @section Sparklines: Table columns containing a comma separated series of
##   numbers (\code{"1,3,5,7,11"}) can be turned into sparkline visualizations.
##   For example, \code{sparklines = list(col_0 = list(type = "line"))} will
##   turn the cells of the first column into a minature line chart.
##
## @param df Data frame, matrix or or \link[crosstalk]{SharedData} object to display as html table
## @param enableTf Enable the features for the "HTML table filter generator"
## @param tableProps A list object describing appearence and function of the
##   table
## @param showRowNames Add the R row names as first column to the table
## @param colNames Named character list to display as column names
## @param extensions List of table filter extensions to load.
## @param selectableRows Enable row selection on (\code{cltr-}) mouse click. If
##   \code{"multi"} multiple rows will be selectable using (\code{cltr click}),
##   if  \code{"single"}  only a single line will be selectable.
## @param selectableRowsClass CSS class of selected row. Could be "active",
##   "success", "info", "warning", or "danger" from Bootstrap3. Default: "info."
## @param tableStyle List css classes to apply to a table. Bootstrap3 provides
##   \code{table}, \code{table-striped}, \code{table-bordered},
##   \code{table-hover}, and \code{table-condensed}. The \code{table-hover}
##   class is applied automatically if \code{selectableRows} is active. If
##   \code{tableStyle} is not NULL, the normal CSS styling of TableFilter is
##   automatically cut down by appending \code{stylesheet =
##   "tablefilter-2.5/filtergridBS.css"} to the tableProps.
## @param rowStyles Character vector of Bootstrap classes to apply to rows.
##   Could be used to pre-select rows when using the \code{selectableRows}
##   interface.
## @param bgColScales List of background colour scales to apply to the columns
## @param fgColScales List of text colour scales to apply to the columns
## @param edit Set whole table or selected columns editable. See details.
## @param radioButtons Turn logical columns into radio buttons
##   (\code{radioButtons = "col_4"}).
## @param checkBoxes Turn logical columns into checkboxes (\code{checkBoxes =
##   "col_3"}).
## @param cellFunctions Run D3 functions to format a column. Can be used to
##   generate D3 graphics in cells.
## @param filterInput Generate an input element named outputid + "_filter"
##   listing filter settings and valid rows
## @param initialFilters List of initial filter settings filter settings and
##   valid rows
## @param footData Data frame or matrix to append as footer to the table. Column
##   names must match the colnames of the main table. Cells in the footer will
##   get an id attribute (e.g. first footer row, second column in "mtcars"
##   output is named "frow_0_fcol_1_tbl_mtcars") allowing them to be used with
##   the "col_operation" option of TableFilter.
## @param footCellFunctions Run D3 functions to format a footer column. Can be
##   used to format table footer or to generate D3 graphics in cells.
## @param  sparklines List of per column options to turn cell values into
##   sparkline visulizations.
## @examples
## # ------------------------------------------------------------------------------
## # colour definition: apply a white to blue linear scale to the background of the
## # first column ("col_0") over a range of values from 0 to 200
## # ------------------------------------------------------------------------------
## bgColScales <- list(
## col_0 = JS('function colorScale(i){
##         var color = d3.scale.linear()
##         .domain([0, 200])
##         .range(["white", "blue"]);
##         return color(i);
##      }'));
## # ----------------------------------------------------------------------------
## # simplified colour definition: first column, linear scale from white to green
## # ----------------------------------------------------------------------------
## bgColScales <- list(
##  col_0 = "auto:white:green"
## )
##
##
#' @import gtools
#' @import htmlwidgets
#' @import crosstalk
## @export JS
## @export
TFD3 <- function(df, enableTf = TRUE, tableProps = NULL, showRowNames = FALSE, colNames = NULL, extensions = NULL, selectableRows = NULL, selectableRowsClass = "info", tableStyle = "table", rowStyles = NULL, bgColScales = list(), fgColScales = list(), edit = FALSE, radioButtons = NULL, checkBoxes = NULL, cellFunctions = list(), filterInput = FALSE, initialFilters = list(), footData = NULL, footCellFunctions = list(), sparklines = list(), width = NULL, height = NULL) {

  if (is.SharedData(df)) {
    key <- df$key()
    group <- df$groupName()
    df <- df$origData()
  } else {
    key <- NULL
    group <- NULL
  }

  if(is.matrix(df)) {
    df <- as.data.frame(df);
  }

  if(showRowNames) {
    df <- cbind(rownames(df), df);
    colnames(df)[1] <- "Rownames";
  }

  if(is.null(tableProps)) {
    tableProps <- list();
  }

  #  if(is.null(tableProps$base_path)) {
  #    tableProps <- c(tableProps, base_path = 'tablefilter-2.5/');
  #  }

  #   if(!is.null(tableStyle)) {
  #     tableProps <- c(tableProps, stylesheet = "tablefilter-2.5/filtergridBS.css");
  #   }
  # if(!is.null(tableStyle)) {
  #   tableProps <- c(tableProps, stylesheet = "style/tablefilter.css");
  # }
  #
  if (!is.null(height)) {
    tableProps <- c(tableProps, grid_height = paste0(height, 'px' ), fixed_headers = TRUE);
  }

  if (!is.null(extensions)) {
    # compatibility: was a character vector: extensions <- c("ColsVisibility", "ColumnsResizer", "FiltersRowVisibility")
    # columns resizer currently not supported in TableFilter
    if (!is.list(extensions)) {
      if (is.vector(extensions)) {
        ext <- list();
        i = 1;
        for (e in extensions) {
          if (e == "ColsVisibility") {
            ext[i] <-  list(list( "name" = "colsVisibility"));
            i <- i + 1;
          } else if (e == "FiltersRowVisibility") {
            ext[i] <-  list(list( "name" = "filtersVisibility"));
            i <- i + 1;
          }
        }
        extensions <- ext;
      }
    }
    if (length(extensions) > 0) {
      tableProps$extensions <- extensions;
    }
  }

  # sort is now an extension
  if (!is.null(tableProps$sort)) {
    if (tableProps$sort) {
      sort <-  list(list( "name" = "sort"))
      if (length(tableProps$extensions) > 0) {
        tableProps$extensions <- c(tableProps$extensions, sort);
      } else  {
        tableProps$extensions <- sort;
      }
      tableProps$sort <- NULL;
      if (!is.null(tableProps$sort_config$sort_types)) {
        colTypes <- tolower(tableProps$sort_config$sort_types);
        tableProps$col_types <- gsub('us|eu', 'number', colTypes);
        tableProps$sort_config$sort_types <- NULL;
      }
    }
  }

  # turn "auto:white:red" in a linear d3 colour scale function
  autoColScale <- function(colScales) {
    if(length(colScales) == 0) {
      return(colScales);
    }
    cols <- names(colScales);
    for(i in 1:length(colScales)) {
      if(! "JS_EVAL" %in% class(colScales[[i]]) )  {
        clrs <- unlist(strsplit(colScales[[i]], ':', fixed = TRUE));
        startColour <- clrs[2];
        endColour <- clrs[3];
        scale <- JS(paste0('function colorScale(tbl, i){
                           var color = d3.scale.linear()
                           .domain(colExtent(tbl, "', cols[i] ,'"))
                           .range(["', startColour, '", "', endColour, '"])
                           .interpolate(d3.interpolateHcl);
                           return color(i);
      }'));
     colScales[cols[i]] <- list(scale);
    }
    }
    return(colScales);
    }

  bgColScales <- autoColScale(bgColScales);
  fgColScales <- autoColScale(fgColScales);

  # make edit a d3 select string
  if (is.character(edit)) {
    edit <- paste0('.',  edit, collapse = ', ');
  }

  # prepare mixed sort order. have already a rownames column if showRownames == TRUE
  sortKeys = NULL;
  if (!is.null(tableProps)) {
    if (!is.null(tableProps$col_types)) {
      mixedCols <- grep("mixed", tableProps$col_types, ignore.case = TRUE);
      if (length(mixedCols) > 0) {
        sortKeys <- lapply(mixedCols, function(x) {
          index <- 1:nrow(df);
          order <- gtools::mixedorder(as.character(df[ , x]));
          index[order] <- 1:length(order);
          return(index);
        });
        names(sortKeys) <- paste0('col_', mixedCols - 1);
        tableProps$col_types <- gsub('mixed', 'number', tableProps$col_types, ignore.case = TRUE);
      }
    }
  }

  x <- list(
    data = df,
    columns = colnames(df),
    enableTf = enableTf,
    tableProps = tableProps,
    selectableRows = selectableRows,
    selectableRowsClass = selectableRowsClass,
    tableStyle = tableStyle,
    rowStyles = rowStyles,
    bgColScales = bgColScales,
    fgColScales = fgColScales,
    cellFunctions = cellFunctions,
    footCellFunctions = footCellFunctions,
    sparklines = sparklines,
    edit = edit,
    radioButtons = radioButtons,
    checkBoxes = checkBoxes,
    showRowNames = showRowNames,
    colNames = colNames,
    filterInput = filterInput,
    initialFilters = initialFilters,
    footData = footData,
    sortKeys = sortKeys,
    key = key,
    group = group
  )

  # create the widget
  htmlwidgets::createWidget("TFD3",
                            x,
                            width = width,
                            height = height, sizingPolicy = htmlwidgets::sizingPolicy(
                              viewer.padding = 0,
                              viewer.paneHeight = 800,
                              browser.fill = TRUE
                            ),
                            dependencies = crosstalk::crosstalkLibs())
  }

## Shiny bindings for tableFilter
##
## Output and render functions for using tableFilter within Shiny
## applications and interactive Rmd documents.
##
## @param outputId output variable to read from
## @param width,height Must be a valid CSS unit (like \code{"100\%"},
##   \code{"400px"}, \code{"auto"}) or a number, which will be coerced to a
##   string and have \code{"px"} appended.
## @param expr An expression that generates tableFilter object
## @param env The environment in which to evaluate \code{expr}.
## @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
##   is useful if you want to save an expression in a variable.
##
## @name tableFilter-shiny
## @export
TFD3Output <- function(outputId, width = "100%", height = "400px") {
  shinyWidgetOutput(outputId, "TFD3", width, height, package = "viser")
}
## @rdname tableFilter-shiny
## @export
renderTFD3 <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, TFD3Output, env, quoted = TRUE)
}

## Give feedback in case of validaton failure.
##
## For each input event in a tableFilter widget a message is sent via a shiny
## input. After input validation \code{rejectEdit} can be used to give visual feedback
## to the user in case of a validation failure.
##
## In edit mode the a tableFilter widget creates a shiny input element. The name
## of this input is the name of the corresponding output element followed by
## "_edit". For each edit event in the html table this input receives a list
## giving a unique ID of the edit event ("id"), the row ("row"), the column
## ("col") and the new value ("val") of the cell. Row and column numbers are in
## R coordinates. If \code{showRowNames} is \code{TRUE}, the column number of
## the rownames is 0.
## @param session Shiny session object.
## @param tbl Name of the table beeing edited.
## @param row Row number as received via edit input.
## @param col Column number as received via edit input.
## @param id Unique identifier of the edit event, received via the edit input
## @param color Text colour of a failed edit.
## @param value Reset the input to this value if not null.
## @export
rejectEdit <- function(session, tbl, row, col, id, value = NULL, color = "red") {
  message <- list(tbl = tbl, row = row, col = col, action = "reject", id = id, value = value, color = color, feedback = FALSE);
  session$sendCustomMessage(type = "setCellValue", message);
}

## Give feedback in case of validaton success
##
## For each input event in a tableFilter widget a message is sent via a shiny
## input. After input validation \code{confirmEdit} can be used to give visual feedback
## to the user.
##
## In edit mode the a tableFilter widget creates a shiny input element. The name
## of this input is the name of the corresponding output element followed by
## "_edit". For each edit event in the html table this input receives a list
## giving a unique ID of the edit event ("id"), the row ("row"), the column
## ("col") and the new value ("val") of the cell. Row and column numbers are in
## R coordinates. If \code{showRowNames} is \code{TRUE}, the column number of
## the rownames is 0.
## @param session Shiny session object.
## @param tbl Name of the table beeing edited.
## @param row Row number as received via edit input.
## @param col Column number as received via edit input.
## @param id Unique identifier of the edit event, received via the edit input
## @param color Transient text colour to indicate success
## @param value Value to set text to after confirmation. Can be used to format input.
## @export
confirmEdit <- function(session, tbl, row, col, id, value = NULL, color = "green") {
  message <- list(tbl = tbl, row = row, col = col, action = "confirm", id = id, value = value, color = color, feedback = FALSE);
  session$sendCustomMessage(type = "setCellValue", message);
}

## Set cell value
## @param Session Shiny session object.
## @param tbl Name of the table.
## @param row Row number (one-based).
## @param col Column number (one-based). If \code{showRowNames == TRUE}, the rownames column is number zero.
## @param value Cell value to set.
## @param feedback Send edit event back to server.
##
## @examples
## setCellValue(session, "mtcars", row = 8, col = 3, val = 8)
## @export
setCellValue <- function(session, tbl, row, col, value, feedback = FALSE) {
  message <- list(tbl = tbl, row = row, col = col, action = "edit", value = value, feedback = feedback, foot = FALSE);
  session$sendCustomMessage(type = "setCellValue", message);
}

## Set foot cell value
## @param Session Shiny session object.
## @param tbl Name of the table.
## @param row Footer row number (one-based).
## @param col Footer olumn number (one-based). If \code{showRowNames == TRUE}, the rownames column is number zero.
## @param value Cell value to set.
##
## @examples
## setFootCellValue(session, "mtcars", row = 1, col = 1, val = 8)
## @export
setFootCellValue <- function(session, tbl, row, col, value, feedback = FALSE) {
  message <- list(tbl = tbl, row = row, col = col, action = "edit", value = value, feedback = FALSE, foot = TRUE);
  session$sendCustomMessage(type = "setCellValue", message);
}

## Enable editing of a tableFilter widget
## @param Session Shiny session object.
## @param tbl Name of the table to be edited.
## @param cols editing of single column (\code{"col_0"}) or multiple columns (\code{c("col_0", "col_1")}).
##
## @examples
## enableEdit(session, "mtcars", c("col_1", "col_2"))
## @export
enableEdit <- function(session, tbl, cols = NULL) {
  if (is.character(cols)) {
    cols <- paste0('.',  cols, collapse = ', ');
  }
  session$sendCustomMessage(type = "enableEdit", list(tbl = tbl, cols = cols));
}

## Disable editing of a tableFilter widget
## @param Session Shiny session object.
## @param tbl Name of the table to disable editing.
## @param cols Disable editing of single column (\code{"col_0"}) or multiple columns (\code{c("col_0", "col_1")}).
##
## @examples
## disableEdit(session, "mtcars", c("col_1", "col_2"))
## @export
disableEdit <- function(session, tbl, cols = NULL) {
  if (is.character(cols)) {
    cols <- paste0('.',  cols, collapse = ', ');
  }
  session$sendCustomMessage(type = "disableEdit", list(tbl = tbl, cols = cols));
}

## Set filter on a column
## @param Session Shiny session object.
## @param tbl Name of the table to filter.
## @param col Set filter on column (\code{"col_0"}).
## @param doFilter Activate the filter after setting it.
##
## @examples
## setFilter(session, "mtcars", col = "col_1", filter = ">20")
## @export
setFilter <- function(session, tbl, col, filterString, doFilter = TRUE) {
  col <- sub('col_', '', col);
  message <- list(tbl = tbl, col = col, filterString = filterString, doFilter = doFilter);
  session$sendCustomMessage(type = "setFilter", message);
}

## Clear all filters from a table
## @param Session Shiny session object.
## @param tbl Name of the table to clear.
## @param doFilter Unfilter the table after clearing the filter strings.
##
## @examples
## clearFilters(session, "mtcars")
## @export
clearFilters <- function(session, tbl, doFilter = TRUE) {
  message <- list(tbl = tbl, doFilter = doFilter);
  session$sendCustomMessage(type = "clearFilters", message);
}

## Highlight a row using bootstrap classes
## @param Session Shiny session object.
## @param tbl Name of the table.
## @param row Number of the row to color.
## @param class Bootstrap contextual class (\code{"active"}, \code{"info"}, \code{"success"}, \code{"warning"}, or \code{"danger"}). \code{"none"} removes the highlighting. \code{"info"} is reserved for selected rows.
##
## @examples
## setRowClass(session, "mtcars", 3, "success")
## @export
setRowClass <- function(session, tbl, row, class) {
  message <- list(tbl = tbl, row = row, class = class);
  session$sendCustomMessage(type = "rowClass", message);
}


## Reset a TFD3 input element
## @param Session Shiny session object.
## @param tbl Name of the input (output name  + "_edit").
##
## @examples
## resetInput(session, "mtcars_edit")
## @export
resetInput <- function(session, input) {
  session$sendCustomMessage(type = "resetTFD3Value",
                            message = input)
}

TFD3.table = function(obj, label = NULL, color = NULL, shape = NULL, config = NULL, ...){
  if((nrow(obj) == 0) & (ncol(obj) == 0)){return(NULL)}
  if (is.null(label)){label = as.list(names(obj))}
  # Verifications:
  # assert(require(TFD3), "Package TFD3 is not installed!", err_src = match.call()[[1]])
  config = TFD3.table.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
    TFD3.config.verify

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

  # # todo: later support for prettyDate and prettyTime!
  # for(cl in colnames(obj)){
  #   if(config$column.type[[cl]] == 'PrettyTimeDate'){
  #     obj[, cl] %<>% time2Char
  #   }
  # }

  # Specify background color from argument 'color':
  bgColScales = list()
  for(i in seq(L$color)){
    if(!is.empty(color[[i]])){
      if(L$color[i] %in% L$label){L$color[i] %<>% paste('1', sep = '.')}
      lin = paste0('col_', i)# list item name
      if (obj[, L$color[i]] %>% unique %>% length == 1){
        bgColScales[[lin]] = TFD3.color.single.js(obj[1, L$color[i]])
      } else if (obj[, L$color[i]] %>% length == nrow(obj)){
        if(inherits(obj[,L$label[i]], valid.numeric.classes)){
          bgColScales[[lin]] = TFD3.color.numeric.js(domain = obj[, L$label[i]], range = obj[, L$color[i]])
        } else {
          bgColScales[[lin]] = TFD3.color.nominal.js(domain = obj[, L$label[i]], range = obj[, L$color[i]])
        }
      }
    }
  }

  if(is.null(L$color)){bgColScales = TFD3.bgColScales(obj, config)}

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
      if      (shp == 'bar'){cellFunctions[[lin]] = TFD3.shape.bar.js()}
      else if (shp %in% c('bubble', 'circle', 'point', 'dot')){cellFunctions[[lin]] = TFD3.shape.bubble.js()}
    }
  }

  # footCellFunctions <- list(
  #   col_0 = TFD3.font.js(side = 'left', format = NULL, weight = 'bold'),
  #   col_1 = TFD3.font.js(side = 'left', format = '.1f', weight = 'bold'),
  #   col_2 = TFD3.font.js(side = 'center', format = '.1f', weight = 'bold'),
  #   col_3 = TFD3.font.js(side = 'right', format = '.1f', weight = 'bold')
  # )

  footCellFunctions = list()
  nms = c('rownames', colnames(obj))
  for (col in names(config$column.footer.font)){
    wch = which(nms == col) - 1
    for (cn in wch){
      lin = paste0('col_', cn)# list item name
      footCellFunctions[[lin]] = TFD3.font.js(
        side   = config$column.footer.font[[col]]$adjust,
        format = config$column.footer.font[[col]]$format,
        weight = config$column.footer.font[[col]]$weight)
    }
  }

  wcb = which(L$shape == 'checkBox')
  wrb = which(L$shape == 'radioButtons')

  obj[, L$label] %>% TFD3(
    colNames     = TFD3.colNames(config) %>% unname,
    bgColScales  = bgColScales,
    cellFunctions = cellFunctions,
    footCellFunctions = footCellFunctions,
    showRowNames = config$withRowNames,
    enableTf     = config$column.filter.enabled,
    filterInput  = config$column.filter.enabled,
    edit         = L$label %>% TFD3.edit(config),
    checkBoxes   = chif(is.empty(wcb), NULL, 'col_' %++% wcb),
    radioButtons = chif(is.empty(wcb), NULL, 'col_' %++% wrb),
    initialFilters = TFD3.initialFilters(L$label, config),
    footData = TFD3.footData(obj[, L$label], config),
    tableStyle = config$table.style,
    selectableRows = config$selection.mode,
    selectableRowsClass = config$selection.color,
    rowStyles = TFD3.rowStyles(obj[, L$label], config),
    tableProps = config %>% TFD3.tableprops,
    extensions = config$extensions,
    height = config$height,
    width  = config$width,
    ...)
}


TFD3.table.molten = function(obj, label = NULL, color = NULL, shape = NULL, group = NULL, config = NULL){
  if (is.null(label)){label = as.list(names(obj))}
  # Verifications:
  # assert(require(TFD3), "Package TFD3 is not installed!", err_src = match.call()[[1]])
  config = TFD3.table.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
    TFD3.config.verify

  # Preparing Aesthetics:
  # Preparing Aesthetics:
  a = prepareAesthetics(label = label, color = color, shape = shape, extend = c('label','color', 'shape'))
  L = a$labels
  A = a$aesthetics %>% list.remove('shape')

  obj %<>% prepare4Plot(A, config)
  if (is.empty(obj)){return(NULL)}

  names(obj) <- names(obj) %>% make.unique('.1')

  # Modify later for sparline tables
}
