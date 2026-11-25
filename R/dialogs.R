
# dialogs.R ----------------------------------------------------------------


# Header
# Filename:      dialogs.R
# Description:   Contains functions for building dialog boxes as rvis containers which can be embedded in a UI or dashboard
# Author:        Nicolas Berta
# Email :        nicolas.berta@gmail.com
# Start Date:    01 June 2018
# Last Revision: 01 June 2018
# Version:       0.1.1

# Version History:

# Version   Date                Action
# ---------------------------------------
# 0.1.0     01 June 2018        Initial issue with function build.container.get.table()
# 0.1.1     01 June 2018        Argument containerType added, specifying container type


#' Dashboard items of a dialog reading a CSV table
#'
#' Builds the items of a dashboard dialog which reads a CSV file chosen by the user and maps its
#' columns to the columns a dashboard needs, with the classes it needs: a file input, one select
#' input per needed column (filled with the column names of the file), and \emph{Load} and
#' \emph{Cancel} buttons, with the services reading, checking and converting the table.
#'
#' Messages are kept in \code{session$userData$temp}: \code{<name>.failmsg} about reading the file
#' (\code{"Ok"}, \code{"Please choose a CSV file."}, \code{"Selected table has no column names"} or
#' \code{"File read failed for some reason!"}) and \code{<name>.failmsg2} about the column classes
#' (\code{"Ok"} or \code{"Column x cannot be coerced to y!"}).
#'
#' @param name Name of the dialog container item; the other items are named after it.
#' @param classes Named list: the names of the columns the dashboard needs and, for each, the accepted
#'   classes (columns of other classes are converted to the first one).
#' @param outputvar Name of the variable receiving the table (kept for backward compatibility).
#' @param fileSelectTitle,loadButtonTitle Labels of the file input and of the load button.
#' @param containerType Type of the dialog container item, like \code{"fluidPage"} or \code{"wellPanel"}.
#' @param ... Other fields of the container item.
#' @return A named list of dashboard items, to add to the \code{items} of a dashboard configuration.
#' @seealso \code{\link{rvisDashboard}}, and the \href{../articles/dashboards.html}{dashboards guide}.
#' @examples
#' \dontrun{
#' classes <- list(caseID = c("character", "factor"), timestamp = "POSIXct")
#' items <- build.container.get.table("my_dialog", classes = classes, containerType = "wellPanel")
#' runDashboard(list(layout = "my_dialog", items = items))
#' }
#' @export
build.container.get.table = function(name, classes, outputvar = name %>% paste('out', sep = '.') , fileSelectTitle = 'Select input CSV file', loadButtonTitle = 'Load selected file', containerType = 'fluidPage', ...){
  colns = names(classes)

  items = list()
  items[[name]] = list(type = containerType %>% verify('character', lengths = 1, domain = valid.container.types, default = 'fluidPage'), ...)
  items[[name %>% paste0('.get.file')]] = list(type = 'fileInput', title = fileSelectTitle, accept = c('.csv'))
  if(containerType == 'fluidPage'){lay = list(list(name %>% paste0('.get.file'), offset = 1))} else {lay = name %>% paste0('.get.file')}
  checkstr = character()
  for(col in colns){
    itmID = paste(name, 'get', col, sep = '.')
    items[[itmID]] = list(type = 'selectInput', title = paste(col, 'Column', 'Label'))
    if(containerType == 'fluidPage'){lay %<>% list.add(list(itmID, offset = 1))} else {lay %<>% c(itmID)}
    checkstr %<>% c(paste0("if(!fail){
                           if(!inherits(session$userData$temp$TBL$", col, " , c(", paste0("'", classes[[col]],"'") %>% paste(collapse = ','), "))){
                           fail = try({session$userData$temp$TBL$", col, " %<>% coerce('", classes[[col]][1], "')}, silent = T) %>% inherits('try-error')
                           if(fail){session$userData$temp$", name, ".failmsg2 = 'Column ", col, "  cannot be coerced to ", classes[[col]][1], "!'}}}"))
}
  checkstr %<>% paste(collapse = '\n')
  updatescr = paste0("updateSelectInput(session, '", name, ".get.", colns, "', choices = ctgrs)") %>% paste(collapse = '\n')
  updatescr = paste("\n", updatescr, "\n")

  if(containerType == 'fluidPage'){lay %<>% list.add('loadfile')} else {lay %<>% c('loadfile')}
  items$loadfile       = list(type = 'actionButton', title = loadButtonTitle)
  items$cancel         = list(type = 'actionButton', title = 'Cancel')
  items[[name]]$layout = lay
  scr = paste0(
    "
    session$userData$temp = list(", name, ".failmsg = 'Ok')
    # Verifications
    # if(length(input$", name, ".get.file$datapath) != 1){session$userData$temp$", name, ".failmsg = 'One and only one file must be selected!'}
    ssp = strsplit(input$", name, ".get.file$name, '.', fixed = T)[[1]]
    nnn = length(ssp)
    if(ssp[nnn] != 'csv'){session$userData$temp$", name, ".failmsg = 'Please choose a CSV file.'}
    if(session$userData$temp$", name, ".failmsg == 'Ok'){
    session$userData$temp$ORGTBL = try(read.csv(input$", name, ".get.file$datapath, as.is = T), silent = T)
    if(inherits(session$userData$temp$ORGTBL, 'data.frame')){
    ctgrs = names(session$userData$temp$ORGTBL)
    if(!is.null(ctgrs)){", updatescr,
    "} else {session$userData$temp$", name, ".failmsg = 'Selected table has no column names'}
    } else {session$userData$temp$", name, ".failmsg = 'File read failed for some reason!'}
    }
    ")
  items[[name %>% paste0('.get.file')]]$service = scr

  inputcolnstr = paste0("input$", name, ".get.", colns) %>% paste(collapse = ' , ')
  colnstr      = paste0("'", colns, "'") %>% paste(collapse = ' , ')


  items$loadfile$service = paste0(
    "
    cat('Reading file: ', session$userData$temp$", name, ".failmsg, '\n')
    if(!is.null(session$userData$temp$", name, ".failmsg)){
    if(session$userData$temp$", name, ".failmsg == 'Ok'){
    session$userData$temp$", name, ".failmsg2 = 'Ok'
    session$userData$temp$TBL = session$userData$temp$ORGTBL[, c(", inputcolnstr, ")]
    names(session$userData$temp$TBL) <- c(", colnstr, ")
    fail = F", "\n", checkstr, "
    cat('Coercing Classes: ', session$userData$temp$", name, ".failmsg2, '\n')
    if(!fail){
    OUT = session$userData$temp$TBL
    session$userData$temp$TBL <- NULL
    print(head(OUT))}}}")

  return(items)
    }

# # Example:
# classes = list(caseID = c('character', 'factor'), activity = c('character', 'factor'), status = c('character', 'factor'), timestamp = 'POSIXct')
#
# dash   <- new('DASHBOARD', items = build.container.get.table(name = 'my_dialog', classes = classes, containerType = 'wellPanel'), king.layout = list('my_dialog'))
# ui     <- dash$dashboard.ui()
# server <- dash$dashboard.server()
# shinyApp(ui, server)
