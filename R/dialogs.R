
# dialogs.R ----------------------------------------------------------------


# Header
# Filename:      dialogs.R
# Description:   Contains functions for building dialog boxes as viser containers which can be embedded in a UI or dashboard
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


# This functions builds a viser container for a dialog box in order to get a CSV table and issue out a table with desired column names and classes.
# The returned container includes all service functions for reading and modifying the input table as well as error handling.
# todo: add Excel tables should be added
# the function returns a list of items that needs to be appended to any UI(dashboard) on which the dialog box is going to appear.
# name: name or ID of the container element
# classes: a named list containing acceptable classes of each column. list names should refer to the desired name of each column.
# outputvar: specifies the name of the final output variable. This can refer to a reactive or non-reactive global
# variable which can be defined anywhere in global.R and is accessible in all service functions of the UI.
# There are two error (fail) messages. The error messages are '<name>.failmsg' and '<name>.failmsg2' will appear in list 'temp' which is an element of the 'userData' property of the session:
# For example, if the name of the dialogbox is 'my_dialog', error messages will be found at:
# session$userData$temp$failmsg & session$userData$temp$failmsg2
# failmsg is NULL if no file has been specified yet or the dialogbox is reset.
# failmsg is 'Ok' if table CSV file is read with no problem (No checking or verification on the contents yet)
# failmsg is 'One and only one file must be selected!' if more than one file is selected by the user
# failmsg is 'Please choose a CSV file.' if a file with extention other than .csv is selected.
# failmsg is 'Selected table has no column names' if the selected table has no column labels.
# failmsg is 'File read failed for some reason!' if the selected file is corrupt and cannot be read.
# failmsg2 is 'Ok' if all the columns inherit desired classes or have been successfully coerced to the first desired class.
# failmsg2 is 'Column x cannot be coerced to class y!' if any of the columns does not inherit desired classes and cannot be coerced to the first desired class for that column.
# arguments ... will be passes to the container list
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


