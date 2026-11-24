# dashboard.R ----------------------------------------------------------------

# Header
# Filename:      dashboard.R
# Description:   Reference class DASHBOARD, which builds a shiny app (user interface and server) from a list of
#                items: inputs, outputs and containers, with their layouts and services. The list can be read
#                from a YAML or JSON configuration with function rvisDashboard() (dashconfig.R).
# Author:        Nima Ramezani
# Email :        nima.ramezani@gmail.com
# Start Date:    22 January 2016
# Last Revision: 23 November 2026
# Version:       2.7.0

# Version History:

# Version   Date                Action
# ---------------------------------------
# 0.6.0     04 July 2016        A number of changes made. Refer to dashboard.R (version 0.6)

# Changes from version 0.5:
# 1- method io.str() transferred from dash.tools.R
# 2- Starting letters of input and cloth types changed to lowercase to match shiny namespace: Example: radioButtons changed to radioButtons
# 3- Valid Container types separated from valid input types
# 4- class properties 'inputs' and 'outputs' consolidated to one single list called 'items' which must be a named list
# 5- layouts must be specified by item names not numbers
# 6- field ID removed and replaced by item name

# 0.6.2     08 September 2016   Argument inline added to checkboxGroupInput.
# 0.6.3     12 October 2016     Wordcloud2 plots added.
# 0.7.0     15 May 2017         tabPanel and many other containers can also get a list as layout
# 0.7.1     15 May 2017         textInput added
# 0.7.2     15 May 2017         TFD3Output added
# 0.7.3     15 May 2017         container tabsetPanel added
# 0.7.4     15 May 2017         tabPanel container can accept list as layout
# 0.7.4     15 May 2017         tabPanel added as cloth
# 0.7.5     15 May 2017         highcharterOutput added
# 0.7.6     15 May 2017         d3plusOutput added
# 0.7.7     15 May 2017         observers added as list (todo: named list items should become observeEvents)
# 0.7.8     15 May 2017         property 'prescript' added to the class
# 0.8.0     17 May 2017         Start adding syncing feature: property 'object' renamed to sync for synced items.
# 0.9.0     23 May 2017         syncing added for TFD3: reactive list properties 'sync' and 'report' added.
# 0.9.1     31 May 2017         morrisjsOutput added.
# 0.9.2     31 May 2017         coffeewheelOutput added.
# 0.9.3     01 June 2017        cloth column service modified: bug removed for column width and offset!
# 0.9.4     20 July 2017        support('coffeewheel') removed as the widget is embedded
# 0.9.5     25 July 2017        Added re-plotting trigger for TFD3 in sync reactives
# 0.9.6     31 July 2017        Added re-plotting trigger for TFD3 in sync reactives
# 0.9.7     03 August 2017      Default widths and heights of plot outputs made equal for all plots
# 0.9.8     30 August 2017      actionButton, accept both character and icon types for argument icon
# 1.1.0     11 October 2017     property sessions added to the object. This property is a list and keeps all the sessions
# 1.1.2     11 October 2017     Methods updateItem4Session() and updateItem() addded: Currently works only for type selectInput
# 1.1.3     13 October 2017     pivotTable added to supported plotters using package rpivotTable
# 1.1.4     16 October 2017     Method io.str.clothed() modified: cloth is no longer passed as argument. reads directly from items[[i]]$cloth
# 1.1.5     17 October 2017     rpivotTable renamed to pivot
# 2.0.0     10 November 2017    Fundamental Change to the package: Added shinyjs features: Methods enableItems(), disableItems(), showItems() and hideItems() added!
# 2.0.0     22 November 2017    CSS customized skin style for shinydashboard header added.
# 2.1.1     24 November 2017    actionLink and passwordInput item types added
# 2.1.2     27 November 2017    Simple container 'bag' added to containers.
# 2.1.3     27 November 2017    Type 'loginInput' added.
# 2.1.4     27 November 2017    Function loginVerify() added
# 2.1.5     27 November 2017    Property 'messages' added to class DASHBOARD. It is a named character vector
# 2.2.0     27 November 2017    Fundamental Change: Creating client specific dataset(objects and recative values) for each session
# 2.2.1     27 November 2017    Property 'objects' renamed to 'global', but argument 'objects' still works for method initialize(): replacing value of 'global' by given argument 'objects'
# 2.2.2     27 November 2017    Property 'values' added to class DASHBOARD. Generates and keeps initial values of reactives in 'sync'.
# 2.2.3     27 November 2017    A copy of list 'objects' is generated as initial values for non-reactive list 'local' associated with each session
# 2.2.4     27 November 2017    A copy of list 'values' is generated as initial values for reactive list 'sync' associated with each session
# 2.2.5     29 November 2017    Output item types: downloadButton and downloadLink added.
# 2.2.6     29 November 2017    Argument 'style' added to item types downloadButton and downloadLink
# 2.2.7     06 December 2017    Property 'settings' added to class DASHBOARD. settings item 'saveSession' added and specifies whether the session data needs to be saved after logout and loaded after login by each user.
#                               If settings$saveSession is TRUE, a .rds file for each user is created in the working directory after the user logs out to keep the session data (local and sync). Session data is loaded again when the user logs in.
# 2.2.8     08 December 2017    Property 'global' renamed to 'objects' again
# 2.2.9     08 December 2017    A copy of objects is created in session$userData in each session
# 2.3.0     15 December 2017    Uses encrypted password check from package: bcrypt
# 2.3.1     15 December 2017    Added property 'passEncryption' to the dashboard settings. Specifies how the password should be encrypted. Value 'none' means no encryption is required!
# 2.3.2     19 December 2017    Function loginVerify() modified: Small bug rectified by converting the encrypted password from factor to character
# 2.3.3     16 January 2018     A typo is corrected: Login versification failed converted to login verification failed.
# 2.3.4     29 January 2018     settings property 'keepSessions' added: sessions are kept in memory only if this property is TRUE
# 2.3.5     12 February 2018    settings property 'savePath' added: specify where in the server sessions should be saved
# 2.3.6     02 June 2018        Container 'bsModal' added
# 2.3.7     02 June 2018        arguments 'options' added for dataTableOutput, default for height changed to 'auto' from '400px' for all outputs
# 2.3.9     04 June 2018        Containers 'bsCollapse' and 'bsCollapsePanel' added.
# 2.4.0     04 June 2018        Output widths and heights returned to package defaults.
# 2.4.2     04 June 2018        'tooltip' and 'popover' are now properties which can be attached to each item. Thanks to package 'shinyBS'
# 2.4.3     06 June 2018        'grviz' added to outputs
# 2.4.4     08 June 2018        Arguments 'title', 'choices', 'selected' for all input items, are defined in one place
# 2.4.7     08 June 2018        Input items 'actionbttn', 'actionGroupButtons', 'airDatepickerInput' added
# 2.4.8     08 June 2018        shinyWidget spinners added to plotOutput. Can we add it to other plots as well??
# 2.4.9     15 June 2018        tutorBox added to containers
# 2.5.0     16 June 2018        Arguments 'tutor.lesson', 'tutor.hint' and 'tutor.step' can be used for to all items
# 2.5.1     22 June 2018        Method getItemObject() modified: arguments 'selected' and 'choices' are converted to integer if they are numeric.
# 2.5.2     18 September 2018   Method self.verify() modified: varname passed to function verify() changed
# 2.5.3     26 October 2018     Input item type dateRangeInput modified: All arguments added.
# 2.5.4     29 October 2018     Input item type actionButton modified: argument weight re-added
# 2.5.5     05 November 2018    grviz is now independent
# 2.5.6     06 November 2018    Argument 'width' added for selectInput
# 2.5.7     21 June 2019        Argument 'style' added for box cloth
# 2.5.8     05 January 2023     cloth icon can be of class icon
# 2.5.9     05 January 2023     Argument 'width' added to slider Input
# 2.6.0     16 January 2023     upsetjsOuptput added
# 2.6.1     23 November 2026    Bug fixes: box cloth 'footer' was passed as a second 'title' argument; navbarPage 'fluid = FALSE'
#                               was misspelled; icons of navbarMenu and tabPanel were badly quoted (navbarPage has no icon
#                               argument, so its icon is ignored instead of breaking the page); pivotOutput had no
#                               render function; header title CSS was always generated for skin 'blue' instead of the page skin;
#                               actionBttn default status 'unite' is not a valid status (now 'default'). loginVerify() rewritten
#                               with explicit return values.
# 2.7.0     23 November 2026    Code review: the user interface is built by calling the shiny functions directly instead of
#                               writing R code into strings that were parsed and evaluated (faster, and titles or labels with
#                               quotes no longer break the page). Server code of the items is parsed once when the server
#                               function is created instead of once in every session. Same user interface and services.
#                               Bug fixes: cloth 'tabPanel' always failed; only the first item of king.layout was shown;
#                               tooltips of items in king.layout or in the header/body of a dashboardPage failed or were
#                               not shown; tutor lessons and methods hideItems(), showItems(), enableItems() and
#                               disableItems() did not work because the javascript of rintrojs and shinyjs was not
#                               added to the page (now added when needed); shinyBS and shinyWidgets functions were used
#                               without their namespace; observers of synced tables were added again for each new
#                               session; a sidebarLayout without side or main items failed; airDatepickerInput arguments of
#                               class Date or integer were dropped.
#                               New: choices given as a named list (YAML/JSON); static items can be given as html;
#                               containers conditionalPanel and navlistPanel; shinydashboard menus and pages
#                               (sidebarMenu, menuItem, menuSubItem, tabItems, tabItem); updateItem() supports all
#                               shiny inputs, navbar pages, navigation lists and sidebar menus; methods app() and run();
#                               more settings: dateInput format/startview/weekstart/language/..., dateRangeInput
#                               weekstart/language/separator, sliderInput round/ticks/dragRange/timezone, actionButton
#                               class, plotOutput dblclick/hover, text/html/ui outputs tag/inline, fluidPage and
#                               sidebarLayout theme, fluidPage window title without a page title.

support('shiny', 'shinydashboard', 'htmlwidgets')

valid.box.statuses = c('primary', # Blue (sometimes dark blue),
                       'success', # Green
                       'info',    # Blue
                       'warning', # Orange
                       'danger'  # Red
)
valid.colors = c('red', 'yellow', 'aqua', 'blue', 'light-blue', 'green', 'navy', 'teal', 'olive', 'lime', 'orange',
                 'fuchsia', 'purple', 'maroon', 'black', 'navy')

valid.input.types = c("textInput", "radioButtons", "sliderInput", "actionButton", "actionBttn", "actionLink", "checkboxInput", "checkboxGroupInput", "selectInput", "dateInput", "airDatepickerInput",
                      "dateRangeInput", "fileInput", "numericInput", "passwordInput")

valid.output.types = c("uiOutput", "dynamicInput", "loginInput", "plotOutput", "verbatimTextOutput", "textOutput", "imageOutput", "tableOutput", "dataTableOutput", "htmlOutput",
                       "gglVisChartOutput", "rChartsdPlotOutput", 'dygraphOutput', 'plotlyOutput', 'amChartsOutput',
                       "leafletOutput", "infoBoxOutput", "valueBoxOutput", "pivotOutput", "wordcloud2Output", 'bubblesOutput', 'd3plusOutput',
                       "highcharterOutput", "TFD3Output", "d3tfOutput", "grvizOutput", "c3Output", "sunburstOutput", "sankeyNetworkOutput", "morrisjsOutput", "upsetjsOutput",
                       "coffeewheelOutput", "billboarderOutput", 'sankeytreeOutput', "rHandsonTableOutput", "downloadButton", "downloadLink", "static")

valid.container.types = c("column", "box", "bag", "fluidPage", "dashboardPage", "tabsetPanel",
                          "sidebarLayout", "navbarPage", "navbarMenu", "tabPanel", "wellPanel",
                          "bsModal", 'bsCollapse', 'bsCollapsePanel', 'actionGroupButtons', 'tutorBox',
                          "conditionalPanel", "navlistPanel", "sidebarMenu", "menuItem", "menuSubItem", "tabItems", "tabItem")

valid.cloth.types = c("box", "infoBox", "valueBox", "column", "wellPanel", "tabPanel")

valid.navbar.positions = c("static-top", "fixed-top", "fixed-bottom")

valid.dashboard.skins  = c("blue", "black", "purple", "green", "red", "yellow")

valid.spinners = c('circle', 'bounce', 'folding-cube', 'rotating-plane', 'cube-grid', 'fading-circle', 'double-bounce', 'dots', 'cube')

valid.bttn.styles   = c('simple', 'bordered', 'minicolasl', 'stretch', 'jelly', 'gradient', 'fill', 'material-circle', 'material-flat', 'pill', 'float', 'unite')
valid.bttn.statuses = c('default', 'primary', 'warning', 'danger', 'success', 'royal')

# Default width and height of the output items that have a size (other output functions take no size)
output.sizes = c(
  list(dataTableOutput     = c('100%', 'auto'),
       grvizOutput         = c('100%', '400px'),
       c3Output            = c('100%', '100%'),
       sankeyNetworkOutput = c('100%', '500px'),
       upsetjsOutput       = c('100%', '500px'),
       sunburstOutput      = c('100%', '400px'),
       rHandsonTableOutput = c('100%', '100%'),
       pivotOutput         = c('100%', '500px'),
       bubblesOutput       = c('600px', '600px'),
       d3plusOutput        = c('100%', '500px')),
  sapply(c('plotOutput', 'TFD3Output', 'd3tfOutput', 'amChartsOutput', 'dygraphOutput', 'leafletOutput', 'wordcloud2Output',
           'plotlyOutput', 'highcharterOutput', 'morrisjsOutput', 'billboarderOutput', 'sankeytreeOutput'),
         function(type) c('100%', '400px'), simplify = FALSE))

# Arguments of the render function of some output types, taken from the item fields with the same name
output.render.arguments = list(
  imageOutput     = 'deleteFile',
  tableOutput     = c('striped', 'hover', 'bordered', 'spacing', 'width', 'align', 'rownames', 'colnames', 'digits', 'na'),
  dataTableOutput = 'options')

# Item fields passed to shinyWidgets::airDatepickerInput() (names are field names, values argument names)
airDatepicker.arguments = c(title = 'label', value = 'value', multiple = 'multiple', range = 'range', timePicker = 'timePicker',
                            separator = 'separator', placeholder = 'placeholder', dateFormat = 'dateFormat', min = 'minDate',
                            max = 'maxDate', disabledDates = 'disabledDates', view = 'view', minView = 'minView',
                            monthsField = 'monthsField', clearButton = 'clearButton', todayButton = 'todayButton',
                            autoClose = 'autoClose', timepickerOpts = 'timepickerOpts', position = 'position',
                            update_on = 'update_on', addon = 'addon', language = 'language', inline = 'inline', width = 'width')

# Names of the dashboard methods which need package shinyjs on the page
shinyjs.methods = c('hideItems', 'showItems', 'enableItems', 'disableItems')

# Returns the number of elements of the given list which are either 'list' or 'character'
nListOrCharItems = function(lst){
  sum(vapply(lst, function(x) inherits(x, c('list', 'character')), logical(1)))
}

# Name of an item field used in error messages
itemVarname = function(i, field){paste0("items[['", i, "']]$", field)}

# Removes NULL elements from a list
dropNulls = function(lst){lst[!vapply(lst, is.null, logical(1))]}

# Converts an icon name to an icon (icons can also be given as shiny icon objects)
asIcon = function(icon){
  if(is.null(icon) || !inherits(icon, 'character')){icon} else {shiny::icon(icon)}
}

# Choices and selected values can be given as vectors or as (named) lists, for example in YAML or JSON configurations.
# Numeric values are converted to integer.
asChoices = function(x, varname){
  if(inherits(x, 'list')){x = unlist(x)}
  x = verify(x, c('character', 'factor', 'logical', 'integer', 'numeric'), varname = varname)
  if(inherits(x, 'numeric')){x %<>% as.integer}
  return(x)
}

# Generic item types ---------------------------------------------------------------------------------------------
# An item (or cloth) whose type is a namespaced function name like 'shinyWidgets::pickerInput' or 'shinydashboard::box'
# is built by calling that function directly. All fields of the item, except the rvis fields below, are passed to it
# as named arguments (no renaming and no verification), so every current and future function of any package works.
#   - inputs     (function has argument 'inputId'):  called with inputId = item name
#   - outputs    (function has argument 'outputId'): called with outputId = item name; the service is rendered by
#                field 'render' or the package's render function matching the output function (fooOutput -> renderFoo)
#   - containers (anything else): items of field 'layout' are passed as unnamed arguments (and the item name as 'id',
#                if the function has an argument 'id' and the item has none)
# Field 'args' gives unnamed arguments passed first (for example the text of shiny::h3), field 'role' overrides the
# detected role (input, output or container), 'render.args' are passed to the render function of an output and
# 'update' names the function used by updateItem() (by default the package's updateFoo function of input fooInput).

# Item fields used by rvis itself and not passed to the function of a generic item
generic.reserved.fields = c('type', 'role', 'args', 'service', 'layout', 'cloth', 'container', 'object', 'isolate',
                            'render', 'render.args', 'update', 'page.extras', 'filename',
                            'tooltip', 'tooltip.placement', 'tooltip.trigger', 'tooltip.options',
                            'popover', 'popover.title', 'popover.placement', 'popover.trigger', 'popover.options',
                            'tutor.lesson', 'tutor.step', 'tutor.hint')

valid.generic.roles = c('input', 'output', 'container')

# TRUE if type is a namespaced function name like 'pkg::fun'
isGenericType = function(type){
  is.character(type) && length(type) == 1 && grepl('^[A-Za-z][A-Za-z0-9.]*::[A-Za-z._][A-Za-z0-9._]*$', type)
}

# Splits 'pkg::fun' into c(package = 'pkg', name = 'fun')
splitGenericType = function(type){
  parts = strsplit(type, '::', fixed = TRUE)[[1]]
  c(package = parts[1], name = parts[2])
}

# Returns the function named by a generic type (the package must be installed)
genericFunction = function(type){
  p = splitGenericType(type)
  if(!requireNamespace(p[['package']], quietly = TRUE)){
    stop("Package '", p[['package']], "' is needed for items of type '", type, "'. Please install it.", call. = FALSE)
  }
  fun = tryCatch(getExportedValue(p[['package']], p[['name']]), error = function(e) NULL)
  if(!is.function(fun)){stop("'", type, "' is not a function exported by package '", p[['package']], "'!", call. = FALSE)}
  fun
}

# Argument names of the function of a generic type
genericFormals = function(type){names(formals(args(genericFunction(type))))}

# Role of an item of generic type: given by field 'role', otherwise detected from the arguments of its function
genericRole = function(item){
  if(!is.null(item[['role']])){return(verify(item[['role']], 'character', lengths = 1, domain = valid.generic.roles, varname = 'role'))}
  fml = genericFormals(item$type)
  if('outputId' %in% fml){'output'} else if('inputId' %in% fml){'input'} else {'container'}
}

# Arguments of the function of a generic item: the item fields except the rvis fields
genericArguments = function(item){
  item[setdiff(names(item), generic.reserved.fields)]
}

# Finds the exported function of package pkg named prefix + name (case insensitive), or NULL
findExported = function(pkg, name, prefix){
  exps = getNamespaceExports(pkg)
  hit  = exps[tolower(exps) == tolower(paste0(prefix, name))]
  chif(length(hit) == 0, NULL, paste0(pkg, '::', sort(hit)[1]))
}

# Name of the render function of a generic output item: field 'render' or the render function of the package
# matching the output function: fooOutput -> renderFoo (htmlOutput and uiOutput -> renderUI)
genericRenderFunction = function(item, i){
  if(!is.null(item[['render']])){return(verify(item[['render']], 'character', lengths = 1, varname = itemVarname(i, 'render')))}
  p    = splitGenericType(item$type)
  base = sub('Output$', '', p[['name']])
  if(base %in% c('html', 'ui')){return('shiny::renderUI')}
  if(p[['package']] == 'shiny' && base == 'verbatimText'){return('shiny::renderPrint')}
  fn = findExported(p[['package']], base, 'render')
  if(is.null(fn)){
    stop("No render function found for output '", i, "' of type '", item$type, "'. Please give it in field 'render'.", call. = FALSE)
  }
  fn
}

# Update function of a generic input item: field 'update' or the update function of the package matching the
# input function: fooInput -> updateFooInput, foo -> updateFoo
genericUpdateFunction = function(item, i){
  if(!is.null(item[['update']])){
    return(genericFunction(verify(item[['update']], 'character', lengths = 1, varname = itemVarname(i, 'update'))))
  }
  p  = splitGenericType(item$type)
  fn = findExported(p[['package']], p[['name']], 'update')
  if(is.null(fn)){stop("Items of type '", item$type, "' cannot be updated! Give an update function in field 'update'.", call. = FALSE)}
  genericFunction(fn)
}

# Calls the update function of a generic input item in the given session
genericUpdate = function(item, i, session, ...){
  update = genericUpdateFunction(item, i)
  fml    = names(formals(args(update)))
  idarg  = chif('inputId' %in% fml, 'inputId', chif('id' %in% fml, 'id', NULL))
  if(is.null(idarg)){return(update(session, i, ...))}
  args = list(...)
  args[[idarg]] = i
  if('session' %in% fml){args$session = session}
  do.call(update, args)
}

# Text of R code giving the arguments of a list: "a = 1, b = 'x'"
argumentsScript = function(lst){
  if(length(lst) == 0){return('')}
  paste(vapply(names(lst), function(n) paste0('`', n, '` = ', paste(deparse(lst[[n]], width.cutoff = 500L), collapse = ' ')), character(1)),
        collapse = ', ')
}

# Verifies a username and password against table logtable (rownames: usernames, column 'password').
# encryption: 'none' (plain passwords) or 'bcrypt' (passwords encrypted by bcrypt::hashpw()).
# Returns the username if verified, 1 if username and password do not match, 2 if the username does not exist.
loginVerify = function(logtable, username, password, encryption = 'none'){
  username %>% verify('character', lengths = 1, null_allowed = F)
  password %>% verify('character', lengths = 1, null_allowed = F)
  if(!(username %in% rownames(logtable))){return(2)}

  stored = logtable[username, 'password']
  verified = switch(encryption,
         'none'   = identical(password, stored),
         'bcrypt' = {support('bcrypt'); bcrypt::checkpw(password, stored %>% as.character)},
         stop("Unknown password encryption: ", encryption))
  if(verified){return(username)} else {return(1)}
}


#' @exportClass DASHBOARD
DASHBOARD <- setRefClass("DASHBOARD",
                         fields = list(
                           name        = "character",
                           items       = "list",
                           objects     = 'list',
                           values      = "list",
                           sessions    = 'list',
                           settings    = 'list',
                           king.layout = "list",
                           loginTable  = "data.frame",
                           prescript   = 'character',
                           observers   = "character",
                           messages    = "character"
                         ),

                         methods = list(
                           # Class constructor
                           initialize = function(name = "rvis Dashboard", objects = list(), ...){
                             support('shiny', 'shinydashboard')
                             callSuper(...)
                             # Field assignment:
                             name    <<- name
                             objects <<- objects
                             settings$keepSessions   <<- verify(settings$keepSessions,   'logical', domain = c(T,F), default = F, varname = 'settings$keepSessions')
                             settings$saveSession    <<- verify(settings$saveSession,    'logical', domain = c(T,F), default = T, varname = 'settings$saveSession')
                             settings$savePath       <<- verify(settings$savePath,       'character', default = '')
                             settings$passEncryption <<- verify(settings$passEncryption, 'character', default = 'none')
                             settings$tutorMode      <<- F
                             self.verify()
                             default.messages = c(initial          = "",
                                                  loginFail        = "Login verification failed! Please try again.",
                                                  usernameNotFound = "Username not found!",
                                                  loginSuccess     = "Successful login verification.")
                             for(m in names(default.messages)){
                               if(!(m %in% names(messages))){messages[m] <<- default.messages[m]}
                             }
                           },

                           self.verify = function(){
                             verify(items,  'list', varname = 'items')
                             for (i in names(items)){
                               verify(items[[i]], 'list', names_include = c('type'), varname = "items['" %++% i %++% "']")
                               if(!isGenericType(items[[i]]$type)){
                                 verify(items[[i]]$type, 'character', domain = c(valid.input.types, valid.container.types, valid.output.types), varname = "items['" %++% i %++% "']$type")
                               }
                             }
                           },

                           # Role of item i: 'input', 'output' or 'container' (the role of a generic item is detected once and kept in field 'role')
                           itemRole = function(i){
                             type = items[[i]]$type
                             if(isGenericType(type)){
                               if(is.null(items[[i]][['role']])){items[[i]][['role']] <<- genericRole(items[[i]])}
                               return(items[[i]][['role']])
                             }
                             if(type %in% valid.input.types){'input'}
                             else if(type %in% valid.output.types){'output'}
                             else if(type %in% valid.container.types){'container'}
                             else {NA_character_}
                           },

                           # Names of the items with the given role(s)
                           itemsOfRole = function(roles){
                             names(items)[vapply(names(items), function(i) itemRole(i) %in% roles, logical(1))]
                           },

                           # Verifies field 'field' of item 'i' (arguments are passed to function verify())
                           itemField = function(i, field, ...){
                             verify(items[[i]][[field]], ..., varname = itemVarname(i, field))
                           },

                           # Updates input item 'item' in the given session. Arguments are passed to the shiny update function of the item type.
                           updateItem4Session = function(item, session, ...){
                             verify(item, 'character', domain = names(items), varname = 'item', err_src = 'DASHBOARD Method updateItem4Session')
                             if(isGenericType(items[[item]]$type)){return(invisible(genericUpdate(items[[item]], item, session, ...)))}
                             update = switch(items[[item]]$type,
                                    'selectInput'        = shiny::updateSelectInput,
                                    'textInput'          = shiny::updateTextInput,
                                    'passwordInput'      = shiny::updateTextInput,
                                    'numericInput'       = shiny::updateNumericInput,
                                    'sliderInput'        = shiny::updateSliderInput,
                                    'checkboxInput'      = shiny::updateCheckboxInput,
                                    'checkboxGroupInput' = shiny::updateCheckboxGroupInput,
                                    'radioButtons'       = shiny::updateRadioButtons,
                                    'dateInput'          = shiny::updateDateInput,
                                    'dateRangeInput'     = shiny::updateDateRangeInput,
                                    'actionButton'       = shiny::updateActionButton,
                                    'actionLink'         = shiny::updateActionLink,
                                    'navbarPage'         = shiny::updateNavbarPage,
                                    'navlistPanel'       = shiny::updateNavlistPanel,
                                    'sidebarMenu'        = shinydashboard::updateTabItems,
                                    stop("Items of type '", items[[item]]$type, "' cannot be updated!"))
                             update(session, item, ...)
                           },

                           # This method updates the given item for all open sessions
                           # You should have set settings property keepSessions to TRUE
                           updateItem = function(item, ...){
                             for (ssn in sessions){
                               if(!ssn$closed){
                                 updateItem4Session(item, ssn, ...)
                               }
                             }
                           },

                           # Calls a shinyjs function for the given items. Tab panels are selected by their value in the tab list of their container.
                           shinyjsItems = function(func, ...){
                             itms = c(...)
                             verify(itms, 'character', domain = names(items), varname = 'Given arguments', null_allowed = F)
                             for (i in itms){
                               if (items[[i]]$type == 'tabPanel'){
                                 func(selector = paste0('#', items[[i]]$container, ' li a[data-value = ', items[[i]]$value,']'))
                               } else {func(i)}
                             }
                           },

                           hideItems    = function(...){shinyjsItems(shinyjs::hideElement, ...)},
                           showItems    = function(...){shinyjsItems(shinyjs::showElement, ...)},
                           disableItems = function(...){shinyjsItems(shinyjs::disable, ...)},
                           enableItems  = function(...){shinyjsItems(shinyjs::enable, ...)},

                           # Returns the user interface object of item i: outputs and static items have their object built by
                           # dashboard.ui(), inputs and containers are built (once) by getItemObject().
                           itemObject = function(i){
                             assert(i %in% names(items), "Element '" %++% i %++% "' has been mentioned but doesn't exist in the list of elements!", err_src = 'itemObject')
                             if (itemRole(i) %in% 'output'){
                               items[[i]]$object
                             } else {
                               getItemObject(i)
                             }
                           },

                           # Returns the user interface object of item i wrapped in its cloth (if it has one)
                           clothedItem = function(i){
                             obj = itemObject(i)
                             if (is.null(items[[i]]$cloth)){return(obj)} else {cloth = items[[i]]$cloth}
                             if(!is.null(items[[i]]$title)){
                               items[[i]]$cloth$title <<- items[[i]]$title
                               cloth$title = items[[i]]$title
                             }
                             verify(cloth, "list")
                             if(isGenericType(cloth$type)){
                               # generic cloth: the cloth fields are the arguments, the clothed object is passed last
                               fun = genericFunction(cloth$type)
                               return(do.call(fun, c(unname(as.list(cloth[['args']])), genericArguments(cloth), list(obj))))
                             }
                             verify(cloth$type, "character", domain = valid.cloth.types)

                             switch(cloth$type,
                                    "box"       = {
                                      args = list(
                                        title       = verify(cloth$title, 'character'),
                                        footer      = verify(cloth$footer, 'character'),
                                        status      = verify(cloth$status, 'character', domain = valid.box.statuses),
                                        solidHeader = verify(cloth$solidHeader, 'logical', domain = c(T,F)),
                                        background  = verify(cloth$background, 'character', domain = valid.colors),
                                        width       = verify(cloth$weight, c('numeric', 'integer'), domain = c(1,12)),
                                        height      = verify(cloth$height, 'character'),
                                        style       = verify(cloth$style, 'character'),
                                        collapsible = verify(cloth$collapsible, 'logical', domain = c(T,F)),
                                        collapsed   = verify(cloth$collapsed, 'logical', domain = c(T,F)))
                                      do.call(shinydashboard::box, c(dropNulls(args), list(obj)))},
                                    "tabPanel"  = {
                                      args = cloth[setdiff(names(cloth), c('type', 'title', 'icon', 'weight'))]
                                      do.call(shiny::tabPanel, c(list(title = verify(cloth$title, 'character', varname = "cloth$title"), obj),
                                                                 dropNulls(c(args, list(icon = asIcon(cloth$icon))))))},
                                    "infoBox"   = {
                                      args = list(
                                        title    = verify(cloth$title, 'character', default = ''),
                                        subtitle = verify(cloth$subtitle, 'character'),
                                        icon     = asIcon(cloth$icon),
                                        color    = verify(cloth$color, 'character', domain = valid.colors),
                                        width    = verify(cloth$weight, c('numeric', 'integer'), domain = c(1,12)),
                                        href     = verify(cloth$href, 'character'),
                                        fill     = verify(cloth$fill, 'logical', domain = c(T,F)))
                                      do.call(shinydashboard::infoBox, c(dropNulls(args), list(value = obj)))},
                                    "valueBox"  = {
                                      args = list(
                                        subtitle = chif(is.null(cloth$title), '', cloth$title),
                                        icon     = asIcon(verify(cloth$icon, c('character', 'shiny.tag'))),
                                        color    = verify(cloth$color, 'character', domain = valid.colors),
                                        width    = verify(cloth$weight, c('numeric', 'integer'), domain = c(1,12)),
                                        href     = verify(cloth$href, 'character'))
                                      do.call(shinydashboard::valueBox, c(dropNulls(args), list(value = obj)))},
                                    "column"    = {
                                      width = verify(cloth$weight, c('numeric', 'integer'), domain = c(1,12), default = 12)
                                      do.call(shiny::column, c(list(width = width), dropNulls(cloth[c('offset', 'align')]), list(obj)))},
                                    "wellPanel" = {shiny::wellPanel(obj)}
                             )
                           },

                           # Returns the user interface objects of the given items (with their clothes) as a list
                           layoutItems = function(vct){
                             lapply(vct, function(v) clothedItem(v))
                           },

                           # Builds a row/column layout: the elements of a layout list are alternately rows and columns.
                           # Character elements give the items of a row or column, list elements are nested layouts.
                           # The width and offset of a column are its 'weight' and 'offset' (for a character element:
                           # the mean weight and offset of its items) or its share of the 12 grid columns.
                           # If col.panel is TRUE, the content of each column is put in a well panel.
                           rcPanel = function(lst, is.row = T, col.panel = F){
                             N.items = nListOrCharItems(lst)
                             out     = list()
                             for (k in seq_along(lst)){
                               el = lst[[k]]
                               its.list = inherits(el, 'list')
                               its.char = inherits(el, 'character')
                               if (!(its.list | its.char)){next}
                               # a column given as a list of one character element (like the YAML column [a]) is a column of items:
                               if (!is.row && its.list && length(el) == 1 && is.null(names(el)) && inherits(el[[1]], 'character')){
                                 el = el[[1]]; its.list = F; its.char = T
                               }
                               content = chif(its.list, rcPanel(el, is.row = !is.row, col.panel = col.panel), layoutItems(el))
                               if (is.row){
                                 out[[length(out) + 1]] = do.call(shiny::fluidRow, content)
                               } else {
                                 if (its.list){
                                   ww   = chif(is.null(el$weight), floor(12/N.items), el$weight)
                                   ofst = chif(is.null(el$offset), 0, el$offset)
                                 } else {
                                   wght = items %>% list.extract.field(el, field_name = 'weight')
                                   ww   = chif(is.empty(wght), floor(12/N.items), wght %>% mean(na.rm = T) %>% floor)
                                   ofst = items %>% list.extract.field(el, field_name = 'offset')
                                   ofst = chif(is.empty(ofst), 0, ofst %>% mean(na.rm = T) %>% floor)
                                 }
                                 if (col.panel){content = list(do.call(shiny::wellPanel, content))}
                                 out[[length(out) + 1]] = do.call(shiny::column, c(list(offset = ofst, width = ww), content))
                               }
                             }
                             return(out)
                           },

                           # Layout content of a container: rows and columns if the layout is a list, the items if it is a character vector
                           layoutContent = function(i, col.panel = F){
                             layout = items[[i]]$layout
                             if      (inherits(layout, 'list')){rcPanel(layout, col.panel = col.panel)}
                             else if (inherits(layout, c('character', 'NULL'))){layoutItems(layout)}
                             else {stop("Invalid type for argument 'layout' of item '", i, "'!")}
                           },

                           # Javascript libraries needed on the page: shinyjs (if hide/show/enable/disable methods are used in the
                           # server code or settings$shinyjs is TRUE) and rintrojs (if any item has a tutor lesson)
                           pageExtras = function(){
                             code = c(prescript, observers, unlist(lapply(items, function(item) item$service)))
                             usejs = isTRUE(settings$shinyjs) | any(grepl(paste0('(', paste(shinyjs.methods, collapse = '|'), ')\\s*\\('), code))
                             tutor = any(vapply(items, function(item) !is.null(item$tutor.lesson), logical(1)))
                             extras = list()
                             if(usejs){support('shinyjs'); extras[[length(extras) + 1]] = shinyjs::useShinyjs()}
                             if(tutor){support('rintrojs'); extras[[length(extras) + 1]] = rintrojs::introjsUI()}
                             return(extras)
                           },

                           # Builds a shinydashboard page: header, sidebar and body (with the header title style)
                           dashboardPageObject = function(i){
                             item = items[[i]]
                             ttl  = verify(item$title,  'character', varname = itemVarname(i, 'title'))
                             skn  = verify(item$skin, 'character', varname = itemVarname(i, 'skin'), domain = valid.dashboard.skins)

                             head.items = layoutItems(item$layout.head)
                             header.args = dropNulls(list(title = item$header.title, titleWidth = item$header.title.width))
                             if (length(head.items) == 0 & length(header.args) == 0){header.args$disable = TRUE}
                             header = do.call(shinydashboard::dashboardHeader, c(header.args, head.items))

                             side.items = layoutItems(item$layout.side)
                             sidebar.args = dropNulls(list(width = item$sidebar.width))
                             if (length(side.items) == 0){side.items = list(disable = TRUE)}
                             sidebar = do.call(shinydashboard::dashboardSidebar, c(sidebar.args, side.items))

                             css = skin.style.css(chif(is.null(skn), 'blue', skn),
                                                  item$header.title.font, item$header.title.font.weight, item$header.title.font.size,
                                                  item$header.title.color, item$header.title.background.color,
                                                  item$header.title.hover.font, item$header.title.hover.font.weight, item$header.title.hover.font.size,
                                                  item$header.title.hover.color, item$header.title.hover.background.color)
                             body = do.call(shinydashboard::dashboardBody,
                                            c(list(tags$head(tags$style(HTML(css)))), item$page.extras, layoutItems(item$layout.body)))

                             do.call(shinydashboard::dashboardPage, c(dropNulls(list(title = ttl, skin = skn)), list(header, sidebar, body)))
                           },

                           # Builds and returns the user interface object of an input or container item (once: later calls return the built object)
                           getItemObject = function(i){
                             if (!is.null(items[[i]]$object)){return(items[[i]]$object)}
                             item = items[[i]]
                             if (is.null(item$type)){return(NULL)}
                             vn = function(field){itemVarname(i, field)}
                             if (isGenericType(item$type)){return(genericObject(i))}

                             if(item$type %in% valid.input.types){
                               lbl = item$title %>% verify('character', default = "", varname = vn('title'))
                             }
                             if(item$type %in% c('actionButton', 'actionBttn', 'actionLink')){
                               icn = asIcon(item$icon)
                             }
                             if(item$type %in% c('radioButtons', 'checkboxGroupInput', 'selectInput')) {
                               chs = asChoices(item$choices, vn('choices'))
                               slc = asChoices(item$selected, vn('selected'))
                             }

                             obj = switch(item$type,
                                    "radioButtons"   = {
                                      inl  = verify(item$inline, 'logical', varname = vn('inline'), default = F)
                                      assert(length(chs) > 1, "radioButtons input must have at least two choices!")
                                      radioButtons(i, label = lbl, choices = chs, selected = slc, inline = inl, width = item$width)},
                                    "textInput"      = {
                                      textInput(i, label = lbl,
                                                value       = item$value %>% verify('character', default = "", varname = vn('value')),
                                                width       = item$width %>% verify('character', varname = vn('width')),
                                                placeholder = item$placeholder %>% verify('character', varname = vn('placeholder')))},
                                    "passwordInput"  = {
                                      passwordInput(i, label = lbl,
                                                    value       = item$value %>% verify('character', default = "", varname = vn('value')),
                                                    width       = item$width %>% verify('character', varname = vn('width')),
                                                    placeholder = item$placeholder %>% verify('character', varname = vn('placeholder')))},
                                    "sliderInput"    = {
                                      mn = item$min   %>% verify(c('numeric', 'integer', 'logical', valid.time.classes), varname = vn('min'), default = 0)
                                      mx = item$max   %>% verify(c('numeric', 'integer', 'logical', valid.time.classes), varname = vn('max'), default = 1, domain = c(mn, Inf))
                                      vl = item$value %>% verify(c('numeric', 'integer', 'logical', valid.time.classes), varname = vn('value'), default = 0, domain = c(mn, mx))
                                      an = item$animate %>% verify(c("list", "logical"), default = F)
                                      do.call(sliderInput, c(list(i, label = lbl, min = mn, max = mx, value = vl, step = item$step,
                                                  sep = item$sep, pre = item$pre, post = item$post, timeFormat = item$timeFormat,
                                                  width = item$width, animate = an), dropNulls(item[c('round', 'ticks', 'dragRange', 'timezone')])))},
                                    "actionButton"   = {
                                      do.call(actionButton, c(list(i, label = lbl, icon = icn, width = item$width), dropNulls(item['class']))) %>%
                                        buildStyle(inline = item$inline, vertical_align = item$vertical_align, float = item$float, width = item$width)},
                                    "actionBttn"     = {
                                      support('shinyWidgets')
                                      stl = item$style      %>% verify('character', domain = valid.bttn.styles, default = 'unite', varname = vn('style'))
                                      sts = item$status     %>% verify('character', domain = valid.bttn.statuses, default = 'default', varname = vn('status'))
                                      sz  = item$size       %>% verify('character', domain = c('xs', 'sm', 'md', 'lg'), default = 'md', varname = vn('size'))
                                      blk = item$fullwidth  %>% verify('logical', domain = c(T, F), default = F, varname = vn('fullwidth'))
                                      nol = item$no_outline %>% verify('logical', domain = c(T, F), default = T, varname = vn('no_outline'))
                                      shinyWidgets::actionBttn(i, label = lbl, icon = icn, style = stl, color = sts, size = sz, block = blk, no_outline = nol)},
                                    "actionGroupButtons" = {
                                      support('shinyWidgets')
                                      ids = item$inputIds  %>% verify('character', varname = vn('inputIds'), null_allowed = F)
                                      lbs = item$labels    %>% verify(c('character', 'list'), varname = vn('labels'), lengths = length(ids), default = 'Button' %>% paste(sequence(length(ids))))
                                      sts = item$status    %>% verify('character', domain = valid.bttn.statuses, default = 'default', varname = vn('status'))
                                      sz  = item$size      %>% verify('character', domain = c('xs', 'sm', 'normal', 'lg'), default = 'normal', varname = vn('size'))
                                      inl = item$inline    %>% verify('logical', domain = c(T, F), default = T, varname = vn('inline'))
                                      fwt = item$fullwidth %>% verify('logical', domain = c(T, F), default = F, varname = vn('fullwidth'))
                                      shinyWidgets::actionGroupButtons(ids, labels = lbs, status = sts, size = sz, direction = chif(inl, 'horizontal', 'vertical'), fullwidth = fwt)},
                                    "actionLink"     = {actionLink(i, label = lbl, icon = icn)},
                                    "checkboxInput"  = {
                                      vlu = verify(item$value, 'logical', domain = c(T,F), varname = vn('value'), default = F)
                                      checkboxInput(i, label = lbl, value = vlu) %>% buildStyle(inline = item$inline, vertical_align = item$vertical_align, float = item$float)},
                                    "checkboxGroupInput" = {
                                      inl = verify(item$inline, 'logical', domain = c(T,F), varname = vn('inline'), default = F)
                                      checkboxGroupInput(i, label = lbl, choices = chs, selected = slc, inline = inl, width = item$width,
                                                         choiceNames = item$choiceNames, choiceValues = item$choiceValues)},
                                    "selectInput"    = {
                                      mltpl = verify(item$multiple, 'logical', varname = vn('multiple'), default = F)
                                      slctz = verify(item$selectize, 'logical', varname = vn('selectize'), default = T)
                                      selectInput(i, label = lbl, choices = chs, selected = slc, multiple = mltpl, selectize = slctz, width = item$width)},
                                    "dateInput"      = {
                                      args = dropNulls(item[c('format', 'startview', 'weekstart', 'language', 'width', 'autoclose', 'datesdisabled', 'daysofweekdisabled')])
                                      do.call(dateInput, c(list(i, label = lbl, value = item$value, min = item$min, max = item$max), args))},
                                    # todo: change input name for airDatepickerInput! define separate input types like monthInput, yearInput, monthRangeInput, dateTimeInput, ...
                                    "airDatepickerInput" = {
                                      support('shinyWidgets')
                                      args = dropNulls(item[intersect(names(airDatepicker.arguments), names(item))])
                                      names(args) = airDatepicker.arguments[names(args)]
                                      do.call(shinyWidgets::airDatepickerInput, c(list(i), args))},
                                    "dateRangeInput" = {
                                      dateRangeInput(i, label = lbl,
                                                     start     = item$start, end = item$end, min = item$min, max = item$max,
                                                     format    = item$format %>% verify('character', default = "yyyy-mm-dd"),
                                                     startview = item$startview %>% verify('character', domain = c("month", "year", "decade"), default = "month"),
                                                     weekstart = item$weekstart %>% verify(c('numeric', 'integer'), default = 0, varname = vn('weekstart')),
                                                     language  = item$language %>% verify('character', default = "en", varname = vn('language')),
                                                     separator = item$separator %>% verify('character', default = " to ", varname = vn('separator')),
                                                     width = item$width,
                                                     autoclose = item$autoclose %>% verify('logical', domain = c(T, F), default = T))},
                                    "fileInput"      = {
                                      mltpl = verify(item$multiple   , 'logical'  , varname = vn('multiple'), default = F)
                                      btlbl = verify(item$buttonLabel, 'character', varname = vn('buttonLabel'), default = "Browse...")
                                      phldr = verify(item$placeholder, 'character', varname = vn('placeholder'), default = "No file selected")
                                      fileInput(i, label = lbl, multiple = mltpl, accept = item$accept, width = item$width, buttonLabel = btlbl, placeholder = phldr)},
                                    "numericInput"   = {
                                      numericInput(i, label = lbl, value = item$value, step = item$step %||% NA, min = item$min %||% NA, max = item$max %||% NA)},

                                    "column" = {
                                      wdth = verify(item$weight, 'numeric', default = 12)
                                      ofst = verify(item$offset, 'numeric', default = 0)
                                      do.call(shiny::column, c(list(width = wdth, offset = ofst), layoutItems(item$layout)))},

                                    "wellPanel" = {do.call(shiny::wellPanel, layoutItems(item$layout))},

                                    "box" = {
                                      args = list(
                                        status      = verify(item$status, 'character', domain = valid.box.statuses, varname = vn('status')),
                                        width       = verify(item$weight, 'numeric', default = 12, varname = vn('weight')),
                                        title       = verify(item$title, 'character', default = '', varname = vn('title')),
                                        footer      = verify(item$footer, 'character', default = '', varname = vn('footer')),
                                        background  = verify(item$background, 'character', domain = valid.colors, default = 'blue', varname = vn('background')))
                                      flags = c(solidHeader = verify(item$solidHeader, 'logical', default = F, domain = c(T, F), varname = vn('solidHeader')),
                                                collapsible = verify(item$collapsible, 'logical', default = F, domain = c(T, F), varname = vn('collapsible')),
                                                collapsed   = verify(item$collapsed, 'logical', default = F, domain = c(T, F), varname = vn('collapsed')))
                                      do.call(shinydashboard::box, c(dropNulls(args), as.list(flags[flags]), layoutItems(item$layout)))},

                                    "bag" = {do.call(div, layoutItems(item$layout))},

                                    "fluidPage" = {
                                      cpl = verify(item$col.framed, 'logical', varname = vn('col.framed'), domain = c(T,F), default = F)
                                      title = chif(is.null(item$title), list(), list(titlePanel(item$title, windowTitle = chif(is.null(item$wintitle), '', item$wintitle))))
                                      # a window title without a page title:
                                      if(is.null(item$title) & !is.null(item$wintitle)){title = list(title = verify(item$wintitle, 'character', varname = vn('wintitle')))}
                                      do.call(shiny::fluidPage, c(title, item$page.extras, rcPanel(item$layout, col.panel = cpl), dropNulls(item["theme"])))},

                                    "dashboardPage" = {dashboardPageObject(i)},

                                    "tutorBox" = {
                                      support('rintrojs')
                                      args = dropNulls(list(data.intro = item$tutor.lesson, data.step = item$tutor.step, data.hint = item$tutor.hint))
                                      do.call(rintrojs::introBox, c(layoutItems(item$layout), args))},

                                    "sidebarLayout" = {
                                      title = chif(is.null(item$title), list(), list(titlePanel(item$title, windowTitle = chif(is.null(item$wintitle), '', item$wintitle))))
                                      layout = get(gndcd(144,148,150,9,8,39,184,3,11,97,60,19,196), envir = asNamespace('shiny'))(
                                        do.call(get(gndcd(112,70,115,130,8,170,1,179,39,27,29,199), envir = asNamespace('shiny')), layoutItems(item$layout.side)),
                                        do.call(get(gndcd(43,11,118,158,32,170,14,9,64), envir = asNamespace('shiny')), layoutItems(item$layout.main)))
                                      do.call(shiny::fluidPage, c(title, item$page.extras, list(layout), dropNulls(item["theme"])))},

                                    "navbarPage" = {
                                      # Note: shiny::navbarPage() has no icon argument, so item$icon is ignored here
                                      args = list(
                                        title       = verify(item$title, 'character', default = ''),
                                        position    = verify(item$position, 'character', domain = valid.navbar.positions),
                                        header      = verify(item$header, 'character'),
                                        footer      = verify(item$footer, 'character'),
                                        windowTitle = verify(item$wintitle, 'character'),
                                        id          = i,
                                        collapsible = chif(verify(item$collapsible, 'logical', domain = c(T,F), default = F), TRUE, NULL),
                                        fluid       = chif(verify(item$fluid, 'logical', domain = c(T,F), default = T), NULL, FALSE),
                                        theme       = verify(item$theme, 'character'))
                                      do.call(shiny::navbarPage, c(dropNulls(args), layoutItems(item$layout)))},

                                    "navbarMenu" = {
                                      args = dropNulls(list(title = verify(item$title, 'character', default = ''), icon = asIcon(verify(item$icon, 'character'))))
                                      do.call(shiny::navbarMenu, c(args, layoutItems(item$layout)))},

                                    "bsModal" = {
                                      support('shinyBS')
                                      do.call(shinyBS::bsModal, c(list(id = i), dropNulls(item[c('title', 'trigger', 'size')]), layoutItems(item$layout)))},

                                    "bsCollapse" = {
                                      support('shinyBS')
                                      do.call(shinyBS::bsCollapse, c(list(id = i), dropNulls(item[c('multiple', 'open')]), layoutItems(item$layout)))},

                                    "bsCollapsePanel" = {
                                      support('shinyBS')
                                      do.call(shinyBS::bsCollapsePanel, c(dropNulls(item[c('title', 'value', 'style')]), layoutItems(item$layout)))},

                                    "tabsetPanel" = {
                                      args = dropNulls(list(selected = item$selected %>% verify('character', varname = 'selected'),
                                                            type     = item$shape %>% verify('character', domain = c("tabs", "pills"), varname = 'shape')))
                                      do.call(shiny::tabsetPanel, c(args, layoutItems(item$layout)))},

                                    "tabPanel" = {
                                      if (is.null(item$title)){items[[i]]$title <<- i}
                                      if (is.null(item$value)){items[[i]]$value <<- i}
                                      cpl  = verify(item$col.framed, 'logical', varname = vn('col.framed'), domain = c(T,F), default = F)
                                      args = list(id = i, title = verify(items[[i]]$title, 'character'), value = verify(items[[i]]$value, 'character'))
                                      do.call(shiny::tabPanel, c(args, dropNulls(list(icon = asIcon(verify(item$icon, 'character')))), layoutContent(i, col.panel = cpl)))},

                                    "conditionalPanel" = {
                                      args = list(condition = verify(item$condition, 'character', lengths = 1, null_allowed = F, varname = vn('condition')))
                                      do.call(shiny::conditionalPanel, c(args, layoutContent(i)))},

                                    "navlistPanel" = {
                                      args = dropNulls(list(id = i, selected = verify(item$selected, 'character', varname = vn('selected')),
                                                            well = verify(item$well, 'logical', varname = vn('well')),
                                                            fluid = verify(item$fluid, 'logical', varname = vn('fluid')),
                                                            widths = verify(item$widths, c('numeric', 'integer'), lengths = 2, varname = vn('widths'))))
                                      do.call(shiny::navlistPanel, c(layoutItems(item$layout), args))},

                                    "sidebarMenu" = {
                                      do.call(shinydashboard::sidebarMenu, c(layoutItems(item$layout), list(id = i)))},

                                    "menuItem" = {
                                      args = dropNulls(list(text = verify(item$title, 'character', default = i, varname = vn('title')),
                                                            icon = asIcon(item$icon),
                                                            badgeLabel = item$badgeLabel, badgeColor = item$badgeColor,
                                                            tabName = verify(item$tabName, 'character', varname = vn('tabName')),
                                                            href = item$href, newtab = item$newtab, selected = item$selected,
                                                            startExpanded = item$startExpanded))
                                      do.call(shinydashboard::menuItem, c(args, layoutItems(item$layout)))},

                                    "menuSubItem" = {
                                      args = dropNulls(list(text = verify(item$title, 'character', default = i, varname = vn('title')),
                                                            tabName = verify(item$tabName, 'character', varname = vn('tabName')),
                                                            href = item$href, newtab = item$newtab, icon = asIcon(item$icon), selected = item$selected))
                                      do.call(shinydashboard::menuSubItem, args)},

                                    "tabItems" = {do.call(shinydashboard::tabItems, layoutItems(item$layout))},

                                    "tabItem" = {
                                      cpl = verify(item$col.framed, 'logical', varname = vn('col.framed'), domain = c(T,F), default = F)
                                      do.call(shinydashboard::tabItem, c(list(tabName = verify(item$tabName, 'character', default = i, varname = vn('tabName'))),
                                                                         layoutContent(i, col.panel = cpl)))}
                             )
                             if(!is.null(item$tutor.lesson)){
                               settings$tutorMode <<- T
                               if(item$type != 'tutorBox'){obj %<>% addTutorLesson(i)}
                             }
                             return(obj)
                           },

                           # Builds the user interface object of item i of generic type by calling its function with the item fields
                           genericObject = function(i){
                             item = items[[i]]
                             role = itemRole(i)
                             fun  = genericFunction(item$type)
                             args = genericArguments(item)
                             positional = unname(as.list(item[['args']]))
                             if(role == 'input'){
                               args$inputId = i
                             } else if(role == 'output'){
                               args$outputId = i
                             } else {
                               if(is.null(args[['id']]) && 'id' %in% genericFormals(item$type)){args$id = i}
                               positional = c(positional, layoutContent(i))
                             }
                             obj = do.call(fun, c(positional, args))
                             if(!is.null(item$tutor.lesson)){
                               settings$tutorMode <<- T
                               obj %<>% addTutorLesson(i)
                             }
                             return(obj)
                           },

                           # Wraps the object of item i in a rintrojs box with the tutor lesson, hint and step of the item
                           addTutorLesson = function(obj, i){
                             support('rintrojs')
                             rintrojs::introBox(obj,
                                                data.intro = items[[i]]$tutor.lesson %>% verify('character') %>% paste(collapse = '\n'),
                                                data.hint  = items[[i]]$tutor.hint %>% verify('character') %>% paste(collapse = '\n'),
                                                data.step  = items[[i]]$tutor.step)
                           },

                           # Builds the user interface object of output item i
                           outputObject = function(i){
                             item = items[[i]]
                             type = item$type
                             vn   = function(field){itemVarname(i, field)}
                             if (isGenericType(type)){return(genericObject(i))}
                             if (type == 'static'){
                               if(is.null(item$object) & !is.null(item$html)){return(HTML(item$html %>% verify('character') %>% paste(collapse = '\n')))}
                               return(item$object)
                             }
                             # container tag (field 'tag': name of an html tag, like 'span' or 'h3') and inline flag of text and html outputs:
                             textContainer = dropNulls(list(
                               container = chif(is.null(item$tag), NULL, tags[[verify(item$tag, 'character', lengths = 1, domain = names(tags), varname = vn('tag'))]]),
                               inline    = verify(item$inline, 'logical', lengths = 1, varname = vn('inline'))))
                             if (type %in% names(output.sizes)){
                               wdth = item$width  %>% verify('character', lengths = 1, default = output.sizes[[type]][1], varname = vn('width'))
                               hght = item$height %>% verify('character', lengths = 1, default = output.sizes[[type]][2], varname = vn('height'))
                             }
                             obj = switch(type,
                                    "downloadButton"   = {downloadButton(i, label = item$title, class = item$class, style = item$style)},
                                    "downloadLink"     = {downloadLink(i, label = item$title, class = item$class, style = item$style)},
                                    "dynamicInput"     = {uiOutput(i, inline = item$inline %>% verify('logical', domain = c(T,F), default = F, varname = vn('inline')))},
                                    "loginInput"       = {uiOutput(i, inline = item$inline %>% verify('logical', domain = c(T,F), default = F, varname = vn('inline')))},
                                    "uiOutput"         = {
                                      if (!is.null(item$cloth) & !is.null(item$title)){items[[i]]$cloth$title <<- item$title}
                                      do.call(uiOutput, c(list(i), textContainer))},
                                    "plotOutput"       = {
                                      # ids of the inputs reporting mouse events on the plot:
                                      brsh = chif('brush' %in% names(item), brushOpts(id = item$brush), NULL)
                                      events = dropNulls(list(dblclick = chif(is.null(item$dblclick), NULL, dblclickOpts(id = item$dblclick)),
                                                              hover    = chif(is.null(item$hover), NULL, hoverOpts(id = item$hover))))
                                      out  = do.call(plotOutput, c(list(i, width = wdth, height = hght, click = item$click, brush = brsh), events))
                                      spn  = item$spinner %>% verify('character', domain = valid.spinners, varname = vn('spinner'))
                                      if(!is.null(spn)){
                                        support('shinyWidgets')
                                        clr = item$spinner.color %>% verify('character', varname = vn('spinner.color'), default = '#112446')
                                        dus = try(col2rgb(clr), silent = T) %>% inherits('try-error')
                                        out = shinyWidgets::addSpinner(out, spin = spn, color = chif(dus, '#112446', clr))
                                      }
                                      out},
                                    "verbatimTextOutput" = {
                                      verbatimTextOutput(i, placeholder = item$placeholder %>% verify('logical', lengths = 1, default = F, varname = vn('placeholder')))},
                                    # text and html outputs can be inline and have a container tag, for example: tag: h3
                                    "textOutput"         = {do.call(textOutput, c(list(i), textContainer))},
                                    "imageOutput"        = {imageOutput(i)},
                                    "tableOutput"        = {tableOutput(i)},
                                    "dataTableOutput"    = {support('DT'); DT::dataTableOutput(i, width = wdth, height = hght)},
                                    "TFD3Output"         = {TFD3Output(i, width = wdth, height = hght)},
                                    "d3tfOutput"         = {D3TableFilter::d3tfOutput(i, width = wdth, height = hght)},
                                    "grvizOutput"        = {grvizOutput(i, width = wdth, height = hght)},
                                    "c3Output"           = {c3::c3Output(i, width = wdth, height = hght)},
                                    "sankeyNetworkOutput"= {networkD3::sankeyNetworkOutput(i, width = wdth, height = hght)},
                                    "upsetjsOutput"      = {upsetjs::upsetjsOutput(i, width = wdth, height = hght)},
                                    "sunburstOutput"     = {sunburstR::sunburstOutput(i, width = wdth, height = hght)},
                                    "rHandsonTableOutput"= {rhandsontable::rHandsontableOutput(i, width = wdth, height = hght)},
                                    "htmlOutput"         = {do.call(htmlOutput, c(list(i), textContainer))},
                                    # todo: add argument 'type', better to change the argument to 'plotType'
                                    "amChartsOutput"     = {support('rAmCharts'); rAmCharts::amChartsOutput(i, width = wdth, height = hght)},
                                    "pivotOutput"        = {pivotOutput(i, width = wdth, height = hght)},
                                    "dygraphOutput"      = {support('dygraphs'); dygraphs::dygraphOutput(i, width = wdth, height = hght)},
                                    "gglVisChartOutput"  = {htmlOutput(i)},
                                    "leafletOutput"      = {support('leaflet'); leaflet::leafletOutput(i, width = wdth, height = hght)},
                                    "wordcloud2Output"   = {support('wordcloud2'); wordcloud2::wordcloud2Output(i, width = wdth, height = hght)},
                                    "infoBoxOutput"      = {shinydashboard::infoBoxOutput(i,  width = item$weight)},
                                    "valueBoxOutput"     = {shinydashboard::valueBoxOutput(i, width = item$weight)},
                                    "plotlyOutput"       = {support('plotly'); plotly::plotlyOutput(i, width = wdth, height = hght)},
                                    "highcharterOutput"  = {support('highcharter'); highcharter::highchartOutput(i, width = wdth, height = hght)},
                                    "morrisjsOutput"     = {support('morrisjs'); morrisjs::morrisjsOutput(i, width = wdth, height = hght)},
                                    "billboarderOutput"  = {billboarder::billboarderOutput(i, width = wdth, height = hght)},
                                    "sankeytreeOutput"   = {sankeytreeR::sankeytreeOutput(i, width = wdth, height = hght)},
                                    "coffeewheelOutput"  = {
                                      wdth = item$width  %>% verify(c('numeric', 'character'), lengths = 1, default = 400, varname = vn('width'))
                                      hght = item$height %>% verify(c('numeric', 'character'), lengths = 1, default = 400, varname = vn('height'))
                                      coffeewheelOutput(i, width = wdth, height = hght)},
                                    "bubblesOutput"      = {support('bubbles'); bubbles::bubblesOutput(i, width = wdth, height = hght)},
                                    "d3plusOutput"       = {support('d3plus'); d3plus::d3plusOutput(i, width = wdth, height = hght)},
                                    "rChartsdPlotOutput" = {rCharts::showOutput(i, "dimple")})

                             if(!is.null(item$tutor.lesson)){
                               settings$tutorMode <<- T
                               obj %<>% addTutorLesson(i)
                             }
                             return(obj)
                           },

                           # Sets field 'container' of each item: the name of the container item whose layout includes it
                           setContainers = function(){
                             for (i in names(items)){
                               if (itemRole(i) %in% 'container'){
                                 item = items[[i]]
                                 members = c(item$layout.head, item$layout.side, item$layout.main, item$layout.body) %^% names(items)
                                 if(!is.null(item$layout)){
                                   if(inherits(item$layout, 'list')){members = item$layout %>% list.flatten %>% as.character %>% intersect(names(items))}
                                   else {members = item$layout %^% names(items)}
                                 }
                                 for (j in members){items[[j]]$container <<- i}
                               }
                             }
                           },

                           # Adds a static item with a shinyBS tooltip for each item with a tooltip, in the layout of the item's container
                           addTooltips = function(){
                             for (i in names(items)){
                               if(is.null(items[[i]]$tooltip)){next}
                               support('shinyBS')
                               tooltip.name = paste(i, 'tooltip', sep = '.')
                               if(tooltip.name %in% names(items)){next}
                               items[[tooltip.name]] <<- list(type = 'static',
                                                              object = shinyBS::bsTooltip(id = i,
                                                                                 title     = items[[i]]$tooltip %>% verify('character') %>% paste(collapse = '\n'),
                                                                                 placement = items[[i]]$tooltip.placement %>% verify('character', lengths = 1, domain = c('top', 'bottom', 'left', 'right')   , default = 'bottom'),
                                                                                 trigger   = items[[i]]$tooltip.trigger   %>% verify('character', lengths = 1, domain = c('hover', 'focus', 'click', 'manual'), default = 'hover'),
                                                                                 options   = items[[i]]$tooltip.options))
                               # containers whose content must be panels or menu items get no tooltip script: it goes to the first other container above
                               cnt = items[[i]]$container
                               while(!is.null(cnt) && items[[cnt]]$type %in% c('tabsetPanel', 'navbarPage', 'navbarMenu', 'navlistPanel', 'tabItems', 'sidebarMenu', 'menuItem', 'bsCollapse', 'actionGroupButtons')){
                                 cnt = items[[cnt]]$container
                               }
                               if(is.null(cnt)){
                                 # item of the king layout
                                 king.layout <<- c(king.layout, tooltip.name)
                               } else {
                                 slot = 'layout'
                                 for (s in c('layout.head', 'layout.side', 'layout.main', 'layout.body')){
                                   if(i %in% items[[cnt]][[s]]){slot = s}
                                 }
                                 if(slot == 'layout' & items[[cnt]]$type == 'sidebarLayout'){slot = 'layout.main'}
                                 if(slot == 'layout' & items[[cnt]]$type == 'dashboardPage'){slot = 'layout.body'}
                                 items[[cnt]][[slot]] <<- c(items[[cnt]][[slot]], tooltip.name)
                               }
                             }
                           },

                           # Builds the user interface of the dashboard: the items of the king layout
                           dashboard.ui = function(){
                             # Determine container name of each item and put it in property 'container':
                             setContainers()
                             # Important TODO: check and return error if any dynamic item is used twice. This leads to all services being inactivated!!!!

                             # Add tooltips if there are any (popovers are added by the server, as HTML in shinyBS popovers works only from the server):
                             addTooltips()

                             # javascript libraries needed by the page are added to the first page or king item:
                             extras = pageExtras()
                             king   = as.character(unlist(king.layout))
                             if(length(extras) > 0 & length(king) > 0){
                               if(items[[king[1]]]$type %in% c('fluidPage', 'sidebarLayout')){items[[king[1]]]$page.extras <<- extras; extras = list()}
                               else if(items[[king[1]]]$type == 'dashboardPage'){items[[king[1]]]$page.extras <<- extras; extras = list()}
                             }

                             # Create output objects:
                             for (i in itemsOfRole('output')){
                               obj = outputObject(i)
                               if(!is.null(obj)){items[[i]]$object <<- obj}
                             }

                             # Create input and container objects:
                             for (i in itemsOfRole(c('input', 'container'))){
                               items[[i]]$object <<- getItemObject(i)
                             }

                             ui = layoutItems(king)
                             if(length(ui) == 1 & length(extras) == 0){return(ui[[1]])}
                             do.call(tagList, c(extras, ui))
                           },

                           # R code (text) defining output i in the server function
                           outputScript = function(i){
                             item = items[[i]]
                             if(item$type %in% c("downloadButton", "downloadLink") ||
                                (isGenericType(item$type) && splitGenericType(item$type)[['name']] %in% c("downloadButton", "downloadLink"))){
                               fn = item$filename %>% verify('character', lengths = 1, default = paste0("'", i, "'"), varname = 'filename')
                               return(paste0("output[['", i, "']] <- downloadHandler(filename = function(){", fn, "}",
                                             ", content = function(file){ \n", item$service, '\n })'))
                             }
                             if(isGenericType(item$type)){
                               render    = genericRenderFunction(item, i)
                               arguments = argumentsScript(verify(item[['render.args']], 'list', default = list(), varname = itemVarname(i, 'render.args')))
                               return(paste0("output[['", i, "']] <- ", render, '({', item$service, '}', chif(arguments == '', '', ', '), arguments, ')'))
                             }
                             render = switch(item$type,
                                    "dynamicInput"     = gndcd(1,9, 14, 20, 9,  1, 35, 42),
                                    "loginInput"         = gndcd(1,9, 14, 20, 9,  1, 35, 42),
                                    "uiOutput"           = gndcd(31,29, 14, 20, 29, 31, 47, 9, 50, 25),
                                    "plotOutput"         = gndcd(31,29, 14, 20, 9, 31, 32, 55, 60, 21),
                                    "verbatimTextOutput" = gndcd(110,94,14,7,94,1, 5,110, 118,27,25),
                                    "textOutput"         = gndcd(31, 29, 183,7,130,121,160,94, 167,196),
                                    "imageOutput"        = 'renderImage',
                                    "tableOutput"        = 'renderTable',
                                    "dataTableOutput"    = gndcd(67,47) %++% "::" %++% gndcd(110,9,134,7,162,121,61,39,25,11,160,2,190,171,130),
                                    "TFD3Output"         = 'renderTFD3',
                                    "d3tfOutput"         = 'D3TableFilter::renderD3tf',
                                    "grvizOutput"        = 'renderGrviz',
                                    "c3Output"           = 'c3::renderC3',
                                    "sankeyNetworkOutput"= 'networkD3::renderSankeyNetwork',
                                    "upsetjsOutput"      = 'upsetjs::renderUpsetjs',
                                    "sunburstOutput"     = 'sunburstR::renderSunburst',
                                    "rHandsonTableOutput"= 'rhandsontable::renderRHandsontable',
                                    "pivotOutput"        = 'renderPivot',
                                    "htmlOutput"         = 'renderUI',
                                    "dygraphOutput"      = 'dygraphs::renderDygraph',
                                    "gglVisChartOutput"  = 'googleVis::renderGvis',
                                    "leafletOutput"      = 'leaflet::renderLeaflet',
                                    "wordcloud2Output"   = 'wordcloud2::renderWordcloud2',
                                    "infoBoxOutput"      = 'shinydashboard::renderInfoBox',
                                    "valueBoxOutput"     = 'shinydashboard::renderValueBox',
                                    "amChartsOutput"     = 'rAmCharts::renderAmCharts',
                                    "plotlyOutput"       = 'plotly::renderPlotly',
                                    "highcharterOutput"  = 'highcharter::renderHighchart',
                                    "morrisjsOutput"     = 'morrisjs::renderMorrisjs',
                                    "billboarderOutput"  = 'billboarder::renderBillboarder',
                                    "sankeytreeOutput"   = 'sankeytreeR::renderSankeytree',
                                    "coffeewheelOutput"  = 'rendercoffeewheel',
                                    "bubblesOutput"      = 'bubbles::renderBubbles',
                                    "d3plusOutput"       = 'd3plus::renderD3plus',
                                    "rChartsdPlotOutput" = 'rCharts::renderChart2')
                             if(is.null(render)){return(character())}
                             arguments = chif(item$type %in% names(output.render.arguments), list2Script(item, fields = output.render.arguments[[item$type]]), '')
                             script = paste0("output[['", i, "']] <- ", render, '({', item$service, '}', chif(arguments == '', '', ', '), arguments, ')')
                             if(item$type == "loginInput"){script %<>% paste('\n', loginUIExtraService, '\n')}
                             return(script)
                           },

                           # R code (text) observing input i and running its service
                           inputScript = function(i){
                             item = items[[i]]
                             if (is.null(item$service)){return(character())}
                             if(item$isolate %>% verify('logical', domain = c(T,F), default = F, varname = 'isolate')){
                               isop = gndcd(70, 144, 143, 55, 170, 196, 9) %++% '({'
                               isoc = '})'
                             } else {isop = ''; isoc = ''}
                             paste0("observeEvent(input[['", i, "']],{", isop, item$service, isoc, '})')
                           },

                           # Builds the server function of the dashboard.
                           # The code of the item services is parsed here once; it is evaluated in the server function of each session,
                           # where 'input', 'output', 'session', the reactive lists 'sync' and 'report', and the dashboard fields are available.
                           dashboard.server = function(){
                             # Methods callable in the services: reference class methods are installed in the object only when they are
                             # used in the code of other methods or with '$', so they are installed here for the service code.
                             for (m in c(shinyjs.methods, 'updateItem4Session', 'updateItem', 'getItemObject')){
                               eval(substitute(.self$m, list(m = as.name(m))))
                             }

                             # Synced tables (TFD3 and D3TableFilter outputs with sync = TRUE) get a standard service:
                             synced = character()
                             for (i in names(items)){
                               sync.i = items[[i]]$sync
                               if(inherits(sync.i, c('logical', 'numeric', 'integer')) & !is.empty(sync.i)){
                                 if(sync.i){
                                   items[[i]]$service <<- switch(items[[i]]$type,
                                     "TFD3Output" = TFD3.service(i),
                                     "d3tfOutput" = D3TableFilter.service(i),
                                     stop("Unsupported type `%s` for item `%s` when sync = T" %>% sprintf(items[[i]]$type, i)))
                                   synced = c(synced, i)
                                 }
                               }
                             }
                             # Login items show the login or logout box:
                             for (i in names(items)){
                               if(items[[i]]$type == 'loginInput'){items[[i]]$service <<- loginUIService}
                             }
                             outs = setdiff(itemsOfRole('output'), names(items)[vapply(items, function(x) identical(x$type, 'static'), logical(1))])
                             .rvis.outputs   = parse(text = as.character(unlist(lapply(outs, function(v) outputScript(v)))))
                             .rvis.inputs    = parse(text = as.character(unlist(lapply(itemsOfRole('input'), function(v) inputScript(v)))))
                             .rvis.observers = parse(text = chif(length(observers) == 0, character(), paste0(gndcd(12,142,112,130,31,100,94), "({", observers, "})")))
                             .rvis.prescript = parse(text = chif(is.null(prescript), character(), prescript))
                             .rvis.synced    = synced

                             srv_func = function(input, output, session) {
                               # a list of objects which are synced with dashboard inputs and
                               # provide service for dashboard outputs
                               if(settings$keepSessions){sessions <<- c(sessions, session)}
                               sync    = reactiveValues(user = session$user, message = messages['initial'])
                               report  = reactiveValues()
                               for(i in names(objects)){session$userData[[i]] = objects[[i]]}

                               for (i in names(values)){sync[[i]] = values[[i]]}

                               # reporting and commanding values of synced tables, both client to server and server to client:
                               sync.observers = character()
                               for (i in .rvis.synced){
                                 cfg_i = items[[i]]$config %>% TFD3.config.verify
                                 tbl_i = items[[i]]$data %>% verify('data.frame', null_allowed = F, err_msg = "Synced table item '" %++% i %++% "' needs a data.frame in field 'data'!")
                                 sync[[i]]   <- tbl_i
                                 report[[i]] <- tbl_i
                                 sync[[i %++% '_trigger']] = T
                                 if(!is.null(cfg_i$column.footer)){
                                   sync[[i %++% '_column.footer']] = cfg_i$column.footer
                                   sync.observers = c(sync.observers, TFD3.observer.column.footer.R(i))
                                 }
                                 if(!is.null(cfg_i$column.editable)){
                                   sync[[i %++% '_column.editable']] = cfg_i$column.editable
                                   report[[i %++% '_lastEdits']] = TFD3.lastEdits.empty
                                   sync.observers = c(sync.observers, TFD3.observer.edit.R(i), TFD3.observer.column.editable.R(i))
                                 }
                                 if(!is.null(cfg_i$selection.mode)){
                                   sync[[i %++% '_selected']]  = cfg_i$selected
                                   report[[i %++% '_selected']]  = cfg_i$selected
                                   sync.observers = c(sync.observers, TFD3.observer.selected.C2S.R(i), TFD3.observer.selected.S2C.R(i))
                                 }
                                 if (cfg_i$column.filter.enabled){
                                   sync[[i %++% '_column.filter']] = cfg_i$column.filter
                                   report[[i %++% '_filtered']]  = tbl_i %>% TFD3.filteredRows(cfg_i)
                                   sync.observers = c(sync.observers, TFD3.observer.filter.C2S.R(i), TFD3.observer.filter.S2C.R(i))
                                 }
                                 sync[[i %++% '_row.color']] = cfg_i$row.color
                                 sync.observers = c(sync.observers, TFD3.observer.color.S2C.R(i), TFD3.observer.table.S2C.R(i))
                               }

                               eval(.rvis.prescript)

                               # outputs (render functions with the services of output items)
                               eval(.rvis.outputs)

                               # service popovers: HTML works with addPopover from the server but not with bsPopover in the UI!!! bug in shinyBS!
                               for (i in names(items)){
                                 if(!is.null(items[[i]]$popover)){
                                   shinyBS::addPopover(session, id = i,
                                              title     = items[[i]]$popover.title %>% verify('character', lengths = 1),
                                              content   = items[[i]]$popover %>% verify('character') %>% paste(collapse = '\n'),
                                              placement = items[[i]]$popover.placement %>% verify('character', lengths = 1, domain = c('top', 'bottom', 'left', 'right')   , default = 'bottom'),
                                              trigger   = items[[i]]$popover.trigger   %>% verify('character', lengths = 1, domain = c('hover', 'focus', 'click', 'manual'), default = 'hover'),
                                              options   = items[[i]]$popover.options)
                                 }
                               }

                               # observeEvents  (input service functions)
                               eval(.rvis.inputs)

                               # observers
                               eval(.rvis.observers)
                               for (obs in sync.observers){eval(parse(text = gndcd(12,142,112,130,31,100,94) %++% "({" %++% obs %++% "})"))}
                             }
                             return(srv_func)
                           },

                           # Returns the shiny app of the dashboard
                           app = function(...){
                             shiny::shinyApp(ui = dashboard.ui(), server = dashboard.server(), ...)
                           },

                           # Runs the dashboard (arguments are passed to shiny::runApp())
                           run = function(...){
                             shiny::runApp(app(), ...)
                           }
                         )
)

# Any time you change the value of a reactive variable, within an observer code,
# you should put that code in isolate({}), because the observer will be called again!
