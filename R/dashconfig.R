# dashconfig.R ----------------------------------------------------------------

# Header
# Filename:      dashconfig.R
# Description:   Builds DASHBOARD objects (dashboard.R) from configurations: YAML or JSON files, or R lists.
# Author:        Nima Ramezani
# Email :        nima.ramezani@gmail.com
# Start Date:    23 November 2026
# Last Revision: 23 November 2026
# Version:       0.1.0

# Version History:

# Version   Date                Action
# ---------------------------------------
# 0.1.0     23 November 2026    Initial issue: rvisDashboard(), runDashboard(), rvisDashboardExample()

#' @include dashboard.R

# Fields of a dashboard configuration and the DASHBOARD fields they set
dashboard.config.fields = c(name = 'name', items = 'items', layout = 'king.layout', king.layout = 'king.layout',
                            objects = 'objects', values = 'values', settings = 'settings', messages = 'messages',
                            prescript = 'prescript', observers = 'observers', loginTable = 'loginTable')

# YAML 1.2 booleans: only true/false are logical values. In YAML 1.1 (the default of package yaml), unquoted words like
# y, n, yes, no, on and off are read as logical values, which would turn item names like 'n' or 'y' into TRUE/FALSE.
yaml12.bool = function(x){
  if(tolower(x) == 'true'){TRUE} else if (tolower(x) == 'false'){FALSE} else {x}
}

# YAML sequences: a sequence of two or more single values becomes a vector, like [a, b] or [4, 6.4] (by default, values
# of different types, like integer 4 and double 6.4, would make a list). A sequence of one value stays a list: by
# default [a] would be read as "a", so a layout row of columns [[a], [b]] would become the row of items [a, b].
yaml.seq = function(x){
  if(length(x) > 1 && all(vapply(x, function(v) is.atomic(v) & length(v) == 1, logical(1)))){simplifyValue(x)} else {x}
}

# Reads a dashboard configuration file (.yml, .yaml or .json) and returns it as a list.
# In YAML files, values tagged !expr are R expressions evaluated when the file is read, for example:
#   choices: !expr names(mtcars)
readDashboardConfig = function(path){
  verify(path, 'character', lengths = 1, null_allowed = F, varname = 'path')
  assert(file.exists(path), paste0("File '", path, "' does not exist!"), err_src = 'readDashboardConfig')
  ext = tolower(tools::file_ext(path))
  switch(ext,
         'yml'  = , 'yaml' = {
           support('yaml')
           yaml::read_yaml(path, eval.expr = TRUE, handlers = list('bool#yes' = yaml12.bool, 'bool#no' = yaml12.bool, seq = yaml.seq))},
         'json' = {
           support('jsonlite')
           jsonlite::fromJSON(path, simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)},
         stop("Dashboard configurations must be YAML (.yml, .yaml) or JSON (.json) files!"))
}

# YAML and JSON readers return a list for a sequence of values of different types, like [4, 6.4] (integer and double).
# Unnamed lists of single numbers, logicals or texts are converted to vectors (lists of lists, like layouts, are kept).
simplifyValue = function(x){
  if(!inherits(x, 'list') || length(x) == 0 || !is.null(names(x))){return(x)}
  single = vapply(x, function(v) is.atomic(v) & length(v) == 1, logical(1))
  if(!all(single)){return(x)}
  types = unique(vapply(x, function(v) chif(is.numeric(v), 'numeric', class(v)[1]), character(1)))
  chif(length(types) == 1, unlist(x), x)
}

# Converts the fields of a configuration (read from YAML or JSON) to the classes of the DASHBOARD fields
dashboardFields = function(config){
  verify(config, 'list', names_domain = names(dashboard.config.fields), varname = 'config')
  fields = list()
  for (f in names(config)){fields[[dashboard.config.fields[[f]]]] = config[[f]]}

  # A single item name or a list of item names:
  if(!is.null(fields$king.layout)){fields$king.layout = as.list(unlist(fields$king.layout))}
  # Code can be a single text or a list of texts:
  for (f in c('prescript', 'observers')){
    if(!is.null(fields[[f]])){fields[[f]] = as.character(unlist(fields[[f]]))}
  }
  if(!is.null(fields$prescript)){fields$prescript = paste(fields$prescript, collapse = '\n')}
  if(!is.null(fields$messages)){fields$messages = unlist(fields$messages)}
  for (f in c('objects', 'values')){
    for (o in names(fields[[f]])){
      if(!is.null(fields[[f]][[o]])){fields[[f]][[o]] = simplifyValue(fields[[f]][[o]])}
    }
  }
  # Item settings (and cloth settings) given as YAML/JSON sequences:
  for (i in names(fields$items)){
    item = fields$items[[i]]
    for (f in setdiff(names(item), 'layout')){item[[f]] = simplifyValue(item[[f]])}
    if(inherits(item$cloth, 'list')){for (f in names(item$cloth)){item$cloth[[f]] = simplifyValue(item$cloth[[f]])}}
    fields$items[[i]] = item
  }
  # The login table can be given as a list: username -> password (or username -> list(password = ...))
  if(inherits(fields$loginTable, 'list')){
    users = names(fields$loginTable)
    fields$loginTable = data.frame(password = vapply(fields$loginTable, function(u) as.character(unlist(u))[1], character(1)),
                                   row.names = users, stringsAsFactors = F)
  }
  return(fields)
}

#' Build a dashboard from a configuration
#'
#' Creates a \code{DASHBOARD} object from a configuration given as a YAML or JSON file, or as an R list.
#' A configuration has these fields (only \code{items} and \code{layout} are required):
#' \describe{
#'   \item{name}{Name of the dashboard.}
#'   \item{items}{Named list of the dashboard items. Each item has a \code{type} (an input, output or container type)
#'   and the settings of that type. Output items have a \code{service}: R code returning the content of the output.
#'   Input items can have a \code{service}: R code run when the input changes.}
#'   \item{layout}{Name(s) of the item(s) making the page, usually a single page container like
#'   \code{fluidPage}, \code{dashboardPage} or \code{navbarPage}.}
#'   \item{objects}{Named list of objects copied into \code{session$userData} of each session.}
#'   \item{values}{Named list of initial values of the reactive list \code{sync} of each session.}
#'   \item{prescript}{R code run at the start of each session (for example to define reactive expressions).}
#'   \item{observers}{R code (one text per observer) run in \code{observe()} in each session.}
#'   \item{settings, messages, loginTable}{Settings, messages and users of dashboards with a login item.}
#' }
#' The \code{type} of an item (or of its \code{cloth}) can also be any function of any package, written as
#' \code{package::function}, like \code{shinyWidgets::pickerInput} or \code{shinydashboard::box}. The item fields
#' are then passed to that function as they are, with the argument names of the package documentation.
#' Functions with argument \code{inputId} make inputs and functions with argument \code{outputId} make outputs
#' (the item name is passed as the id); all other functions are containers and get the items of their
#' \code{layout} as unnamed arguments. A few fields are used by rvis and not passed to the function:
#' \code{args} (unnamed arguments passed first, like the text of \code{shiny::h3}), \code{service},
#' \code{layout}, \code{cloth}, \code{role} (\code{input}, \code{output} or \code{container}, if the detected
#' role is wrong), \code{render} (render function of an output, by default the package's \code{renderFoo} for
#' output \code{fooOutput}), \code{render.args} (arguments of the render function), \code{update} (function
#' used by \code{updateItem()}, by default the package's \code{updateFoo} for input \code{foo}) and the
#' tooltip, popover and tutor fields. See \code{rvisDashboardExample("generic-types")}.
#'
#' In YAML files, values tagged \code{!expr} are R expressions evaluated when the file is read
#' (for example \code{choices: !expr names(mtcars)}). Only \code{true} and \code{false} are read as
#' logical values (words like \code{yes}, \code{no}, \code{y} or \code{n} stay text).
#'
#' @param config Path of a YAML (\code{.yml}, \code{.yaml}) or JSON (\code{.json}) file, or a list.
#' @param ... Configuration fields added to (or replacing those of) \code{config}, for example
#'   \code{objects = list(data = mydata)}.
#' @return A \code{DASHBOARD} object. Run it with \code{$run()} or get the shiny app with \code{$app()}.
#' @examples
#' \dontrun{
#' dash = rvisDashboard(rvisDashboardExample("hello"))
#' dash$run()
#'
#' # from a list:
#' dash = rvisDashboard(list(
#'   layout = "page",
#'   items  = list(
#'     page = list(type = "fluidPage", title = "Hello", layout = list("n", "hist")),
#'     n    = list(type = "sliderInput", title = "Observations", min = 10, max = 500, value = 100),
#'     hist = list(type = "plotOutput", service = "hist(rnorm(input$n))"))))
#' shiny::runApp(dash$app())
#' }
#' @export
rvisDashboard = function(config, ...){
  if(inherits(config, 'character')){config = readDashboardConfig(config)}
  verify(config, 'list', varname = 'config')
  extra = list(...)
  for (f in names(extra)){config[[f]] = extra[[f]]}
  do.call(methods::new, c(list('DASHBOARD'), dashboardFields(config)))
}

#' Run a dashboard
#'
#' Builds a dashboard from a configuration with \code{\link{rvisDashboard}()} and runs it.
#'
#' @param config Path of a YAML or JSON file, a list, or a \code{DASHBOARD} object.
#' @param ... Arguments passed to \code{shiny::runApp()}, like \code{port} or \code{launch.browser}.
#' @export
runDashboard = function(config, ...){
  dash = chif(inherits(config, 'DASHBOARD'), config, rvisDashboard(config))
  shiny::runApp(dash$app(), ...)
}

#' Example dashboards
#'
#' Lists the example dashboard configurations that come with rvis, or returns the path of one of them.
#'
#' @param name Name of an example (file name without extension). If \code{NULL}, the examples are listed.
#' @return A data frame of the examples (name, group, title and file) if \code{name} is \code{NULL},
#'   otherwise the path of the example file.
#' @examples
#' rvisDashboardExample()
#' \dontrun{
#' runDashboard(rvisDashboardExample("faithful"))
#' }
#' @export
rvisDashboardExample = function(name = NULL){
  root  = system.file('dashboards', package = 'rvis')
  files = list.files(root, pattern = '[.](yml|yaml|json)$', recursive = TRUE, full.names = TRUE)
  names(files) = tools::file_path_sans_ext(basename(files))
  if(is.null(name)){
    titles = vapply(files, function(f){
      line = grep('^name:', readLines(f, n = 20, warn = FALSE), value = TRUE)
      chif(length(line) == 0, '', gsub("^name:\\s*|[\"']", '', line[1]))
    }, character(1))
    return(data.frame(name = names(files), group = basename(dirname(files)), title = titles, file = files,
                      row.names = NULL, stringsAsFactors = FALSE))
  }
  verify(name, 'character', lengths = 1, domain = names(files), varname = 'name')
  files[[name]]
}
