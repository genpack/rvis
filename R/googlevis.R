# googlevis.R ----------------------------------------------------------------

# Header
# Filename:       googlevis.R
# Description:    Contains functions for plotting various googleVis charts using standrad inputs.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     06 October 2016
# Last Revision:  23 November 2026
# Version:        1.3.6
#

# Version History:

# Version   Date               Action
# -----------------------------------
# 1.0.0     06 October 2016    Initial issue transferred from niraPlotter.R
# 1.1.0     25 November 2016   function googleVis.lineChart() modified: Turned to standard horizontal barChart and lineChart inputs: currently only takes x, y where x is nominal and y is a set of numeric figures
# 1.2.0     29 November 2016   Sankey chart added: Function googleVis.sankeyChart()
# 1.2.1     01 March 2017      Function googleVis.lineChart() renamed to googleVis.line() and modified: Calls verifyPlotInputs() in visgen.R to verify inputs.
# 1.2.2     01 March 2017      Function googleVis.barChart() renamed to googleVis.bar() and modified: Calls verifyPlotInputs() in visgen.R to verify inputs.
# 1.2.3     01 March 2017      Function googleVis.gauge() modified: Calls verifyPlotInputs() in visgen.R to verify inputs, also supports multiple columns for argument 'theta'
# 1.3.0     02 April 2017      Fundamental changes to the functions: Using standard visgen.R fucnctions prepare4Plot() and prepareAesthetics()
# 1.3.2     19 June 2018       Function googleVis.gauge() added.
# 1.3.4     20 June 2018       Function googleVis.bar() and googleVis.line() modified: y2 is now a new diemnsion
# 1.3.5     23 November 2026   Bug fixes: googleVis.options() used undefined 'L' instead of its arguments, googleVis.line() used
#                              undefined 'yy' (both functions always failed). googleVis.bar() uses isHorizontal(). Old commented code removed.
# 1.3.6     23 November 2026   knit_print.gvis() added: googleVis charts are displayed in knitr/R Markdown documents, each in its own
#                              frame (Google Charts loads its own d3 version which broke other widgets of the page).

#' @include visgen.R

googleVis.line.defset = defset %<==>% list(
  # Valid classes for all dimensions
  dimclass   = list(
    x     = 'character',
    y     = 'numeric',
    y2    = 'numeric'),
  multiples  = c('y', 'y2'),
  essentials = c('x', 'y')
)

googleVis.bar.defset = defset %<==>% list(
  # Valid classes for all dimensions
  dimclass   = list(
    x     = c('character', 'numeric', 'factor', 'integer'),
    y     = c('numeric', 'character', 'factor', 'integer')),
  multiples  = c('x', 'y'),
  essentials = c('x', 'y')
)

googleVis.gauge.defset = defset %<==>% list(
  # Valid classes for all dimensions
  dimclass   = list(
    label = 'character',
    theta = c('numeric', 'integer')),
  multiples  = c(),
  essentials = c('label', 'theta')
)

googleVis.sankey.defset = defset %<==>% list(
  dimclass = list(
    key          = 'character',
    source       = 'character',
    target       = 'character',
    linkWidth    = 'numeric'),
  multiples  = c(),
  essentials = c('key', 'source', 'target')
)

# Builds googleVis chart options specifying the axis of each series (Ly: series on the left axis, Ly2: series on the right axis)
googleVis.options = function(config, Ly, Ly2){
  opt = list()

  # Specify y axis sides and labels:
  yy = c(Ly, Ly2)
  srs = '['
  vax = '['
  for(i in seq_along(yy)){
    srs %<>% paste0(chif(srs == '[', "", ","), "{targetAxisIndex:", (yy[i] %in% Ly2) %>% as.integer, "}")
    if((yy[i] %in% Ly) & (!is.null(config$yAxis.label))){
      vax %<>% paste0(chif(vax == '[', "", ","), "{title:'", config$yAxis.label, "'}")
    }
    if((yy[i] %in% Ly2) & (!is.null(config$y2Axis.label))){
      vax %<>% paste0(chif(vax == '[', "", ","), "{title:'", config$y2Axis.label, "'}")
    }
  }
  opt$series = srs %>% paste0("]")
  opt$vAxes  = vax %>% paste0("]")
  return(opt)
}

googleVis.line = function(obj, x = NULL, y = NULL, y2 = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(googleVis), "Package googleVis is not installed!", err_src = match.call()[[1]])
  config = googleVis.line.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, y2 = y2)
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config = config)

  gvisLineChart(obj, xvar = L$x, yvar = c(L$y, L$y2), options = googleVis.options(config, L$y, L$y2), ...)
}

googleVis.area = function(obj, x = NULL, y = NULL, y2 = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(googleVis), "Package googleVis is not installed!", err_src = match.call()[[1]])
  config = googleVis.line.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, y2 = y2, extend = c('y', 'y2'))
  L = a$labels
  A = a$aesthetics %>% list.remove(gndcd(138,166,118,20,94))

  obj %<>% prepare4Plot(A, config = config)

  gvisAreaChart(obj, xvar = L$x, yvar = L$y, options = googleVis.options(config, L$y, L$y2), ...)
}

googleVis.bar = function(obj, x = NULL, y = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(googleVis), "Package googleVis is not installed!", err_src = match.call()[[1]])
  config = googleVis.bar.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, extend = c())
  L = a$labels
  A = a$aesthetics %>% list.remove(gndcd(138,166,118,20,94))

  obj %<>% prepare4Plot(A, config = config)

  if(isHorizontal(obj, L$x, L$y)){
    g = gvisBarChart(obj, yvar = L$x, xvar = L$y, options = list(), ...)}
  else {
    g = gvisColumnChart(obj, xvar = L$x, yvar = L$y, options = list(), ...)
  }
  return(g)
}


googleVis.gauge = function(obj = data.frame(), theta = NULL, label = NULL, config = NULL, ...){
  # Verifications:
  assert(require(googleVis), "Package googleVis is not installed!", err_src = match.call()[[1]])
  config = googleVis.gauge.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  # Preparing Aesthetics:
  a = prepareAesthetics(theta = theta, label = label)
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config = config)

  if(is.empty(obj)){return(NULL)}

  # config$thetaAxis.zone: list of three zones (green, yellow, red), each a list with elements min and max
  obj[, c(L$label, L$theta)] %>%
    gvisGauge(
      options = list(
        min = config$theta.min,
        max = config$theta.max,
        greenFrom    = config$thetaAxis.zone[[1]]$min,
        greenTo      = config$thetaAxis.zone[[1]]$max,
        yellowFrom   = config$thetaAxis.zone[[2]]$min,
        yellowTo     = config$thetaAxis.zone[[2]]$max,
        redFrom      = config$thetaAxis.zone[[3]]$min,
        redTo        = config$thetaAxis.zone[[3]]$max, ...)
    )
}

# It is a sankeytree not a sankey!!!!
googleVis.sankey = function(obj, key = NULL, source = NULL, target = NULL, linkWidth = NULL, config = NULL){
  obj %>% verify('list', lengths = 2, names_identical = c('nodes', 'links'), varname = 'obj', null_allowed = F, err_src = 'googleVis.sankey')
  obj$nodes %>% verify('data.frame', varname = 'obj$nodes', null_allowed = F, err_src = 'googleVis.sankey')
  obj$links %>% verify('data.frame', varname = 'obj$links', null_allowed = F, err_src = 'googleVis.sankey')

  assert(require(googleVis), "Package googleVis is not installed!", err_src = match.call()[[1]])

  config = googleVis.sankey.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
    verifyConfig(plotter = 'googleVis')

  # Preparing Aesthetics:
  a = prepareAesthetics(key = key, source = source, target = target, linkWidth = linkWidth)
  L = a$labels
  A = a$aesthetics

  obj$nodes %<>% prepare4Plot(A %>% list.extract('key'), config)
  obj$nodes = obj$nodes[!duplicated(obj$nodes[[L$key]]), , drop = FALSE]
  obj$links %<>% prepare4Plot(A %>% list.extract('source', 'target', 'linkWidth'), config)

  assert(obj$links[, L$source] %<% obj$nodes[, L$key], "Value in column '" %++% L$source %++% "' must be a subset of node IDs'", err_src = 'googleVis.sankey')
  assert(obj$links[, L$target] %<% obj$nodes[, L$key], "Value in column '" %++% L$target %++% "' must be a subset of node IDs'", err_src = 'googleVis.sankey')

  obj$links %>% gvisSankey(from = L$source, to = L$target, weight = L$linkWidth)
}

# Displays googleVis charts in knitr/R Markdown documents (they were printed as raw html text unless the chunk
# had results = 'asis'). The chart is drawn by Google Charts, so viewing it needs an internet connection.
# Each chart is shown as a complete page inside its own frame because:
#  - Google Charts loads its own libraries (for example d3 version 5 for sankey charts) into the page, which breaks
#    other widgets of the document that need another d3 version (d3plus, ggvis, ...);
#  - in self-contained documents pandoc would inline a copy of the Google loader that never calls the chart callback.
#' @exportS3Method knitr::knit_print
knit_print.gvis = function(x, ...){
  page = paste(c(x$html$header, x$html$chart, x$html$footer), collapse = '\n')
  page = gsub('&', '&amp;', page, fixed = TRUE)
  page = gsub('"', '&quot;', page, fixed = TRUE)
  height = suppressWarnings(as.numeric(sub('.*options\\["height"\\] = ([0-9]+);.*', '\\1', paste(x$html$chart[['jsDrawChart']], collapse = ' '))))
  if (is.na(height)){height = 400}
  knitr::asis_output(sprintf('<iframe srcdoc="%s" style="width:100%%; height:%dpx; border:none;"></iframe>', page, as.integer(height + 40)))
}
