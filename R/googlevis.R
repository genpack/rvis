# googlevis.R ----------------------------------------------------------------

# Header
# Filename:       googlevis.R
# Description:    Contains functions for plotting various googleVis charts using standrad inputs.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     06 October 2016
# Last Revision:  20 June 2018
# Version:        1.3.4
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


googleVis.line.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    x     = 'character',
    y     = 'numeric',
    y2    = 'numeric'),
  multiples  = c('y', 'y2'),
  essentials = c('x', 'y')
)

googleVis.bar.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    x     = c('character', 'numeric', 'factor', 'integer'),
    y     = c('numeric', 'character', 'factor', 'integer')),
  multiples  = c('x', 'y'),
  essentials = c('x', 'y')
)

googleVis.gauge.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    label = 'character',
    theta = c('numeric', 'integer')),
  multiples  = c(),
  essentials = c('label', 'theta')
)

googleVis.sankey.defset = defset %>% list.edit(
  dimclass = list(
    key          = 'character',
    source       = 'character',
    target       = 'character',
    linkWidth    = 'numeric'),
  multiples  = c(),
  essentials = c('key', 'source', 'target')
)

googleVis.options = function(config, Ly, Ly2){
  opt = list()

  # Specify y axis sides and labels:
  yy = c(L$y,L$y2)
  N = length(yy)
  srs = '['
  vax = '['
  for(i in sequence(N)){
    srs %<>% paste0(chif(srs == '[', "", ","), "{targetAxisIndex:", (yy[i] %in% L$y2) %>% as.integer, "}")
    if((yy[i] %in% L$y) & (!is.null(config$yAxis.label))){
      vax %<>% paste0(chif(vax == '[', "", ","), "{title:'", config$yAxis.label, "'}")
    }
    if((yy[i] %in% L$y2) & (!is.null(config$y2Axis.label))){
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

  gvisLineChart(obj, xvar = L$x, yvar = yy, options = googleVis.options(config, L$y, L$y2), ...)
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

  hor = T
  for (i in L$x){hor = hor & inherits(obj[,i], c('numeric', 'integer'))}
  for (i in L$y){hor = hor & inherits(obj[,i], c('character', 'factor'))}

  options = list()

  if(hor){
    g = gvisBarChart(obj, yvar = L$x, xvar = L$y, options = options, ...)}
  else {
    g = gvisColumnChart(obj, xvar = L$x, yvar = L$y, options = options, ...)
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
  obj %>% verify('list', lengths = 2, names_identical = c('nodes', 'links'), varname = 'obj', null_allowed = F, err_src = 'visNetwork.graph')
  obj$nodes %>% verify('data.frame', varname = 'obj$nodes', null_allowed = F, err_src = 'googleVis.sankey')
  obj$links %>% verify('data.frame', varname = 'obj$links', null_allowed = F, err_src = 'googleVis.sankey')

  assert(require(googleVis), "Package googleVis is not installed!", err_src = match.call()[[1]])

  config = googleVis.sankey.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
    verifyConfig(plotter = 'googleVis')

  # Preparing Aesthetics:
  a = prepareAesthetics(key = key, source = source, target = target, linkWidth = linkWidth)
  L = a$labels
  A = a$aesthetics

  obj$nodes %<>% prepare4Plot(A %>% list.extract('key'), config) %>% distinct_(L$key, .keep_all = T)
  obj$links %<>% prepare4Plot(A %>% list.extract('source', 'target', 'linkWidth'), config)

  assert(obj$links[, L$source] %<% obj$nodes[, L$key], "Value in column '" %++% L$source %++% "' must be a seubset of node IDs'", err_src = 'googleVis.sankey')
  assert(obj$links[, L$target] %<% obj$nodes[, L$key], "Value in column '" %++% L$target %++% "' must be a seubset of node IDs'", err_src = 'googleVis.sankey')

  obj$links %>% gvisSankey(from = L$source, to = L$target, weight = L$linkWidth)

}



























# OLD FUNCTIONS
# gglvis.click.jscript = function(input_id){
#   "var row = chart.getSelection()[0].row + 1;" %++%
#     "Shiny.onInputChange('" %++% input_id %++% "', row)"
# }
#
#
# # Default Settings:
#
# # default.gglvis.tsline.settings = list(width="800px", height="600px", displayExactValues=TRUE, scaleType='maximized') # todo transfer to settings
# gglvis.tsline.settings = list(displayExactValues = TRUE, scaleType = 'maximized')
#
# gglvis.column.settings <- list(width = "600px", height = "600px", vAxis.minValue = 0)
#
# gglvis.sankey.settings <- list(width = 800, height = 600)
#
# gglvis.calendar.settings <- list(
#   height    = 'auto',
#   yearLabel = list(fontName = 'Times-Roman', fontSize = 32, color = '#1A8763', bold = TRUE),
#   cellSize  =  10,
#   cellColor = list(stroke ='red', strokeOpacity = 0.2),
#   focusedCellColor = list(stroke = 'red'))
#
# gglVis.gauge.settings = list(width = 400, height = 300)
#
# gglvis.motionChart.settings <- list(width="800px", height="600px", displayExactValues=TRUE, scaleType='maximized')
#
#
# # All Java function scripts:
# getMonth <- 'function(d){
# var monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
# return monthNames[d.getMonth()];
# }'
#
# getMonthLabel <- 'function(d){
# var monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
# var S    = monthNames[d - 1];
# if (S == null){return "";} else {return S;}
# }'
#
# #the x values are passed as milliseconds, turn them into a date and extract month and day
# getMonthDay <- 'function(d) {
# var monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
# date = new Date(d);
# return monthNames[date.getMonth()] + " " +date.getUTCDate(); }'
#
# # googleVis Plots:
#
# # Argument 'x' can only refer to a categorical
# # Argument 'y' must refer to one or more than one numeric columns of obj
# googleVis.line.old = function(obj, x = NULL, y = NULL, func = mean, options = list(), ...){
#   assert(require(googleVis), "Package googleVis is not installed!", err_src = match.call()[[1]])
#   obj = verifyPlotInputs(obj, x, y, package = 'googleVis', type = 'line')
#
#   if (!is.null(func)){
#     if (length(y) == 1){frm = as.formula(paste0(y,' ~ ', x))} else {frm = as.formula(paste0('cbind(', paste(y,collapse = ','), ') ~ ', x))}
#     obj = aggregate(frm, data = obj, FUN = func)
#   }
#
#   g = gvisLineChart(obj, xvar = x, yvar = y, options = options, ...)
#
#   return(g)
# }
#
# googleVis.bar.old = function(obj, x = NULL, y = NULL, func = mean, options = list(), horizontal = F, ...){
#   assert(require(googleVis), "Package googleVis is not installed!", err_src = match.call()[[1]])
#   obj = verifyPlotInputs(obj, x, y, package = 'googleVis', type = 'line')
#
#   if (!is.null(func)){
#     if (length(y) == 1){frm = as.formula(paste0(y,' ~ ', x))} else {frm = as.formula(paste0('cbind(', paste(y,collapse = ','), ') ~ ', x))}
#     obj = aggregate(frm, data = obj, FUN = func)
#   }
#   if(horizontal){
#     g = gvisBarChart(obj, xvar = x, yvar = y, options = options, ...)}
#   else {
#     g = gvisColumnChart(obj, xvar = x, yvar = y, options = options, ...)
#   }
#   return(g)
# }
#
# # package.plotType.objectClass.whatToPlot
#
# # molten table required!
# googleVis.gauge.old = function(obj, theta = NULL, label = NULL, func = mean, thetaLegend = NULL, config = NULL, ...){
#   assert(require(googleVis), "Package googleVis is not installed!", err_src = match.call()[[1]])
#   obj = verifyPlotInputs(obj, theta = theta, label = label, package = 'googleVis', type = 'gauge')
#
#   if (is.null(theta)){theta = numerics(obj)[1]}
#
#   thetaLegend = verifyThetaLegend(thetaLegend, obj, theta)
#
#
#   if(length(theta) > 1){
#     assert(is.null(label), "In gauge charts, dimension 'label' cannot be defined when multiple columns are chosen! In this case, column names, will be the labels.", match.call()[[1]])
#     obj = melt(obj[,theta])
#     label = 'variable'
#     theta = 'value'
#   }
#
#   if (!is.null(func)){
#     if (length(theta) == 1){frm = as.formula(paste0(theta,' ~ ', label))} else {frm = as.formula(paste0('cbind(', paste(theta,collapse = ','), ') ~ ', label))}
#     obj = aggregate(frm, data = obj, FUN = func)
#   }
#
#   if (thetaLegend$percentage){
#     thetaLegend$levels  = verify(thetaLegend$levels , 'numeric', varname = 'thetaLegend$levels' , lengths = 4, domain = c(0.0, 100.0), default = c(0.0, 30.0, 70.0, 100.0))
#     obj[,theta]    = 100*(obj[,theta] - thetaLegend$min)/(thetaLegend$max - thetaLegend$min)
#     thetaLegend$max     = 100.0
#     thetaLegend$min     = 0.0
#   } else {
#     thetaLegend$levels  = verify(thetaLegend$levels , 'numeric', varname = 'thetaLegend$levels' , lengths = 4, default = c(thetaLegend$min, thetaLegend$min + 0.3*(thetaLegend$max - thetaLegend$min), thetaLegend$min + 0.7*(thetaLegend$max - thetaLegend$min), thetaLegend$max))
#   }
#
#   thetaLegend$levels = sort(thetaLegend$levels)
#
#   if (is.null(config)){config = gglVis.gauge.settings}
#
#   config$min        = thetaLegend$min
#   config$max        = thetaLegend$max
#   config$redFrom    = thetaLegend$levels[1]
#   config$redTo      = thetaLegend$levels[2]
#   config$yellowFrom = thetaLegend$levels[2]
#   config$yellowTo   = thetaLegend$levels[3]
#   config$greenFrom  = thetaLegend$levels[3]
#   config$greenTo    = thetaLegend$levels[4]
#
#   g = gvisGauge(obj, options = config)
#   return(g)
# }
#
# googleVis.sankey.old = function(obj, linkSource = NULL, linkTarget = NULL, linkWidth = NULL, config = NULL, ...){
#   assert(require(googleVis), "Package googleVis is not installed!", err_src = match.call()[[1]])
#   argEdges = list(table = obj,
#                   linkSource = list(colName = linkSource, type = 'nominal'),
#                   linkTarget = list(colName = linkTarget, type = 'nominal'),
#                   linkWidth  = list(colName = linkWidth,  type = 'numeric'))
#   links    = visPrepare(argEdges)
#
#   if(is.null(config)){config = gglvis.sankey.settings}
#
#   gvisSankey(links, from = linkSource,
#              to = linkTarget, weight = linkWidth,
#              options = config, ...)
# }
#


