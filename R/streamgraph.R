# streamgraph.R ----------------------------------------------------------------



# Header
# Filename:       streamgraph.R
# Description:    Contains functions for plotting various charts from js package 'streamgraph' using standrad inputs.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     25 September 2018
# Last Revision:  25 September 2018
# Version:        0.0.1
#

# Version History:

# Version   Date                 Action
# ----------------------------------
# 0.0.1     25 September 2018    Initial issue


# Does not work with POSIXct, don't try!
streamgraph.tsarea.defset = defset %>% list.edit(
  dimclass   = list(
    x       = c('Date', 'integer', 'numeric'),
    y       = 'numeric',
    group   = c('factor', 'character', 'integer')),
  multiples  = c('y'),
  essentials = c('x', 'y')
)

streamgraph.applyConfig = function(chart, config){
  return(chart)
}


# ' Create a new streamgraph
# '
# ' \code{streamgraph()} initializes the streamgraph html widget
# ' and takes a data frame in "long" format with columns for the
# ' category (by default, it looks for \code{key}) and its associated
# ' \code{date}  and \code{value}. You can supply the names for those
# ' columns if they aren't named as such in your data.\cr
# ' \cr
# ' By default, interactivity is on, but you can disable that by setting
# ' the \code{interactive} parameter to \code{FALSE}.
# '
# ' @param data data frame
# ' @param key bare or quoted name of the category column (defaults to \code{key})
# ' @param value bare or quoted name of the value column (defaults to \code{value})
# ' @param date bare or quoted name of the date column (defaults to \code{date})
# ' @param width Width in pixels (optional, defaults to automatic sizing)
# ' @param height Height in pixels (optional, defaults to automatic sizing)
# ' @param offset see d3's \href{https://github.com/mbostock/d3/wiki/Stack-Layout#offset}{offset layout} for more details.
# '        The default is probably fine for most uses but can be one of \code{silhouette} (default),
# '        \code{wiggle}, \code{expand} or \code{zero}
# ' @param interpolate see d3's \href{https://github.com/mbostock/d3/wiki/SVG-Shapes#area_interpolate}{area interpolation} for more details.
# '        The default is probably fine fore most uses, but can be one of \code{cardinal} (default),
# '        \code{linear}, \code{step}, \code{step-before}, \code{step-after}, \code{basis}, \code{basis-open},
# '        \code{cardinal-open}, \code{monotone}
# ' @param interactive set to \code{FALSE} if you do not want an interactive streamgraph
# ' @param scale axis scale (\code{date} [default] or \code{continuous})
# ' @param top top margin (default should be fine, this allows for fine-tuning plot space)
# ' @param right right margin (default should be fine, this allows for fine-tuning plot space)
# ' @param bottom bottom margin (default should be fine, this allows for fine-tuning plot space)
# ' @param left left margin (default should be fine, this allows for fine-tuning plot space)
# ' @import htmlwidgets htmltools
# ' @importFrom tidyr expand
# ' @return streamgraph object
# ' @export
# ' @examples \dontrun{
# ' library(dplyr)
# ' library(streamgraph)
# ' ggplot2movies::movies %>%
# ' select(year, Action, Anicolastion, Comedy, Drama, Documentary, Romance, Short) %>%
# '   tidyr::gather(genre, value, -year) %>%
# '   group_by(year, genre) %>%
# '   tally(wt=value) %>%
# '   ungroup %>%
# '   mutate(year=as.Date(sprintf("%d-01-01", year))) -> dat
# '
# ' streamgraph(dat, "genre", "n", "year")
# ' }
stream <- function(data, key, value, date, width=NULL, height=NULL,
                   offset = c("silhouette", "wiggle", "expand", "zero"),
                   interpolate=c("cardinal", "linear", "step", "step-before", "step-after", "basis", "basis-open", "cardinal-open", "monotone"),
                   interactive=TRUE, scale = c("date", "continuous"),
                   top=20, right=40, bottom=30, left=50, config = config) {

  offset      = match.arg(offset)
  interpolate = match.arg(interpolate)
  scale       = match.arg(scale)

  data <- data.frame(data)
  data <- data[, c(key, value, date)]
  colnames(data) <- c("key", "value", "date")

  xti <- config$xAxis.tick.interval %>% verify(c('integer', 'numeric'), lengths = 1, default = 1)

  if (scale=="date") {

    xtu <- config$xAxis.tick.unit %>% verify('character', lengths = 1, default = "day")
    xtf <- config$xAxis.tick.format %>% verify('character', lengths = 1, default = "%Y-%m-%d")
    # date format
    # if (all(class(data$date) %in% c("numeric", "character", "integer"))) {
    #   if (all(nchar(as.character(data$date)) == 4)) {
    #     data %>%
    #       mutate(date=sprintf("%04d-01-01", as.numeric(date))) -> data
    #     xtu <- 'Year'
    #     xtf <- "%Y"
    #   }
    # }
  } else {
    xtu <- NULL
    xtf <- ",.0f"
  }

  config$aggregator %<>% verify('function', default = sum)
  if(!inherits(data$date, 'numeric')){data %<>% group_by(key, date) %>% summarise(value = do.call(config$aggregator, list(value, na.rm = T)))}
  # call config aggregator function

  params = list(
    data=data,
    markers=NULL,
    annotations=NULL,
    offset=offset,
    interactive=interactive,
    interpolate=interpolate,
    palette="Spectral",
    text="black",
    tooltip="black",
    x_tick_interval=xti,
    x_tick_units=xtu,
    x_tick_format=xtf,
    y_tick_count=5,
    y_tick_format=",g",
    top=top,
    right=right,
    bottom=bottom,
    left=left,
    legend=FALSE,
    legend_label="",
    fill="brewer",
    label_col="black",
    x_scale=scale
  )

  htmlwidgets::createWidget(
    name = 'streamgraph',
    x = params,
    width = width,
    height = height,
    package = 'streamgraph'
  )

}

# ' Add a title to the streamgraph
# '
# ' @param sg streamgraph object
# ' @param title title
# ' @return THIS DOES NOT RETURN AN \code{htmlwidget}!! It returns a \code{shiny.tag}
# '         class HTML (the widget is wrapped in a \code{<div>}). It should be the LAST
# '         call in a magrittr pipe chain or called to wrap a streamgraph object for
# '         output
# ' @export
sg_title <- function(sg, title="") {

  div(style="margin:auto;text-align:center", strong(title), br(), sg)

}

streamgraph_html <- function(id, style, class, width, height, ...) {
  list(tags$div(id = id, class = class, style = style),
       tags$div(id = sprintf("%s-legend", id), style=sprintf("width:%s", width), class = sprintf("%s-legend", class),
                HTML(sprintf("<center><label style='padding-right:5px' for='%s-select'></label><select id='%s-select' style='visibility:hidden;'></select></center>", id, id))))
}


streamgraph.tsarea = function(obj, x = NULL, y = NULL, group = NULL, config = NULL, ...){
  assert(require(streamgraph), "Package streamgraph is not installed!", err_src = match.call()[[1]])

  config = streamgraph.tsarea.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
    verifyConfig(plotter = 'streamgraph')

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, group = group)
  L = a$labels
  A = a$aesthetics %>% list.remove('shape')

  obj %<>% prepare4Plot(A, config)

  configscale = chif(inherits(obj[, L$x], 'Date'), 'date', 'continuous')

  if(is.null(L$group)){
    if(length(L$y) > 1){
      sg = obj %>% reshape2::melt(id.vars = L$x, measure.vars = L$y) %>%
        stream(date = L$x, value = 'value', key = 'variable', scale = configscale, config = config)
    } else {
      sg = obj %>% stream(value = L$y, date = L$x, scale = configscale, config = config)
    }
  } else {
    assert(length(L$x) == 1 & length(L$y) == 1, 'You can define series by either grouping the values or selecting multiple columns!')
    sg = stream(data = obj, date = L$x, value = L$y, key = L$group, scale = configscale)
  }

  return(sg)

  # if(obj[, L$x] %>% duplicated %>% sum > 0 & !is.null(config$aggregator)){
  #   # if(inherits(config$aggregator, 'function')){config$aggregator = as.character(substitute(config$aggregator))}
  #   config$aggregator %>% verify('character')
  #   obj %<>% dplyr::group_by_(L$x)
  #   scr = "obj %>% dplyr::summarise("
  #   N   = length(L$y)
  #   for (i in sequence(N)){
  #     scr %<>% paste0("`", L$y[i], "` = ", config$aggregator, "(`", L$y[i], "`)", chif(i < N, ", ", ")"))
  #   }
  #   obj = parse(text = scr) %>% eval %>% as.data.frame
  # }
  #
  # if (!is.null(L$x)){
  #   if (inherits(obj[,L$x], 'Date')){ct = obj %>% streamgraph(x = L$x)} else {
  #     ct = obj %>% streamgraph %>% xAxis(type = 'category', categories = obj[, L$x] %>% as.character)
  #   }
  # } else {ct = obj %>% streamgraph}
  # if(!is.null(L$shape)){names(L$shape) <- L$y}
  # if(is.null(L$shape)){
  #   if(!is.null(config$shape)){
  #     L$shape = config$shape
  #   } else {L$shape = 'line'}
  # }
  #
  # L$shape %<>% vect.extend(length(L$y))
  # if(is.empty(config$stack.groups)){config$stack.groups = L$y}
  # options(warn = -1)
  # ct %<>% streamgraph_mixedGeom(type = most.common(L$shape), types = L$shape %>% as.list, stacked = chif(config$stack.enabled, config$stack.groups, NULL))
  # options(warn = 1)
  #
  # return(ct %>% streamgraph.applyConfig(config))
}

# streamgraph.tsline = function(obj, x = NULL, y = NULL, config = NULL, ...){
#   config = streamgraph.tsline.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
#   obj %>% streamgraph.combo(x = x, y = y, shape = 'line', config = config, ...)
# }
#
# streamgraph.tsarea = function(obj, x = NULL, y = NULL, config = NULL, ...){
#   config = streamgraph.tsline.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
#     verifyConfig(plotter = 'streamgraph')
#
#   obj %>% streamgraph.combo(x = x, y = y, shape = 'area', config = config, ...)
# }
#
# streamgraph.area = function(obj, x = NULL, y = NULL, config = NULL, ...){
#   config = streamgraph.bar.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
#     verifyConfig(plotter = 'streamgraph')
#
#   obj %>% streamgraph.combo(x = x, y = y, shape = 'area', config = config, ...)
# }

# streamgraph.line = function(obj, x = NULL, y = NULL, config = NULL, ...){
#   config = streamgraph.bar.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
#     verifyConfig(plotter = 'streamgraph')
#
#   obj %>% streamgraph.combo(x = x, y = y, shape = 'line', config = config, ...)
# }

