# streamgraph.R ----------------------------------------------------------------



# Header
# Filename:       streamgraph.R
# Description:    Contains functions for plotting various charts from js package 'streamgraph' using standrad inputs.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     25 September 2018
# Last Revision:  23 November 2026
# Version:        0.0.2
#

# Version History:

# Version   Date                 Action
# ----------------------------------
# 0.0.1     25 September 2018    Initial issue
# 0.0.2     23 November 2026     Bug fixes: stream() had a self-referencing default 'config = config' (failed when config was not
#                                passed, i.e. for grouped tables); a single series without group failed (no key column);
#                                dplyr verbs namespaced. Old commented code removed.

#' @include visgen.R


# Does not work with POSIXct, don't try!
streamgraph.tsarea.defset = defset %<==>% list(
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
                   top=20, right=40, bottom=30, left=50, config = list()) {

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
  } else {
    xtu <- NULL
    xtf <- ",.0f"
  }

  # Values of duplicated (key, date) pairs are aggregated by config$aggregator (default: sum)
  config$aggregator %<>% verify('function', default = sum)
  if(!inherits(data$date, 'numeric')){data %<>% dplyr::group_by(key, date) %>% dplyr::summarise(value = do.call(config$aggregator, list(value, na.rm = T)))}

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
    # Each y column is a series (also for a single series: stream() always needs a key column)
    sg = obj %>% reshape2::melt(id.vars = L$x, measure.vars = L$y) %>%
      stream(date = L$x, value = 'value', key = 'variable', scale = configscale, config = config)
  } else {
    assert(length(L$x) == 1 & length(L$y) == 1, 'You can define series by either grouping the values or selecting multiple columns!')
    sg = stream(data = obj, date = L$x, value = L$y, key = L$group, scale = configscale, config = config)
  }

  return(sg)
}
