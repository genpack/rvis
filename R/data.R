# data.R ----------------------------------------------------------------
# Documentation of the data sets of rvis (data/properties.RData and data/plotters.RData).

#' Configuration settings of the plotters
#'
#' The settings each plotter accepts in argument \code{config} of \code{\link{rvisPlot}()}: their
#' class, default value and valid values. The wrappers check \code{config} against this table and
#' fill missing settings with their defaults. The same table, with the meaning of each setting, is
#' shown in the \href{../articles/configuration.html}{configuration reference}.
#'
#' @format A data frame with one row per plotter and setting:
#' \describe{
#'   \item{plotters}{Name of the plotter (the \code{plotter} argument of \code{rvisPlot()}).}
#'   \item{Property}{Name of the setting, like \code{title} or \code{legend.position}.}
#'   \item{Class}{Valid classes of its value, separated by spaces.}
#'   \item{Default}{Default value (empty: none).}
#'   \item{Domain}{Valid values, separated by spaces (empty: any value). For numbers, the minimum and maximum.}
#' }
#' @source \code{data/properties.csv} of the package.
#' @examples
#' properties[properties$plotters == "echarts", c("Property", "Default", "Domain")]
"properties"

#' Chart types of the plotters
#'
#' A table of the plotters (rows) and the chart types (columns) they draw: \code{"x"} if the plotter
#' has a wrapper for the chart type. The complete list of chart types and wrappers, with their
#' arguments, is in the \href{../articles/chart-types.html}{chart type and plotter reference}.
#'
#' @format A data frame with column \code{Plotter} and one column per chart type (\code{bar}, \code{pie},
#'   \code{scatter}, \code{line}, \code{tsline}, \code{graph}, \code{sankey}, ...).
#' @source \code{data/plotters.csv} of the package.
#' @examples
#' plotters[!is.na(plotters$graph), "Plotter"]   # plotters drawing graphs
"plotters"
