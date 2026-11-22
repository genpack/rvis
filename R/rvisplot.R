# rvisplot.R ----------------------------------------------------------------
# Header
# Filename:       rvisplot.R
# Description:    Contains the major plotting function for rvis: rvisplot
# Author:         Nima Ramezani
# Email :         nima.ramezani@gmail.com
# Start Date:     13 July 2017
# Last Revision:  13 July 2017
# Version:        0.0.1
#

# Version History:

# Version   Date               Action
# ------------------------------------
# 0.0.1     13 July 2017       Initial issue


#' Plot with an rvis wrapper
#'
#' Dispatch a chart request to a wrapper function named by combining `plotter`
#' and `type` (for example, `plotter = "amCharts"` and `type = "bar"` calls
#' `amCharts.bar()`). Wrapper functions standardize common data and aesthetic
#' arguments while retaining plotter-specific configuration.
#'
#' @param type Character scalar. Exact chart method suffix implemented by the
#'   selected wrapper.
#' @param plotter Character scalar. Exact wrapper prefix. The corresponding
#'   function must be named `paste0(plotter, ".", type)`.
#' @param ... Arguments passed to the selected wrapper, commonly including
#'   `obj`, dimension mappings such as `x` and `y`, and a named `config` list.
#'
#' @return The chart object returned by the selected wrapper, usually an
#'   htmlwidget.
#'
#' @seealso The “Chart type and plotter reference” and “Configuration reference”
#'   vignettes.
#'
#' @examples
#' \dontrun{
#' species <- aggregate(Petal.Length ~ Species, data = iris, FUN = mean)
#' rvisPlot(
#'   type = "bar", plotter = "amCharts", obj = species,
#'   x = "Species", y = "Petal.Length",
#'   config = list(title = "Mean petal length by species")
#' )
#' }
#' @export
rvisPlot = function(type, plotter, ...){
  parse(text = paste0(plotter, '.', type, "(...)")) %>% eval
}
