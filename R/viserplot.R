# viserplot.R ----------------------------------------------------------------
# Header
# Filename:       viserplot.R
# Description:    Contains the major plotting function for viser: viserplot
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     13 July 2017
# Last Revision:  13 July 2017
# Version:        0.0.1
#

# Version History:

# Version   Date               Action
# ------------------------------------
# 0.0.1     13 July 2017       Initial issue


#' @export
viserPlot = function(type, plotter, ...){
  parse(text = paste0(plotter, '.', type, "(...)")) %>% eval
}
