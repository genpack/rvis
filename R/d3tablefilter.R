# d3tablefilter.R ----------------------------------------------------------------


# Header
# Filename:       d3tablefilter.R
# Description:    Contains functions for plotting table charts from D3TableFilter package using standrad inputs.
#                 D3TableFilter is the published version of the TFD3 widget embedded in rvis, so all helper functions are shared with tfd3.R
# Author:         Nicolas Berta
# Email :         nima.ramezani@gmail.com
# Start Date:     02 February 2023
# Last Revision:  23 November 2026
# Version:        0.0.2
#

# Version History:

# Version   Date                Action
# ----------------------------------
# 0.0.1     02 February 2023    Initial issue
# 0.0.2     23 November 2026    Copies of TFD3 functions replaced by aliases; D3TableFilter.table() uses TFD3.prepare() (tfd3.R).
#                               Bug fix: radio button columns were only passed when checkbox columns existed.

#' @include visgen.R
#' @include tfd3.R

D3TableFilter.table.defset    = TFD3.table.defset
D3TableFilter.addColumnTypes  = TFD3.addColumnTypes
D3TableFilter.tableprops      = TFD3.tableprops
D3TableFilter.config.verify   = TFD3.config.verify
D3TableFilter.footData        = TFD3.footData
D3TableFilter.rowStyles       = TFD3.rowStyles
D3TableFilter.edit            = TFD3.edit
D3TableFilter.lastEdits.empty = TFD3.lastEdits.empty
D3TableFilter.initialFilters  = TFD3.initialFilters
D3TableFilter.applyFilterstr  = TFD3.applyFilterstr
D3TableFilter.filteredRows    = TFD3.filteredRows
D3TableFilter.colNames        = TFD3.colNames
D3TableFilter.bgColScales     = TFD3.bgColScales

D3TableFilter.table = function(obj, label = NULL, color = NULL, shape = NULL, config = NULL, ...){
  prep = TFD3.prepare(obj, label = label, color = color, shape = shape, config = config)
  if (is.null(prep)){return(NULL)}
  do.call(D3TableFilter::d3tf, c(list(prep$table), prep$args, list(...)))
}
