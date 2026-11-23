# rcharts.R ----------------------------------------------------------------


# Header
# Filename:       rcharts.R
# Description:    Contains functions for plotting various charts from rCharts package using standrad inputs.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     31 March 2017
# Last Revision:  23 November 2026
# Version:        1.1.6
#

# Version History:

# Version   Date                Action
# ----------------------------------
# 1.0.0     31 March 2017       Initial issue
# 1.0.1     31 March 2017       rCharts.scatter.defset added
# 1.0.2     14 April 2017       rCharts.scatter modified
# 1.1.0     12 May 2017         dimple charts separated and transferred to dimple.R
# 1.1.1     12 May 2017         nvd3 charts separated and transferred to nvd3.R
# 1.1.2     12 May 2017         xCharts charts separated and transferred to xCharts.R
# 1.1.3     12 May 2017         highcharts charts separated and transferred to highcharts.R
# 1.1.4     23 November 2026    Bug fix: rCharts.scatter() used undefined rCharts.scatter.defset and translateShape (always failed).
#                               Defset added; shapes translated by rCharts.hPlotType() (highcharts.R); colour mapped to hPlot groups.
# 1.1.5     23 November 2026    knit_print.rCharts() added: rCharts objects are embedded in knitr/R Markdown documents
#                               (failed with "object 'opts_current' not found" unless knitr was attached).
# 1.1.6     23 November 2026    knit_print.rCharts() embeds the chart libraries that come with rCharts instead of loading them from CDN
#                               (the xCharts CDN no longer exists); charts also work offline. Helper rCharts.inlineAssets() added.

#' @include visgen.R
#' @include highcharts.R

rCharts.scatter.defset = defset %<==>% list(
  # Valid classes for all dimensions
  dimclass   = list(
    x     = c('numeric', 'character'),
    y     = c('numeric', 'character'),
    size  = 'numeric',
    color = valid.classes,
    shape = 'character'),
  multiples  = c(),
  essentials = c('x', 'y'),
  colorize   = F,
  horizontal = F
)

# Scatter chart using rCharts hPlot. Points are grouped (coloured) by column 'color'.
rCharts.scatter = function(obj, x = NULL, y = NULL, size = NULL, shape = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(rCharts), "Package rCharts is not installed!", err_src = match.call()[[1]])
  config = rCharts.scatter.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, size = size, shape = shape, color = color)
  L = a$labels
  A = a$aesthetics %>% list.remove('shape') # Only x, y, color, size will be columns of the table

  obj %<>% prepare4Plot(A, config = config)

  hPlot(x = L$x, y = L$y, data = obj,
        type = rCharts.hPlotType(L$shape, config$horizontal), group = L$color, size = L$size)
}

# Replaces references to local script and stylesheet files in an html page by the content of the files
rCharts.inlineAssets = function(html){
  inline = function(html, pattern, wrap){
    refs = unique(regmatches(html, gregexpr(pattern, html))[[1]])
    for (ref in refs){
      path = sub(pattern, '\\1', ref)
      if (file.exists(path)){
        content = paste(readLines(path, warn = FALSE, encoding = 'UTF-8'), collapse = '\n')
        # plain splicing (sub() would interpret backslashes of the javascript code in the replacement)
        pos  = regexpr(ref, html, fixed = TRUE)
        html = paste0(substr(html, 1, pos - 1), sprintf(wrap, content), substr(html, pos + nchar(ref), nchar(html)))
      }
    }
    html
  }
  html = inline(html, "<script[^>]*src=['\"]([^'\"]+)['\"][^>]*>\\s*</script>", "<script>%s</script>")
  inline(html, "<link[^>]*href=['\"]([^'\"]+\\.css)['\"][^>]*>", "<style>%s</style>")
}

# Displays rCharts objects (nvd3, dimple, highcharts hPlot, xCharts charts) in knitr/R Markdown documents.
# rCharts' own show() method needs knitr to be attached and writes the chart to a separate html file.
# This method embeds the chart into the document as a self-contained frame, with the chart libraries that come
# with package rCharts (the CDN of some libraries, like xCharts, no longer exists).
#' @exportS3Method knitr::knit_print
knit_print.rCharts = function(x, ...){
  page = rCharts.inlineAssets(paste(x$render(cdn = FALSE), collapse = '\n'))
  page = gsub('&', '&amp;', page, fixed = TRUE)
  page = gsub('"', '&quot;', page, fixed = TRUE)
  height = suppressWarnings(as.numeric(x$params$height))
  if (length(height) != 1 || is.na(height)){height = 400}
  knitr::asis_output(sprintf('<iframe srcdoc="%s" class="rChart" style="width:100%%; height:%dpx; border:none;"></iframe>', page, as.integer(height + 30)))
}
