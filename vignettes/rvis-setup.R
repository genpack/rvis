# Setup shared by the chart vignettes of rvis (sourced by their hidden setup chunk).

knitr::opts_chunk$set(collapse = TRUE, comment = "#>", message = FALSE, warning = FALSE)
suppressPackageStartupMessages(library(rvis))

# Plotting packages are optional dependencies of rvis: an example is evaluated only when the
# package(s) its wrapper needs are installed, otherwise only its code is shown.
has_plotter <- function(...) all(vapply(c(...), requireNamespace, logical(1), quietly = TRUE))

# Charts of different plotters bring different versions of the d3 javascript library
# (c3, billboarder and networkD3 use d3 version 4 or 5, ggvis d3 3.5.2, d3plus d3 3.5.15).
# Only one global d3 can exist on a page, so the charts of these plotters are shown in
# their own frame, which keeps all charts of a page working.
show_in_frame <- function(x, ...) {
  f <- tempfile(fileext = ".html")
  if (inherits(x, "ggvis")) {
    # ggvis is not an htmlwidget: its page is saved with its libraries, which are then inlined
    spec <- ggvis:::as.vega(x, dynamic = FALSE)
    htmltools::save_html(ggvis:::ggvisOutputElements(spec = spec, shiny = FALSE), f, libdir = "lib")
    old <- setwd(dirname(f)); on.exit(setwd(old))
    writeLines(rvis:::rCharts.inlineAssets(paste(readLines(f, warn = FALSE), collapse = "\n")), f)
  } else {
    htmlwidgets::saveWidget(x, f, selfcontained = TRUE)
  }
  height <- if (is.numeric(x$height)) x$height else 420
  html <- paste(readLines(f, warn = FALSE), collapse = "\n")
  html <- gsub("&", "&amp;", html, fixed = TRUE)
  html <- gsub("\"", "&quot;", html, fixed = TRUE)
  knitr::asis_output(sprintf("<iframe srcdoc=\"%s\" style=\"width:100%%; height:%dpx; border:none;\"></iframe>",
                             html, as.integer(height + 30)))
}

# ggvis registers its own knit_print method when it is loaded, so it is loaded first to be overridden
if (has_plotter("ggvis")) loadNamespace("ggvis")
for (cls in c("c3", "billboarder", "ggvis", "sankeyNetwork", "forceNetwork", "streamgraph"))
  registerS3method("knit_print", cls, show_in_frame, envir = asNamespace("knitr"))
