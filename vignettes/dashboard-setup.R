# Setup shared by the dashboard vignettes of rvis (sourced by their hidden setup chunk).

knitr::opts_chunk$set(collapse = TRUE, comment = "#>", message = FALSE, warning = FALSE, out.width = "100%")
suppressPackageStartupMessages(library(rvis))

# Shows the YAML configuration of an example dashboard as a code block
show_yaml <- function(name) {
  x <- readLines(rvisDashboardExample(name), warn = FALSE, encoding = "UTF-8")
  knitr::asis_output(paste0("```yaml\n", paste(x, collapse = "\n"), "\n```\n"))
}

# Shows the screenshot of an example dashboard
show_shot <- function(name) {
  knitr::include_graphics(file.path("figures", paste0("dash-", name, ".png")), dpi = NA)
}
