# Show a chart in the web browser

Shows an htmlwidget (or rCharts) chart in the default web browser
instead of the viewer pane of the IDE. Useful for charts which need a
full browser window. Note: it sets `options(viewer = ...)` for the rest
of the session, so later charts also open in the browser.

## Usage

``` r
plotHtml(obj)
```

## Arguments

- obj:

  A chart, for example returned by
  [`rvisPlot()`](https://genpack.github.io/rvis/reference/rvisPlot.md).

## Value

Called for its side effect (the chart is shown).

## Examples

``` r
if (FALSE) { # \dontrun{
plotHtml(rvisPlot("bar", "echarts", obj = data.frame(a = c("x", "y"), b = 1:2), x = "a", y = "b"))
} # }
```
