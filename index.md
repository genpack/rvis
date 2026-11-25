# rvis

**Interactive charts with one calling convention, and shiny dashboards
from a configuration file.**

- **Charts**:
  [`rvisPlot()`](https://genpack.github.io/rvis/reference/rvisPlot.md)
  draws interactive charts with about 30 plotting packages (echarts4r,
  highcharter, plotly, apexcharter, amCharts, billboarder, c3, dygraphs,
  visNetwork, networkD3, DT, …) using the same arguments: the data, the
  columns to show and a list of settings. Changing the package that
  draws a chart is changing one word.
- **Dashboards**:
  [`rvisDashboard()`](https://genpack.github.io/rvis/reference/rvisDashboard.md)
  builds a complete shiny app from a YAML or JSON file (or an R list)
  naming its inputs, outputs, containers and layout, with the R code of
  its outputs. Any function of any package can be a dashboard item.
- **Interactive charts**: clicks on nodes, links, bars, points, table
  cells or map markers become shiny inputs: directly for 13 plotting
  packages, and for any other htmlwidget with a CSS selector, without
  changing the javascript of the packages.

## Installation

``` r

# install.packages("remotes")
remotes::install_github("genpack/rvis")
```

The plotting packages are optional: install those whose charts you use,
for example `install.packages(c("echarts4r", "highcharter", "plotly"))`.
See [Get
started](https://genpack.github.io/rvis/articles/rvis.html#installation)
for the full list.

## A chart

``` r

library(rvis)
deaths <- data.frame(age = rownames(VADeaths), VADeaths, check.names = FALSE)

rvisPlot(type = "bar", plotter = "echarts", obj = deaths,
         x = "age", y = list("Rural Male", "Urban Male"),
         config = list(title = "Death rates in Virginia", yAxis.label = "per 1000"))

# the same chart drawn by another package:
rvisPlot(type = "bar", plotter = "highcharter", obj = deaths,
         x = "age", y = list("Rural Male", "Urban Male"))
```

## A dashboard

``` yaml
name: Hello rvis
layout: page
items:
  page: {type: sidebarLayout, title: Hello rvis!, layout.side: [bins], layout.main: [hist]}
  bins: {type: sliderInput, title: "Number of bins:", min: 1, max: 50, value: 30}
  hist:
    type: plotOutput
    service: hist(faithful$waiting, breaks = input$bins)
```

``` r

runDashboard("hello.yml")
rvisDashboardExample()      # 40 example dashboards, including the apps of the Shiny gallery
```

## Learn rvis

1.  [Get started](https://genpack.github.io/rvis/articles/rvis.md):
    installation, a first chart, a first dashboard.
2.  Charts: [plotting
    basics](https://genpack.github.io/rvis/articles/getting-started.md),
    then the chart guides with an example of every chart type drawn by
    every plotter: [comparing
    categories](https://genpack.github.io/rvis/articles/comparing-categories.md),
    [parts of a
    whole](https://genpack.github.io/rvis/articles/parts-of-a-whole.md),
    [relationships and
    distributions](https://genpack.github.io/rvis/articles/relationships-distributions.md),
    [time
    series](https://genpack.github.io/rvis/articles/time-series.md),
    [networks and
    hierarchies](https://genpack.github.io/rvis/articles/networks-hierarchies.md)
    and [tables](https://genpack.github.io/rvis/articles/tables.md); the
    [configuration
    reference](https://genpack.github.io/rvis/articles/configuration.md)
    and [interactive
    charts](https://genpack.github.io/rvis/articles/click-events.md).
3.  Dashboards: [getting
    started](https://genpack.github.io/rvis/articles/dashboards.md),
    [layouts](https://genpack.github.io/rvis/articles/dashboard-layouts.md),
    [item
    reference](https://genpack.github.io/rvis/articles/dashboard-reference.md),
    [Shiny
    packages](https://genpack.github.io/rvis/articles/dashboard-extensions.md)
    and the
    [gallery](https://genpack.github.io/rvis/articles/dashboard-gallery.md).
4.  [Examples and
    galleries](https://genpack.github.io/rvis/articles/examples.md): all
    chart examples and example dashboards in one place.
5.  Reference:
    [functions](https://genpack.github.io/rvis/reference/index.md),
    [chart types and
    plotters](https://genpack.github.io/rvis/articles/chart-types.md),
    [plotters covered and
    planned](https://genpack.github.io/rvis/articles/adding-plotters.md),
    [changes](https://genpack.github.io/rvis/news/index.md).
