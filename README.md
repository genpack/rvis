# rvis

**Interactive charts with one calling convention, and shiny dashboards from a configuration file.**

- **Charts**: `rvisPlot()` draws interactive charts with about 30 plotting packages (echarts4r,
  highcharter, plotly, apexcharter, amCharts, billboarder, c3, dygraphs, visNetwork, networkD3, DT, ...)
  using the same arguments: the data, the columns to show and a list of settings. Changing the package
  that draws a chart is changing one word.
- **Dashboards**: `rvisDashboard()` builds a complete shiny app from a YAML or JSON file (or an R list)
  naming its inputs, outputs, containers and layout, with the R code of its outputs. Any function of
  any package can be a dashboard item.
- **Interactive charts**: clicks on nodes, links, bars, points, table cells or map markers become shiny
  inputs: directly for 13 plotting packages, and for any other htmlwidget with a CSS selector, without
  changing the javascript of the packages.

## Installation

```r
# install.packages("remotes")
remotes::install_github("genpack/rvis")
```

The plotting packages are optional: install those whose charts you use, for example
`install.packages(c("echarts4r", "highcharter", "plotly"))`. See
[Get started](articles/rvis.html#installation) for the full list.

## A chart

```r
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

```yaml
name: Hello rvis
layout: page
items:
  page: {type: sidebarLayout, title: Hello rvis!, layout.side: [bins], layout.main: [hist]}
  bins: {type: sliderInput, title: "Number of bins:", min: 1, max: 50, value: 30}
  hist:
    type: plotOutput
    service: hist(faithful$waiting, breaks = input$bins)
```

```r
runDashboard("hello.yml")
rvisDashboardExample()      # 40 example dashboards, including the apps of the Shiny gallery
```

## Learn rvis

1. [Get started](articles/rvis.html): installation, a first chart, a first dashboard.
2. Charts: [plotting basics](articles/getting-started.html), then the chart guides with an example of
   every chart type drawn by every plotter:
   [comparing categories](articles/comparing-categories.html),
   [parts of a whole](articles/parts-of-a-whole.html),
   [relationships and distributions](articles/relationships-distributions.html),
   [time series](articles/time-series.html),
   [networks and hierarchies](articles/networks-hierarchies.html) and
   [tables](articles/tables.html); the [configuration reference](articles/configuration.html) and
   [interactive charts](articles/click-events.html).
3. Dashboards: [getting started](articles/dashboards.html), [layouts](articles/dashboard-layouts.html),
   [item reference](articles/dashboard-reference.html), [Shiny packages](articles/dashboard-extensions.html)
   and the [gallery](articles/dashboard-gallery.html).
4. [Examples and galleries](articles/examples.html): all chart examples and example dashboards in one place.
5. Reference: [functions](reference/index.html), [chart types and plotters](articles/chart-types.html),
   [plotters covered and planned](articles/adding-plotters.html), [changes](news/index.html).
