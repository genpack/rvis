# Get started with rvis

rvis does two things:

1.  **Charts with one calling convention.**
    [`rvisPlot()`](https://genpack.github.io/rvis/reference/rvisPlot.md)
    draws interactive charts with about 30 plotting packages
    (htmlwidgets like echarts4r, highcharter, plotly, apexcharter,
    visNetwork or DT), always with the same arguments: the data, the
    columns to show, and a list of settings. Changing the package that
    draws a chart is changing one word.
2.  **Shiny dashboards from a configuration.**
    [`rvisDashboard()`](https://genpack.github.io/rvis/reference/rvisDashboard.md)
    builds a complete shiny app from a YAML (or JSON) file or an R list
    naming its inputs, outputs, containers and layout, with the R code
    of its outputs. Clicks on chart items can be inputs of the
    dashboard.

This page takes you from installation to your first chart and your first
dashboard, and shows where to go next.

## Installation

rvis is installed from GitHub. It needs package
[rutils](https://github.com/genpack/rutils), also on GitHub, which is
installed with it:

``` r

# install.packages("remotes")
remotes::install_github("genpack/rvis")
```

The plotting packages are **optional**: install the ones whose charts
you want to use. Each chart of the [chart guides](#where-next) names the
package it needs. The packages on CRAN:

``` r

install.packages(c("echarts4r", "apexcharter", "highcharter", "plotly", "billboarder", "c3",
                   "dygraphs", "rAmCharts", "googleVis", "networkD3", "visNetwork", "DiagrammeR",
                   "DT", "rpivotTable", "leaflet"))
```

For dashboards, install shiny and shinydashboard (and, for the features
that use them, shinyWidgets, shinyBS, shinyjs, rintrojs, shinythemes,
yaml and jsonlite):

``` r

install.packages(c("shiny", "shinydashboard", "yaml", "shinyWidgets", "shinyBS", "shinyjs", "rintrojs", "shinythemes"))
```

A few plotters use packages which are only on GitHub, like rCharts
(`remotes::install_github("ramnathv/rCharts")`), D3plusR
(`"paulofelipe/D3plusR"`), streamgraph (`"hrbrmstr/streamgraph"`) or
sankeytreeR (`"timelyportfolio/sankeytreeR"`). The [plotter
list](https://genpack.github.io/rvis/articles/adding-plotters.md) shows
every plotter with its package.

## Your first chart

[`rvisPlot()`](https://genpack.github.io/rvis/reference/rvisPlot.md)
takes the chart `type`, the `plotter` (the package drawing it), the data
`obj`, the columns of the dimensions of the chart (here `x` and `y`) and
a `config` list of settings:

``` r

library(rvis)
deaths <- data.frame(age = rownames(VADeaths), VADeaths, check.names = FALSE)

rvisPlot(type = "bar", plotter = "echarts", obj = deaths,
         x = "age", y = list("Rural Male", "Urban Male"),
         config = list(title = "Death rates in Virginia (1940)", yAxis.label = "per 1000"))
```

Several series are given as a list of columns, like
`y = list("Rural Male", "Urban Male")`. The same call draws the chart
with another package by changing `plotter`:

``` r

rvisPlot(type = "bar", plotter = "apexcharter", obj = deaths,
         x = "age", y = list("Rural Male", "Urban Male"),
         config = list(title = "Death rates in Virginia (1940)", yAxis.label = "per 1000"))
```

The charts are htmlwidgets: they are shown in the RStudio viewer, in R
Markdown and Quarto documents, and in shiny apps (with the output and
render functions of their package).

## Your first dashboard

A dashboard is a list of named **items**, inputs, outputs and
containers, and a **layout** naming the item that makes the page.
Outputs have a **service**: the R code of their content, run again when
an input it uses changes. Save this as `hello.yml`:

``` yaml
name: Hello rvis
layout: page
items:
  page:
    type: sidebarLayout
    title: Hello rvis!
    layout.side: [bins]
    layout.main: [hist]
  bins:
    type: sliderInput
    title: "Number of bins:"
    min: 1
    max: 50
    value: 30
  hist:
    type: plotOutput
    service: |
      x <- faithful$waiting
      hist(x, breaks = seq(min(x), max(x), length.out = input$bins + 1),
           col = "#75AADB", border = "white", main = "Waiting times")
```

and run it:

``` r

runDashboard("hello.yml")
```

![The Hello rvis dashboard: a slider for the number of bins and a
histogram of the waiting times of the Old Faithful
geyser](figures/dash-hello.png)

The Hello rvis dashboard: a slider for the number of bins and a
histogram of the waiting times of the Old Faithful geyser

This example comes with rvis. 40 example dashboards do, from single
features to complete apps:
[`rvisDashboardExample()`](https://genpack.github.io/rvis/reference/rvisDashboardExample.md)
lists them, and `runDashboard(rvisDashboardExample("hello"))` runs one.

``` r

head(rvisDashboardExample()[, c("name", "group", "title")], 10)
#>               name  group               title
#> 1           charts basics         rvis charts
#> 2     click-events basics        Click events
#> 3          clothes basics             Clothes
#> 4          dynamic basics   Dynamic interface
#> 5    generic-types basics  Generic item types
#> 6            hello basics          Hello rvis
#> 7           inputs basics         Input items
#> 8      layout-grid basics         Layout grid
#> 9            login basics               Login
#> 10 modals-tooltips basics Modals and tooltips
```

Charts of rvis are dashboard outputs too, and any function of any
package can be an item, written as `package::function`:

``` yaml
  chart:
    type: echarts4r::echarts4rOutput
    click.events: true      # clicks on the bars set input$chart_item_click
    service: rvisPlot("bar", "echarts", obj = mtcars, x = "cyl", y = "mpg")
```

## Where next

**Learn the charts**

- [Plotting
  basics](https://genpack.github.io/rvis/articles/getting-started.md):
  the arguments of
  [`rvisPlot()`](https://genpack.github.io/rvis/reference/rvisPlot.md),
  several series, settings.
- The chart guides, with an example of every chart type drawn by every
  plotter, each on one R data set: [comparing
  categories](https://genpack.github.io/rvis/articles/comparing-categories.md)
  (bar, line, area, radar, combo), [parts of a
  whole](https://genpack.github.io/rvis/articles/parts-of-a-whole.md)
  (pie, funnel, pyramid, gauge, bullet, word cloud), [relationships and
  distributions](https://genpack.github.io/rvis/articles/relationships-distributions.md)
  (scatter, bubble, histogram, box, heat map), [time
  series](https://genpack.github.io/rvis/articles/time-series.md) (line,
  area, bar, stock, calendar), [networks and
  hierarchies](https://genpack.github.io/rvis/articles/networks-hierarchies.md)
  (graph, sankey, sunburst, tree, treemap) and
  [tables](https://genpack.github.io/rvis/articles/tables.md)
  (interactive, filterable and pivot tables).
- [Configuration
  reference](https://genpack.github.io/rvis/articles/configuration.md):
  every setting of every plotter.
- [Interactive charts: clicks in
  shiny](https://genpack.github.io/rvis/articles/click-events.md):
  clicks on nodes, bars and points as inputs.

**Build dashboards**

- [Dashboards: getting
  started](https://genpack.github.io/rvis/articles/dashboards.md), then
  [layouts](https://genpack.github.io/rvis/articles/dashboard-layouts.md),
  the [item
  reference](https://genpack.github.io/rvis/articles/dashboard-reference.md)
  and the [Shiny
  packages](https://genpack.github.io/rvis/articles/dashboard-extensions.md)
  usable in dashboards.
- [Gallery](https://genpack.github.io/rvis/articles/dashboard-gallery.md):
  the apps of the Shiny gallery rebuilt from YAML, and two large
  showcase dashboards.

**Find an example**

- [Examples and
  galleries](https://genpack.github.io/rvis/articles/examples.md): an
  index of all chart examples and example dashboards.

**Look something up**

- [Function
  reference](https://genpack.github.io/rvis/reference/index.md), [chart
  type and plotter
  reference](https://genpack.github.io/rvis/articles/chart-types.md),
  [plotters covered and
  planned](https://genpack.github.io/rvis/articles/adding-plotters.md).
