# Plotting basics with rvisPlot

[`rvisPlot()`](https://genpack.github.io/rvis/reference/rvisPlot.md)
dispatches a chart request to a wrapper function. Set `type` to the
chart method and `plotter` to the wrapper prefix, then pass the data and
the dimension mappings that the chart needs.

``` r

species <- aggregate(Petal.Length ~ Species, data = iris, FUN = mean)

rvisPlot(
  type = "bar",
  plotter = "amCharts",
  obj = species,
  x = "Species",
  y = "Petal.Length",
  config = list(title = "Mean petal length by species")
)
```

The same input and dimension mappings can be sent to other wrappers that
implement `bar`, such as `billboarder`, `c3`, `googleVis`,
`highcharter`, or `plotly`. Each wrapper has its own configuration
options and may impose additional data requirements. The [chart type
manual](https://genpack.github.io/rvis/articles/chart-types.md) lists
the exact wrapper methods and their arguments.

Examples of every chart type with each of its plotters are in the chart
guides: [comparing
categories](https://genpack.github.io/rvis/articles/comparing-categories.md),
[parts of a
whole](https://genpack.github.io/rvis/articles/parts-of-a-whole.md),
[relationships and
distributions](https://genpack.github.io/rvis/articles/relationships-distributions.md),
[time series](https://genpack.github.io/rvis/articles/time-series.md),
[networks and
hierarchies](https://genpack.github.io/rvis/articles/networks-hierarchies.md)
and [tables](https://genpack.github.io/rvis/articles/tables.md).

## Dimension arguments

Dimension arguments such as `x`, `y`, `label`, and `theta` refer to
columns in `obj`. Use a character column name for a single field. Use a
named list when the wrapper supports multiple series and the series need
distinct names, for example
`y = list(actual = "actual", forecast = "forecast")`.

``` r

series <- data.frame(
  date = as.POSIXct("2025-01-01", tz = "UTC") + (0:11) * 86400,
  actual = 30 + sin((0:11) * pi / 6) * 5,
  forecast = 31 + sin((0:11) * pi / 6) * 4
)

rvisPlot(
  type = "tsline",
  plotter = "amCharts",
  obj = series,
  x = "date",
  y = list(actual = "actual", forecast = "forecast"),
  config = list(title = "Actual and forecast")
)
```

## Configuration

Supply wrapper settings through `config`, a named list. Common settings
include titles, axes, legends, colors, and layout options. Configuration
keys are wrapper-specific: consult the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
the accepted type, default, and valid values of every key recorded by
the package. Extra `...` arguments are forwarded to the underlying chart
constructor where the wrapper supports them.

## Choosing a wrapper

The wrappers are optional dependencies. Install the plotting package for
the wrapper you plan to use. Then change `plotter` while keeping the
chart type, data, and mappings when both wrappers implement the same
chart. The package’s chart type index shows which exact methods are
available.

## Next steps

- Pick a chart in the chart guides, which show every chart type drawn by
  every plotter: [comparing
  categories](https://genpack.github.io/rvis/articles/comparing-categories.md),
  [parts of a
  whole](https://genpack.github.io/rvis/articles/parts-of-a-whole.md),
  [relationships and
  distributions](https://genpack.github.io/rvis/articles/relationships-distributions.md),
  [time series](https://genpack.github.io/rvis/articles/time-series.md),
  [networks and
  hierarchies](https://genpack.github.io/rvis/articles/networks-hierarchies.md)
  and [tables](https://genpack.github.io/rvis/articles/tables.md).
- [Examples and
  galleries](https://genpack.github.io/rvis/articles/examples.md) lists
  all examples, by chart type and plotter.
- [Interactive
  charts](https://genpack.github.io/rvis/articles/click-events.md):
  clicks on chart items as inputs of shiny apps and dashboards.
- [Dashboards](https://genpack.github.io/rvis/articles/dashboards.md):
  put the charts in a shiny dashboard built from a YAML file.
