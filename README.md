# rvis

`rvis` provides a common dispatch function for chart wrappers from several R
visualization packages. Call `rvisPlot()` with a chart `type`, a wrapper
`plotter`, the input data, and the dimension mappings required by that chart.

```r
species <- aggregate(Petal.Length ~ Species, data = iris, FUN = mean)

rvisPlot(
  type = "bar", plotter = "amCharts", obj = species,
  x = "Species", y = "Petal.Length",
  config = list(title = "Mean petal length by species")
)
```

The chart method is dispatched using the exact function name
`paste0(plotter, ".", type)`. For example, `plotter = "amCharts"` and
`type = "bar"` call `amCharts.bar()`. Wrapper dependencies are optional; install
the plotting package for the wrapper you want to use.

## User guides

- [Getting started with `rvisPlot`](articles/getting-started.html)
- Chart guides, with examples of every chart type and plotter:
  - [Comparing categories: bar, line, area and combo charts](articles/comparing-categories.html)
  - [Parts of a whole: pie, funnel, pyramid, gauge, bullet and word cloud charts](articles/parts-of-a-whole.html)
  - [Relationships and distributions: scatter, bubble, histogram and box plots](articles/relationships-distributions.html)
  - [Time series: line, area, bar and calendar charts over time](articles/time-series.html)
  - [Networks, flows and hierarchies: graph, sankey, sunburst, tree and treemap charts](articles/networks-hierarchies.html)
  - [Tables: interactive, filterable and pivot tables](articles/tables.html)
- [Chart type and plotter reference](articles/chart-types.html)
- [Configuration reference](articles/configuration.html)
- [Plotters: what is covered and how to add more](articles/adding-plotters.html)

## Main function

- [`rvisPlot()`](reference/rvisPlot.html)
