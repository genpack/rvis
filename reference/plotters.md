# Chart types of the plotters

A table of the plotters (rows) and the chart types (columns) they draw:
`"x"` if the plotter has a wrapper for the chart type. The complete list
of chart types and wrappers, with their arguments, is in the [chart type
and plotter
reference](https://genpack.github.io/rvis/articles/chart-types.md).

## Usage

``` r
data(plotters)
```

## Format

A data frame with column `Plotter` and one column per chart type (`bar`,
`pie`, `scatter`, `line`, `tsline`, `graph`, `sankey`, ...).

## Source

`data/plotters.csv` of the package.

## Examples

``` r
plotters[!is.na(plotters$graph), "Plotter"]   # plotters drawing graphs
#> Error: object 'plotters' not found
```
