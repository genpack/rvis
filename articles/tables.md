# Tables: interactive, filterable and pivot tables

rvis table wrappers show a data frame as an interactive html table:
sortable and searchable (DT), filterable and editable with cell graphics
(TFD3, D3TableFilter), or reshaped by the reader (pivot). Examples are
evaluated when the package of the wrapper is installed; otherwise only
their code is shown.

## Data: violent crime rates by US state (`USArrests`)

`USArrests` gives arrests per 100,000 residents for assault, murder and
rape in each US state in 1973, and the percent of the population living
in urban areas. `state.region` adds the region of each state.

``` r

arrests <- data.frame(USArrests, region = as.character(state.region))
head(arrests)
#>            Murder Assault UrbanPop Rape region
#> Alabama      13.2     236       58 21.2  South
#> Alaska       10.0     263       48 44.5   West
#> Arizona       8.1     294       80 31.0   West
#> Arkansas      8.8     190       50 19.5  South
#> California    9.0     276       91 40.6   West
#> Colorado      7.9     204       78 38.7   West
```

## Tables

The `label` dimension selects the columns to show (all columns by
default). Row names are shown when `config$withRowNames` is `TRUE`.

### DT

``` r

rvisPlot(type = "table", plotter = "DT", obj = arrests,
         config = list(paging.length = 5))
```

### TFD3 (embedded in rvis)

TFD3 tables have a filter row (try `>10` in the Murder column) and can
turn cells into graphics with `config$column.shape` and colour columns
with `config$column.color`.

``` r

rvisPlot(type = "table", plotter = "TFD3", obj = arrests,
         label = list("Murder", "Assault", "UrbanPop", "region"),
         config = list(column.shape = list(Assault = "bar"),
                       column.color = list(UrbanPop = c("white", "steelblue")),
                       column.footer = list(Murder = mean, Assault = mean),
                       height = 400))
```

### D3TableFilter

`D3TableFilter` is the published version of the TFD3 widget and takes
the same settings.

``` r

rvisPlot(type = "table", plotter = "D3TableFilter", obj = arrests,
         config = list(column.shape = list(Murder = "bubble")))
```

## Tables from a long table

### TFD3

``` r

rvisPlot(type = "table.molten", plotter = "TFD3", obj = arrests, label = list("region", "Murder", "Assault"))
```

## Pivot tables

A pivot table lets the reader drag columns into rows and columns and
choose how values are aggregated and shown (table, heat map, charts).

### pivot

``` r

rvisPlot(type = "table", plotter = "pivot", obj = arrests, rows = "region",
         vals = "Murder", aggregatorName = "Average", rendererName = "Heatmap")
```

## Legacy table function

[`DT.Table()`](https://genpack.github.io/rvis/reference/DT.Table.md) is
an older function kept for backward compatibility: it takes the table as
`x` and DataTables `options` directly.

``` r

DT.Table(arrests, options = list(pageLength = 5))
```
