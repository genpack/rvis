# Plot with an rvis wrapper

Dispatch a chart request to a wrapper function named by combining
\`plotter\` and \`type\` (for example, \`plotter = "amCharts"\` and
\`type = "bar"\` calls \`amCharts.bar()\`). Wrapper functions
standardize common data and aesthetic arguments while retaining
plotter-specific configuration.

## Usage

``` r
rvisPlot(type, plotter, ...)
```

## Arguments

- type:

  Character scalar. Exact chart method suffix implemented by the
  selected wrapper.

- plotter:

  Character scalar. Exact wrapper prefix. The corresponding function
  must be named \`paste0(plotter, ".", type)\`.

- ...:

  Arguments passed to the selected wrapper, commonly including \`obj\`,
  dimension mappings such as \`x\` and \`y\`, and a named \`config\`
  list.

## Value

The chart object returned by the selected wrapper, usually an
htmlwidget.

## See also

The “Chart type and plotter reference” and “Configuration reference”
vignettes.

## Examples

``` r
if (FALSE) { # \dontrun{
species <- aggregate(Petal.Length ~ Species, data = iris, FUN = mean)
rvisPlot(
  type = "bar", plotter = "amCharts", obj = species,
  x = "Species", y = "Petal.Length",
  config = list(title = "Mean petal length by species")
)
} # }
```
