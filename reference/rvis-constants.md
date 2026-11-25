# Names, chart types and settings shared by the plotters

`valid.dim.names` lists the dimension arguments of the wrappers (like
`x`, `y`, `color`, `size`, `label`, `source` or `linkWidth`): the
columns of the data a wrapper maps to its chart. `valid.plot.types` and
`valid.plotters` are short lists kept for backward compatibility; the
complete list of chart types and plotters is in the [chart type and
plotter
reference](https://genpack.github.io/rvis/articles/chart-types.md) and
in the `plotters` data set. `defset` holds the default settings that
every wrapper's own defaults are built on: the colour and shape
palettes, `withRowNames` and `colorize`.

## Format

Character vectors (`valid.dim.names`, `valid.plot.types`,
`valid.plotters`) and a list (`defset`).

## See also

[`rvisPlot`](https://genpack.github.io/rvis/reference/rvisPlot.md),
[`plotters`](https://genpack.github.io/rvis/reference/plotters.md),
[`properties`](https://genpack.github.io/rvis/reference/properties.md).

## Examples

``` r
valid.dim.names
#>  [1] "key"            "x"              "y"              "y2"            
#>  [5] "z"              "t"              "high"           "low"           
#>  [9] "color"          "size"           "shape"          "label"         
#> [13] "tooltip"        "labelColor"     "borderColor"    "linkColor"     
#> [17] "theta"          "ySide"          "group"          "source"        
#> [21] "target"         "linkWidth"      "linkLength"     "linkLabel"     
#> [25] "linkLabelColor" "linkLabelSize"  "linkTooltip"   
defset$palette$color
#> [1] "purple" "blue"   "cyan"   "green"  "yellow" "orange" "red"    "black" 
#> [9] "white" 
```
