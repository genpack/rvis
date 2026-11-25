# Configuration settings of the plotters

The settings each plotter accepts in argument `config` of
[`rvisPlot()`](https://genpack.github.io/rvis/reference/rvisPlot.md):
their class, default value and valid values. The wrappers check `config`
against this table and fill missing settings with their defaults. The
same table, with the meaning of each setting, is shown in the
[configuration
reference](https://genpack.github.io/rvis/articles/configuration.md).

## Usage

``` r
data(properties)
```

## Format

A data frame with one row per plotter and setting:

- plotters:

  Name of the plotter (the `plotter` argument of
  [`rvisPlot()`](https://genpack.github.io/rvis/reference/rvisPlot.md)).

- Property:

  Name of the setting, like `title` or `legend.position`.

- Class:

  Valid classes of its value, separated by spaces.

- Default:

  Default value (empty: none).

- Domain:

  Valid values, separated by spaces (empty: any value). For numbers, the
  minimum and maximum.

## Source

`data/properties.csv` of the package.

## Examples

``` r
properties[properties$plotters == "echarts", c("Property", "Default", "Domain")]
#> Error: object 'properties' not found
```
