# Build a network from the levels of a table

Converts a table into the `nodes` and `links` tables used by the graph
and sankey wrappers (for example `visNetwork`, `grviz`, `echarts`,
`networkD3` and `googleVis`). The columns `id_cols` are consecutive
levels of a flow: each value of a level is a node, and a link joins each
value of a level to each value of the next level it occurs with, with
the sum of `value_col` over the rows as its value.

## Usage

``` r
df2Network(df, id_cols = names(df), value_col, percentage = F)
```

## Arguments

- df:

  A data frame.

- id_cols:

  Names of the columns giving the consecutive levels of the network.

- value_col:

  Name of the numeric column summed over the links.

- percentage:

  If `TRUE`, adds column `pathratio` to the links: the share (percent)
  of the flow through the link along the whole path.

## Value

A list with two data frames: `nodes` (columns `id` and `label`, like
`"Class=1st"`) and `links` (columns `source`, `target`, `value`,
`svname`, `tvname`, `hovertext`, `sumval` and `ratio`: the percent of
the flow of the source going through the link).

## See also

The [networks and
hierarchies](https://genpack.github.io/rvis/articles/networks-hierarchies.md)
guide.

## Examples

``` r
titanic <- as.data.frame(Titanic, stringsAsFactors = FALSE)
flows <- df2Network(titanic, id_cols = c("Class", "Sex", "Survived"), value_col = "Freq")
head(flows$nodes)
#>           id      label
#> 1 Sex=Female Sex=Female
#> 2   Sex=Male   Sex=Male
#> 3  Class=1st  Class=1st
#> 4  Class=2nd  Class=2nd
#> 5  Class=3rd  Class=3rd
#> 6 Class=Crew Class=Crew
head(flows$links[, c("source", "target", "value", "ratio")])
#> # A tibble: 6 × 4
#> # Groups:   source [3]
#>   source     target       value ratio
#>   <chr>      <chr>        <dbl> <dbl>
#> 1 Sex=Female Survived=No    126  26.8
#> 2 Sex=Female Survived=Yes   344  73.2
#> 3 Sex=Male   Survived=No   1364  78.8
#> 4 Sex=Male   Survived=Yes   367  21.2
#> 5 Class=1st  Sex=Female     145  44.6
#> 6 Class=1st  Sex=Male       180  55.4
if (FALSE) { # \dontrun{
rvisPlot("sankey", "networkD3", obj = flows, key = "id", label = "label",
         source = "source", target = "target", linkWidth = "value")
} # }
```
