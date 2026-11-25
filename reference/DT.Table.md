# Legacy DT table functions

Older functions for DT tables, kept for existing code. For new code, use
`rvisPlot("table", "DT", ...)` (see the [tables
guide](https://genpack.github.io/rvis/articles/tables.md)).

## Usage

``` r
DT.Table(x, links = NULL, session = NULL, options = list(), rownames = T)

DT.Table.Classify.bold(x, classColumn, boldValues = "TRUE")
```

## Arguments

- x:

  A data frame (`DT.Table()`) or a DT table
  (`DT.Table.Classify.bold()`).

- links:

  Optional list of links added to the cells.

- session:

  Shiny session, needed with `links`.

- options:

  List of DataTables options, passed to
  [`DT::datatable()`](https://rdrr.io/pkg/DT/man/datatable.html).

- rownames:

  Whether to show the row names.

- classColumn:

  Name of the column deciding which rows are bold.

- boldValues:

  Values of `classColumn` whose rows are bold.

## Value

A DT table (htmlwidget).

## Details

`DT.Table()` makes a DT table of `x` without escaping html. A list
`options$header` gives CSS properties of the header row. `links` adds
action links to cells (they need the shiny `session`).
`DT.Table.Classify.bold()` makes the rows bold whose value in column
`classColumn` is one of `boldValues`.

## Examples

``` r
if (FALSE) { # \dontrun{
tbl <- DT.Table(head(mtcars), options = list(header = list(`background-color` = "#ddd")))
DT.Table.Classify.bold(DT.Table(data.frame(a = 1:3, big = c(FALSE, TRUE, FALSE))), "big")
} # }
```
