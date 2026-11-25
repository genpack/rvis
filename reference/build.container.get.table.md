# Dashboard items of a dialog reading a CSV table

Builds the items of a dashboard dialog which reads a CSV file chosen by
the user and maps its columns to the columns a dashboard needs, with the
classes it needs: a file input, one select input per needed column
(filled with the column names of the file), and *Load* and *Cancel*
buttons, with the services reading, checking and converting the table.

## Usage

``` r
build.container.get.table(
  name,
  classes,
  outputvar = name %>% paste("out", sep = "."),
  fileSelectTitle = "Select input CSV file",
  loadButtonTitle = "Load selected file",
  containerType = "fluidPage",
  ...
)
```

## Arguments

- name:

  Name of the dialog container item; the other items are named after it.

- classes:

  Named list: the names of the columns the dashboard needs and, for
  each, the accepted classes (columns of other classes are converted to
  the first one).

- outputvar:

  Name of the variable receiving the table (kept for backward
  compatibility).

- fileSelectTitle, loadButtonTitle:

  Labels of the file input and of the load button.

- containerType:

  Type of the dialog container item, like `"fluidPage"` or
  `"wellPanel"`.

- ...:

  Other fields of the container item.

## Value

A named list of dashboard items, to add to the `items` of a dashboard
configuration.

## Details

Messages are kept in `session$userData$temp`: `<name>.failmsg` about
reading the file (`"Ok"`, `"Please choose a CSV file."`,
`"Selected table has no column names"` or
`"File read failed for some reason!"`) and `<name>.failmsg2` about the
column classes (`"Ok"` or `"Column x cannot be coerced to y!"`).

## See also

[`rvisDashboard`](https://genpack.github.io/rvis/reference/rvisDashboard.md),
and the [dashboards
guide](https://genpack.github.io/rvis/articles/dashboards.md).

## Examples

``` r
if (FALSE) { # \dontrun{
classes <- list(caseID = c("character", "factor"), timestamp = "POSIXct")
items <- build.container.get.table("my_dialog", classes = classes, containerType = "wellPanel")
runDashboard(list(layout = "my_dialog", items = items))
} # }
```
