# Build a dashboard from a configuration

Creates a `DASHBOARD` object from a configuration given as a YAML or
JSON file, or as an R list. A configuration has these fields (only
`items` and `layout` are required):

- name:

  Name of the dashboard.

- items:

  Named list of the dashboard items. Each item has a `type` (an input,
  output or container type) and the settings of that type. Output items
  have a `service`: R code returning the content of the output. Input
  items can have a `service`: R code run when the input changes.

- layout:

  Name(s) of the item(s) making the page, usually a single page
  container like `fluidPage`, `dashboardPage` or `navbarPage`.

- objects:

  Named list of objects copied into `session$userData` of each session.

- values:

  Named list of initial values of the reactive list `sync` of each
  session.

- prescript:

  R code run at the start of each session (for example to define
  reactive expressions).

- observers:

  R code (one text per observer) run in
  [`observe()`](https://rdrr.io/pkg/shiny/man/observe.html) in each
  session.

- settings, messages, loginTable:

  Settings, messages and users of dashboards with a login item.

The `type` of an item (or of its `cloth`) can also be any function of
any package, written as `package::function`, like
[`shinyWidgets::pickerInput`](https://dreamrs.github.io/shinyWidgets/reference/pickerInput.html)
or
[`shinydashboard::box`](https://rdrr.io/pkg/shinydashboard/man/box.html).
The item fields are then passed to that function as they are, with the
argument names of the package documentation. Functions with argument
`inputId` make inputs and functions with argument `outputId` make
outputs (the item name is passed as the id); all other functions are
containers and get the items of their `layout` as unnamed arguments. A
few fields are used by rvis and not passed to the function: `args`
(unnamed arguments passed first, like the text of
[`shiny::h3`](https://rstudio.github.io/htmltools/reference/builder.html)),
`service`, `layout`, `cloth`, `role` (`input`, `output` or `container`,
if the detected role is wrong), `render` (render function of an output,
by default the package's `renderFoo` for output `fooOutput`),
`render.args` (arguments of the render function), `update` (function
used by `updateItem()`, by default the package's `updateFoo` for input
`foo`) and the tooltip, popover and tutor fields. See
`rvisDashboardExample("generic-types")`.

## Usage

``` r
rvisDashboard(config, ...)
```

## Arguments

- config:

  Path of a YAML (`.yml`, `.yaml`) or JSON (`.json`) file, or a list.

- ...:

  Configuration fields added to (or replacing those of) `config`, for
  example `objects = list(data = mydata)`.

## Value

A `DASHBOARD` object. Run it with `$run()` or get the shiny app with
`$app()`.

## Details

In YAML files, values tagged `!expr` are R expressions evaluated when
the file is read (for example `choices: !expr names(mtcars)`). Only
`true` and `false` are read as logical values (words like `yes`, `no`,
`y` or `n` stay text).

## Examples

``` r
if (FALSE) { # \dontrun{
dash = rvisDashboard(rvisDashboardExample("hello"))
dash$run()

# from a list:
dash = rvisDashboard(list(
  layout = "page",
  items  = list(
    page = list(type = "fluidPage", title = "Hello", layout = list("n", "hist")),
    n    = list(type = "sliderInput", title = "Observations", min = 10, max = 500, value = 100),
    hist = list(type = "plotOutput", service = "hist(rnorm(input$n))"))))
shiny::runApp(dash$app())
} # }
```
