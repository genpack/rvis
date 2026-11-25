# Dashboard objects

A `DASHBOARD` object holds a shiny dashboard: its items (inputs, outputs
and containers), its layout and the R code of its services. It is
usually made from a YAML, JSON or R list configuration with
[`rvisDashboard()`](https://genpack.github.io/rvis/reference/rvisDashboard.md),
and run with its `run()` method or
[`runDashboard()`](https://genpack.github.io/rvis/reference/runDashboard.md).

## Fields

- `name`:

  Name of the dashboard.

- `items`:

  Named list of the items. Each item is a list with a `type` and the
  settings of that type; see the [item
  reference](https://genpack.github.io/rvis/articles/dashboard-reference.md).

- `king.layout`:

  The item(s) making the page (field `layout` of a configuration).

- `objects`:

  Objects copied into `session$userData` of each session.

- `values`:

  Initial values of the reactive list `sync` of each session.

- `prescript`, `observers`:

  R code run at the start of each session, and in observers.

- `settings`, `messages`, `loginTable`:

  Settings, messages and users of dashboards with a login item.

## Methods

- `run(...)`:

  Runs the dashboard (arguments are passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html)).

- `app(...)`:

  Returns the shiny app object.

- `dashboard.ui()`, `dashboard.server()`:

  Return the user interface and the server function.

- `updateItem4Session(item, session, ...)`:

  Updates an input item in a session (like
  [`shiny::updateSelectInput()`](https://rdrr.io/pkg/shiny/man/updateSelectInput.html));
  usable in services.

- `updateItem(item, ...)`:

  Updates an input item in all open sessions (needs setting
  `keepSessions`).

- `hideItems(...)`, `showItems(...)`, `enableItems(...)`,
  `disableItems(...)`:

  Hide, show, enable or disable items (package shinyjs); usable in
  services.

- `itemRole(i)`:

  Role of item `i`: `"input"`, `"output"` or `"container"`.

## See also

[`rvisDashboard`](https://genpack.github.io/rvis/reference/rvisDashboard.md),
[`runDashboard`](https://genpack.github.io/rvis/reference/runDashboard.md),
[`rvisDashboardExample`](https://genpack.github.io/rvis/reference/rvisDashboardExample.md),
and the [dashboards
guide](https://genpack.github.io/rvis/articles/dashboards.md).

## Examples

``` r
if (FALSE) { # \dontrun{
dash <- rvisDashboard(rvisDashboardExample("hello"))
class(dash)
names(dash$items)
dash$run()
} # }
```
