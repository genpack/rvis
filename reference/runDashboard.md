# Run a dashboard

Builds a dashboard from a configuration with
[`rvisDashboard()`](https://genpack.github.io/rvis/reference/rvisDashboard.md)
and runs it.

## Usage

``` r
runDashboard(config, ...)
```

## Arguments

- config:

  Path of a YAML or JSON file, a list, or a `DASHBOARD` object.

- ...:

  Arguments passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html), like
  `port` or `launch.browser`.
