# rvis: interactive charts and shiny dashboards

rvis draws interactive charts with about 30 plotting packages using one
calling convention,
[`rvisPlot()`](https://genpack.github.io/rvis/reference/rvisPlot.md),
and builds shiny dashboards from a YAML or JSON configuration with
[`rvisDashboard()`](https://genpack.github.io/rvis/reference/rvisDashboard.md).
Clicks on chart items can be shiny inputs
([`rvisClickEvents()`](https://genpack.github.io/rvis/reference/rvisClickEvents.md)).

## Learn rvis

Start with the [Get
started](https://genpack.github.io/rvis/articles/rvis.md) article, then
the chart guides and the dashboard guides of the package website (also
available as vignettes: `browseVignettes("rvis")`). [Examples and
galleries](https://genpack.github.io/rvis/articles/examples.md) lists
all examples.

## Class DASHBOARD

Dashboards are objects of the reference class
[`DASHBOARD`](https://genpack.github.io/rvis/reference/DASHBOARD-class.md):
their items (inputs, outputs and containers), layout and services. They
are made by
[`rvisDashboard()`](https://genpack.github.io/rvis/reference/rvisDashboard.md)
and run by their `run()` method or
[`runDashboard()`](https://genpack.github.io/rvis/reference/runDashboard.md).

## See also

Useful links:

- <https://github.com/genpack/rvis>

- Report bugs at <https://github.com/genpack/rvis/issues>
