# Defunct functions

These functions are no longer supported and only stop with an error
naming their replacement. They are kept so that old code calling them
gets a clear message.

- `verifyPlotInputs()`: replaced by the internal functions
  `prepareAesthetics()` and `prepare4Plot()` used by all wrappers.

- `visNetwork.graphChart()`: replaced by
  `rvisPlot("graph", "visNetwork", ...)`.

## Usage

``` r
visNetwork.graphChart(
  nodes,
  links,
  size = NULL,
  sizeLegend = NULL,
  shape = NULL,
  shapeLegend = NULL,
  color = NULL,
  colorLegend = NULL,
  label = NULL,
  linkColor = NULL,
  linkLength = NULL,
  linkLengthLegend = NULL,
  linkSource = NULL,
  linkTarget = NULL,
  linkWidth = NULL,
  ...
)
```

## Arguments

- obj, x, y, z, t, color, size, shape, label, labelColor, theta,
  linkSource, linkTarget, tooltip, palette.color, palette.labelColor,
  nodes, links, sizeLegend, shapeLegend, colorLegend, linkColor,
  linkLength, linkLengthLegend, linkWidth, ...:

  Ignored.
