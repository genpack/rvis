# Legacy leaflet map functions

Older map functions, kept for existing code. `leaflet.map.plot()` works
only for objects of class `STORE.GROUP` of an older package (it reads
their `spec` table with columns `Latitude` and `Longitude`).
`leaflet.map.Zoom()` zooms a leaflet map to the given points, with a
margin of `dist` degrees, and closes its popups.

## Usage

``` r
leaflet.map.plot(x, stores = rownames(sg$spec), tiles = F, icon = NULL)

leaflet.map.Zoom(x, lat, long, dist = 0.01)
```

## Arguments

- x:

  A `STORE.GROUP` object (`leaflet.map.plot()`) or a leaflet map
  (`leaflet.map.Zoom()`).

- stores:

  Names of the stores to show.

- tiles:

  Whether to add the default map tiles.

- icon:

  Optional marker icon
  ([`leaflet::makeIcon()`](https://rstudio.github.io/leaflet/reference/makeIcon.html));
  circle markers if `NULL`.

- lat, long:

  Latitudes and longitudes of the points to zoom to.

- dist:

  Margin around the points, in degrees.

## Value

A leaflet map.

## Details

For new code, use package leaflet directly: leaflet maps work as
dashboard outputs (`leafletOutput`), and clicks on their markers can be
sent to shiny with
[`rvisClickEvents()`](https://genpack.github.io/rvis/reference/rvisClickEvents.md).

## Examples

``` r
if (FALSE) { # \dontrun{
map <- leaflet::addTiles(leaflet::leaflet())
leaflet.map.Zoom(map, lat = c(-33.9, -37.8), long = c(151.2, 144.9), dist = 1)
} # }
```
