# leaflet.R ----------------------------------------------------------------


# Header
# Filename:       leaflet.R
# Description:    Contains functions for plotting various maps from package 'leaflet' using standrad inputs.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     29 December 2016
# Last Revision:  29 December 2016
# Version:        1.0.0
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 1.0.0     29 December 2016   Initial issue



#' Legacy leaflet map functions
#'
#' Older map functions, kept for existing code. \code{leaflet.map.plot()} works only for objects of
#' class \code{STORE.GROUP} of an older package (it reads their \code{spec} table with columns
#' \code{Latitude} and \code{Longitude}). \code{leaflet.map.Zoom()} zooms a leaflet map to the given
#' points, with a margin of \code{dist} degrees, and closes its popups.
#'
#' For new code, use package leaflet directly: leaflet maps work as dashboard outputs
#' (\code{leafletOutput}), and clicks on their markers can be sent to shiny with
#' \code{\link{rvisClickEvents}()}.
#'
#' @param x A \code{STORE.GROUP} object (\code{leaflet.map.plot()}) or a leaflet map (\code{leaflet.map.Zoom()}).
#' @param stores Names of the stores to show.
#' @param tiles Whether to add the default map tiles.
#' @param icon Optional marker icon (\code{leaflet::makeIcon()}); circle markers if \code{NULL}.
#' @param lat,long Latitudes and longitudes of the points to zoom to.
#' @param dist Margin around the points, in degrees.
#' @return A leaflet map.
#' @examples
#' \dontrun{
#' map <- leaflet::addTiles(leaflet::leaflet())
#' leaflet.map.Zoom(map, lat = c(-33.9, -37.8), long = c(151.2, 144.9), dist = 1)
#' }
#' @name leaflet.map.plot
#' @export
leaflet.map.plot = function(x, stores = rownames(sg$spec), tiles = F, icon = NULL){
  if (inherits(x, "STORE.GROUP")){
    spc  = sg$spec[stores,]
    atms = rownames(spc)
    map  <- leaflet(spc, height = "100px") %>%
      fitBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude))
    if (is.null(icon)){
      map <- addCircleMarkers(map, lng = ~Longitude, lat = ~Latitude, layerId = atms, popup = atms)
    } else {map  <- addMarkers(map, lng = ~Longitude, lat = ~Latitude, layerId = atms, popup = atms, icon = icon)}
    if (tiles){map <- addTiles(map)}
  } else {cat("OTHER CLASSES ARE NOT SUPPORTED YET!")}
  return(map)
}


#' @rdname leaflet.map.plot
#' @export
leaflet.map.Zoom = function(x, lat, long, dist = 0.01){
  # Check out x is of class leaflet map
  x %>% clearPopups() %>% fitBounds(min(long) - dist, min(lat) - dist, max(long) + dist, max(lat) + dist)
}
