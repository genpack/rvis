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


#' @export
leaflet.map.Zoom = function(x, lat, long, dist = 0.01){
  # Check out x is of class leaflet map
  x %>% clearPopups() %>% fitBounds(min(long) - dist, min(lat) - dist, max(long) + dist, max(lat) + dist)
}


