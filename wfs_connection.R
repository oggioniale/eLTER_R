# install.packages("rwfs")
library(rwfs)
# install.packages("leaflet")
library(leaflet)
# install.packages("mapview")
library(mapview)
# install.packages("raster")
library(raster)
library(dplyr)

####
# Accessing WFS with rwfs package
####

fileName <- tempfile()
download.file("https://data.lter-europe.net/geoserver/deims/wfs?SERVICE=WFS&VERSION=1.0.0&REQUEST=GetFeature&TYPENAME=deims:deims_all_sites&SRSNAME=EPSG:4326", fileName)
request <- rwfs::GMLFile$new(fileName)
client <- rwfs::WFSCachingClient$new(request)
allSites <- client$getLayer("deims_all_sites")
itSites <- filter(allSites, field_coordinates_lon > 6.7499552751, field_coordinates_lon < 18.4802470232, field_coordinates_lat > 36.619987291, field_coordinates_lat < 47.1153931748)
print(itSites)
plot(itSites)
unlink(fileName)

img <- "http://www.get-it.it/assets/img/loghi/eLTERH2020.png"
p <- leaflet() %>% 
  addTiles() %>% 
  addMouseCoordinates() %>% 
  addLogo(img, 
          url = "http://www.lter-europe.net",
          position = "bottomleft",
          offset.x = 5,
          offset.y = 10,
          width = 100,
          height = 50) %>%
  addCircleMarkers(data = allSites,
                   clusterOptions = markerClusterOptions(),
                   popup = paste0(
                     "Name: <b>", allSites$name, "</b><br>",
                     "UUID: ", allSites$deimsid, "<br>",
                     "<a href=\"", allSites$deimsid, "\" target=\"_blank\">DEIMS-SDR record</a>"
                   )
  ) %>% 
  addHomeButton(ext = extent(allSites), layer.name = "DEIMS-SDR LTER-Italy Sites")

p

htmlwidgets::saveWidget(p, "test.html")
