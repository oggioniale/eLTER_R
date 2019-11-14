library(rwfs)
# install.packages("leaflet")
library(leaflet)
# install.packages("mapview")
library(mapview)
# install.packages("raster")
library(raster)
library(dplyr)
library(tidyr)
library(sp)

lterSite9555GetFOI <- read_xml("https://deims.org/node/9555/emf")
pos <- xml_find_all(lterSite9555GetFOI, ".//gml:pos/text()")[1]
name <- xml_find_all(lterSite9555GetFOI, ".//ef:name/text()")
deimsID <- xml_find_all(lterSite9555GetFOI, ".//ef:onlineResource/text()")[1]
poly <- as.character(xml_find_all(lterSite9555GetFOI, ".//gml:Polygon/gml:exterior/gml:LinearRing/gml:posList/text()"))
matPoly <- matrix(as.numeric(strsplit(poly," ")[[1]]), ncol=2, byrow=TRUE)
matPoly[ , c(1,2)] <- matPoly[ , c(2,1)]
polygonSite <- Polygons(list(Polygon(matPoly)),"s1")

siteCoords <- do.call(rbind, lapply(pos, function(i) {
  return(data.frame(coords = as.character(i)))
})
) %>% separate(coords, c("lat", "long"), sep = " ")

siteName <- do.call(rbind, lapply(name, function(i) {
  return(data.frame(names = as.character(i)))
})
)

site9555 <- data.frame(siteName, siteCoords)

coordinates(site9555) <- cbind(as.numeric(site9555$long) , as.numeric(site9555$lat))
crs(site9555) <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"
img <- "http://www.get-it.it/assets/img/loghi/eLTERH2020.png"
map <- leaflet() %>%
  addTiles() %>%
  addMouseCoordinates() %>%
  addLogo(img, 
          url = "http://www.lter-europe.net",
          position = "bottomleft",
          offset.x = 5,
          offset.y = 10,
          width = 100,
          height = 50) %>%
  addCircleMarkers(data = site9555,
                   popup = paste0(
                     "Name: <b>", name, "</b><br>",
                     "UUID: ", deimsID, "<br>",
                     "<a href=\"", deimsID, "\" target=\"_blank\">DEIMS-SDR record</a>"
                   )) %>%
  addPolygons(data = polygonSite, color = "blue", weight = 4, 
              smoothFactor = 0.5, opacity = .8, fillOpacity = 0.3, fillColor = 'blue',
              popup = paste0(
                "Name: <b>", name, "</b><br>",
                "UUID: ", deimsID, "<br>",
                "<a href=\"", deimsID, "\" target=\"_blank\">DEIMS-SDR record</a>"
              )) %>% 
  addHomeButton(ext = extent(site9555), layer.name = paste0(name, " (LTER-Belgium)"))

map


