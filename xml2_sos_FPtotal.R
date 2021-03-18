# install.packages("tidyr")
library("tidyr")
library("leaflet")
library("mapview")
library("raster")
library("sp")
library("sf")
library("stringr")
#install.packages("tsibble")
library("tsibble")
library("xts")
#install.packages("tsbox")
library("tsbox")
library("plotly")
# install.packages("timeSeries")
library("timeSeries")
library("xml2")
library("DT")

# FOI
lterItalyGetFOI <- xml2::read_xml("http://getit.lteritalia.it/observations/service?service=SOS&version=2.0.0&request=GetFeatureOfInterest&observedProperty=http://vocabs.lter-europe.net/EnvThes/20166")
pos <- xml2::xml_find_all(lterItalyGetFOI, ".//gml:pos/text()")
name[l] <- xml2::xml_find_all(lterItalyGetFOI, ".//gml:name/text()")

siteCoords <- do.call(rbind, lapply(pos, function(i) {
  return(data.frame(coords = as.character(i)))
})
) %>% separate(coords, c("lat","long"), sep = " ")

siteName <- do.call(rbind, lapply(name, function(i) {
  return(data.frame(names = as.character(i)))
})
)

sites <- data.frame(siteName, siteCoords) %>% 
    as_tibble() %>% 
    dplyr::mutate(
      LatLong = paste0(lat, ",", long)
    ) %>% 
    # dplyr::select(LatLong) %>% 
    unique()

sp::coordinates(sites) <- cbind(as.numeric(sites$long) , as.numeric(sites$lat))

map <- leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leafem::addMouseCoordinates() %>%
  leaflet::addMarkers(
    data = sites,
    # popup = paste0("Name: <b>", sites$names),
    clusterOptions = leaflet::markerClusterOptions()
  )
map

# Obs
procedure <- xml2::read_xml("http://www.get-it.it/sensors/getit.lteritalia.it/procedure/noOwnerDeclared/noModelDeclared/noSerialNumberDeclared/1286194C-A5DF-11DF-8ED7-1602DFD72090")
ObsProperties <- xml2::xml_attr(xml2::xml_find_all(procedure, ".//sml:output"), "name")

lterItalyGetObs <- xml2::read_xml("http://getit.lteritalia.it/observations/service?service=SOS&version=2.0.0&request=GetObservation&observedProperty=http://vocabs.lter-europe.net/EnvThes/20166")
obsProp <- xml2::xml_attr(xml2::xml_find_first(lterItalyGetObs, ".//om:observedProperty"), "href")
obsPropLab <- xml2::xml_attr(xml2::xml_find_first(lterItalyGetObs, ".//om:observedProperty"), "title")
uomFirst <- xml2::xml_attr(xml2::xml_find_first(lterItalyGetObs, ".//swe:Quantity/swe:uom"), "code")
valuesAll <- xml2::xml_find_all(lterItalyGetObs, ".//swe:values/text()")
numValues <- xml2::xml_find_all(lterItalyGetObs, ".//swe:Count/swe:value/text()")
foiLabels <- xml2::xml_attr(xml2::xml_find_all(lterItalyGetObs, ".//om:featureOfInterest"), "title")

aa <- append(valuesAll, foiLabels) 

calate <- as_tibble(sapply(ObsProperties, function(x) character())) %>% 
  dplyr::mutate(phenomenonTime = lubridate::ymd_hms(phenomenonTime)) %>% 
  dplyr::mutate(foiLabels = as.character())
calate[2:10] <- lapply(calate[2:10], function(x) as.numeric())

for (l in 1:length(valuesAll)) {
  calata <- data.frame(x = unlist(strsplit(as.character(valuesAll[l]), "##"))) %>% 
    tidyr::separate(x, ObsProperties, sep = ";", convert = TRUE) %>% 
    as_tibble() %>% 
    dplyr::mutate(foiLabels = foiLabels[l]) %>% 
    dplyr::mutate(phenomenonTime = lubridate::ymd_hms(phenomenonTime))
  calate <- calate %>% tibble::add_row(calata)
}

calate <- merge(calate, sites, by.x="foiLabels", by.y="names") %>% 
  as_tibble()

ggplot2::ggplot(calate, ggplot2::aes(x = depth, y = water_temp))
