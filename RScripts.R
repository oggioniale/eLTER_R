devtools::install_github("nuest/sos4R", ref = "dev", force = TRUE)
library("sos4R")
library("ggplot2")
library("plotly")
library("xts")
#install.packages("tsbox")
library("tsbox")
#install.packages("tsibble")
library("tsibble")
library("stringr")
library("xml2")

######
# Connection to SOS v2.0.0 with sos4R package
######

CEH <- sos4R::SOS("http://192.171.139.63/observations/service", 
           binding = "KVP", 
           version = "2.0.0"
           # dataFieldConverters = myConverters
           # useDCPs = FALSE
    )

myOff <- sos4R::sosOfferings(CEH)[['/ECN/T04/RAIN/2/raw/']]

myTemporalFilter <- sos4R::sosCreateEventTimeList(sosCreateTimePeriod(sos = CEH,
                                                               begin = as.POSIXct("1992-01-01"), #* 180),
                                                               end = as.POSIXct("1992-01-10")))

jan1992 <- sos4R::getObservation(sos = CEH, 
                             responseFormat = "http://www.opengis.net/om/2.0", 
                             offering = myOff, 
                             verbose = FALSE,
                             observedProperty = list("http://vocabs.lter-europe.net/EnvThes/USLterCV_443"),
                             eventTime = myTemporalFilter
                             )

a <- do.call(rbind, lapply(jan1992, function(obj) {
  return(data.frame(time = obj@phenomenonTime@timePosition@time, results = as.numeric(obj@result)))
  })
  )
# Transformation to R TimeSeries
b <- ts_tsibble(a)

# Plotly chart
plot_ly(data = a, x = ~time, y = ~results, type = "scatter", mode = "lines", name = 'precipitation measure') %>%
  layout(
    xaxis = list(
      title = ""
    ),
    yaxis = list(
      title = "precipitation measure (mm)"
    ),
    legend = list(
      x = 0.9,
      y = 0.9,
      bgcolor = "#FFF",
      bordercolor = "#000",
      borderwidth = 1
    ),
    showlegend = TRUE)


######
# Connection to SOS v2.0.0 with xml2 package
######
OMfile <- read_xml("http://sk.ise.cnr.it/observations/sos/kvp?service=SOS&version=2.0.0&request=GetObservation&offering=offering:http://sp7.irea.cnr.it/sensors/sk.ise.cnr.it/procedure/SAFAS/SAFAS-UVmc2/noSerialNumberDeclared/2015042820079863/observations&observedProperty=http://vocab.nerc.ac.uk/collection/P01/current/MDMAP004/&procedure=http://sp7.irea.cnr.it/sensors/sk.ise.cnr.it/procedure/SAFAS/SAFAS-UVmc2/noSerialNumberDeclared/2015042820079863&featureOfInterest=http://sp7.irea.cnr.it/sensors/sk.ise.cnr.it/foi/SSF/SP/EPSG:4326/46.17583/8.19111&MergeObservationsIntoDataArray=true")
type <- xml_attr(xml_find_all(OMfile, ".//om:type"), "href")
procedure <- xml_attr(xml_find_all(OMfile, ".//om:procedure"), "href")
observedProperty <- xml_attr(xml_find_all(OMfile, ".//om:observedProperty"), "href")
foi <- xml_attr(xml_find_all(OMfile, ".//om:featureOfInterest"), "href")
foiTitle <- xml_attr(xml_find_all(OMfile, ".//om:featureOfInterest"), "title")
uom <- xml_attr(xml_find_all(OMfile, ".//ns:uom"), "code")
values <- xml_find_all(OMfile, ".//ns:values/text()")
numValues <- xml_find_all(OMfile, ".//ns:Count/ns:value/text()")

v <- strsplit(as.character(values), ";")
m <- matrix(unlist(v), nrow = 79, byrow=FALSE)
d <- as.data.frame(m)
a <- as.data.frame(str_split_fixed(d$V1, ",", 2))
colnames(a) <- c('time', 'results')
a$results <- as.double(a$results)
ts_tsibble(a)

# Plotly chart
plot_ly(data = a, x = ~time, y = ~results, type = "scatter", mode = "lines", name = 'precipitation measure') %>%
  layout(
    xaxis = list(
      title = ""
    ),
    yaxis = list(
      title = 'precipitation measure (mm)'
    ),
    legend = list(
      x = 0.9,
      y = 0.9,
      bgcolor = "#FFF",
      bordercolor = "#000",
      borderwidth = 1
    ),
    showlegend = TRUE)


####
# try to use these classes
####
# SensorML
weathersos <- SOS("http://v-swe.uni-muenster.de:8080/WeatherSOS/sos")
proc1 <- sosProcedures(weathersos)[[1]][[1]]
proc1.descr <- describeSensor(weathersos, proc1, verbose = TRUE)
plot(proc1.descr)
class(proc1.descr)
print(proc1.descr)


####
# Accessing WFS with rwfs package
####
library(rwfs)
library(leaflet)
library(mapview)
library(raster)

fileName <- tempfile()
download.file("https://data.lter-europe.net/geoserver/deims/wfs?SERVICE=WFS&VERSION=1.0.0&REQUEST=GetFeature&TYPENAME=deims:deims_all_sites&SRSNAME=EPSG:4326", fileName)
request <- rwfs::GMLFile$new(fileName)
client <- rwfs::WFSCachingClient$new(request)
allSites <- client$getLayer("deims_all_sites")
print(allSites)
plot(allSites)
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
  addHomeButton(ext = extent(allSites), layer.name = "DEIMS-SDR All Sites")

htmlwidgets::saveWidget(p, "test.html")


####
# QGIS Connection
####

# install.packages("RQGIS")
library("RQGIS")

#Example from https://www.youtube.com/watch?v=rGNy3ux-LPc
set_env(dev = FALSE)
# search all algoritms in the QGIS
find_algorithms(search_term = "")
find_algorithms("intersec", name_only = TRUE)

get_usage("qgis:intersection")
get_usage("grass7:r.slope.aspect")


######
# 2nd example of connection to SOS v2.0.0 with XML parsing
######
# install.packages("tidyr")
library(tidyr)
library(leaflet)
library(mapview)
library(raster)
library(sp)
library(sf)
library("stringr")
#install.packages("tsibble")
library("tsibble")
library("xts")
#install.packages("tsbox")
library("tsbox")
library("plotly")

# FOI
lterItalyGetFOI <- read_xml("http://getit.lteritalia.it/observations/service?service=SOS&version=2.0.0&request=GetFeatureOfInterest&observedProperty=http://vocabs.lter-europe.net/EnvThes/22035")
pos <- xml_find_all(lterItalyGetFOI, ".//gml:pos/text()")
name <- xml_find_all(lterItalyGetFOI, ".//gml:name/text()")

siteCoords <- do.call(rbind, lapply(pos, function(i) {
  return(data.frame(coords = as.character(i)))
})
) %>% separate(coords, c("lat","long"), sep = " ")

siteName <- do.call(rbind, lapply(name, function(i) {
  return(data.frame(names = as.character(i)))
})
)

sites <- data.frame(siteName, siteCoords)

coordinates(sites) <- cbind(as.numeric(sites$long) , as.numeric(sites$lat))

leaflet() %>%
  addTiles() %>%
  addMouseCoordinates() %>%
  addCircleMarkers(data = sites,
                   popup = paste0("Name: <b>", sites$names)) %>%
  addHomeButton(ext = extent(sites), layer.name = "SOS Sites")

# Obs
lterItalyGetObs <- read_xml("http://getit.lteritalia.it/observations/service?service=SOS&version=2.0.0&request=GetObservation&observedProperty=http://vocabs.lter-europe.net/EnvThes/22035")
obsProp <- xml_attr(xml_find_first(lterItalyGetObs, ".//om:observedProperty"), "href")
obsPropLab <- xml_attr(xml_find_first(lterItalyGetObs, ".//om:observedProperty"), "title")
uomFirst <- xml_attr(xml_find_first(lterItalyGetObs, ".//swe:Quantity/swe:uom"), "code")
valuesAll <- xml_find_all(lterItalyGetObs, ".//swe:values/text()")
numValues <- xml_find_all(lterItalyGetObs, ".//swe:Count/swe:value/text()")
foiLabels <- xml_attr(xml_find_all(lterItalyGetObs, ".//om:featureOfInterest"), "title")

for (l in 1:length(valuesAll)) {
  vTemp <- strsplit(as.character(valuesAll[l]), ";")
  mTemp <-
    matrix(unlist(vTemp),
           nrow = as.numeric(as.character(numValues[l])),
           byrow = FALSE)
  dTemp <- as.data.frame(mTemp)
  aTemp <- as.data.frame(str_split_fixed(dTemp$V1, ",", 2))
  aTemp$V2 <- as.numeric(aTemp$V2)
  colnames(aTemp) <- c('time', 'results')
  assign(paste0("tempLTERSites", l), ts_tsibble(aTemp))
}

# Plotly chart
plot_ly(
  data = tempLTERSites1,
  x = ~ time,
  y = ~ results,
  type = "scatter",
  mode = "lines",
  name = paste0(as.character(obsPropLab), 'of ', as.character(foiLabels[1]))
) %>%
  add_lines(data = tempLTERSites2, name = paste0(as.character(obsPropLab), 'of ', as.character(foiLabels[2]))) %>%
  add_lines(data = tempLTERSites3, name = paste0(as.character(obsPropLab), 'of ', as.character(foiLabels[3]))) %>%
  add_lines(data = tempLTERSites4, name = paste0(as.character(obsPropLab), 'of ', as.character(foiLabels[4]))) %>%
  add_lines(data = tempLTERSites5, name = paste0(as.character(obsPropLab), 'of ', as.character(foiLabels[5]))) %>%
  add_lines(data = tempLTERSites6, name = paste0(as.character(obsPropLab), 'of ', as.character(foiLabels[6]))) %>%
  add_lines(data = tempLTERSites7, name = paste0(as.character(obsPropLab), 'of ', as.character(foiLabels[7]))) %>%
  add_lines(data = tempLTERSites8, name = paste0(as.character(obsPropLab), 'of ', as.character(foiLabels[8]))) %>%
  add_lines(data = tempLTERSites9, name = paste0(as.character(obsPropLab), 'of ', as.character(foiLabels[9]))) %>%
  add_lines(data = tempLTERSites10, name = paste0(as.character(obsPropLab), 'of ', as.character(foiLabels[10]))) %>%
  add_lines(data = tempLTERSites11, name = paste0(as.character(obsPropLab), 'of ', as.character(foiLabels[11]))) %>%
  layout(
    xaxis = list(title = ""),
    yaxis = list(title = paste0(
      as.character(obsPropLab), ' (', as.character(uomFirst), ')'
    )),
    legend = list(
      #x = 0.9,
      #y = 0.9,
      bgcolor = "#FFF",
      bordercolor = "#000",
      borderwidth = 1
    ),
    showlegend = TRUE
  )

######
# 3th example of connection to SOS v2.0.0 with XML parsing
######
# install.packages("tidyr")
library(tidyr)
library(leaflet)
library(mapview)
library(raster)
library(sp)
library(sf)
library("stringr")
#install.packages("tsibble")
library("tsibble")
library("xts")
#install.packages("tsbox")
library("tsbox")
library("plotly")
# install.packages("timeSeries")
library("timeSeries")

# FOI
lterItalyGetFOI <- read_xml("http://getit.lteritalia.it/observations/service?service=SOS&version=2.0.0&request=GetFeatureOfInterest&procedure=http://www.get-it.it/sensors/getit.lteritalia.it/procedure/noOwnerDeclared/noModelDeclared/noSerialNumberDeclared/1286194C-A5DF-11DF-8ED7-1602DFD72084")
pos <- xml_find_all(lterItalyGetFOI, ".//gml:pos/text()")
name <- xml_find_all(lterItalyGetFOI, ".//gml:name/text()")

siteCoords <- do.call(rbind, lapply(pos, function(i) {
  return(data.frame(coords = as.character(i)))
})
) %>% separate(coords, c("lat","long"), sep = " ")

siteName <- do.call(rbind, lapply(name, function(i) {
  return(data.frame(names = as.character(i)))
})
)

sites <- data.frame(siteName, siteCoords)

coordinates(sites) <- cbind(as.numeric(sites$long) , as.numeric(sites$lat))
crs(sites) <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"
leaflet() %>%
  addTiles() %>%
  addMouseCoordinates() %>%
  addCircleMarkers(data = sites,
                   popup = paste0("Name: <b>", sites$names)) %>%
  addHomeButton(ext = extent(sites), layer.name = "SOS Sites")

# Obs
lterItalyGetObs <- read_xml("http://getit.lteritalia.it/observations/service?service=SOS&version=2.0.0&request=GetObservation&observedProperty=http://vocabs.lter-europe.net/EnvThes/20166")
obsProp <- xml_attr(xml_find_first(lterItalyGetObs, ".//om:observedProperty"), "href")
obsPropLab <- xml_attr(xml_find_first(lterItalyGetObs, ".//om:observedProperty"), "title")
uomFirst <- xml_attr(xml_find_first(lterItalyGetObs, ".//swe:Quantity/swe:uom"), "code")
valuesAll <- xml_find_all(lterItalyGetObs, ".//swe:values/text()")
numValues <- xml_find_all(lterItalyGetObs, ".//swe:Count/swe:value/text()")
foiLabels <- xml_attr(xml_find_all(lterItalyGetObs, ".//om:featureOfInterest"), "title")

for (l in 1:length(valuesAll)) {
  vTemp <- strsplit(as.character(valuesAll[l]), ";")
  mTemp <-
    matrix(unlist(vTemp),
           nrow = as.numeric(as.character(numValues[l])),
           byrow = FALSE)
  dTemp <- as.data.frame(mTemp)
  aTemp <- as.data.frame(str_split_fixed(dTemp$V1, ",", 8))
  aTemp$V2 <- as.numeric(aTemp$V7)
  colnames(aTemp) <- c('time', 'chla', 'green_algae_chla', 'bluegreen_algae_chla', 'diatom_algae_chla', 'crypto_algae_chla', 'water_temp', 'depth')
  assign(paste0("tempLTERSites", l), ts_tsibble(aTemp))
}

p1 <- plot_ly(
  tempLTERSites1,
  x = ~ as.numeric(paste(water_temp)),
  y = ~ as.numeric(paste(depth)),
  color = ~ as.numeric(paste(water_temp))
) %>% layout(
  xaxis = list(title = "water temp (°C)"),
  yaxis = list(title = "depth (m)"))

p2 <- plot_ly(
  tempLTERSites1,
  x = ~ as.numeric(paste(chla)),
  y = ~ as.numeric(paste(depth)),
  color = ~ as.numeric(paste(chla))
) %>% layout(
  xaxis = list(title = "chla (ug/l)"),
  yaxis = list(title = "depth (m)"))

subplot(p1, p2, titleX = TRUE, titleY = TRUE) %>%
  layout(showlegend = FALSE,
    title = "Lake Candia - Station 1")
