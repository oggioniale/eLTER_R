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
library("xml2")

######
# 2nd example of connection to SOS v2.0.0 with XML parsing
######

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

map <- leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leafem::addMouseCoordinates() %>%
  leaflet::addCircleMarkers(data = sites,
                   popup = paste0("Name: <b>", sites$names)) #%>%
  # leafem::addHomeButton(ext = extent(sites), layer.name = "SOS Sites")

map

htmlwidgets::saveWidget(map, "testAlesssandro.html")

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
  aTemp$V2 <- as.numeric(as.character(aTemp$V2))
  colnames(aTemp) <- c('time', 'results')
  assign(paste0("tempLTERSites", l), ts_tsibble(aTemp))
}

# Plotly chart of all stations
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
  add_lines(data = tempLTERSites1, name = paste0(as.character(obsPropLab), 'of ', as.character(foiLabels[11]))) %>%
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

# Plotly chart for compare longest time series
plot_ly(
  data = tempLTERSites5,
  x = ~ time,
  y = ~ results,
  type = "scatter",
  mode = "lines",
  name = paste0(as.character(obsPropLab), 'of ', as.character(foiLabels[5]))
) %>%
  add_lines(data = tempLTERSites10, name = paste0(as.character(obsPropLab), 'of ', as.character(foiLabels[10]))) %>%
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

