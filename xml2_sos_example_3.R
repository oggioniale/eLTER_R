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

######
# 3th example of connection to SOS v2.0.0 with XML parsing
######

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
map <- leaflet() %>%
  addTiles() %>%
  addMouseCoordinates() %>%
  addCircleMarkers(data = sites,
                   popup = paste0("Name: <b>", sites$names)) %>%
  addHomeButton(ext = extent(sites), layer.name = "SOS Sites")

map

htmlwidgets::saveWidget(map, "test.html")

# Obs
lterItalyGetObs <- read_xml("http://getit.lteritalia.it/observations/service?service=SOS&version=2.0.0&request=GetObservation&procedure=http://www.get-it.it/sensors/getit.lteritalia.it/procedure/noOwnerDeclared/noModelDeclared/noSerialNumberDeclared/1286194C-A5DF-11DF-8ED7-1602DFD72084")
obsProp <- xml_attr(xml_find_first(lterItalyGetObs, ".//om:observedProperty"), "href")
obsPropLab <- xml_attr(xml_find_first(lterItalyGetObs, ".//om:observedProperty"), "title")
uomFirst <- xml_attr(xml_find_first(lterItalyGetObs, ".//swe:Quantity/swe:uom"), "code")
valuesAll <- xml_find_all(lterItalyGetObs, ".//swe:values/text()")
numValues <- xml_find_all(lterItalyGetObs, ".//swe:Count/swe:value/text()")
foiLabels <- xml_attr(xml_find_all(lterItalyGetObs, ".//om:featureOfInterest"), "title")

###
# Table
###
vTemp <- strsplit(as.character(valuesAll[1]), ";")
mTemp <-
  matrix(unlist(vTemp),
         nrow = as.numeric(as.character(numValues[1])),
         byrow = FALSE)
dTemp <- as.data.frame(mTemp)
aTemp <- as.data.frame(str_split_fixed(dTemp$V1, ",", 8))
aTemp <- subset(aTemp, select=c(V1, V8, V2:V7))
colnames(aTemp) <- c('Data & Time', 'depth', 'chla', 'green_algae_chla', 'bluegreen_algae_chla', 'diatom_algae_chla', 'crypto_algae_chla', 'water_temp')
DT::renderDataTable({
  datatable(
    aTemp,
    selection = "single",
    colnames = colnames(aTemp),
    filter = "top",
    rownames = FALSE,
    extensions="Scroller",
    style="bootstrap",
    class="compact",
    # width="100%",
    # height = "100%",
    escape = FALSE,
    options=list(
      pageLength = 10,
      # deferRender=TRUE, 
      # scrollY=300, 
      # scroller=TRUE,
      order = list(1, 'desc')
    )
  )
},
server = FALSE
)

###
# Charts
###
# for (l in 1:length(valuesAll)) {
  vTemp <- strsplit(as.character(valuesAll[1]), ";")
  mTemp <-
    matrix(unlist(vTemp),
           nrow = as.numeric(as.character(numValues[1])),
           byrow = FALSE)
  dTemp <- as.data.frame(mTemp)
  aTemp <- as.data.frame(str_split_fixed(dTemp$V1, ",", 8))
  #aTemp$V2 <- as.numeric(as.character(aTemp$V7))
  colnames(aTemp) <- c('time', 'chla', 'green_algae_chla', 'bluegreen_algae_chla', 'diatom_algae_chla', 'crypto_algae_chla', 'water_temp', 'depth')
  #assign(paste0("tempLTERSites", 1), ts_tsibble(aTemp))
# }

p1 <- plot_ly(
  aTemp,
  x = ~ as.numeric(paste(water_temp)),
  y = ~ as.numeric(paste(depth)),
  color = ~ as.numeric(paste(water_temp))
) %>% layout(
  xaxis = list(title = "water temp (°C)"),
  yaxis = list(title = "depth (m)"))

p2 <- plot_ly(
  aTemp,
  x = ~ as.numeric(paste(chla)),
  y = ~ as.numeric(paste(depth)),
  color = ~ as.numeric(paste(chla))
) %>% layout(
  xaxis = list(title = "chla (ug/l)"),
  yaxis = list(title = "depth (m)"))

subplot(p1, p2, titleX = TRUE, titleY = TRUE) %>%
  layout(showlegend = FALSE,
         title = "Lake Candia - Station 1")


