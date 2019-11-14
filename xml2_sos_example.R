#install.packages("xml2")
library("xml2")
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
m <- matrix(unlist(v), nrow = as.numeric(as.character(numValues)), byrow=FALSE)
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
