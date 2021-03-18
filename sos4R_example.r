#devtools::install_github("nuest/sos4R", ref = "dev", force = TRUE)
#devtools::install_github("nuest/sos4R", ref = "bbea3e182538e9a748480ed4ede9fa87a65f350b", force = TRUE)
library("sos4R")
library("ggplot2")
library("plotly")
library("tsbox")
library("tsibble")

######
# Connection to SOS v2.0.0 with sos4R package
######
GETIT_LTERItaly <- sos4R::SOS("http://getit.lteritalia.it/observations/service", 
                  binding = "KVP",
                  version = "2.0.0"
                  # dataFieldConverters = myConverters
                  # useDCPs = FALSE
)

# List of offering
sos4R::sosOfferings(GETIT_LTERItaly)

myOff <- sos4R::sosOfferings(GETIT_LTERItaly)[['offering:http://www.get-it.it/sensors/getit.lteritalia.it/procedure/noOwnerDeclared/noModelDeclared/noSerialNumberDeclared/1286194C-A5DF-11DF-8ED7-1602DFD72084/observations']]

myTemporalFilter <- sos4R::sosCreateEventTimeList(sos4R::sosCreateTimePeriod(sos = GETIT_LTERItaly,
                                                                      begin = as.POSIXct("2005-05-31"), #* 180),
                                                                      end = as.POSIXct("2008-01-31")))

jan1992 <- sos4R::getObservation(sos = GETIT_LTERItaly, 
                                 #responseFormat = "http://www.opengis.net/om/2.0", 
                                 offering = myOff, 
                                 # verbose = TRUE,
                                 # observedProperty = list("http://vocabs.lter-europe.net/EnvThes/20166"),
                                 eventTime = myTemporalFilter)

a <- do.call(rbind, lapply(jan1992, function(obj) {
  return(data.frame(time = obj@phenomenonTime@timePosition@time, results = as.numeric(obj@result)))
})
)
# Transformation to R TimeSeries
b <- ts_tsibble(a)

# Plotly chart
plotly::plot_ly(data = a, x = ~time, y = ~results, type = "scatter", mode = "lines", name = 'precipitation measure') %>%
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
