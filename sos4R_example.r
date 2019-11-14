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

CEH <- sos4R::SOS("http://192.171.139.63/observations/service", 
                  binding = "KVP",
                  version = "2.0.0"
                  # dataFieldConverters = myConverters
                  # useDCPs = FALSE
)

# List of offering
sos4R::sosOfferings(CEH)

myOff <- sos4R::sosOfferings(CEH)[['/ECN/T04/RAIN/2/raw/']]

myTemporalFilter <- sos4R::sosCreateEventTimeList(sosCreateTimePeriod(sos = CEH,
                                                                      begin = as.POSIXct("1992-01-01"), #* 180),
                                                                      end = as.POSIXct("1992-01-10")))

jan1992 <- sos4R::getObservation(sos = CEH, 
                                 responseFormat = "http://www.opengis.net/om/2.0", 
                                 offering = myOff, 
                                 verbose = TRUE,
                                 observedProperty = list("http://vocabs.lter-europe.net/EnvThes/USLterCV_443"),
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
