# tsm site/station
sos_site <- "ZOE_0551P00"
# tsm parameter   
sos_parameter <- "SurfaceWaterConcentration_DOC_0.2m"
# beginning of study period
sos_startperiod <- "2018-04-20TT00:00:00+01:00"

library(remotes)
library(sos4R)
library(lubridate)
  
.verbose <- FALSE
.saveOriginal <- FALSE
.version <- "2.0.0"
.binding <- "KVP"
.responseFormat <- "http://www.opengis.net/om/2.0"
.sos <- "http://ltercwn01.umweltbundesamt.at:8080/cwn.all.sos2/service"
sos <- SOS(url = .sos, version = .version, verboseOutput = .verbose, binding = .binding)

.procedure <- sos_site
  
myoffering <- sosOfferings(sos)[[sos_site]]

.observedProperty <- list(sos_parameter)

sos_startperiod <- as_datetime(sos_startperiod)
#sos_endperiod <- as_datetime(sos_endperiod)

sos_startperiod_utc <- floor_date(sos_startperiod,unit="month")

source <- sos_startperiod_utc
target <- source+months(1)-seconds(1)

period <- sosCreateTimePeriod(sos = sos,
                              begin = source,
                              end = target)
.eventTime <- sosCreateEventTimeList(period)

myGetObservation <- getObservation(sos = sos,
                                   offering = sosOfferings(sos)[[.procedure]],
                                   observedProperty = .observedProperty,
                                   responseFormat = .responseFormat,
                                   eventTime = .eventTime,
                                   verbose = .verbose,
                                   saveOriginal = .saveOriginal)

restab <- t(data.frame(rep(NA,8)))
colnames(restab) <- c("procedure","phentime","obsprop","result","resTime","resQual","parameter","metadata")

for (k in c(1:dim(summary(myGetObservation))[1])) {
  
  res00 <- data.frame(procedure=toString(myGetObservation[[k]]@procedure))
  res00$phentime <- toString(myGetObservation[[k]]@phenomenonTime)
  res00$obsprop <-  unlist(myGetObservation[[k]]@observedProperty@href)
  res00$result <- toString(myGetObservation[[k]]@result)
  res00$resTime <- toString(myGetObservation[[k]]@resultTime)
  res00$resQual <- toString(myGetObservation[[k]]@resultQuality)
  res00$parameter <- toString(myGetObservation[[k]]@parameter)
  res00$metadata <- toString(myGetObservation[[k]]@metadata)
  
  restab <- rbind(restab,res00)
}

restab
