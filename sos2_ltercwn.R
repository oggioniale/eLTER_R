install.packages('remotes')
library('remotes')
devtools::install_github("52North/sos4R","feature/0.4")
library('sos4R')

.verbose <- FALSE
.saveOriginal <- FALSE
.version <- "SOS_2.0.0"
.binding <- "KVP"
.responseFormat <- "http://www.opengis.net/om/2.0"
.procedure <- "ZOE_0551P00"
.observedProperty <- list("SurfaceWaterConcentration_DOC_0.2m")
.sos <- "http://ltercwn01.umweltbundesamt.at:8080/cwn.all.sos2/service"
.temporalFilter <- "2017-12-20T17:45:15+02:00/2017-12-24T17:45:15+02:00"

sos <- SOS(url = .sos, 
           # version = .version, 
           verboseOutput = .verbose, binding = .binding)
myGetObservation <- getObservation(sos = sos,
                                   offering = sosOfferings(sos)[[.procedure]],
                                   observedProperty = .observedProperty,
                                   responseFormat = .responseFormat,
                                   eventTime = sosCreateTime(sos = sos, time = .temporalFilter),
                                   verbose = .verbose,
                                   saveOriginal = .saveOriginal)
summary(myGetObservation)
myClass <- class(myGetObservation[[1]])
toString(myClass)
myResultClass <- class(myGetObservation[[1]]@result)
toString(myResultClass)

