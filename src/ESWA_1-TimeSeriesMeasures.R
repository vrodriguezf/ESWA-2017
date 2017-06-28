#
# This script process simulation data from the simulator Drone Watch & Rescue (DWR), computing 
# the performance measures associated to each simulation
#
# NOTE: Use this script only if you have access to the DWR database
# 
#

library('ProjectTemplate')
load.project()

##
# INPUT
##
# validSimulations <- data.simulations %>%
#   group_by(simulationID) %>%
#   filter(!dataHelper.isOutlier(simulationID)) %>%
#   ungroup()

validSimulations <- data.simulations %>%
  group_by(simulationID) %>%
  filter(
    mission == 3,
    !outlier.dwr.isSimulationTooShort(simulationID),
    !outlier.dwr.fewInteractions(simulationID),
    !outlier.dwr.noIncidents(simulationID),
    !outlier.dwr.isTutorial(simulationID)
  )

#Same dataset as JUCS!!
#validSimulations <- validSimulations %>% 
 # filter(difftime(make_datetime(year = 2015,month=12L,day=1L),createdAt)>0)

#This calculation takes into account outliers
#MEASURE_RESOLUTION <- dataHelper.getAverageTimeBetweenInteractions(validSimulations) #ms
MEASURE_RESOLUTION <- 2000

print("MEASURE RESOLUTION FOR THIS TIME SERIES = " %+% MEASURE_RESOLUTION)

##
# MAIN
##
simSeries <- dlply(validSimulations, .(simulationID), function (simulation) {
  simulationID <- simulation$simulationID
  duration <- dataHelper.getRealDuration(simulationID)
  print(duration)
  measurePoints <- seq(0,duration,by=MEASURE_RESOLUTION)

  list(
    scoreTS = laply(measurePoints,function (mp) metric.score(simulationID,startTime=0,endTime=mp)),
    agilityTS = laply(measurePoints,function (mp) metric.agility(simulationID,startTime=0,endTime=mp)),
    attentionTS = laply(measurePoints,function (mp) metric.attention(simulationID,startTime=0,endTime=mp)),
    cooperationTS = laply(measurePoints,function (mp) metric.cooperation(simulationID,startTime=0,endTime=mp)),
    aggressivenessTS = laply(measurePoints,function (mp) metric.aggressiveness(simulationID,startTime=0,endTime=mp)),    
    precisionTS = laply(measurePoints,function (mp) metric.precision(simulationID,startTime=0,endTime=mp))
  )
})

#Remove those simulations with NAs in any of the time series
#simSeries <- simSeries[-which(laply(lapply(simSeries,function (simTS) simTS$precisionTS),function (ts) any(is.na(ts))))]

lapply(names(simSeries), function (simulationID) {
  aux <- dataHelper.getSimulationIncidentSnapshots(simulationID)
  aux$elapsedSimulationTime
})

#Output

#SimSeries in cache
save(resultsDf, file = paste("output/ESWA/simSeries","(",Sys.Date(),")",".Rda"))
cache("validSimulations")
cache("simSeries")
