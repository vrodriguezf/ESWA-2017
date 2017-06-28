#
# Input: SimulationID (String)
# Output: Dataframe containing all the simulation snapshots representing user interactions on that simulation
#
dataHelper.getSimulationInteractions = function (simulationID,startTime=NULL,endTime=NULL) 
{
  aux <- data.simulationSnapshots %>%
    filter(simulation == simulationID,
           cause.id == SnapshotCauses$USER_INPUT,
           elapsedRealTime!=0,
           !is.na(cause.params.inputId))
  
  if (!is.null(startTime)) {
    aux <- aux %>% filter(elapsedRealTime>=startTime)
  }
  if (!is.null(endTime)) {
    aux <- aux %>% filter(elapsedRealTime<=endTime)
  }
  return(aux)
}

###
#
##
dataHelper.getFirstSimulationSnapshot = function (simulationID) 
{
  return(
    data.simulationSnapshots %>%
      filter(simulation == simulationID) %>%
      filter(elapsedRealTime == min(elapsedRealTime)) %>%
      head(n=1)
  )
}

###
#
##
dataHelper.getLastSimulationSnapshot = function (simulationID) 
{
  return(
    data.simulationSnapshots %>%
      filter(simulation == simulationID) %>%
      filter(elapsedRealTime == max(elapsedRealTime)) %>%
      head(n=1)
  )
}

##
#
##
dataHelper.getFirstDroneSnapshots = function (simulationID) {
  return(
    data.droneSnapshots %>%
      filter(simulation == simulationID) %>%
      filter(elapsedRealTime == min(elapsedRealTime))
      %>% head(n=1)
  )  
}

##
#
##
dataHelper.getLastDroneSnapshots = function (simulationID) {
  return(
    data.droneSnapshots %>%
      filter(simulation == simulationID) %>%
      filter(elapsedRealTime == max(elapsedRealTime))
  )  
}

##
#
##
dataHelper.getSimulationIncidentSnapshots = function (simulationID) {
  return(
    data.simulationSnapshots %>% 
      filter(simulation == simulationID,
             cause.id == SnapshotCauses$INCIDENT_STARTED)
  )
}

##
#
##
dataHelper.getWaypointInteractions = function (simulationID,startTime=0,endTime=Inf) {
  return(
    data.simulationSnapshots %>%
      filter(simulation == simulationID,
             cause.id == SnapshotCauses$USER_INPUT,
             cause.params.inputId == UserInputs$CHANGE_DRONE_PATH,
             elapsedRealTime >= startTime,
             elapsedRealTime <= endTime
      )
  )  
}

dataHelper.isOutlier = function (simulationID) {
    
  return(
      (((dataHelper.getLastSimulationSnapshot(simulationID))$elapsedRealTime[[1]]) < constants.OUTLIER_TIME_THRESHOLD) ||
        (nrow(dataHelper.getSimulationInteractions(simulationID = simulationID)) < Constants$MIN_INTERACTIONS_PER_SIMULATION)
    )
}

#
# NOTE: Include the parameter ss as last row or not? (< or <=)
#
dataHelper.getPreviousSimulationSnapshots = function (ss) {
  return(
      data.simulationSnapshots %>%
        filter(simulation == ss$simulation,elapsedRealTime < ss$elapsedRealTime) %>%
        arrange(elapsedRealTime)
    )
}

##
# Get the current control mode given a simulation snapshot
##
dataHelper.getControlMode = function (ss) {
  #1. Get the previous snapshots (ordered in time) of the given ss
  previousInteractions <- dataHelper.getPreviousSimulationSnapshots(ss) %>%
                    filter(cause.id == SnapshotCauses$USER_INPUT)
  
  lastUAVSelection <- previousInteractions %>%
                      filter(cause.params.inputId == UserInputs$SELECT_DRONE) %>%
                      tail(n=1)
  
  #If there is no uav selection the control mode is MONITOR
  if (nrow(lastUAVSelection) == 0) return(ControlModes$MONITOR)
    
  #Find the last SET_CONTROL_MODE interaction between the last UAV selection and the given snapshot
  lastControlModeSelection <- previousInteractions %>%
                              filter(elapsedRealTime > lastUAVSelection$elapsedRealTime,
                                     cause.params.inputId == UserInputs$SET_CONTROL_MODE) %>%
                              tail(n=1)
    
  #If there is no control mode selection, we are monitoring
  if (nrow(lastControlModeSelection) == 0) return(ControlModes$MONITOR)
  else {
    return(lastControlModeSelection$cause.params.controlMode.id)
  }
}

##
# Get current selected drone of a given simulation snapshot
##
dataHelper.getCurrentSelectedUAV <- function (ss,snapsDf) {
  #1. Get the previous snapshots (ordered in time) of the given ss
  previousInteractions <- dataHelper.getPreviousSimulationSnapshots(ss,snapsDf) %>%
    filter(cause.id == SnapshotCauses$USER_INPUT)
  
  lastUAVSelection <- previousInteractions %>%
    filter(cause.params.inputId == UserInputs$SELECT_DRONE) %>%
    tail(n=1)
  
  #If there is no uav selection the current selected UAV is null!
  if (nrow(lastUAVSelection) == 0 || lastUAVSelection$elapsedRealTime == 0) return(NULL)
  
  #Return the last drone selected ID
  return(lastUAVSelection$cause.params.droneId)
  
}

##
#
##
dataHelper.getRealDuration = function(simulationID) {
  return(
      (dataHelper.getLastSimulationSnapshot(simulationID))$elapsedRealTime - 
        (dataHelper.getFirstSimulationSnapshot(simulationID))$elapsedRealTime
    )
}

##
# Get planning duration (Only for planning missions)
##
dataHelper.getPlanningDuration = function (simulationID) {
  
  planningTime <- NULL
  
  #Get the duration of the simulation
  simulationDuration <- dataHelper.getRealDuration(simulationID)
  
  # The threshold between planning and replanning is considered as the moment when the user acceleartes the simulation
  # after making some changes
  timeControls <- data.simulationSnapshots %>%
                    filter(simulation==simulationID,
                           cause.id==SnapshotCauses$USER_INPUT,
                           cause.params.inputId==UserInputs$SET_SIMULATION_TIME_RATIO) %>%
                    arrange(elapsedRealTime)
  
  #If there has been no time control, we consider all the simulation as a planning time
  if (nrow(timeControls) == 0) return(simulationDuration)
  
  n <- 1
  finished <- FALSE
  waypointInteractions <- dataHelper.getWaypointInteractions(simulationID)
  
  while(!finished && n<=nrow(timeControls)) {
    timeThreshold <- (timeControls[n,])$elapsedRealTime
    #check if there are waypoint interactions before this acceleration
    waypointInteractionsBeforeAcceleration <- waypointInteractions %>%
                                              filter(elapsedRealTime < timeThreshold)
        
    if (nrow(waypointInteractionsBeforeAcceleration) > 0 ) {
      finished = TRUE
    }
    else {
      n <- n+1
    }
  }
  
  if (!finished) return(0)
  else {
    return(timeControls[n,]$elapsedRealTime)
  }
  
}

##
# GEt the moment (Real time) in which the simulation begins to run with some speed
##
dataHelper.getMonitoringStartTime <- function (simulationID) {
  timeControls <- data.simulationSnapshots %>%
    filter(simulation==simulationID,
           cause.id==SnapshotCauses$USER_INPUT,
           cause.params.inputId==UserInputs$SET_SIMULATION_TIME_RATIO) %>%
    arrange(elapsedRealTime)
  
  #If there has been no time control, we consider all the simulation as a planning time
  if (nrow(timeControls) == 0) return(dataHelper.getRealDuration(simulationID))
  
  return(timeControls[1,]$elapsedRealTime)
}

#
# OUTPUT : Vector with timegeneral time betwen interactions in DWR
#
dataHelper.getAverageTimeBetweenInteractions <- function (sims) {
  
  getAvgTBIBySimulation <- function (simulationID) {
    
    interactions <- dataHelper.getSimulationInteractions(simulationID)
    return(median(diff(interactions$elapsedRealTime)))
  }
  
  #Remove outliers for the process
  sims %>%
    group_by(simulationID) %>%
    filter(!dataHelper.isOutlier(simulationID)) %>%
    summarise(aux=getAvgTBIBySimulation(simulationID)) %>%
    summarise(median(aux)) %>%
    as.numeric
}