########################################
#### METRICS
########################################

##
# AGILITY
#
# Computes the agility of a simulation as an average of the simulation speed for all the interactions
# performed during the simulation
#
# Input : SimulationID: Identifies the simulation 
# Output: A number between [0,1] representing the simulation agility
##
metric.agility = function (simulationID,startTime=NULL,endTime=NULL) 
{
  # Filter the snapshots dataframe to get only the interactions
  simInteractions <- dataHelper.getSimulationInteractions(simulationID,startTime,endTime)
  
  if (nrow(simInteractions) == 0) return(0)
  
  #Compute the average of the interaction speed (normalized with the maximum speed)
  result <- mean(simInteractions$simulationSpeed/constant.MAX_SIMULATION_SPEED)
  
  return(result)
}


##
# CONSUMPTION
#
# Computes the resource consumption of a simulation, taking account of the fuel consumption and the UAVs used
# during the simulation time
#
# Input : SimulationID: Identifies the simulation 
# Output: A number between [0,1] representing the simulation resource consumption
##
metric.consumption = function (simulationID,startTime=NULL,endTime=NULL) {
  
  # 1. Amount of fuel at the start of the mission
  dronesInitFuel <- dataHelper.getFirstDroneSnapshots(simulationID) %>%
                              group_by(droneId) %>%
                              summarise(initFuel = max(remainingFuel))
  
  missionEnvironment <- missionHelper.getSimulationEnvironment(simulationID)
  missionRefuelingStations <- missionEnvironment$refueling_stations
  missionUAVS <- missionEnvironment$uavs
  
  initFuel <- sum(dronesInitFuel$initFuel) + sum(missionRefuelingStations$fuel_capacity)
  
  # 2. Amount of fuel at the end of the missions
  
  # 2.1. Amount of fuel at the end of the simulation on the drones (still alive)
  dronesFinalFuel <- dataHelper.getLastDroneSnapshots(simulationID) %>%
                                group_by(droneId) %>%
                                summarise(finalFuel = min(remainingFuel))
  
  
  # 2.2 Amount of fuel consumed in refueling actions (any refueling station)
  #NOTE: We are supposing that infor each refueling, the drone gets its max fuel value again (the data log
  #doesn't give us the exact amount of fuel charged')
  refuelings <- students.droneSnapshots %>%
                        filter(simulation == simulationID,
                               cause.id == SnapshotCauses$ACTION_STARTED,
                               cause.params.id == 'refueling',
                               status == 2) %>%
                        merge(
                            missionUAVS,
                            by.x="droneId",
                            by.y="uavs_id"
                          ) %>%
                        dplyr::select(droneId,droneSnapshotID,remainingFuel,max_fuel)
  
  endFuel <- sum(dronesFinalFuel$finalFuel) + sum(missionRefuelingStations$fuel_capacity) - sum(refuelings$max_fuel-refuelings$remainingFuel)
  
  return(endFuel/initFuel)
}

##
# SCORE
#
# Computes the score obtained for a simulation, in terms of the number of targets detected and the number of
# survining drones.
#
# Input : SimulationID: Identifies the simulation 
# Output: A number between [0,1] representing the simulation score
##
metric.score = function (simulationID,startTime=0,endTime=Inf)
{
  targetsDetected <- data.simulationSnapshots %>% filter(simulation == simulationID,
                                                             cause.id==SnapshotCauses$TARGET_DETECTED,
                                                              elapsedRealTime >= startTime,
                                                              elapsedRealTime <= endTime
                                                             )
  dronesDestroyed <- data.simulationSnapshots %>% filter(simulation == simulationID,
                                                            cause.id == SnapshotCauses$DRONE_DESTROYED,
                                                            elapsedRealTime >= startTime,
                                                            elapsedRealTime <= endTime)
  
  #firstDroneSnapshots <- dataHelper.getFirstDroneSnapshots(simulationID)
  
  #Get the mission Info necessary from the first drone snapshots
  missionUAVs <- missionHelper.getEnvironmentById(missionHelper.getMissionID(simulationID))$uavs
  missionTargets <- missionHelper.getTargetsDefinitionById(missionHelper.getMissionID(simulationID))$targets
  
  #Calculate UAVs that have returned to base at the end of the simulation
#   airportPositions <- missionHelper.getSimulationEnvironment(simulationID)$airports$position
#   lastDronePositions <- dataHelper.getLastDroneSnapshots(simulationID) %>%
#                         group_by(droneId) %>%
#                         summarise(
#                             position.x=first(position.x),
#                             position.y=first(position.y)
#                           )
#   baseSnapshots <- merge(airportPositions,lastDronePositions,
#                          by.x=c("x","y"),
#                          by.y=c("position.x","position.y")
#                          )
        
  return(
      (
        (nrow(targetsDetected)/nrow(missionTargets)) + 
          (1- nrow(dronesDestroyed)/nrow(missionUAVs)) 
          #(nrow(baseSnapshots)/nrow(firstDroneSnapshots))
        )/
        2
    )
}

##
# ATTENTION LEVEL
#
# Computes the attention of a player into a simulation, evaluating the number of interactions
# he has performed
#
# Input : SimulationID: Identifies the simulation 
# Output: A number between [0,1] representing the simulation attention level
##
metric.attention = function (simulationID,startTime=0,endTime=Inf)
{ 
  return( 
      1 - 1/(sqrt(nrow(dataHelper.getSimulationInteractions(simulationID,startTime,endTime))+1))  
    )
}

##
# PRECISION
#
# Computes the attention of a player into a simulation, evaluating the number of interactions
# he has performed for each incident
#
# Input : SimulationID: Identifies the simulation 
# Output: A number between [0,1] representing the simulation precision
##
metric.precision = function (simulationID,startTime=0,endTime=Inf) 
{
  
  ##
  # INCIDENT PRECISION
  ##
  #Get the snapshots of the triggered incidents during this simulation
  incidentsTriggered <- dataHelper.getSimulationIncidentSnapshots(simulationID) %>%
    dplyr::rename(incidentID = cause.params.incidentId,
           realTriggerInstant = elapsedRealTime,
           simulationTriggerInstant = elapsedSimulationTime) %>% ## TODO
    dplyr::select(incidentID,realTriggerInstant,simulationTriggerInstant)
  
  
  #Extreme case - No incidents triggered during the simulation (Omit this metric - Not available)
  if (nrow(incidentsTriggered) == 0) {
    return(NA)
  }
  
  waypointInteractions <- dataHelper.getWaypointInteractions(simulationID,startTime,endTime)
  
  # Create a list containing, for each incident, a dataframe containing all the waypoint interactions made to
  # overcome that incident
  waypointInteractionsByIncident <- dlply(incidentsTriggered,
                                     .(incidentID),
                                     function (dfRow) 
                                       (waypointInteractions %>% 
                                          filter(elapsedRealTime >= dfRow$realTriggerInstant,
                                                 elapsedRealTime <= dfRow$realTriggerInstant + constant.INCIDENT_ACTION_THRESHOLD)
                                        )
                                     )
  
  #Calculate theiuncident precision counting the interactions for each incident
  incidentPrecision <- Reduce(sum,
                              llply(waypointInteractionsByIncident, 
                                    function (e) (
                                        1 - (1/(nrow(e) + 1))
                                      )
                                    )
                              )/
                      (length(waypointInteractionsByIncident) + 1) # +1 para evitar denominador a 0
  
  
  ##
  # MONITORING PRECISION
  ##
  
  #Calculate the monitoring precision penalizing the waypoint interactions outside incidents time
  monitoringPrecision = 1/(1 + nrow(helper.dataFrameDiff(waypointInteractions,
                                                         unique(do.call(rbind, waypointInteractionsByIncident)))))
  
  #The final precision will be an average between the two types of precision (incident, monitoring)
  return(
    (incidentPrecision + monitoringPrecision)/2
    )

  #return(monitoringPrecision)
}

##
# AGRESSIVENESS
#
# Computes the agressiveness of a player, based on the type of waypoint movements the have performed
#
# Input : SimulationID: Identifies the simulation 
# Output: A number between [0,1] representing the simulation precision
##
metric.aggressiveness = function (simulationID,startTime=0,endTime=Inf) 
{
  #PARAMETERS
  alpha <- 1
  beta <- 0.5
  gamma <- 0.15
  
  #Get the set of waypoint interactions for that simulation
  waypointInteractions <- dataHelper.getWaypointInteractions(simulationID,startTime,endTime)
  
  if (nrow(waypointInteractions) == 0) return(0)
  
  # Distinguish the type of interactions being made
  aux <- ddply(waypointInteractions,.(`_id`),function (x) {dataHelper.getControlMode(x)}) %>%
    dplyr::rename(controlMode=V1)
  
  return(
      (alpha*nrow(aux %>% filter(controlMode==ControlModes$MANUAL)) + 
        beta*nrow(aux %>% filter(controlMode==ControlModes$ADD_WAYPOINT)) + 
        gamma*nrow(aux %>% filter(controlMode==ControlModes$MONITOR))) /
      nrow(waypointInteractions)
      )
}

##
# COOPERATION
#
# Computes, for a multiUAV mission, to what extent the user has interacted with all the UAVs equally
#
# Input : SimulationID: Identifies the simulation 
# Output: A number between [0,1] (1 = best, 0 = worst)
##
metric.cooperation = function (simulationID,startTime=0,endTime=Inf) {
  missionUAVs <- (missionHelper.getEnvironmentById(missionHelper.getMissionID(simulationID)))$uavs
  
  # TODO: Control error for single-UAV missions??
  if (nrow(missionUAVs) <= 1) {
    return(0)
  }
  
  #Interactions per UAV
  aux <- dataHelper.getSimulationInteractions(simulationID,startTime,endTime) %>%
    filter(cause.params.droneId != "") %>%
    group_by(cause.params.droneId) %>%
    dplyr::summarise(interactionCount=n()) %>% 
    merge(missionUAVs, all.y =TRUE, by.x="cause.params.droneId",by.y="uavs_id") %>%
    dplyr::select(cause.params.droneId,interactionCount)
  
  #replace NA's by 0
  aux[is.na(aux)] <- 0
  
  #Formula
  return(1/(1 + sqrt(var(aux$interactionCount))))
}

##
# INITIAL PLAN COMPLEXITY
#
# Computes, for a planning mission, its complexity, based on the time spent on planning and the waypoints planned
#
# Input : SimulationID: Identifies the simulation 
# Output: A number between [0,1] (1 = best, 0 = worst)
##
metric.IPC = function (simulationID) {
  
  #Get the duration of the simulaiton and the duration of planning
  simulationDuration <- dataHelper.getRealDuration(simulationID)
  planningDuration <- dataHelper.getPlanningDuration(simulationID)
  
  #Get the waypoints interactions, total and during planning
  waypointInteractions <- dataHelper.getWaypointInteractions(simulationID)
  planningwaypointInteractions <- waypointInteractions %>%
                                  filter(elapsedRealTime < planningDuration)
  
  if (nrow(waypointInteractions) == 0) return(0)
  
  return(
    0.5*(nrow(planningwaypointInteractions)/nrow(waypointInteractions)) + 
           0.5*(planningDuration/simulationDuration)
                )
}

##
# REFLEXES
#
# Computes how near the interactions of a user, belonging to a same action, are near of each other
#
# Input : SimulationID: Identifies the simulation 
# Output: A number between [0,1] (1 = best, 0 = worst)
##
metric.reflexes = function (simulationID) {
  #Get the interactions of that simulation ,ordered in time
  interactions <- dataHelper.getSimulationInteractions(simulationID) %>% arrange(elapsedRealTime)
  
  if (nrow(interactions)==0 || nrow(interactions)==1) return(0)
  
  interactionTimeStamps <- interactions$elapsedRealTime
  #Get the timestamp differences vector and filter 
  timeBetweenInteractionsInSeconds <- diff(interactionTimeStamps)/1000
  timeBetweenInteractionsInSeconds <- timeBetweenInteractionsInSeconds[timeBetweenInteractionsInSeconds < Constants$MAX_TIME_BETWEEN_INTERACTIONS_THRESHOLD]
  
  if (length(timeBetweenInteractionsInSeconds) == 0) return(0)
  
  #Formula
  return(
      1/(1+sqrt(mean(timeBetweenInteractionsInSeconds)))
    )
}