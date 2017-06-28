
##
# Too short durations
##
outlier.dwr.isSimulationTooShort <- function (simulationID, 
                                              durationThreshold = Constants$OUTLIER_REAL_TIME_THRESHOLD) {
  #print("Duration = " %+% dataHelper.getRealDuration(simulationID = simulationID))
  return(
    dataHelper.getRealDuration(simulationID = simulationID) < durationThreshold
    )
}

##
# Too short durations
##
outlier.dwr.fewInteractions <- function (simulationID, 
                                              interactionThreshold = Constants$MIN_INTERACTIONS_PER_SIMULATION) {
    
  return(nrow(dataHelper.getSimulationInteractions(simulationID = simulationID)) < interactionThreshold)
}

##
# Simulations without incidents are not interesting
##
outlier.dwr.noIncidents <- function (simulationID) {
  incidentSnaps <- data.simulationSnapshots %>% 
    filter(simulation==simulationID,
           cause.id == SnapshotCauses$INCIDENT_STARTED
           )
  
  return(nrow(incidentSnaps) == 0)
}



##
# The simulation is a tutorial. We detect tutorials cuase the got more than two instructions sent
##
outlier.dwr.isTutorial <- function(simulationID) {
    instructionsSent <- data.simulationSnapshots %>%
        filter(
            simulation == simulationID,
            cause.id == SnapshotCauses$INSTRUCTION_SENT
        )
    return(nrow(instructionsSent) > 2)
}

