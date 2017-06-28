########################################
## Mission helpers
########################################

missionHelper.getEnvironmentById = function (environmentID) {
  return(
    missions$environment[[match(environmentID,llply(missions$environment, function (e) e$id))]]
  )
}

missionHelper.getSimulationEnvironment = function (simulationID) {
  return(
      missionHelper.getEnvironmentById(
        (dataHelper.getFirstSimulationSnapshot(simulationID))$importedEnvironment.id[[1]]
      )
    )
}

missionHelper.getTargetsDefinitionById = function (targetsDefinitionId) {
  return(
    missions$targets[[match(targetsDefinitionId,llply(missions$targets, function (t) t$id))]]
  )
}

missionHelper.getMissionID = function (simID) {
  return(
      (data.simulations %>% filter(simulationID == simID))[1,]$mission
    )
}

#
# Tabla de correspondencia entre identificadores y nombres de misiones (e.g testMission1 -> 1)
#
missionHelper.identificationMapping <- function(stringMissionID) {
  map <- switch(stringMissionID,
                testMission01=1,
                testMission02=0,
                testMission03=3,
                testMission04=4,
                stringMissionID) #Default returns the same
}
