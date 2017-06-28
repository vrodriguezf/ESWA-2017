#
# DATASET FOR ESWA 2016
# Simulation snapshots played in mission 3 in DWR, for both student experiments in 2014 and 2015
#


#Rmongodb
host <- "localhost"
db <- "DroneWatchAndRescue"
simNs <- paste(db,"simulations",sep=".")

snapNs <- paste(db,"simulationSnapshots",sep=".")
oldSnapNs <- paste(db,"oldSS",sep=".")

droneSnapsNs <- paste(db,"droneSnapshots",sep=".")
oldDroneSnapsNs <- paste(db,"oldDS",sep=".")

#
# Connection to mongoDB database
#
dwrConn <- mongo.create(
  host = host, 
  db="admin", 
  username = "admin", 
  password = "mongoPassword1620"
)

##############################
#Simulations
##############################
simulations2015 <- mongo.find.all(mongo = dwrConn,
                                 ns = simNs,
                                 query = '{
                                    "clientIP" : {
                                    "$ne" : "127.0.0.1"
                                    },
                                    "missionPlan.id" : {
                                        "$exists" : true
                                    },
                                    "missionPlan.test" : true
                                 }',
                   fields = '{"_id":1,
                   "clientIP":1,
                   "createdAt":1,
                   "missionPlan.id" : 1,
                   "name" : 1
                   }',
                  data.frame = TRUE)

simulations2014 <- mongo.find.all(mongo = dwrConn,
                                 ns = simNs,
                                 query = '{
                                 "clientIP" : {
                                 "$ne" : "127.0.0.1"
                                 },
                                    "importedMissionPlan.id" : {
                                        "$exists" : true
                                    }
                                }',
                               fields = '{"_id":1,
                               "clientIP":1,
                               "createdAt":1,
                               "importedMissionPlan.id" : 1
                               }',
                               data.frame = TRUE
                               )


data.simulations <- rbind.fill(simulations2015 %>% rename(mission = id),
                               simulations2014 %>% rename(mission = id))

#Eliminar simulationes más allá de Diciembre de 2015 (Fecha del JUCS)


# Fix data
data.simulations$mission <- laply(data.simulations$mission, 
                                  function(missionID) missionHelper.identificationMapping(missionID))

##############################
#Simulation Snapshots
##############################
newSnaps <- mongo.find.all(mongo = dwrConn,ns = snapNs)
oldSnaps <- mongo.find.all(mongo = dwrConn, ns = oldSnapNs)

newSimulationSnapList <- newSnaps[sapply(newSnaps,function (snap) snap$simulation %in% data.simulations$`_id`)]
oldSimulationSnapList <- oldSnaps[sapply(oldSnaps,function (snap) snap$simulation %in% data.simulations$`_id`)]

newSimulationSnapsDf <- rbind.fill(lapply(newSimulationSnapList, helper.goodUnlist))
oldSimulationSnapsDf <- rbind.fill(lapply(oldSimulationSnapList, helper.goodUnlist))

data.simulationSnapshots <- rbind.fill(newSimulationSnapsDf,oldSimulationSnapsDf)

#Fix data (Interactions with dead drones for example)
data.simulationSnapshots <- data.simulationSnapshots %>% filter(!(!is.na(cause.id) & cause.id== 0 & !is.na(cause.params.inputId) & cause.params.inputId == 0 & is.na(cause.params.droneId)))

##############################
# Drone Snapshots
##############################
newDroneSnaps <- mongo.find.all(mongo = dwrConn, ns = droneSnapsNs)
oldDroneSnaps <- mongo.find.all(mongo = dwrConn, ns = oldDroneSnapsNs)

newDroneSnapList <- newDroneSnaps[sapply(newDroneSnaps,function (snap) snap$simulationSnapshot %in% data.simulationSnapshots$`_id`)]
oldDroneSnapList <- oldDroneSnaps[sapply(oldDroneSnaps,function (snap) snap$simulationSnapshot %in% data.simulationSnapshots$`_id`)]

newDroneSnapsDf <- rbind.fill(lapply(newDroneSnapList, helper.goodUnlist))
oldDroneSnapsDf <- rbind.fill(lapply(oldDroneSnapList, helper.goodUnlist))

data.droneSnapshots <- rbind.fill(newDroneSnapsDf,oldDroneSnapsDf)

# Final fixes (Rename problems)
data.simulations <- data.simulations %>% 
    rename(simulationID = `_id`) %>% 
    filter(simulationID %in% data.simulationSnapshots$simulation)