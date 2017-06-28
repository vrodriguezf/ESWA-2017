# #Rmongodb
# host <- "aida.ii.uam.es"
# db <- "DroneWatchAndRescue"
# simNs <- paste(db,"simulations",sep=".")
# snapNs <- paste(db,"simulationSnapshots",sep=".")
# droneSnapsNs <- paste(db,"droneSnapshots",sep=".")
# 
# oldSnapNs <- paste(db,"simulationsnapshots",sep=".")
# oldDroneSnapsNs <- paste(db,"dronesnapshots",sep=".")
# 
# 
# dwrConn <- mongo.create(host = host, db=db)
# 
# newSimulations <- mongo.find.all(mongo = dwrConn,
#                                  ns = simNs,
#                                  query = '{
#                                  "clientIP" : {
#                                  "$ne" : "127.0.0.1"
#                                  },
#                                  "missionPlan.id" : "testMission03"
#                                  }',
#                    fields = '{"_id":1,
#                    "clientIP":1,
#                    "createdAt":1,
#                    "missionPlan.id" : 1,
#                    "name" : 1
#                    }',
#                   data.frame = TRUE)
# 
# oldSimulations <- mongo.find.all(mongo = dwrConn,
#                                  ns = simNs,
#                                  query = '{
#                                  "clientIP" : {
#                                  "$ne" : "127.0.0.1"
#                                  },
#                                  "importedMissionPlan.id" : 3
#                                  }',
#                                fields = '{"_id":1,
#                                "clientIP":1,
#                                "createdAt":1,
#                                "importedMissionPlan.id" : 1
#                                }',
#                                data.frame = TRUE)
# 
# allSimulations <- rbind.fill(oldSimulations %>% rename(mission = id),
#                       newSimulations %>% rename(mission = id))
# allSimulations$mission <- 3 #TODO: Esto esta para referenciar bien los ficheros JSON de mision
# 
# newSnaps <- mongo.find.all(mongo = dwrConn,ns = snapNs)
# oldSnaps <- mongo.find.all(mongo = dwrConn, ns = oldSnapNs)
# 
# ##############################
# #Simulation Snapshots
# ##############################
# newSimulationSnapList <- newSnaps[sapply(newSnaps,function (snap) snap$simulation %in% newSimulations$`_id`)]
# oldSimulationSnapList <- oldSnaps[sapply(oldSnaps,function (snap) snap$simulation %in% oldSimulations$`_id`)]
# 
# goodUnlist <- function (x) {
# v <- as.data.frame(t(unlist(x)),stringsAsFactors = FALSE)
# vc <- t(apply(v,1,as.character))
# vn <- t(apply(v,1,as.numeric))
# nas <- which(is.na(vn))
# v[-nas] <- vn[-nas]
# v[nas] <- vc[nas]
# v
# }
# 
# newSimulationSnapsDf <- rbind.fill(lapply(newSimulationSnapList, goodUnlist))
# oldSimulationSnapsDf <- rbind.fill(lapply(oldSimulationSnapList, goodUnlist))
# mission3Snaps <- rbind.fill(newSimulationSnapsDf,oldSimulationSnapsDf)
# 
# #Fix data problems (Interactions with dead drones for example)
# mission3Snaps <- mission3Snaps %>% filter(!(!is.na(cause.id) & cause.id== 0 & !is.na(cause.params.inputId) & cause.params.inputId == 0 & is.na(cause.params.droneId)))
# 
# 
# ##############################
# # Drone Snapshots
# ##############################
# newDroneSnaps <- mongo.find.all(mongo = dwrConn, ns = droneSnapsNs)
# oldDroneSnaps <- mongo.find.all(mongo = dwrConn, ns = oldDroneSnapsNs)
# 
# newDroneSnapList <- newDroneSnaps[sapply(newDroneSnaps,function (snap) snap$simulationSnapshot %in% newSimulationSnapsDf$`_id`)]
# oldDroneSnapList <- oldDroneSnaps[sapply(oldDroneSnaps,function (snap) snap$simulationSnapshot %in% oldSimulationSnapsDf$`_id`)]
# 
# 
# newDroneSnapsDf <- rbind.fill(lapply(newDroneSnapList, goodUnlist))
# oldDroneSnapsDf <- rbind.fill(lapply(oldDroneSnapList, goodUnlist))
# mission3DroneSnaps <- rbind.fill(newDroneSnapsDf,oldDroneSnapsDf)
# 
# 
# #cache('mission3Snaps')
# 
# 
# ##
# # Save into general data
# ##
# data.simulations <- allSimulations %>% rename(simulationID = `_id`)
# data.simulationSnapshots <- mission3Snaps
# data.droneSnapshots <- mission3DroneSnaps
# 
# cache('data.simulationSnapshots')
# cache('data.droneSnapshots')