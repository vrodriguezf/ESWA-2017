# # Filter the data and take only the stuend simulations.
# students.simulations <- rename(
#                             subset(simulations,
#                               grepl("150.244.65",clientIP) & 
#                               (createdAt == '2014-11-05' | createdAt == '2014-11-07')),
#                             simulationID=X_id
#                           )
# 
# #Join simulationSnapshots
# students.simulationSnapshots <- merge(
#                                 rename(simulationSnapshots,simulationSnapshotID=X_id),
#                                 students.simulations,
#                                 by.x="simulation",
#                                 by.y="simulationID"
#                               )
# 
# #Join droneSnapshots
# students.droneSnapshots <- merge(
#                             rename(droneSnapshots,droneSnapshotID=X_id),
#                             students.simulationSnapshots,
#                             by.x="simulationSnapshot",
#                             by.y="simulationSnapshotID"
#                           )
# 
# # Join waypoints
# students.waypoints <- merge(
#                         rename(waypoints,waypointID=X_id),
#                         students.droneSnapshots,
#                         by.x="droneSnapshot",
#                         by.y="droneSnapshotID"
#                       )
# 
# #Cache the dataframes
# cache('students.simulations')
# cache('students.simulationSnapshots')
# cache('students.droneSnapshots')
# cache('students.waypoints')
