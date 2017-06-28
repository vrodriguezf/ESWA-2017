UserProfiles.CAEPIA = function() {
  userProfiles <- students.simulations %>%
    group_by(simulationID) %>%
    filter(importedMissionPlan.id == 4, !dataHelper.isOutlier(simulationID)) %>%  
    summarize(
      user = helper.getUserName(clientIP,createdAt),
      score=metric.score(simulationID),
      aggressiveness=metric.aggressiveness(simulationID),
      cooperation=metric.cooperation(simulationID),
      IPC=metric.IPC(simulationID)
    )  %>%
    na.omit %>%
    group_by(user) %>%
    summarise(
      score=mean(score),
      aggressiveness=mean(aggressiveness),
      cooperation=mean(cooperation),
      IPC=mean(IPC)
    )
  
  return(userProfiles)
}