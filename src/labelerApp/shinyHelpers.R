shinyHelper.simulationTS <- function(simSeries,
                                    simulationID,
                                    measureResolution,
                                    measures = "all",
                                    incidentsDrawing=TRUE,
                                    markExecutionStartTime=FALSE,
                                    customXLim = NULL) {
  
  metric_labeller <- function (variable, value) {
    metric_names <- list(
      'scoreTS' = "S",
      'agilityTS' = "A",
      'attentionTS'= "At",
      'cooperationTS' = "C",
      'aggressivenessTS' = "Ag",
      'precisionTS' = "P"
    )
    
    return(metric_names[value])
  }
  
  TSList <- simSeries[[simulationID]]
  
  aux <- melt(as.matrix(data.frame(TSList)))
  aux$Var1 <- ((aux$Var1 -1)*measureResolution) #From time steps to seconds
  
  #Incidents data
  if (incidentsDrawing) {
    incidents <- dataHelper.getSimulationIncidentSnapshots(simulationID)
  }
  
  #Simulation monitoring real start time
  if (markExecutionStartTime) {
    firstSimulationSpeedUp <- dataHelper.getMonitoringStartTime(simulationID)
  }
  
  p <- ggplot(data = aux, mapping = aes(x = Var1, y = value, group=1))
  if (measures == "ALL" || length(measures) > 2) {
    p <- p + facet_grid(Var2~., scales="free_x", labeller = metric_labeller)
  }
  
  for (i in 1:length(TSList)) {
    
    if (measures == "ALL" || names(TSList[i]) %in% measures) {
      p <- p + layer(data= aux %>% filter(Var2==names(TSList)[i]),
                     geom = c("line"),
                     stat = "identity",
                     position = "identity") + 
        ylim(c(0,1))
      
      if (!is.null(customXLim)) p <- p + xlim(customXLim)
      
      
      if (incidentsDrawing) {
        p <- p + geom_vline(xintercept=incidents$elapsedRealTime, colour="red",linetype="longdash")
      }
      
      #Draw a vertical line where the simulation time is accelerated (simulation start)
      if (markExecutionStartTime) {
        p <- p + geom_vline(xintercept=firstSimulationSpeedUp, colour="green",linetype="longdash")
      }
    }
  }
  
  p <- p + labs(x="Time (ms)")
  if (measures == "ALL" || length(measures) > 1) {
    p <- p + labs(y="Performance Measure [0,1]")
  } else {
    p <- p + labs(y=paste(measures[[1]],"[0,1]"))
  }
  # p <- ifelse(measures == "ALL" || length(measures) > 2,
  #             p + labs(y=paste(measures[[1]],"[0,1]"))
  # )
  p <- p + theme(axis.text = element_text(size=24),
                 axis.title=element_text(size=27), 
                 #axis.text.y = element_blank(),
                 #axis.ticks.y = element_blank(),
                 strip.text = element_text(size=27))
  
  #p <- p + scale_y_continuous(breaks=c(0,0.5,1), minor_breaks=c(0.25,0.75))
  p
}

##
#
##
shinyHelpers.narrowSimSeries <- function (originalSimSeries, simsCount = 20, fixedSimIds = c()) {
  #Bound the possibilities to tag
  randomSimsNumber <- simsCount - length(fixedSimIds)
  randomSims <- originalSimSeries[!(names(originalSimSeries) %in% fixedSimIds)] %>% sample(size=randomSimsNumber)
  c(originalSimSeries[fixedSimIds],randomSims)
}

##
# TODO: Add connection params?
##
shinyHelper.loadHumanGroundTruth <- function() {
  host <- "savier.ii.uam.es"
  db <- "DroneWatchAndRescue"
  humanGroundTruthNs <- paste(db,"humanGroundTruth",sep=".")
  dwrConn <- mongo.create(host = host, db=db, username = "dwr", password = "dwr")
  humanGroundTruth <- mongo.find.all(mongo = dwrConn,ns = humanGroundTruthNs,data.frame = TRUE) %>% as_data_frame()
  mongo.destroy(dwrConn)
  return(humanGroundTruth)
}

#
# Input: human groundtruth, as it comes from the database
# Output: 
#
shinyHelper.filterHumanGroundTruth <- function(df,validSimSeries) {
  df %>%
    filter(
      labeler!="Test",
      sim1 %in% names(validSimSeries),
      sim2 %in% names(validSimSeries)
    ) %>%
    rowwise() %>% 
    filter(
      difftime(mongo.oid.time(mongo.oid.from.string(`_id`)),make_datetime(year=2016,month=9L,day=16L)) > 0
    ) %>% 
    ungroup() %>% 
    as_data_frame()
}

##
#
##
shinyHelper.calculate_evaluatedShowdowns <- function(validSimSeries) {
  #Load and filter current human ground truth
  humanGroundTruth <- shinyHelper.loadHumanGroundTruth() %>% 
    shinyHelper.filterHumanGroundTruth(validSimSeries = validSimSeries)
  
  #Calculate the different combinations of sim1-sim2-performanceMeasure found
  humanGroundTruth %>% 
    mutate(sim1_ = pmin(sim1,sim2), sim2_ = pmax(sim1,sim2)) %>% 
    group_by(sim1_,sim2_,performanceMeasure) %>% 
    dplyr::summarise(n()) %>% 
    nrow 
}