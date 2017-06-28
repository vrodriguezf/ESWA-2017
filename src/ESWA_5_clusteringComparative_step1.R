#
# This script takes the detailed clustering results of each distance-method combination from script 2,
# and perform script 3 to check the results of applying script 3 to some other (worse) results from script 2
# The comparison is made by a human-based evaluation criteria
#

library('ProjectTemplate')
load.project()
source("src/ESWA_helpers.R")

#Input files form Step 1 of the methodology (after execution of script 2)
performanceMeasures <- list(" aggressiveness "," agility "," attention "," cooperation "," score "," precision ")

aggregatedInput <- bind_rows(lapply(performanceMeasures, function (pm) {
  dat <- read_csv(paste("output/ESWA/",pm,".csv",sep = ""))
  dat$performanceMeasure <-pm
  return(dat)
}))

clusteringResultsDf <- aggregatedInput %>%
  unite("clusteringResults",which(colnames(aggregatedInput) %>% startsWith("clusteringResult"))) %>% 
  group_by(dissMeasure,clustMethod,k) %>% 
  do(clusteringResults=.$clusteringResults %>% setNames(performanceMeasures) %>% strsplit("_") %>% lapply(as.numeric))

#
# Introduce every row for script 3 (pipeline mode) and save the results
#

# Needed to call other scripts with the ProjectTemplate structure without loading the preamble
PIPELINE_CALL <- TRUE
SAVE_PLOT <- FALSE
SAVE_OUTPUT <- FALSE
EVALUATE_ALL <- FALSE

#Calculate assignation clusters using script 3
assignationClusters <- apply(clusteringResultsDf,1, function (row) {
  print(paste(row$dissMeasure,row$clustMethod,row$k))
  input <- lapply(row$clusteringResults, function (x) list(clustering=x))
  
  saveRDS(object = input, file = "output/ESWA/performanceMeasuresClusterization.Rds")
  source("src/ESWA_3-SimulationClusters.R")
  
  #Take the results from the script (simClust object)
  names(simClust$clustering) <- names(simSeries)
  row$simClust <- simClust
  row
}) %>% setNames(paste(clusteringResultsDf$dissMeasure,clusteringResultsDf$clustMethod,clusteringResultsDf$k,sep = "-"))

# Evaluate the assignation clusters using the human ground truth
humanGroundTruth <- ESWA_helper.loadHumanGroundTruth()

step1ComparativeResults <- lapply(assignationClusters, function (x) {
  ESWA_helper.clusteringEvaluation(clusteringResults = x$simClust$clustering,
                                   humanGroundTruth = humanGroundTruth)
})

#Save results
print(step1ComparativeResults)

#Reset pipeline params
PIPELINE_CALL <- FALSE