if (!exists("PIPELINE_CALL") || PIPELINE_CALL == FALSE) {
  library(ProjectTemplate)
  load.project()
  SAVE_PLOT <- TRUE
  SAVE_OUTPUT <- FALSE
}

#Check dependencies with previos source script files
source("./src/ESWA/ESWA_helpers.R")

#Parameters

##
# Program
##

#Load human ground truth
humanGroundTruth <- ESWA_helper.loadHumanGroundTruth()

#Filter data
humanGroundTruth <- ESWA_helper.filterHumanGroundTruth(humanGroundTruth)

# Calcular Indice de concordancia (ICC)
ESWA_helper.kendallCoefficientOfConcordance(humanGroundTruth = humanGroundTruth)

# Calcular porcentaje de coverage de evaluacion
NUMBER_OF_TEST_SIM_SERIES <- 20
preprocessedHGT <- ESWA_helper.preprocessHumanGroundTruth(humanGroundTruth)
print(
  paste(
    "Number of showdowns covered:",
    nrow(preprocessedHGT),
      "/",
      NUMBER_OF_TEST_SIM_SERIES*(NUMBER_OF_TEST_SIM_SERIES-1)/2
  )
)

print(
  paste(
    "Number of showdowns useful (passing threshold):",
    preprocessedHGT %>% filter(avgRating<=Constants$HUMAN_GROUND_TRUTH$REJECT_MATCH_RATING_THRESHOLD |
                                 avgRating>=Constants$HUMAN_GROUND_TRUTH$ACCEPT_MATCH_RATING_THRESHOLD) %>% 
      nrow(),
    "/",
    NUMBER_OF_TEST_SIM_SERIES*(NUMBER_OF_TEST_SIM_SERIES-1)/2
  )
)

#Examinar que usuarios han rateado mas...
print("### Ranking of raters ###")
humanGroundTruth %>% dplyr::count(labeler)