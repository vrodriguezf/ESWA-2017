## Constants

constant.MAX_SIMULATION_SPEED <- 1000

# Measures (in ms) the amount of time considered to respond against an incident
constant.INCIDENT_ACTION_THRESHOLD <- 10000

Constants <- list(
  # The minimum time that a simulation is considered to last to not being considered as an outlier
  OUTLIER_REAL_TIME_THRESHOLD = 20000,
  MIN_TIME_BETWEEN_INTERACTIONS = 0.5,
  MAX_TIME_BETWEEN_INTERACTIONS_THRESHOLD = 8, # seconds
  MIN_INTERACTIONS_PER_SIMULATION = 10,
  HUMAN_GROUND_TRUTH=list(
    MIN_PERFORMANCE_MEASURES_RATED_PER_SIMULATION = 4,
    REJECT_MATCH_RATING_THRESHOLD = 2.5,
      ACCEPT_MATCH_RATING_THRESHOLD = 3.5
    )
  )