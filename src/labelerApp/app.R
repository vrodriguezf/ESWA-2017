#Labeler app

#Package dependencies
library(shiny)
library(shinyjs)
library(ShinyRatingInput)
library(reshape2)
library(plyr)
library(dplyr)
library(ggplot2)
library(rmongodb)
library(tibble)
library(lubridate)


#Source dependencies
source("shinyHelpers.R")


#MongoDB connection
host <- "savier.ii.uam.es"
db <- "DroneWatchAndRescue"
humanGroundTruthNs <- paste(db,"humanGroundTruth",sep=".")


#Variables
labelers <- list("Test","Victor","David","Hector")
performanceMeasures <- c(
  'scoreTS',
  'agilityTS',
  'attentionTS',
  'cooperationTS',
  'aggressivenessTS',
  'precisionTS'
)


ui <- fluidPage(
  titlePanel(title = "TS-Labeler", windowTitle = "TS-Labeler"),
  fluidRow(
    column(width = 4,
           wellPanel(
             selectInput(inputId = "labelerName",
                         choices = labelers,
                         label = "Who is going to label?",
                         multiple = FALSE,
                         selectize = TRUE,
                         selected = NULL) 
           )
    )
  ),
  wellPanel(
    fluidRow(
      column(width = 6,offset = 0,
             ratingInput(
               inputId = "rating",
               label = "Evaluate the degree of similarity between these two time series",
               dataFilled = "fa fa-star fa-3x orange",
               dataEmpty = "fa fa-star-o fa-3x white",
               value = 3
             )
      ),
      column(width = 2,
             actionButton(
               inputId = "save",
               label = "Save"
             )),
      column(width = 4,
             textOutput(outputId = "evaluatedShowdowns")
      )
    )
  ),
  fluidRow(
    column(width = 10,offset = 1,{
      plotOutput(outputId = "sim1")
    })
  ),
  br(),
  fluidRow(
    column(width = 10,offset = 1,{
      plotOutput(outputId = "sim2")
    })  )
)

##
# Server
##
server <- function (input,output) {
  
  findEvaluation <- function(sim1,sim2,pm,labeler) {
    dwrConn <- mongo.create(host = host, db=db, username = "dwr", password = "dwr")
    print(sim1)
    print(sim2)
    print(pm)
    print(labeler)
    
    buf <- mongo.bson.buffer.create()
      mongo.bson.buffer.append(buf,"labeler",labeler)
      mongo.bson.buffer.append(buf,"sim1",sim1)
      mongo.bson.buffer.append(buf,"sim2",sim2) 
      mongo.bson.buffer.append(buf,"performanceMeasure",pm)
    
    result <- mongo.find.one(mongo = dwrConn,
                   ns = humanGroundTruthNs,
                   query = mongo.bson.from.buffer(buf)
                   )
    mongo.destroy(mongo = dwrConn)
    
    return(result)
  }
  
  getNewOpponents <- function() {
    candidate <- list()
    repeat{
      print("Choosing new shodown candidates...")
      candidate <- list(
        opponents = sample(names(SIM_SERIES),2),
        performanceMeasure = sample(performanceMeasures,1)
      )
      if (is.null(findEvaluation(candidate$opponents[[1]],
                                  candidate$opponents[[2]],
                                  candidate$performanceMeasure,
                                  isolate(input$labelerName)))) {
        break
      }
    }
      return(candidate)
  }

  #Reactive values
  #showdown <- getNewOpponents()
  values <- reactiveValues(
    opponents = sample(names(SIM_SERIES),2),
    measureToEvaluate = sample(performanceMeasures,1),
    #opponents = showdown$opponents,
    #measureToEvaluate =showdown$performanceMeasure,
    evaluatedShowdowns = shinyHelper.calculate_evaluatedShowdowns(validSimSeries = SIM_SERIES)
  )
  
  observeEvent(input$save,{
    #Save data (MongoDB)
    dwrConn <- mongo.create(host = host, db=db, username = "dwr", password = "dwr")
    success <- mongo.insert(mongo = dwrConn,
                            ns = humanGroundTruthNs,
                            b = list(labeler=input$labelerName,
                                     sim1=values$opponents[[1]],
                                     sim2=values$opponents[[2]],
                                     performanceMeasure=values$measureToEvaluate,
                                     similarityRating=as.numeric(input$rating))
    )
    
    ifelse(success,
           print("Evaluation was successfully saved"),
           print(mongo.get.last.err(mongo = dwrConn,db=db))
    )
    
    mongo.destroy(mongo = dwrConn)
    
    #New showdown!
    newShowdown <- getNewOpponents()
    
    #values$opponents <- sample(names(SIM_SERIES),2)
    #values$measureToEvaluate <- sample(performanceMeasures,1)
    values$opponents <- newShowdown$opponents
    values$measureToEvaluate <- newShowdown$performanceMeasure
    
    #Recalculate the showdowns evaluated!
    values$evaluatedShowdowns <- shinyHelper.calculate_evaluatedShowdowns(validSimSeries = SIM_SERIES)
  })
  
  output$evaluatedShowdowns <- renderText({
    paste("Evaluated showdowns:", values$evaluatedShowdowns,"/", (20*19/2)*6)
  })
  
  output$sim1 <- renderPlot(
    shinyHelper.simulationTS(simSeries=SIM_SERIES,
                             simulationID = values$opponents[[1]],
                             measureResolution = MEASURE_RESOLUTION,
                             measures = c(values$measureToEvaluate),
                             incidentsDrawing = FALSE,
                             markExecutionStartTime = FALSE,
                             customXLim = c(0,MEASURE_RESOLUTION*max(
                               length(SIM_SERIES[[values$opponents[[1]]]][[values$measureToEvaluate]]),
                               length(SIM_SERIES[[values$opponents[[2]]]][[values$measureToEvaluate]])
                             )))
  )
  
  output$sim2 <- renderPlot(
    shinyHelper.simulationTS(simSeries=SIM_SERIES,
                             simulationID = values$opponents[[2]],
                             measureResolution = MEASURE_RESOLUTION,
                             measures = c(values$measureToEvaluate),
                             incidentsDrawing = FALSE,
                             markExecutionStartTime = FALSE,
                             customXLim = c(0,MEASURE_RESOLUTION*max(
                               length(SIM_SERIES[[values$opponents[[1]]]][[values$measureToEvaluate]]),
                               length(SIM_SERIES[[values$opponents[[2]]]][[values$measureToEvaluate]])
                             )))
  )
  
  # output$sim1 <- renderText(expr = values$opponents[[1]])
  # output$sim2 <- renderText(expr= values$opponents[[2]])
}

##
# Run the application
##

#Initialize data
SIM_SERIES <- readRDS(file = "data/narrowSimSeries.Rds")
MEASURE_RESOLUTION <- 2150

#Bound the possibilities to tag
# FIXED_SIMS_IDS <- c("56152346334cf1dc03a2d0e6",
#                 "561523ce334cf1dc03a2f363",
#                 "561525d6334cf1dc03a3d877",
#                 "5617c8a68924e2483a08a1a3",
#                 "5617c8c68924e2483a08b040")
# 
# RANDOM_SIMS_NUMBER <- 15
# RANDOM_SIMS <- SIM_SERIES[!(names(SIM_SERIES) %in% FIXED_SIMS)] %>% sample(size=RANDOM_SIMS_NUMBER)
# SIM_SERIES <- c(SIM_SERIES[FIXED_SIMS_IDS],RANDOM_SIMS)

#
# Connection to mongoDB database
#

#Deploy shiny
shinyApp(ui = ui, server = server)