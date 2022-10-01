
# This is the server logic of a Shiny web application

# load Shiny library ----------------------------------------------
suppressPackageStartupMessages(library(shiny))

# source code for prediction model --------------------------------
source('predict_word.R')

# server code for Shiny -------------------------------------------
shinyServer(
  function(input, output, session) {
    output$pred <- renderText({
      preds <- predict_next(input$str, input$n)
      paste(preds, collapse="\n")
    })
  }
)