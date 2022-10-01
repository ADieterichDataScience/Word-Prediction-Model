#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(fluidPage(
  navbarPage("Word Prediction App",
             tabPanel("App: Predict new word",
                      width=6 ,uiOutput("topPanel") ,p(),
                      tags$textarea(id="str", rows=6, cols=50) ,p(),
                      # uiOutput("pred") ,p(),
                      sliderInput("n", "How many words in prediction?",
                                  value=5,min=1,max=10, step=1)
             ),
             
             strong("Click  button to see predicted words"),
             actionButton("buttonPredict", "Predict"),
             
             mainPanel(uiOutput("pred"),p()),
             br(),
             
             br(),
            tabPanel("About", includeMarkdown("WordPredictMarkdown.Rmd")),
  )
))