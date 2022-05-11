#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidymodels)
library(tidyverse)
library(xgboost)

#load model
xgb_model_shinny<- readRDS("vanco_xgb_firstdose_070122.rds")


##App shiny


ui <- 
  fluidPage(
    
    # Application title
    titlePanel(("vancomycin first dose")),
    
    
    # Sidebar layout with a input and output definitions ----
    sidebarLayout(
      
      # Sidebar panel for inputs ----
      sidebarPanel(
        
        
        
        # warning research only ----
         helpText("For research purposes only, not dedicated to clinical use"),
        
        ### ref 
        
        helpText("ref = ponthier et al PhD thesis "),
        
        # Input: Numeric entry for CW
        numericInput(inputId = "CW",
                     label = "current weight (grams):",
                     value= 1200, min = 500, max = 5000),
        
        # range of CW
        helpText(" Values between 500 and 5000 are accepted, 
                 model has not been tested for values out 
                 of this range"),
        
        # Input: Numeric entry for creat----
        numericInput(inputId = "CREA",
                     label = "creatinine values (micromoles/L):",
                     value= 45, min = 40, max = 145),
        # range of creat
        helpText(" Values between 40 and 145 are accepted, 
                 model has not been tested for values out 
                 of this range"),
        
        
        # Input: Numeric entry for PMA----
        numericInput(inputId = "PMA",
                     label = "post natal age (sem):",
                     value= 32, min = 24, max = 45),
        # range of time
        helpText(" Values between 24 and 45 are accepted, 
                 model has not been tested for values out 
                 of this range")),
      
      # Main panel for displaying outputs ----
      mainPanel(
        
        # Output: Verbatim text for data summary ----
        h1(tableOutput("First_dose"))
        
        
        
      )
        )
        )



# Define server logic ----
server <- function(input, output) {
  
  
  # Reactive expression to create data frame of all input values ----
  textValues <- reactive({
    test <- tibble(
      ID = 1,
      PMA= input$PMA,
      CW = input$CW,
      CREA=input$CREA,
      dose_charge= if_else(input$PMA <32,10*input$CW/1000,15*input$CW/1000),
      dose_entretien = case_when(input$PMA < 28 ~ (20*input$CW)/1000,
                                 between(input$PMA,28,32.1)~ (25*input$CW)/1000,
                                 between(input$PMA,32.11, 34) ~ (30*input$CW)/1000,
                                 input$PMA >34.01 ~ (35*input$CW)/1000) )
    
    
    
    # round(predict(xgb_model_shinny,test1), digits = 0)
    round(500 * test$dose_entretien /(round(predict(xgb_model_shinny,test), digits = 0)))
  })
  
  # Show the values in an HTML table ----
  output$First_dose <- renderText({
    str_c("vancomycin continous dose to infuse", textValues(),"mg/24h", sep = " ")
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)




