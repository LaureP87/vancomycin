#This is a Shiny web application. You can run the application by clicking
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
library(shinyvalidate)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(lubridate)

#load model
xgb_model_shinny<- readRDS("vanco_xgb_firstdose_070122.rds")


##App shiny


ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = tagList(
                      tags$span(
                        class = "logo-mini", "V1D"
                      ),
                      tags$span(
                        class = "logo-lg", "vancomycin first dose"
                      )
                    )),
                    dashboardSidebar(collapsed = T,
                                     sidebarMenu(
                                       menuItem("Home", tabName = "Home",icon = icon("home")),
                                       menuItem("Vancomycine", tabName = "Vancomycin",icon = icon("medkit"))
                                     )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "Home",
                                userBox(
                                  title = userDescription(
                                    title = "Optimization of vancomycin initial dose in term and preterm neonates by Machine Learning",
                                    subtitle = "Laure Ponthier PhD works",
                                    type = 2,
                                    image = "hex-V1D.png"
                                  ),
                                  width = 12,
                                  status = "primary",
                                  collapsible = F,
                                  gradient = TRUE,
                                  background = "navy",
                                  boxToolSize = "l",  
                                  "For research purposes only, not dedicated to clinical use"
                                ),
                                box(width=12, title = "Abstract of study",
                                    
                                    status = 'navy',
                                    h3("Introduction:"), h5("Vancomycin is one of the antibiotics most used in neonates. Continuous infusion has many advantages over intermittent infusions, but no consensus has been achieved regarding the optimal starting initial dose. The objectives of this study were: (i) to develop a Machine learning (ML) algorithm based on pharmacokinetic profiles obtained by Monte Carlo simulations using a population pharmacokinetic model (POPPK) from the literature, in order to derive the best vancomycin starting initial dose in preterm and term neonates, and(ii) to compare ML performances with those of an literature equation (LE) derived from a POPPK (EPOPPK) previously published." ),
                                    h3(" Materials and methods:"), h5("The parameters of a previously published POPPK model of vancomycin in children and neonates were used in the mrgsolve R package to simulate 1900 PK profiles. ML algorithms were developed from these simulations using Xgboost, GLMNET and MARS in parallel, benchmarked and used to calculate the ML first dose. Performances were evaluated in a second simulation set and in an external set of 82 real patients and compared to those of a EPOPPKLE."), 
                                    h3("Results:"), h5("The Xgboost algorithm yielded numericallythe best performances and target attainment rates: 46.9% in the second simulation set of 400-600 AUC/MIC ratio vs.  41.4% for the EPOPPK LE model (p=0.0018<0.005); and 35.3% vs. 28% in real patients (p=0.401), respectively(p<0.005). The Xgboost model resulted in less AUC/MIC>600, thus decreasing the risk of nephrotoxicity."), 
                                    h3("Conclusion:"), h5("The Xgboost algorithm developed to estimate the starting initial dose of vancomycin in term or preterm infants has better performances than a previous validated EPOPPK LE and should be evaluated prospectively.")
                                )
                        ),
                        #### Vancomycin dashboard-----
                        tabItem(tabName = "Vancomycin",
                                box(width = 12, title = "Patient data",  
                                    numericInput(inputId = "CW",label = "Current weight (grams):", value= NULL, min = 500, max = 5000),
                                    helpText(" Weight values between 500 and 5000 g are accepted,  model has not been tested for values out of this range"), 
                                    numericInput(inputId = "CREA",label = "Creatinine values (µmol/L):",value= NULL, min = 40, max = 145),
                                    helpText(" Serum creatinine values between 40 and 145 µmol/L are accepted, model has not been tested for values out  of this range"),
                                    div(style="display: inline-block;vertical-align:top; width: 400px;",  numericInput(inputId = "PMA_w", label = "post natal age (weeks):", value= NULL, min = 24, max = 45)),
                                    div(style="display: inline-block;vertical-align:top; width: 400px;",numericInput(inputId = "PMA_d", label = "+ post natal age (days):", value= NULL , min = 0, max = 6)),
                                    helpText(" Post natal age (weeks+day(s)) between 24 and 45 weeks are accepted, model has not been tested for values out of this range")),
                                valueBoxOutput("poso")
                                
                                
                                
                        ))))





# Define server logic ----
server <- function(input, output) {
  
  
  # Reactive expression to create data frame of all input values ----
  
  
  
  output$poso<- renderValueBox({
    req(iv$is_valid())
    SA <- input$PMA_w+input$PMA_d/7
    
    test <- tibble(
      ID = 1,
      PMA= ifelse(between(SA,24,45),SA, NULL),
      CW = ifelse(between(input$CW,500,5000),input$CW, NULL),
      CREA= ifelse(between(input$CREA,40,145),input$CREA, NULL),
      dose_charge= if_else(SA <32,10*input$CW/1000,15*input$CW/1000),
      dose_entretien = case_when(SA < 28 ~ (20*input$CW)/1000,
                                 between(SA,28,32.1) ~ (25*input$CW)/1000,
                                 between(SA,32.11, 34) ~ (30*input$CW)/1000,
                                 SA >34.01 ~ (35*input$CW)/1000))
    
    
    
    # round(predict(xgb_model_shinny,test1), digits = 0)
    poso <- round(500 * test$dose_entretien /(round(predict(xgb_model_shinny,test), digits = 0)))
    
    valueBox(
      paste(poso, "mg/24h", sep = " ") ,"vancomycin continous dose to infuse",  icon = icon("chart-area"),
      color = "blue"
    )
  })
  
  
  ## validation  Inputs
  
  iv <- InputValidator$new()
  iv$add_rule("CW", sv_required())
  iv$add_rule("CREA", sv_required())
  iv$add_rule("PMA_w", sv_required())
  iv$add_rule("PMA_d", sv_required())
  iv$add_rule("CW", sv_between(500, 5000))
  iv$add_rule("CREA", sv_between(40, 145))
  iv$add_rule("PMA_w", sv_between(24, 45))
  iv$add_rule("PMA_d", sv_between(0, 6))
  iv$enable()
  
  
  
  
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)
