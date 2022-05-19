library(shiny)
library(plotly)

 shinyUI( fluidPage(
   titlePanel("DDP Week 4 Assigment"),
   sidebarLayout(
     sidebarPanel(
       sliderInput("sliderBMI", "What is your BMI ?", min=10, max = 55, value= 30),
       checkboxInput("showSmoker", "Show/Hide Smoker", value = TRUE),
       checkboxInput("showNonSmoker", "Show/Hide Non Smoker", value = TRUE),
       submitButton("Submit")
     ),
     
     mainPanel(plotlyOutput("graph"),
               h5("Predicated Insurence Costs for Smoker:"),
               textOutput("predSmoker"),
               h5("Predicated Insurence Costs for Smoker and Non Smoker:"),
               textOutput("predNonSmoker")
     )
     
   ),
   
   
   
   
 # selectInput("choice", "Choose", choices = names(iris), selected = NULL),
  
 )
)