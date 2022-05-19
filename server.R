library(shiny)
library(plotly)
library(tidyverse)


server <- function(input, output, session) {
  # Read and organize the data in Smoker and NonSmoker
  
  data <- read.csv(file = "insurance.csv")
  data1 <- data
  dataSmoker <- subset(data, smoker == "yes")
  dataNonSmoker <- subset(data, smoker == "no")
  
  # Calculation Linear Regression Model
  
  modelSmoker <-   lm(charges ~ bmi, data = dataSmoker)
  modelNonSmoker <- lm(charges ~ bmi, data = dataNonSmoker)
  modelBoth <- lm(charges ~ bmi, data = data1)
  
  # print(modelSmoker)
  #print(modelNonSmoker)
  #print(modelBoth)
  
  # Organisze data from the models
  # Smoker
  
  interceptSmoker <- modelSmoker$coefficients
  dataSmoker$regression <-
    interceptSmoker[1] + interceptSmoker[2] * dataSmoker$bmi
  # NonSmoker
  
  interceptNonSmoker <- modelNonSmoker$coefficients
  dataNonSmoker$regression <-
    interceptNonSmoker[1] + interceptNonSmoker[2] * dataNonSmoker$bmi
  
  # Smoker and NonSmoker both
  
  interceptBoth <- modelBoth$coefficients
  data1$regression <-
    interceptBoth[1] + interceptBoth[2] * data1$bmi
  
  
  
  predSmokerModel <- reactive({
    bmiInput <- input$sliderBMI
    predict(modelSmoker, newdata = data.frame(bmi = bmiInput))
  })
  
  predNonSmokerModel <- reactive({
    bmiInput <- input$sliderBMI
    predict(modelNonSmoker, newdata = data.frame(bmi = bmiInput))
  })
  
  
  
  
  
  
  output$predSmoker <- renderText({
    predSmokerModel()
  })
  
  
  output$predNonSmoker <- renderText({
    predNonSmokerModel()
  })
  
  
  
  
  
  output$graph <- renderPlotly({
    if (!input$showSmoker && !input$showNonSmoker) {
      stop("Invalid selection: please select Smoker , Non Smoker or both. ")
    }
    
    # Check if smoker is on
    if (input$showSmoker) {
      datax <- dataSmoker
      
      predSmokerModel <- reactive({
        bmiInput <- input$sliderBMI
        predict(modelSmoker, newdata = data.frame(bmi = bmiInput))
      })
      
      #   print(predSmokerModel())
      x <- predSmokerModel()
      #    print(x)
      bmiInput <- input$sliderBMI
      #     print(bmiInput)
      dataPred <-
        data.frame(bmi = bmiInput,
                   pred = x,
                   smoker = "yes")
      #   print(dataPred)
      
    }
    # Check if NonSmoker is on
    if (input$showNonSmoker) {
      datax <- dataNonSmoker
      
      
      predNonSmokerModel <- reactive({
        bmiInput <- input$sliderBMI
        predict(modelNonSmoker, newdata = data.frame(bmi = bmiInput))
      })
      
      #    print(predNonSmokerModel())
      x <- predNonSmokerModel()
      #   print(x)
      bmiInput <- input$sliderBMI
      #    print(bmiInput)
      dataPred <-
        data.frame(bmi = bmiInput,
                   pred = x,
                   smoker = "no")
      #   print(dataPred)
      
      
      
      
    }
    # Check if Smoker and NonSmoker is on
    if (input$showSmoker && input$showNonSmoker) {
      datax <- data1
      
      pal <- c("blue", "red")
      plot_ly(
        data = datax,
        x = ~ bmi,
        y = ~ charges,
        color = ~ smoker,
        colors = pal,
        type = "scatter",
        mode = "markers"
      ) %>%
        layout(
          title = "BMI depend of BMI and Charges ",
          legend = list(title = list(text = 'Smoker')),
          xaxis = list(title = 'BMI') ,
          yaxis = list(title = 'Charges')
        )
    } else {
      #--anpassen an den Ablauf
      pal <- c("blue", "red")
      plot_ly(
        data = datax,
        x = ~ bmi,
        y = ~ charges,
        color = ~ smoker,
        colors = pal,
        type = "scatter",
        size = 3,
        mode = "markers"
      ) %>%
        add_trace(
          data = datax,
          x =  ~ bmi,
          y = ~ regression,
          line = list(width = 4, color = "black"),
          mode  = "lines",
          name  = "Regression"
        )  %>%
        add_trace(
          data = dataPred,
          x = ~ bmi,
          y = ~ pred,
          name = "Predication",
          size = 4,
          color = "black",
          type = "scatter",
          mode = "markers"
        ) %>%
        layout(
          title = "BMI depend of BMI and Charges ",
          legend = list(title = list(text = 'Smoker')),
          xaxis = list(title = 'BMI') ,
          yaxis = list(title = 'Charges')
        )
      
      
      
      
    }
    
    
    
  })
}
