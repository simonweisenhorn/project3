
library(shiny)
library(tidyverse)


shinyServer(function(input, output, session) {
  
  
  
  url <- a("Kaggle", href="https://www.kaggle.com/datasets/ashydv/housing-dataset")
  
  output$tab <- renderUI({
    tagList("Data Source:", url)
  })
  
  
  getData <- reactive({
    newData <- read_csv("../Housing.csv")
  })
  
  
  observeEvent(input$trainSplit, {
    change <- 1-input$trainSplit
    updateNumericInput(session, "testSplit", value=change)
  })
  
  observeEvent(input$testSplit, {
    change <- 1-input$testSplit
    updateNumericInput(session, "trainSplit", value=change)
    })
  
  output$table <- renderTable({
    getData()
  })
  
  observeEvent(input$runModels, {
    
    newData <- getData()
    
    newData$mainroad <- as.factor(newData$mainroad)
    newData$guestroom <- as.factor(newData$guestroom)
    newData$basement <- as.factor(newData$basement)
    newData$hotwaterheating <- as.factor(newData$hotwaterheating)
    newData$airconditioning <- as.factor(newData$airconditioning)
    newData$prefarea <- as.factor(newData$prefarea)
    newData$furnishingstatus <- as.factor(newData$furnishingstatus)
    
    
    
    trainSplit <- input$trainSplit
    splitIndexes <- createDataPartition(newData$price, p=trainSplit, list=FALSE)
    dataTrain <- newData[splitIndexes, ]
    dataTest <- newData[-splitIndexes, ]
    
    cvMlrNumber <- input$mlrCvFold

    if ((input$mlrInteractions == 0) & (input$mlrVariables == "all")) {
      mlrFit <- train(price ~ ., data = dataTrain,
                    method = "lm",
                    preProcess = c("center", "scale"),
                    trControl = trainControl(method = "cv", number = cvMlrNumber))
    } else if ((input$mlrInteractions == 1) & (input$mlrVariables == "all")){
      mlrFit <- train(price ~ .^2, data = dataTrain,
                    method = "lm",
                    preProcess = c("center", "scale"),
                    trControl = trainControl(method = "cv", number = cvMlrNumber))
    }
    
    
    

    if ((input$mlrInteractions == 0) & (input$mlrVariables == "custom")) {
      
      my.formula <- paste("price", '~', paste(input$mlrCustom, collapse = "+") )
      my.formula <- as.formula(my.formula)
      
      mlrFit <- train(my.formula, data = dataTrain,
                      method = "lm",
                      preProcess = c("center", "scale"),
                      trControl = trainControl(method = "cv", number = cvMlrNumber))
      
    } else if((input$mlrInteractions == 1) & (input$mlrVariables == "custom")){
      
      my.formula <- paste("price", '~', "(", paste(input$mlrCustom, collapse = "+"), ")^2")
      my.formula <- as.formula(my.formula)
      
      mlrFit <- train(my.formula, data = dataTrain,
                      method = "lm",
                      preProcess = c("center", "scale"),
                      trControl = trainControl(method = "cv", number = cvMlrNumber))
    }
    
    output$modelSummary <- renderPrint(summary(mlrFit))
    
  })
  
  
})

