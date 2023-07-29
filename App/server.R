
library(shiny)
library(tidyverse)
library(caret)
library(scales)


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
  
  getTitle1 <- reactiveVal("Select the Model Options on the Side Panel and Click the Run Models Button to View Results!")
  getTitle2 <- reactiveVal("Select the Model of Interest for Making Predictions, Enter the Attributes of a Given House, and Click the Get Prediction Button to Receive a Prediction of the Price!")
  getTitle3 <- reactiveVal("")
  
  getSubTitle1 <- reactiveVal("")
  getSubTitle2 <- reactiveVal("")
  getSubTitle3 <- reactiveVal("")
  
  getDescTitle1 <- reactiveVal("")
  getDescTitle2 <- reactiveVal("")
  getDescTitle3 <- reactiveVal("")
  getDescTitle4 <- reactiveVal("")
  getDescTitle5 <- reactiveVal("")
  getDescTitle6 <- reactiveVal("")
  getDescTitle7 <- reactiveVal("")
  getDescTitle8 <- reactiveVal("")

  output$text_header1 <- renderUI({
    h1(getTitle1(), align = "center")
  })
  
  output$text_header2 <- renderUI({
    h1(getTitle2(), align = "center")
  })
  
  output$text_header3 <- renderUI({
    h1(getTitle3(), align = "center")
  })
  
  output$subtext_header1 <- renderUI({
    h4(getSubTitle1(), align = "left")
  })
  
  output$subtext_header2 <- renderUI({
    h4(getSubTitle2(), align = "left")
  })
  
  output$subtext_header3 <- renderUI({
    h4(getSubTitle3(), align = "left")
  })
  
  output$desc_header1 <- renderUI({
    h5(getDescTitle1(), align = "left")
  })
  
  output$desc_header2 <- renderUI({
    h5(getDescTitle2(), align = "left")
  })
  
  output$desc_header3 <- renderUI({
    h5(getDescTitle3(), align = "left")
  })
  
  output$desc_header4 <- renderUI({
    h5(getDescTitle4(), align = "left")
  })
  
  output$desc_header5 <- renderUI({
    h5(getDescTitle5(), align = "left")
  })
  
  output$desc_header6 <- renderUI({
    h5(getDescTitle6(), align = "left")
  })
  
  output$desc_header7 <- renderUI({
    h5(getDescTitle7(), align = "left")
  })
  
  output$desc_header8 <- renderUI({
    h5(getDescTitle8(), align = "left")
  })

  
  observeEvent(input$runModels, {
    
    set.seed(558) #setting seed for reproducibility 
    
    newData <- getData()
    
    newData$mainroad <- as.factor(newData$mainroad)
    newData$guestroom <- as.factor(newData$guestroom)
    newData$basement <- as.factor(newData$basement)
    newData$hotwaterheating <- as.factor(newData$hotwaterheating)
    newData$airconditioning <- as.factor(newData$airconditioning)
    newData$prefarea <- as.factor(newData$prefarea)
    newData$furnishingstatus <- as.factor(newData$furnishingstatus)
    
    withProgress(message='Please wait',detail='This may take a few minutes...', 
                 value=0, {
    
    n <- 6
    
    incProgress(1/n, detail = paste("Splitting Data..."))
        
    trainSplit <- input$trainSplit
    splitIndexes <- createDataPartition(newData$price, p=trainSplit, list=FALSE)
    dataTrain <- newData[splitIndexes, ]
    dataTest <- newData[-splitIndexes, ]
    
    incProgress(1/n, detail = paste("Running MLR Model..."))
    
    cvMlrFolds <- input$mlrCvFold

    if ((input$mlrInteractions == 0) & (input$mlrVariables == "all")) {
      mlrFit <- train(price ~ ., data = dataTrain,
                    method = "lm",
                    preProcess = c("center", "scale"),
                    trControl = trainControl(method = "cv", number = cvMlrFolds))
    }else if ((input$mlrInteractions == 1) & (input$mlrVariables == "all")){
      mlrFit <- train(price ~ .^2, data = dataTrain,
                    method = "lm",
                    preProcess = c("center", "scale"),
                    trControl = trainControl(method = "cv", number = cvMlrFolds))
    }
    
    
    if ((input$mlrInteractions == 0) & (input$mlrVariables == "custom")) {
      
      my.formula1 <- paste("price", '~', paste(input$mlrCustom, collapse = "+"))
      my.formula1 <- as.formula(my.formula1)
      
      mlrFit <- train(my.formula1, data = dataTrain,
                      method = "lm",
                      preProcess = c("center", "scale"),
                      trControl = trainControl(method = "cv", 
                                               number = cvMlrFolds))
      
    } else if((input$mlrInteractions == 1) & (input$mlrVariables == "custom")){
      
      my.formula1 <- paste("price", '~', "(", paste(input$mlrCustom, 
                                                    collapse = "+"), ")^2")
      my.formula1 <- as.formula(my.formula1)
      
      mlrFit <- train(my.formula1, data = dataTrain,
                      method = "lm",
                      preProcess = c("center", "scale"),
                      trControl = trainControl(method = "cv", 
                                               number = cvMlrFolds))
    }
    
    mlrRMSE <- mlrFit$results[2]
    
    mlrVarImp <- varImp(mlrFit, scale = FALSE)
    
    incProgress(1/n, detail = paste("Running Bagged Tree Model..."))
    
    cvTreeFolds <- input$treeCvFold
    cvTreeRepeats <- input$treeCvRepeats
    
    if (input$treeVariables == "all") {
      bagTreeFit <- train(price ~ ., data = dataTrain,
                                     method = "treebag",
                                     preProcess = c("center", "scale"),
                                     trControl = trainControl(method = "repeatedcv", 
                                                              number = cvTreeFolds, 
                                                              repeats=cvTreeRepeats))
    } else if (input$treeVariables == "custom"){
      
      my.formula2 <- paste("price", '~', paste(input$treeCustom, collapse = "+"))
      my.formula2 <- as.formula(my.formula2)
      
      bagTreeFit <- train(my.formula2, data = dataTrain,
                      method = "treebag",
                      preProcess = c("center", "scale"),
                      trControl = trainControl(method = "repeatedcv", 
                                               number = cvTreeFolds, 
                                               repeats=cvTreeRepeats))
    }
    
    bagTreeVarImp <- varImp(bagTreeFit, scale = FALSE)
    
    incProgress(1/n, detail = paste("Running Random Forest Model..."))
    
    cvForestFolds <- input$forestCvFold
    cvForestRepeats <- input$forestCvRepeats
    
    if (input$forestVariables == "all") {
      
      mtryValues <- data.frame(mtry=1:12) 
      ranForestFit <- train(price ~ ., data = dataTrain,
                            method = "rf",
                            preProcess = c("center", "scale"),
                            trControl = trainControl(method = "repeatedcv", 
                                                     number = cvForestFolds, 
                                                     repeats=cvForestRepeats),
                            tuneGrid=mtryValues)
    } else if (input$forestVariables == "custom"){
      
      my.formula3 <- paste("price", '~', paste(input$forestCustom, 
                                               collapse = "+"))
      my.formula3 <- as.formula(my.formula3)
      
      maxMtry <- length(input$forestCustom)
      mtryValues <- data.frame(mtry=1:maxMtry)
      
      ranForestFit <- train(my.formula3, data = dataTrain,
                            method = "rf",
                            preProcess = c("center", "scale"),
                            trControl = trainControl(method = "repeatedcv", 
                                                     number = cvForestFolds, 
                                                     repeats=cvForestRepeats),
                            tuneGrid=mtryValues)
      
    }
    
    ranForestVarImp <- varImp(ranForestFit, scale = FALSE)
    
                        
    incProgress(1/n, detail = paste("Comparing Each Model to Test Set..."))
    
    testResults <- rbind.data.frame(t(postResample(predict(mlrFit, 
                                                           newdata = dataTest), 
                                                   obs = dataTest$price)), 
                                    t(postResample(predict(bagTreeFit, 
                                                           newdata = dataTest), 
                                                   obs = dataTest$price)),
                                    t(postResample(predict(ranForestFit, 
                                                           newdata = dataTest), 
                                                   obs = dataTest$price)))
    testResults$Model <- c("Multiple Linear Regression Model", 
                           "Bagged Tree Regression Model", 
                           "Random Forest Model")
    testResults <- testResults %>% select(Model, everything())
    
    incProgress(1/n, detail = paste("Finished!"))})
    
    getTitle1("Here are the Model Results!")
    
    getSubTitle1("The following are fit statistics as well as appropriate summaries on the training data for each model:")
    
    getDescTitle1("Here is output of the summary function on the multiple linear regression model:")
    output$modelSummary1 <- renderPrint(summary(mlrFit))
    
    getDescTitle2("Here is the RMSE of the multiple linear regression model:")
    output$RMSE1 <- renderPrint(print(mlrRMSE, row.names=FALSE))
    
    getDescTitle3("Here is the resulting bagged tree regression model:")
    output$modelSummary2 <- renderPrint(bagTreeFit)
    
    getDescTitle4("Here is the resulting random forest model:")
    output$modelSummary3 <- renderPrint(ranForestFit)
    
    getSubTitle2("The following are variable importance plots for each model:")
    
    getDescTitle5("Here is the resulting variable importance plot for the multiple linear regression model:")
    output$plot1 <- renderPlot(plot(mlrVarImp, main="MLR Variable Importance Plot"))
    
    getDescTitle6("Here is the resulting variable importance plot for the bagged tree regression model:")
    output$plot2 <- renderPlot(plot(bagTreeVarImp, main="BT Variable Importance Plot"))
    
    getDescTitle7("Here is the resulting variable importance plot for the random forest model:")
    output$plot3 <- renderPlot(plot(ranForestVarImp, main="RF Variable Importance Plot"))
    
    getSubTitle3("Finally, the following are fit statistics of each model compared on the test set")
    
    output$testTable <- renderTable(testResults)
    
    getDescTitle8(paste("Based on the given inputs and fit statistics above, 
                        the best model for predicting price is the ", 
                        testResults$Model[which.min(testResults$RMSE)]))
    
  })
  
  observeEvent(input$getPrediction, {

    newData <- getData()
    
    set.seed(558)
    
    trainSplit <- input$trainSplit
    splitIndexes <- createDataPartition(newData$price, p=trainSplit, list=FALSE)
    dataTrain <- newData[splitIndexes, ]
    dataTest <- newData[-splitIndexes, ]
    
    withProgress(message='Please wait',detail='This may take a few minutes...', 
                 value=0, {
    
    n <- 2
    
    incProgress(1/n, detail = paste("Running Selected Model..."))
    
    if(input$predictModelType == "predictWithMLR"){
      predictModel <- train(price ~ ., data = dataTrain,
                            method = "lm",
                            preProcess = c("center", "scale"),
                            trControl = trainControl(method = "cv", number = 5))
      predictionTitle <- "Multiple Linear Regression Model,"
      
    } else if (input$predictModelType == "predictWithBT") {
      predictModel <- train(price ~ ., data = dataTrain,
                            method = "treebag",
                            preProcess = c("center", "scale"),
                            trControl = trainControl(method = "repeatedcv", 
                                                     number = 5, 
                                                     repeats= 3))
      predictionTitle <- "Bagged Tree Regression Model,"
      
    } else if (input$predictModelType == "predictWithRF") {
      mtryValues <- data.frame(mtry=1:12) 
      predictModel <- train(price ~ ., data = dataTrain,
                            method = "rf",
                            preProcess = c("center", "scale"),
                            trControl = trainControl(method = "repeatedcv", 
                                                     number = 5, 
                                                     repeats= 3),
                            tuneGrid=mtryValues)
      predictionTitle <- "Random Forest Model,"
    }
    
    getTitle2(paste("Based on the", 
                    predictionTitle,
                    " the Price of a House with those Attributes Rounded to the Nearest Dollar is Predicted to be:"))
    
    incProgress(1/n, detail = paste("Making Prediction..."))
    
    newPredictData <- data.frame(area=input$predictEntryArea,
                                 bedrooms=input$predictEntryBedrooms,
                                 bathrooms=input$predictEntryBathrooms,
                                 stories=input$predictEntryStories,
                                 mainroad=input$predictEntryMainroad,
                                 guestroom=input$predictEntryGuestroom,
                                 basement=input$predictEntryBasement,
                                 hotwaterheating=input$predictEntryHotwaterheating,
                                 airconditioning=input$predictEntryAirconditioning,
                                 parking=input$predictEntryParking,
                                 prefarea=input$predictEntryPrefarea,
                                 furnishingstatus=input$predictEntryFurnished)
    
    Prediction <- predict(predictModel, newdata = newPredictData)
    
    finalPrediction <- dollar(Prediction)
    
    getTitle3(finalPrediction)
    
    })
    
  })
  
  
})

