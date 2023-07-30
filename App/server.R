
library(shiny)
library(tidyverse)
library(caret)
library(scales)
library(DT)


shinyServer(function(input, output, session) {
  
  
  
  url <- a("Kaggle", href="https://www.kaggle.com/datasets/ashydv/housing-dataset")
  
  output$tab <- renderUI({
    tagList("Data Source:", url)
  })
  
  
  getData <- reactive({
    newData <- read_csv("../Housing.csv")
  })
  
  output$table <- renderTable({
    getData()
  })
  
  getTitle1 <- reactiveVal("Select the Model Options on the Side Panel and 
                           Click the Run Models Button to View Results!")
  getTitle2 <- reactiveVal("Select the Model of Interest for Making Predictions, 
                           Enter the Attributes of a Given House, and Click the 
                           Get Prediction Button to Receive a Prediction of the 
                           Price!")
  getTitle3 <- reactiveVal("")
  getTitle4 <- reactiveVal("Select a Model on the Left to Learn More About it!")
  
  getSubTitle1 <- reactiveVal("")
  getSubTitle2 <- reactiveVal("")
  getSubTitle3 <- reactiveVal("")
  getSubTitle4 <- reactiveVal("")
  getSubTitle5 <- reactiveVal("")
  getSubTitle6 <- reactiveVal("")
  
  getDescTitle1 <- reactiveVal("")
  getDescTitle2 <- reactiveVal("")
  getDescTitle3 <- reactiveVal("")
  getDescTitle4 <- reactiveVal("")
  getDescTitle5 <- reactiveVal("")
  getDescTitle6 <- reactiveVal("")
  getDescTitle7 <- reactiveVal("")
  getDescTitle8 <- reactiveVal("")
  
  getParagraph1 <- reactiveVal("")
  getParagraph2 <- reactiveVal("")
  getParagraph3 <- reactiveVal("")

  output$text_header1 <- renderUI({
    h1(getTitle1(), align = "center")
  })
  output$text_header2 <- renderUI({
    h1(getTitle2(), align = "center")
  })
  output$text_header3 <- renderUI({
    h1(getTitle3(), align = "center")
  })
  output$text_header4 <- renderUI({
    h1(getTitle4(), align = "center")
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
  output$subtext_header4 <- renderUI({
    h4(getSubTitle4(), align = "left")
  })
  output$subtext_header5 <- renderUI({
    h4(getSubTitle5(), align = "left")
  })
  output$subtext_header6 <- renderUI({
    h4(getSubTitle6(), align = "left")
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
  
  output$paragraph1 <- renderUI({
    p(getParagraph1(), align = "left")
  })
  output$paragraph2 <- renderUI({
    p(getParagraph2(), align = "left")
  })
  output$paragraph3 <- renderUI({
    p(getParagraph3(), align = "left")
  })

  
  
  observeEvent(input$trainSplit, {
    change <- 1-input$trainSplit
    updateNumericInput(session, "testSplit", value=change)
  })
  
  observeEvent(input$testSplit, {
    change <- 1-input$testSplit
    updateNumericInput(session, "trainSplit", value=change)
  })
  
  observeEvent(input$lowAreaRange | input$highAreaRange, {
    
    updateSliderInput(session, "areaRange", value=c(input$lowAreaRange, input$highAreaRange))
    
  })
  
  observeEvent(input$areaRange, {
    
    value <- input$areaRange
    lower <- value[1]
    upper <- value[2]
    
    updateNumericInput(session, "lowAreaRange", value=lower)
    updateNumericInput(session, "highAreaRange", value=upper)
    
  })
  
  observeEvent(input$lowPriceRange | input$highPriceRange, {
    
    updateSliderInput(session, "priceRange", value=c(input$lowPriceRange, input$highPriceRange))
    
  })
  
  observeEvent(input$priceRange, {
    
    value <- input$priceRange
    lower <- value[1]
    upper <- value[2]
    
    updateNumericInput(session, "lowPriceRange", value=lower)
    updateNumericInput(session, "highPriceRange", value=upper)
    
  })
  
  
  
  
  
  observeEvent(input$modelTypeInfo, {
    if(input$modelTypeInfo == "aboutMLR"){
      
      getTitle4("Multiple Linear Regression Model")
      getSubTitle4("What is a Multiple Linear Regression Model?")
      getParagraph1("A multiple linear regression model is used to model the 
                    relationship between a response variable (price) and two or 
                    more predictor variables (the remaining housing features). 
                    The goal of this model is to find the best fitting linear 
                    equation that minimizes the sum of squared errors and most 
                    accurately predicts the value of the response variable 
                    based on the values of the predictor variables.")
      
      output$modelMath <- renderUI({
        withMathJax(
          helpText("Mathematically, the multiple linear regression model can be 
                   represented as follows:$$Y_i = \\beta_0 + \\beta_1x_{1,i} + 
                   \\beta_2x_{2,i} + ... + \\beta_{p-1}x_{p-1,i} + E_i$$", 
                   style="color:#555555"),
          helpText("Where $Y_i$ represents the response variable (price), 
                   $x_{1,i}, x_{2,i}, ..., x_{p-1,i}$ are the predictor variables, and
                   $\\beta_0, \\beta_1, \\beta_2, ..., \\beta_p$ are the coefficients
                   of the regression model. We also assume that the error term, 
                   $E_i$, is independent and identically distributed with 
                   mean 0 and variance $\\sigma^2$.", style="color:#555555")
        )
      })
      
      getSubTitle5("What are the advantages of a Multiple Linear Regression 
                   Model?")
      getParagraph2("The advantages of the multiple linear regression model include
                    being far more simple and easy to interpret than the
                    ensemble based tree methods, being highly effective when
                    the relationship between the response variable and predictors
                    is linear, and the magnitude and signs of the coefficients
                    allow interpretation of the importance and direction of 
                    the effect of each predictor on the response. The multiple 
                    linear regression model is also very versatile in that it
                    can be adapted to handle many different types of data.")
      getSubTitle6("What are the disadvantages of a Multiple Linear Regression 
                   Model?")
      getParagraph3("The disadvantages of the multiple linear regression model
                    include requiring a few necessary statistical assumptions, its 
                    sensitivity to outliers, and multicollinearity which occurs
                    when predictors are highly correlated with each other.")
      
    }else if(input$modelTypeInfo == "aboutBT"){
      
      getTitle4("Bagged Tree Regression Model")
      getSubTitle4("What is a Bagged Tree Regression Model?")
      getParagraph1("A bagged tree regression model is an ensemble based learning
                    method built from multiple decision trees for regression.
                    'Bagged' stands for bootstrap aggregated, meaning that the model 
                    randomly selects n samples with replacement from the dataset
                    to create a new dataset so that a decision tree can be created
                    using the new boostrapped dataset. Based on this process, 
                    each decision tree may have a different structure and make 
                    different predictions. These steps are repeated many times
                    to create multiple decision trees and then combined so that
                    the final prediction for a new data point is the average of
                    the predictions from all the decision trees.")
      
      output$modelMath <- renderUI({
        withMathJax(
          helpText("The mathematical process for bagged regression trees is as follows:", 
                   style="color:#555555"),
          helpText("1. Create a bootstrap sample (same size as actual sample)", 
                   style="color:#555555"),
          helpText("2. Train the tree on this sample by calling the prediction 
                   for a given set of $x$ values $\\hat{y}^{*1}(x)$", 
                   style="color:#555555"),
          helpText("3. Repeat many times (perhaps $B$ = 1000) and obtain 
                   $\\hat{y}^{*j}(x), j = 1, ..., B$", 
                   style="color:#555555"),
          helpText("4. The final prediction is the average of these predictions
                   $\\hat{y}(x) = \\frac{1}{B}\\Sigma_{j=1}^B\\hat{y}^{*j}(x)$", 
                   style="color:#555555")
        )
        })
      getSubTitle5("What are the advantages of a Bagged Tree Regression Model?")
      getParagraph2("A bagged tree regression model improves prediction over 
                    a single tree fit by reducing the variance and overfitting 
                    of the prediction. It is also advantageous in that there are 
                    no statistical assumptions required and predictors do not 
                    need to be scaled.")
      getSubTitle6("What are the disadvantages of a Bagged Tree Regression 
                   Model?")
      getParagraph3("The main disadvantage of the bagged tree regression 
                    model is the reduced interpretability compared to a single 
                    decision tree due to the ensemble of trees. The other significant
                    disadvantage of the bagged tree regression model is that they are
                    computationally intensive meaning that they take lots of 
                    computing power to run effectively.")
      
    }else if(input$modelTypeInfo == "aboutRF"){
      
      getTitle4("Random Forest Model")
      getSubTitle4("What is a Random Forest Model?")
      
      getParagraph1("A random forest model is an ensemble based learning
                    method built from multiple decision trees for regression. A 
                    random forest model uses the same idea as bagging in that
                    the model randomly selects n samples with replacement from 
                    the dataset to create a new dataset so that a decision tree 
                    can be created using the new boostrapped dataset. 
                    Based on this process, each decision tree may have a different 
                    structure and make different predictions. These steps are 
                    repreated many times to create multiple decision trees and 
                    then combined so that the final prediction for a new data 
                    point is the average of the predictions from all the decision 
                    trees. However, the random forest model takes it a step
                    further from the bagged tree regression model and does not 
                    use all of the predictors and, instead, uses a random subset 
                    of predictors for each boostrap sample/tree fit.")
      
      output$modelMath <- renderUI({
        withMathJax(
          helpText("The mathematical process for random forest trees is as follows:", 
                   style="color:#555555"),
          helpText("1. Create a bootstrap sample (same size as actual sample)", 
                   style="color:#555555"),
          helpText("2. Train the tree on this sample by calling the prediction 
                   for a given set of $x$ values $\\hat{y}^{*1}(x)$", 
                   style="color:#555555"),
          helpText("3. Repeat many times (perhaps $B$ = 1000) and obtain 
                   $\\hat{y}^{*j}(x), j = 1, ..., B$", 
                   style="color:#555555"),
          helpText("4. The final prediction is the average of these predictions
                   $\\hat{y}(x) = \\frac{1}{B}\\Sigma_{j=1}^B\\hat{y}^{*j}(x)$", 
                   style="color:#555555")
        )
      })
    
      getSubTitle5("What are the advantages of a Random Forest Model?")
      getParagraph2("The random forest model is advantageous similarly to the 
                    bagged tree regression model in that it reduces the variance 
                    and overfitting of the prediction, as well as there are
                    no statistical assumptions required and predictors do not 
                    need to be scaled. The random forest model is more
                    advantageous than the bagged tree regression model because
                    it randomly selects a subset of predictors, which prevents 
                    a good predictor or two from dominating the tree fits. Thus,
                    if a really good predictor exists, each tree will probably 
                    use it for its first split, which makes bagged tree regression 
                    predictions more correlated.")
      getSubTitle6("What are the disadvantages of a Random Forest Model?")
      getParagraph3("Just like the bagged tree regression model, the main 
                    disadvantage of the random forest model is the 
                    reduced interpretability compared to a single decision tree 
                    due to the ensemble of trees. The other significant
                    disadvantage of the random forest model is that they are
                    computationally intensive meaning that they take lots of 
                    computing power to run effectively.")
      
    }
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
    
    getSubTitle1("The following are fit statistics as well as appropriate 
                 summaries on the training data for each model:")
    
    getDescTitle1("Here is output of the summary function on the multiple linear 
                  regression model:")
    output$modelSummary1 <- renderPrint(summary(mlrFit))
    
    getDescTitle2("Here is the RMSE of the multiple linear regression model:")
    output$RMSE1 <- renderPrint(print(mlrRMSE, row.names=FALSE))
    
    getDescTitle3("Here is the resulting bagged tree regression model:")
    output$modelSummary2 <- renderPrint(bagTreeFit)
    
    getDescTitle4("Here is the resulting random forest model:")
    output$modelSummary3 <- renderPrint(ranForestFit)
    
    getSubTitle2("The following are variable importance plots for each model:")
    
    getDescTitle5("Here is the resulting variable importance plot for the 
                  multiple linear regression model:")
    output$plot1 <- renderPlot(plot(mlrVarImp, main="MLR Variable Importance 
                                    Plot"))
    
    getDescTitle6("Here is the resulting variable importance plot for the bagged 
                  tree regression model:")
    output$plot2 <- renderPlot(plot(bagTreeVarImp, main="BT Variable Importance 
                                    Plot"))
    
    getDescTitle7("Here is the resulting variable importance plot for the random 
                  forest model:")
    output$plot3 <- renderPlot(plot(ranForestVarImp, main="RF Variable Importance 
                                    Plot"))
    
    getSubTitle3("Finally, the following are fit statistics of each model 
                 compared on the test set")
    
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
                    " the Price of a House with those Attributes Rounded to the 
                    Nearest Dollar is Predicted to be:"))
    
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
  
  output$dataTable <- renderDataTable({
    newData <- getData()
    datatable(newData, options = list(pageLength = 10,
                                      lengthMenu = c(10, 25, 50, 100), 
                                      scrollX = T))
  })
  
  observeEvent(input$subsetData, {
    
    
      newData <- getData()
      
      variables <- c("price", "area", "bedrooms", "bathrooms", "stories",
                     "mainroad", "guestroom", "basement", "hotwaterheating",
                     "airconditioning", "parking", "prefarea", "furnishingstatus")
      
      newData <- newData %>% 
        filter(price >= input$lowPriceRange,
               price <= input$highPriceRange,
               area >= input$lowAreaRange,
               area <= input$highAreaRange,
               bedrooms %in% input$bedroomSubset,
               bathrooms %in% input$bathroomSubset,
               stories %in% input$storiesSubset,
               mainroad %in% input$mainroadSubset,
               guestroom %in% input$guestroomSubset,
               basement %in% input$basementSubset,
               hotwaterheating %in% input$hotWaterHeatingSubset,
               airconditioning %in% input$airConditioningSubset,
               parking %in% input$parkingSubset,
               prefarea %in% input$prefareaSubset,
               furnishingstatus %in% input$furnishedSubset) %>%
        select(input$columnSubsets[input$columnSubsets %in% variables]) 
      
      
      output$dataTable <- renderDataTable({
      datatable(newData, options = list(pageLength = 10,
                                        lengthMenu = c(10, 25, 50, 100), 
                                        scrollX = T))
    })
    
  })
  
  
})

