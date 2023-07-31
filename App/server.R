
library(shiny)
library(tidyverse)
library(caret)
library(scales)
library(DT)
library(stringr)
library(ggrepel)
#require libraries for the following code

#start of server page
shinyServer(function(input, output, session) {
  
  url <- a("Kaggle", href="https://www.kaggle.com/datasets/ashydv/housing-dataset")
  #url for data source
  
  #output for data source link on about page
  output$tab <- renderUI({
    tagList("Data Source:", url)
  })
  
  #function to get data with read_csv function
  getData <- reactive({
    newData <- read_csv("../data/Housing.csv")
  })
  
  #render table for data page
  output$table <- renderTable({
    getData()
  })
  
  #The following get title cluster is for the titles on the main page that remain
  #before the user clicks a submit button
  getTitle1 <- reactiveVal("Select the Model Options on the Side Panel and 
                           Click the Run Models Button to View Results!")
  getTitle2 <- reactiveVal("Select the Model of Interest for Making Predictions, 
                           Enter the Attributes of a Given House, and Click the 
                           Get Prediction Button to Receive a Prediction of the 
                           Price!")
  getTitle3 <- reactiveVal("")
  getTitle4 <- reactiveVal("Select a Model on the Left to Learn More About it!")
  getTitle5 <- reactiveVal("Select a Graph on the Left and Click 
                           Create Graph to View it Here!")
  getTitle6 <- reactiveVal("Select a Summary on the Left and Click 
                           Create Summary to View it Here!")
  
  #The following get subtitle cluster is set to blank so that they can be populated
  #when an action button is pressed.
  getSubTitle1 <- reactiveVal("")
  getSubTitle2 <- reactiveVal("")
  getSubTitle3 <- reactiveVal("")
  getSubTitle4 <- reactiveVal("")
  getSubTitle5 <- reactiveVal("")
  getSubTitle6 <- reactiveVal("")
  getSubTitle7 <- reactiveVal("")
  getSubTitle8 <- reactiveVal("")
  getSubTitle9 <- reactiveVal("")
  
  #The following get descriptive title cluster is set to blank so that they 
  #can be populated when an action button is pressed.
  getDescTitle1 <- reactiveVal("")
  getDescTitle2 <- reactiveVal("")
  getDescTitle3 <- reactiveVal("")
  getDescTitle4 <- reactiveVal("")
  getDescTitle5 <- reactiveVal("")
  getDescTitle6 <- reactiveVal("")
  getDescTitle7 <- reactiveVal("")
  getDescTitle8 <- reactiveVal("")
  
  #The following paragraph cluster is set to blank so that they 
  #can be populated when an action button is pressed.
  getParagraph1 <- reactiveVal("")
  getParagraph2 <- reactiveVal("")
  getParagraph3 <- reactiveVal("")

  #The following are to output the main title headers
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
  output$text_header5 <- renderUI({
    h1(getTitle5(), align = "center")
  })
  output$text_header6 <- renderUI({
    h1(getTitle6(), align = "center")
  })
  
  #The following are to output the subtitle headers
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
  output$subtext_header7 <- renderUI({
    h4(getSubTitle7(), align = "center")
  })
  output$subtext_header8 <- renderUI({
    h4(getSubTitle8(), align = "left")
  })
  output$subtext_header9 <- renderUI({
    h4(getSubTitle9(), align = "left")
  })
  
  #The following are to output the descriptive title headers
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
  
  #The following are to output the paragraphs for the model info tab
  output$paragraph1 <- renderUI({
    p(getParagraph1(), align = "left")
  })
  output$paragraph2 <- renderUI({
    p(getParagraph2(), align = "left")
  })
  output$paragraph3 <- renderUI({
    p(getParagraph3(), align = "left")
  })

  #This chunk is for someone clicks the run graph action button
  observeEvent(input$runGraph, {
    
    getTitle5("Here is the Graph!")
    #update the title
    
    #get the data
    newData <- getData()
    
    #conditional statement for if histogram is selected
    if (input$typeOfGraph == "hist"){
      getSubTitle7("")
      if(input$histogramSubset=="no"){
        basePlot <- ggplot(newData, aes(!!sym(input$typeOfHistogram)))
        #Creating the base of the plot
        finalGraph <- basePlot + geom_histogram(bins=input$numBins, fill="#428bca") +
          labs(x = str_to_title(input$typeOfHistogram), y = "Frequency", 
              title = paste("Histogram of", str_to_title(input$typeOfHistogram))) +
          #Adding descriptive labels
          theme(plot.title = element_text(hjust = 0.5)) 
          #Centering the title
      }else if(input$histogramSubset == "yes") {
        
        subsettedData <- newData %>% 
          filter((!!sym(input$typeOfHistogram)) >= input$lowRangeHist,
                 (!!sym(input$typeOfHistogram)) <= input$highRangeHist)

        basePlot <- ggplot(subsettedData, aes(!!sym(input$typeOfHistogram)))
        #Creating the base of the plot
        finalGraph <- basePlot + geom_histogram(bins=input$numBins, fill="#428bca") +
          labs(x = str_to_title(input$typeOfHistogram), y = "Frequency", 
               title = paste("Histogram of", str_to_title(input$typeOfHistogram))) +
          #Adding descriptive labels
          theme(plot.title = element_text(hjust = 0.5)) 
        #Centering the title
      }
      output$finalPlot <- renderPlot({finalGraph})
    }
    
    #conditional statement for if piechart is selected
    if (input$typeOfGraph == "pie"){
      if(input$typeOfPiechart == "mainroad"){
        getSubTitle7("Piechart of Mainroad Variable")
        counts <- data.frame(table(newData$mainroad))
        names(counts)[1] <- "group"
        names(counts)[2] <- "value"
        df2 <- counts %>% 
          mutate(csum = rev(cumsum(rev(value))), 
                 pos = value/2 + lead(csum, 1),
                 pos = if_else(is.na(pos), value/2, pos))
        
        finalGraph <- ggplot(counts, 
                             aes(x = "" , y = value, fill = fct_inorder(group))) +
          geom_col(width = 1, color = 1) +
          coord_polar(theta = "y") +
          geom_label_repel(data = df2,
                           aes(y = pos, 
                               label = paste0((round(value/545,4)*100), "%")),
                           size = 4.5, nudge_x = 1, show.legend = FALSE) +
          guides(fill = guide_legend(title = "Mainroad")) +
          theme_void()
      }else if(input$typeOfPiechart == "guestroom"){
        getSubTitle7("Piechart of Guestroom Variable")
        counts <- data.frame(table(newData$guestroom))
        names(counts)[1] <- "group"
        names(counts)[2] <- "value"
        df2 <- counts %>% 
          mutate(csum = rev(cumsum(rev(value))), 
                 pos = value/2 + lead(csum, 1),
                 pos = if_else(is.na(pos), value/2, pos))
        
        finalGraph <- ggplot(counts, 
                             aes(x = "" , y = value, fill = fct_inorder(group))) +
          geom_col(width = 1, color = 1) +
          coord_polar(theta = "y") +
          geom_label_repel(data = df2,
                           aes(y = pos, 
                               label = paste0((round(value/545,4)*100), "%")),
                           size = 4.5, nudge_x = 1, show.legend = FALSE) +
          guides(fill = guide_legend(title = "Guestroom")) +
          theme_void()
      }else if(input$typeOfPiechart == "basement"){
        getSubTitle7("Piechart of Basement Variable")
        counts <- data.frame(table(newData$basement))
        names(counts)[1] <- "group"
        names(counts)[2] <- "value"
        df2 <- counts %>% 
          mutate(csum = rev(cumsum(rev(value))), 
                 pos = value/2 + lead(csum, 1),
                 pos = if_else(is.na(pos), value/2, pos))
        
        finalGraph <- ggplot(counts, 
                             aes(x = "" , y = value, fill = fct_inorder(group))) +
          geom_col(width = 1, color = 1) +
          coord_polar(theta = "y") +
          geom_label_repel(data = df2,
                           aes(y = pos, 
                               label = paste0((round(value/545,4)*100), "%")),
                           size = 4.5, nudge_x = 1, show.legend = FALSE) +
          guides(fill = guide_legend(title = "Basement")) +
          theme_void()
      }else if(input$typeOfPiechart == "hotwaterheating"){
        getSubTitle7("Piechart of HotWaterHeating Variable")
        counts <- data.frame(table(newData$hotwaterheating))
        names(counts)[1] <- "group"
        names(counts)[2] <- "value"
        df2 <- counts %>% 
          mutate(csum = rev(cumsum(rev(value))), 
                 pos = value/2 + lead(csum, 1),
                 pos = if_else(is.na(pos), value/2, pos))
        
        finalGraph <- ggplot(counts, 
                             aes(x = "" , y = value, fill = fct_inorder(group))) +
          geom_col(width = 1, color = 1) +
          coord_polar(theta = "y") +
          geom_label_repel(data = df2,
                           aes(y = pos, label = paste0((round(value/545,4)*100), "%")),
                           size = 4.5, nudge_x = 1, show.legend = FALSE) +
          guides(fill = guide_legend(title = "HotWaterHeating")) +
          theme_void()
      }else if(input$typeOfPiechart == "airconditioning"){
        getSubTitle7("Piechart of Airconditioning Variable")
        counts <- data.frame(table(newData$airconditioning))
        names(counts)[1] <- "group"
        names(counts)[2] <- "value"
        df2 <- counts %>% 
          mutate(csum = rev(cumsum(rev(value))), 
                 pos = value/2 + lead(csum, 1),
                 pos = if_else(is.na(pos), value/2, pos))
        
        finalGraph <- ggplot(counts, 
                             aes(x = "" , y = value, fill = fct_inorder(group))) +
          geom_col(width = 1, color = 1) +
          coord_polar(theta = "y") +
          geom_label_repel(data = df2,
                           aes(y = pos, 
                               label = paste0((round(value/545,4)*100), "%")),
                           size = 4.5, nudge_x = 1, show.legend = FALSE) +
          guides(fill = guide_legend(title = "Airconditioning")) +
          theme_void()
      }else if(input$typeOfPiechart == "prefarea"){
        getSubTitle7("Piechart of PreferredArea Variable")
        counts <- data.frame(table(newData$prefarea))
        names(counts)[1] <- "group"
        names(counts)[2] <- "value"
        df2 <- counts %>% 
          mutate(csum = rev(cumsum(rev(value))), 
                 pos = value/2 + lead(csum, 1),
                 pos = if_else(is.na(pos), value/2, pos))
        
        finalGraph <- ggplot(counts, aes(x = "" , y = value, 
                                         fill = fct_inorder(group))) +
          geom_col(width = 1, color = 1) +
          coord_polar(theta = "y") +
          geom_label_repel(data = df2,
                           aes(y = pos, 
                               label = paste0((round(value/545,4)*100), "%")),
                           size = 4.5, nudge_x = 1, show.legend = FALSE) +
          guides(fill = guide_legend(title = "PreferredArea")) +
          theme_void()
      }else if(input$typeOfPiechart == "furnishingstatus"){
        getSubTitle7("Piechart of FurnishingStatus Variable")
        counts <- data.frame(table(newData$furnishingstatus))
        names(counts)[1] <- "group"
        names(counts)[2] <- "value"
        df2 <- counts %>% 
          mutate(csum = rev(cumsum(rev(value))), 
                 pos = value/2 + lead(csum, 1),
                 pos = if_else(is.na(pos), value/2, pos))
        
        finalGraph <- ggplot(counts, aes(x = "" , y = value, 
                                         fill = fct_inorder(group))) +
          geom_col(width = 1, color = 1) +
          coord_polar(theta = "y") +
          geom_label_repel(data = df2,
                           aes(y = pos, label = paste0((round(value/545,4)*100), "%")),
                           size = 4.5, nudge_x = 1, show.legend = FALSE) +
          guides(fill = guide_legend(title = "FurnishingStatus")) +
          theme_void()
      }
      output$finalPlot <- renderPlot({finalGraph})
    }
    #conditional statement for if scatterplot is selected
    if (input$typeOfGraph == "scatter"){
      getSubTitle7("")
      if(input$scatterSubset == "no"){
        basePlot <- ggplot(newData, aes(x=!!sym(input$xScatter), 
                                        y=!!sym(input$yScatter)))
        #Creating the base of the plot
        finalGraph <- basePlot + geom_point(color="#428bca") +
          labs(x = str_to_title(input$xScatter), y = str_to_title(input$yScatter), 
               title = paste("Scatter Plot of", str_to_title(input$yScatter), "Vs", 
                             str_to_title(input$xScatter))) +
          #Adding descriptive labels
          theme(plot.title = element_text(hjust = 0.5)) 
        #Centering the title
      }else if(input$scatterSubset == "yes") {
        
        subsettedData <- newData %>% 
          filter((!!sym(input$yScatter)) >= input$lowRangeScatter1,
                 (!!sym(input$yScatter)) <= input$highRangeScatter1,
                 (!!sym(input$xScatter)) >= input$lowRangeScatter2,
                 (!!sym(input$xScatter)) <= input$highRangeScatter2)
        
        basePlot <- ggplot(subsettedData, aes(x=!!sym(input$xScatter), 
                                              y=!!sym(input$yScatter)))
        #Creating the base of the plot
        finalGraph <- basePlot + geom_point(color="#428bca") +
          labs(x = str_to_title(input$xScatter), y = str_to_title(input$yScatter), 
               title = paste("Scatter Plot of", str_to_title(input$yScatter), "Vs", 
                             str_to_title(input$xScatter))) +
          #Adding descriptive labels
          theme(plot.title = element_text(hjust = 0.5)) 
        #Centering the title
      }
      output$finalPlot <- renderPlot({finalGraph})
    }
    #conditional statement for if barplot is selected
    if (input$typeOfGraph == "bar"){
      getSubTitle7("")
      if(input$barplotSubset == "no"){
        numeric_summary <- newData %>% group_by(!!sym(input$typeOfBarplotCat)) %>% 
          summarise(Average = mean(!!sym(input$typeOfBarplotNum)))
        
        basePlot <- ggplot(numeric_summary, 
                           aes(x=!!sym(input$typeOfBarplotCat), y=Average, 
                               fill=!!sym(input$typeOfBarplotCat))) 
        
        finalGraph <- basePlot + geom_bar(stat='identity') +
          labs(x = str_to_title(input$typeOfBarplotCat), 
               y = paste("Average", str_to_title(input$typeOfBarplotNum)), 
               title = paste("Boxplot of Average", 
                             str_to_title(input$typeOfBarplotNum), "by", 
                             str_to_title(input$typeOfBarplotCat))) +
          #Adding descriptive labels
          theme(plot.title = element_text(hjust = 0.5)) 
        #Centering the title
      }else if(input$barplotSubset == "yes"){
        
        subsettedData <- newData %>% 
          filter((!!sym(input$typeOfBarplotNum)) >= input$lowRangeBar,
                 (!!sym(input$typeOfBarplotNum)) <= input$highRangeBar)
        
        numeric_summary <- subsettedData %>% group_by(!!sym(input$typeOfBarplotCat)) %>% 
          summarise(Average = mean(!!sym(input$typeOfBarplotNum)))
        
        basePlot <- ggplot(numeric_summary, 
                           aes(x=!!sym(input$typeOfBarplotCat),y=Average, 
                               fill=!!sym(input$typeOfBarplotCat))) 
        
        finalGraph <- basePlot + geom_bar(stat='identity') +
          labs(x = str_to_title(input$typeOfBarplotCat), 
               y = paste("Average", str_to_title(input$typeOfBarplotNum)), 
               title = paste("Barplot of Average", 
                             str_to_title(input$typeOfBarplotNum), "by", 
                             str_to_title(input$typeOfBarplotCat))) +
          #Adding descriptive labels
          theme(plot.title = element_text(hjust = 0.5)) 
        #Centering the title
      }
      output$finalPlot <- renderPlot({finalGraph})
    }
    #conditional statement for if boxplot is selected
    if (input$typeOfGraph == "box"){
      getSubTitle7("")
      if(input$boxplotSubset == "no"){
        set.seed(558) #setting seed for reproducibility 
        basePlot <- ggplot(newData, aes(x = (!!sym(input$typeOfBoxplotCat)), 
                                        y = (!!sym(input$typeOfBoxplotNum)))) 
        #Creating the base of the plot
        finalGraph <- basePlot + geom_boxplot() + 
          geom_point(aes(color=(!!sym(input$typeOfBoxplotCat))), position="jitter") +
          labs(x = str_to_title(input$typeOfBoxplotCat), y = str_to_title(input$typeOfBoxplotNum), 
               title = paste("Boxplot of", str_to_title(input$typeOfBoxplotNum), "by", 
                             str_to_title(input$typeOfBoxplotCat))) +
          #Adding descriptive labels
          theme(plot.title = element_text(hjust = 0.5)) 
        #Centering the title
      }else if(input$boxplotSubset == "yes"){
        
        subsettedData <- newData %>% 
          filter((!!sym(input$typeOfBoxplotNum)) >= input$lowRangeBox,
                 (!!sym(input$typeOfBoxplotNum)) <= input$highRangeBox)
        
        set.seed(558) #setting seed for reproducibility 
        basePlot <- ggplot(subsettedData, aes(x = (!!sym(input$typeOfBoxplotCat)), 
                                        y = (!!sym(input$typeOfBoxplotNum)))) 
        #Creating the base of the plot
        finalGraph <- basePlot + geom_boxplot() + 
          geom_point(aes(color=(!!sym(input$typeOfBoxplotCat))), position="jitter") +
          labs(x = str_to_title(input$typeOfBoxplotCat), y = str_to_title(input$typeOfBoxplotNum), 
               title = paste("Boxplot of", str_to_title(input$typeOfBoxplotNum), "by", 
                             str_to_title(input$typeOfBoxplotCat))) +
          #Adding descriptive labels
          theme(plot.title = element_text(hjust = 0.5)) 
        #Centering the title
        
      }
      output$finalPlot <- renderPlot({finalGraph})
    }
    
  }) #end of graph tab
  
  #The following is if someone clicks on the run summary action button
  observeEvent(input$runSummary, {
    #update title
    getTitle6("Here is the Summary!")
    #get data
    newData <- getData()
    #The following is if someone wants a contingency table
    if (input$typeOfSummary == "contingency"){
      getSubTitle8("Contingency Table")
      if(input$typeOfContingencyTable == "one"){
        finalSum <- table(newData[, input$singleCatVar])
      }else if(input$typeOfContingencyTable == "two"){
        finalSum <- table(newData[, c(input$doubleCatVar1, input$doubleCatVar2)])
      }else if(input$typeOfContingencyTable == "three"){
        finalSum <- table(newData[, c(input$tripleCatVar1, input$tripleCatVar2, input$tripleCatVar3)])
      }
      output$finalSummary1 <- renderPrint(finalSum)
      
    }
    #The following is if someone wants a numeric summary
    if (input$typeOfSummary == "summary"){
      getSubTitle9("Numeric Summary")
      if(input$summarySubset == "no"){
        finalSum <- newData %>% summarise(Min = min(!!sym(input$typeOfSummaryNum)),
                                          firstQuartile = quantile(!!sym(input$typeOfSummaryNum), 0.25),
                                          Avg=round(mean(!!sym(input$typeOfSummaryNum)),2),
                                          Med=median(!!sym(input$typeOfSummaryNum)),
                                          thirdQuartile = quantile(!!sym(input$typeOfSummaryNum), 0.75),
                                          max = max(!!sym(input$typeOfSummaryNum)),
                                          stdDev = round(sd(!!sym(input$typeOfSummaryNum)),2))
        names(finalSum)[1] <- "Minimum"
        names(finalSum)[2] <- "First Quartile"
        names(finalSum)[3] <- "Average"
        names(finalSum)[4] <- "Median"
        names(finalSum)[5] <- "Third Quartile"
        names(finalSum)[6] <- "Maximum"
        names(finalSum)[7] <- "Standard Deviation"
      }else if(input$summarySubset == "yes"){
        
        subsettedData <- newData %>% 
          filter((!!sym(input$typeOfSummaryNum)) >= input$lowRangeSum,
                 (!!sym(input$typeOfSummaryNum)) <= input$highRangeSum)
        
        finalSum <- subsettedData %>% summarise(Min = min(!!sym(input$typeOfSummaryNum)),
                                          firstQuartile = quantile(!!sym(input$typeOfSummaryNum), 0.25),
                                          Avg=round(mean(!!sym(input$typeOfSummaryNum)),2),
                                          Med=median(!!sym(input$typeOfSummaryNum)),
                                          thirdQuartile = quantile(!!sym(input$typeOfSummaryNum), 0.75),
                                          max = max(!!sym(input$typeOfSummaryNum)),
                                          stdDev = round(sd(!!sym(input$typeOfSummaryNum)),2)) 
        names(finalSum)[1] <- "Minimum"
        names(finalSum)[2] <- "First Quartile"
        names(finalSum)[3] <- "Average"
        names(finalSum)[4] <- "Median"
        names(finalSum)[5] <- "Third Quartile"
        names(finalSum)[6] <- "Maximum"
        names(finalSum)[7] <- "Standard Deviation"
      }
      output$finalSummary2 <- renderDataTable({
        datatable(finalSum, options = list(scrollX = T,
                                           searching=F,
                                           paging=F))
      })
      
    }

  }) #end of summary tab
  
  #The following is if someone clicks on a model info button
  observeEvent(input$modelTypeInfo, {
    
    #The following are explanations about the MLR model
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
      
      #mathjax output
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
      
      #The following are explanations about the BT model
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
      
      #mathjax output
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
      
      #The following are explanations about the RF model
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
      
      #Math jax output
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
    
  #This chunk is to update the test split to be 1-trainsplit  
  observeEvent(input$trainSplit, {
    change <- 1-input$trainSplit
    updateNumericInput(session, "testSplit", value=change)
  })
  #This chunk is to update the train split to be 1-testsplit  
  observeEvent(input$testSplit, {
    change <- 1-input$testSplit
    updateNumericInput(session, "trainSplit", value=change)
  })
  
  #The following is for if someone clicks on the run models action button 
  #on the fitting tab  
  observeEvent(input$runModels, {
    
    set.seed(558) #setting seed for reproducibility 
    
    newData <- getData() #getting data
    
    #ensuring variables are factored
    newData$mainroad <- as.factor(newData$mainroad)
    newData$guestroom <- as.factor(newData$guestroom)
    newData$basement <- as.factor(newData$basement)
    newData$hotwaterheating <- as.factor(newData$hotwaterheating)
    newData$airconditioning <- as.factor(newData$airconditioning)
    newData$prefarea <- as.factor(newData$prefarea)
    newData$furnishingstatus <- as.factor(newData$furnishingstatus)
    
    #Start of progress bar
    withProgress(message='Please wait',detail='This may take a few minutes...', 
                 value=0, {
    
    n <- 6 #set for number of progress updates
    
    incProgress(1/n, detail = paste("Splitting Data..."))
    
    #splitting data based on user input
    trainSplit <- input$trainSplit
    splitIndexes <- createDataPartition(newData$price, p=trainSplit, list=FALSE)
    dataTrain <- newData[splitIndexes, ]
    dataTest <- newData[-splitIndexes, ]
    
    #The following are for running the MLR model
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
    
    #The following is for running the BT model
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
    
    #the following is for running the random forest model
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
    
    #creation of model result comparisons
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
    
    #the following are for updating the text and exporting the outputs
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
  
  #The following is for is someone clicks on the get prediction action button
  observeEvent(input$getPrediction, {

    #getting data
    newData <- getData()
    
    #setting seed for reproducibility
    set.seed(558)
    
    #spliting the data into training and testing split defaulting to the split 
    #from fitting tab
    trainSplit <- input$trainSplit
    splitIndexes <- createDataPartition(newData$price, p=trainSplit, list=FALSE)
    dataTrain <- newData[splitIndexes, ]
    dataTest <- newData[-splitIndexes, ]
    
    withProgress(message='Please wait',detail='This may take a few minutes...', 
                 value=0, {
    
    n <- 2
    
    incProgress(1/n, detail = paste("Running Selected Model..."))
    
    #Fitting the MLR model if the user selected that
    if(input$predictModelType == "predictWithMLR"){
      predictModel <- train(price ~ ., data = dataTrain,
                            method = "lm",
                            preProcess = c("center", "scale"),
                            trControl = trainControl(method = "cv", number = 5))
      predictionTitle <- "Multiple Linear Regression Model,"
      
      #Fitting the BT model if the user selected that
    } else if (input$predictModelType == "predictWithBT") {
      predictModel <- train(price ~ ., data = dataTrain,
                            method = "treebag",
                            preProcess = c("center", "scale"),
                            trControl = trainControl(method = "repeatedcv", 
                                                     number = 5, 
                                                     repeats= 3))
      predictionTitle <- "Bagged Tree Regression Model,"
      
      #Fitting the RF model if the user selected that
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
    
    #outputting new title
    getTitle2(paste("Based on the", 
                    predictionTitle,
                    " the Price of a House with those Attributes Rounded to the 
                    Nearest Dollar is Predicted to be:"))
    
    incProgress(1/n, detail = paste("Making Prediction..."))
    
    #creating newdata dataframe for prediction
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
    
    #creating prediction
    Prediction <- predict(predictModel, newdata = newPredictData)
    
    #converting number into dollar
    finalPrediction <- dollar(Prediction)
    
    #outputting prediction
    getTitle3(finalPrediction)
    
    })
    
  })
  
  #This is to update the slider on the data page based on the numeric input for the area var
  observeEvent(input$lowAreaRange | input$highAreaRange, {
    updateSliderInput(session, "areaRange", value=c(input$lowAreaRange, input$highAreaRange))
  })
  
  #This is to update the numeric inputs based on the slider on the data page for the area var
  observeEvent(input$areaRange, {
    value <- input$areaRange
    lower <- value[1]
    upper <- value[2]
    updateNumericInput(session, "lowAreaRange", value=lower)
    updateNumericInput(session, "highAreaRange", value=upper)
  })
  
  #This is to update the slider on the data page based on the numeric input for the price var
  observeEvent(input$lowPriceRange | input$highPriceRange, {
    updateSliderInput(session, "priceRange", value=c(input$lowPriceRange, input$highPriceRange))
  })
  
  #This is to update the numeric inputs based on the slider on the data page for the price var
  observeEvent(input$priceRange, {
    value <- input$priceRange
    lower <- value[1]
    upper <- value[2]
    updateNumericInput(session, "lowPriceRange", value=lower)
    updateNumericInput(session, "highPriceRange", value=upper)
  })
  
  #Output of data table prior to subsets
  output$dataTable <- renderDataTable({
    newData <- getData()
    datatable(newData, options = list(pageLength = 10,
                                      lengthMenu = c(10, 25, 50, 100), 
                                      scrollX = T))
  })
  
  #The following is if a user clicks the subset data action button
  observeEvent(input$subsetData, {

      newData <- getData() #getting data
      
      #Creating list of variables for column subset
      variables <- c("price", "area", "bedrooms", "bathrooms", "stories",
                     "mainroad", "guestroom", "basement", "hotwaterheating",
                     "airconditioning", "parking", "prefarea", "furnishingstatus")
      
      #filtering rows/columns based on user input
      newDataSubsetted <- newData %>% 
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
      
      datasetInput <- reactive({newDataSubsetted}) #saving subsetted data
      
      #Download subsetted csv option
      output$downloadCSV <- downloadHandler(
        filename = function() {
          paste("SubsettedHousingData", ".csv", sep = "")
        },
        content = function(file) {
          write.csv(datasetInput(), file, row.names = FALSE)
        }
      )
      
      #output of subsetted data to viewer on main page
      output$dataTable <- renderDataTable({
      datatable(newDataSubsetted, options = list(pageLength = 10,
                                        lengthMenu = c(10, 25, 50, 100), 
                                        scrollX = T))
      })

  })
  
  #the following is if the user does not subset the data but still tries to 
  #download the data
  datasetInput <- reactive({getData()})
  
  output$downloadCSV <- downloadHandler(
    filename = function() {
      paste("NonSubsettedHousingData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
}) #End of shiny server
