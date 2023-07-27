
library(shiny)


function(input, output, session) {
  
  
  
  url <- a("Kaggle", href="https://www.kaggle.com/competitions/house-prices-advanced-regression-techniques/overview")
  
  output$tab <- renderUI({
    tagList("Data Source:", url)
  })
  
}

