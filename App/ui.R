

library(shiny)
library(shinythemes)


fluidPage(theme = shinytheme("cerulean"),
        navbarPage("Navigation Tabs",
                   tabPanel("About",
                            mainPanel(
                                h1("Welcome to my Housing App!", align="center"),
                                div(tags$img(src="house.png", 
                                             height="60%", 
                                             width="60%", 
                                             alt="Something went wrong", 
                                             deleteFile=FALSE),
                                    align="center"),
                                h3("Purpose of this App", align="center"),
                                p("The purpose of this app is to show the data",
                                  align="center"),
                                h3("Data Source", align="center"),
                                p("The data has been sourced from Kaggle",
                                  align="center"),
                                uiOutput("tab", align="center"),
                                h3("Purpose of Tabs", align="center"),
                                p("The tabs are to organize this app",
                                  align="center"),
                                br(),
                                width=12
                            )
                   ),
                   tabPanel("Data Exploration",
                            sidebarLayout(
                              sidebarPanel(
                                
                              ),
                              mainPanel(
                                
                              )
                            )
                   ),
                   tabPanel("Modeling",
                            tabsetPanel(
                              tabPanel("Modeling Info",
                                       sidebarLayout(
                                         sidebarPanel(
                                           
                                         ),
                                         mainPanel(
                                
                                         )
                                       )
                              ),
                              tabPanel("Model Fitting",
                                       sidebarLayout(
                                         sidebarPanel(
                                           numericInput("trainSplit",
                                                  label="Select Training Split",
                                                  value=.75, min=0, max=1,
                                                  step=.05),
                                           numericInput("testSplit",
                                                  label="Select Testing Split",
                                                  value=.25, min=0, max=1,
                                                  step=.05),
                                           h4(strong("Multiple Linear Regression Model"), style = "color:red;"),
                                           radioButtons("mlrVariables", 
                                                              "What Variables Should be Included?",
                                                              c("All Variables" = "all",
                                                                "Custom Subset" = "custom")),
                                           conditionalPanel(condition = "input.mlrVariables.includes('custom')",
                                           checkboxGroupInput("mlrCustom", 
                                                              "Variables Used in Model:",
                                                              c("Square Feet" = "area",
                                                                "Bedrooms" = "bedrooms",
                                                                "Bathrooms" = "bathrooms",
                                                                "Stories" = "stories",
                                                                "Mainroad" = "mainroad",
                                                                "Guestroom" = "guestroom",
                                                                "Basement" = "basement",
                                                                "HotWaterHeating" = "hotwaterheating",
                                                                "Airconditioning" = "airconditioning",
                                                                "Parking" = "parking",
                                                                "PreferredArea" = "prefarea",
                                                                "FurnishingStatus" = "furnishingstatus"),
                                                              selected = "area")),
                                           checkboxInput("mlrInteractions",
                                                         "Interactions among all variables considered"),
                                           numericInput("mlrCvFold",
                                                        label="Number of Folds for Cross Validation",
                                                        value=5, min=1, max=10,
                                                        step=1),
                                           h4(strong("Bagged Tree Regression Model"), style = "color:red;"),
                                           radioButtons("treeVariables", 
                                                        "What Variables Should be Included?",
                                                        c("All Variables" = "all",
                                                          "Custom Subset" = "custom")),
                                           conditionalPanel(condition = "input.treeVariables.includes('custom')",
                                                            checkboxGroupInput("treeCustom", 
                                                                               "Variables Used in Model:",
                                                                               c("Square Feet" = "area",
                                                                                 "Bedrooms" = "bedrooms",
                                                                                 "Bathrooms" = "bathrooms",
                                                                                 "Stories" = "stories",
                                                                                 "Mainroad" = "mainroad",
                                                                                 "Guestroom" = "guestroom",
                                                                                 "Basement" = "basement",
                                                                                 "HotWaterHeating" = "hotwaterheating",
                                                                                 "Airconditioning" = "airconditioning",
                                                                                 "Parking" = "parking",
                                                                                 "PreferredArea" = "prefarea",
                                                                                 "FurnishingStatus" = "furnishingstatus"),
                                                                               selected = "area")),
                                           numericInput("treeCvFold",
                                                        label="Number of Folds for Cross Validation",
                                                        value=5, min=1, max=10,
                                                        step=1),
                                           numericInput("treeCvRepeats",
                                                        label="Number of Repeats for Cross Validation",
                                                        value=3, min=1, max=10,
                                                        step=1),
                                           h4(strong("Random Forest Model"), style = "color:red;"),
                                           radioButtons("forestVariables", 
                                                        "What Variables Should be Included?",
                                                        c("All Variables" = "all",
                                                          "Custom Subset" = "custom")),
                                           conditionalPanel(condition = "input.forestVariables.includes('custom')",
                                                            checkboxGroupInput("forestCustom", 
                                                                               "Variables Used in Model:",
                                                                               c("Square Feet" = "area",
                                                                                 "Bedrooms" = "bedrooms",
                                                                                 "Bathrooms" = "bathrooms",
                                                                                 "Stories" = "stories",
                                                                                 "Mainroad" = "mainroad",
                                                                                 "Guestroom" = "guestroom",
                                                                                 "Basement" = "basement",
                                                                                 "HotWaterHeating" = "hotwaterheating",
                                                                                 "Airconditioning" = "airconditioning",
                                                                                 "Parking" = "parking",
                                                                                 "PreferredArea" = "prefarea",
                                                                                 "FurnishingStatus" = "furnishingstatus"),
                                                                               selected = "area")),
                                           numericInput("forestCvFold",
                                                        label="Number of Folds for Cross Validation",
                                                        value=5, min=1, max=10,
                                                        step=1),
                                           numericInput("forestCvRepeats",
                                                        label="Number of Repeats for Cross Validation",
                                                        value=3, min=1, max=10,
                                                        step=1),
                                           br(),
                                           actionButton("runModels", strong("Run Models"),style = "color: #35a7e8;")
                                         ),
                                         mainPanel(
                                           verbatimTextOutput(outputId = "modelSummary"),
                                           verbatimTextOutput(outputId = "vars")
                                         )
                                       )
                              ),
                              tabPanel("Prediction",
                                       sidebarLayout(
                                         sidebarPanel(
                                           
                                         ),
                                         mainPanel(
                                           
                                         )
                                       )
                              )
                            )  
                   ),
                   tabPanel("Data",
                            sidebarLayout(
                              sidebarPanel(
                                
                              ),
                              mainPanel(
                                
                              )
                            )
                   ),
        fluid=TRUE)
)


