

library(shiny)
library(shinythemes)


fluidPage(theme = shinytheme("cerulean"),
          tags$head(tags$style(".shiny-notification {position: fixed; top: 40%;left: 50%")),
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
                                                  label="Select Training Split Proportion",
                                                  value=.75, min=0, max=1,
                                                  step=.05),
                                           numericInput("testSplit",
                                                  label="Select Testing Split Proportion",
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
                                                              c("Area" = "area",
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
                                                                               c("Area" = "area",
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
                                                                               c("Area" = "area",
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
                                           uiOutput("text_header1"),
                                           br(),
                                           uiOutput("subtext_header1"),
                                           br(),
                                           uiOutput("desc_header1"),
                                           verbatimTextOutput(outputId = "modelSummary1"),
                                           uiOutput("desc_header2"),
                                           verbatimTextOutput(outputId = "RMSE1"),
                                           uiOutput("desc_header3"),
                                           verbatimTextOutput(outputId = "modelSummary2"),
                                           uiOutput("desc_header4"),
                                           verbatimTextOutput(outputId = "modelSummary3"),
                                           br(),
                                           uiOutput("subtext_header2"),
                                           br(),
                                           uiOutput("desc_header5"),
                                           plotOutput("plot1"),
                                           uiOutput("desc_header6"),
                                           plotOutput("plot2"),
                                           uiOutput("desc_header7"),
                                           plotOutput("plot3"),
                                           br(),
                                           uiOutput("subtext_header3"),
                                           tableOutput("testTable"),
                                           uiOutput("desc_header8"),
                                           br(),
                                           br()
                                         )
                                       )
                              ),
                              tabPanel("Prediction",
                                       sidebarLayout(
                                         sidebarPanel(
                                           h2(strong("Prediction Adjustments")),
                                           br(),
                                           radioButtons("predictModelType", 
                                                        "Which Model Would You Like to Make Predictions With?",
                                                        c("Multiple Linear Regression Model" = "predictWithMLR",
                                                          "Bagged Tree Regression Model" = "predictWithBT",
                                                          "Random Forest Model" = "predictWithRF"),
                                                        selected = character(0)),
                                           conditionalPanel(condition = "input.predictModelType",
                                           h3("House Attributes:"),
                                           numericInput("predictEntryArea",
                                                        label="Area (in Square Ft)",
                                                        value=2000, min=0, max=100000,
                                                        step=1),
                                           numericInput("predictEntryBedrooms",
                                                        label="Number of Bedrooms",
                                                        value=3, min=1, max=10,
                                                        step=1),
                                           numericInput("predictEntryBathrooms",
                                                        label="Number of Bathrooms",
                                                        value=2, min=1, max=10,
                                                        step=1),
                                           numericInput("predictEntryStories",
                                                        label="Number of Stories",
                                                        value=1, min=1, max=5,
                                                        step=1),
                                           radioButtons("predictEntryMainroad", 
                                                        "Is the house connected to a mainroad?",
                                                        c("Yes" = "yes",
                                                          "No" = "no")),
                                           radioButtons("predictEntryGuestroom", 
                                                        "Is there a guestroom?",
                                                        c("Yes" = "yes",
                                                          "No" = "no")),
                                           radioButtons("predictEntryBasement", 
                                                        "Is there a basement?",
                                                        c("Yes" = "yes",
                                                          "No" = "no")),
                                           radioButtons("predictEntryHotwaterheating", 
                                                        "Is there a hot water heater?",
                                                        c("Yes" = "yes",
                                                          "No" = "no")),
                                           radioButtons("predictEntryAirconditioning", 
                                                        "Is there air conditioning?",
                                                        c("Yes" = "yes",
                                                          "No" = "no")),
                                           numericInput("predictEntryParking",
                                                        label="Number of Parking Spots",
                                                        value=1, min=0, max=12,
                                                        step=1),
                                           radioButtons("predictEntryPrefarea", 
                                                        "Is the house in a perferred area?",
                                                        c("Yes" = "yes",
                                                          "No" = "no")),
                                           radioButtons("predictEntryFurnished", 
                                                        "What is the furnishing status of the home?",
                                                        c("Furnished" = "furnished",
                                                          "Semi-Furnished" = "semi-furnished",
                                                          "Unfurnished" = "unfurnished"))),
                                           br(),
                                           actionButton("getPrediction", strong("Get Prediction"),style = "color: #35a7e8;")
                                                            
                                                            
                                                            
                                           
                                         ),
                                         mainPanel(
                                           uiOutput("text_header2"),
                                           uiOutput("prediction"),
                                           uiOutput("text_header3")
                                           
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


