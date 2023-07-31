
library(shiny)
library(shinythemes)
library(DT)
#Required libraries to run the following code

#The following is the complete UI for this app
fluidPage(theme = shinytheme("cerulean"),
          #the line below is for centering the display bar
          tags$head(tags$style(".shiny-notification {position: fixed; top: 40%;left: 50%")),
          # section below allows in-line LaTeX via $ in mathjax.
          tags$div(HTML("<script type='text/x-mathjax-config' >
            MathJax.Hub.Config({
            tex2jax: {inlineMath: [['$','$']]}
            });
            </script >
            ")),
          #Start of navigation tab wrapper
        navbarPage("Navigation Tabs",
                   tabPanel("About", #Creation of about page
                            mainPanel(
                              #title header for about page
                                h1("Welcome to my Housing App!", align="center"),
                                #insert of house picture
                                div(tags$img(src="house.png", 
                                             height="60%", 
                                             width="60%", 
                                             alt="Something went wrong", 
                                             deleteFile=FALSE),
                                    align="center"),
                                #explanation of the purpose of the app
                                h3("Purpose of this App", align="center"),
                                p("The purpose of this app is to allow users an
                                  interactive, investigative and exploratory 
                                  experience with some housing data, all while 
                                  also avoiding the need to know how to code in 
                                  R. The user should be able to adjust or 
                                  customize all of the inputs for the code 
                                  running on the backend through simple and easy 
                                  to understand widgets, effectively simplifying 
                                  the barrier between the user and the analysis."),
                                #explanation of the data source
                                h3("Data Source", align="center"),
                                p("The data used on this shiny application comes
                                   from the user 'Ashish' on Kaggle and provides
                                   the characteristics of 545 different homes ranging
                                   from the number of bedrooms and bathrooms to
                                   the furnished status of the home. The data
                                   contains a total of 13 variables, where the
                                   singular sale price variable will be used as
                                   the target response and the other 12 will be
                                   used as predictors. Unfortunately, there was
                                   not a proper file provided on the meta data
                                   so it is not clear which country the houses
                                   are based in or the currency of the sale price,
                                   but for the purposes of this app, we will assume
                                   that these are houses from the United States
                                   and the currency is USD."),
                                #Link to data
                                uiOutput("tab", align="center"),
                                #explanation of the purpose of the tabs
                                h3("Purpose of Tabs", align="center"),
                                p("This app is organized into 4 pages so that
                                  the user can move through each step of the 
                                  analysis. Currently, we are on the about page. 
                                  This page is to give users a chance to
                                  be introduced to the app and provide an intution
                                  into the data source. The next page over is 
                                  the data exploration page and is dedicated to 
                                  allow the user to explore the data. The page has
                                  been split into 2 tabs so that graphs and 
                                  summarizations were more organized. Both tabs offer
                                  a large selection of graphs and summarizations 
                                  for interesting combinations between different 
                                  variables. Moreover, the third page is dedicated
                                  to modeling the data. This page is also broken
                                  up into multiple tabs for organizational purposes.
                                  The first tab is for learning about each of the
                                  three models, both mathematically as well as
                                  verbally. The second tab is for fitting each 
                                  of the three models in which options for every possible
                                  argument and adjustment are provided so that the
                                  user can properly fit each model and customize
                                  them to their own analysis. The third tab is
                                  for making predictions with the user's preferred
                                  model so that they can get a predicted price of 
                                  a house given all of the remaining attributes.
                                  Finally, the last page is the data page and is
                                  dedicated to displaying the dataset, allowing
                                  the user to subset the rows and columns, as well
                                  as downloading the dataset as a csv. Please
                                  enjoy exploring this app!",
                                  align="center"),
                                br(),
                                br(),
                                width=12 #make sure the page extends the whole screen
                            )
                   ),
                   tabPanel("Data Exploration", #creation of data exploration page
                            tabsetPanel(
                              tabPanel("Graphs", #start of graphs tab
                                       sidebarLayout(
                                         sidebarPanel(h2("Select a Graph"),
                                                      #Selection of type of graph
                                           radioButtons("typeOfGraph", 
                                                        "Which Type of Graph Would You Like to See?",
                                                        c("A Histogram of a Single Numeric Variable" = "hist",
                                                          "A Piechart of a Single Categorical Variable" = "pie",
                                                          "A Scatter Plot of Two Numerical Variables" = "scatter",
                                                          "A Bar Plot of the Average Value of a Numeric Variable 
                                                          by a Categorical Variable" = "bar",
                                                          "A Boxplot of a Numerical Variable by a Categorical 
                                                          Variable" = "box"),
                                                        selected = character(0)),
                                           #The following conditional panels are additional options for each type of graph
                                           conditionalPanel(condition = "input.typeOfGraph == 'hist'",
                                                            radioButtons("typeOfHistogram", 
                                                                         "Which Numeric Variable Should Be Used?",
                                                                         c("Price" = "price",
                                                                           "Area" = "area",
                                                                           "Bedrooms" = "bedrooms",
                                                                           "Bathrooms" = "bathrooms",
                                                                           "Stories" = "stories",
                                                                           "Parking" = "parking"),
                                                                         selected = character(0)),
                                                            numericInput("numBins",
                                                                         label="Select the Number of Bins",
                                                                         value=30, min=0, max=100,
                                                                         step=5),
                                                            radioButtons("histogramSubset", 
                                                                        "Should the Values of the Numeric Variable be Subsetted?",
                                                                        c("Yes" = "yes",
                                                                          "No" = "no"),
                                                                        selected = "no"),
                                                            conditionalPanel(condition = "input.histogramSubset == 'yes'",
                                                                             numericInput("lowRangeHist",
                                                                                          "Choose a Minimum for the Numeric Variable:",
                                                                                          min = 0, max = 15000000, value = 1),
                                                                             numericInput("highRangeHist",
                                                                                          "Choose a Maximum for the Numeric Variable:",
                                                                                          min = 0, max = 15000000, value = 15))),
                                           conditionalPanel(condition = "input.typeOfGraph == 'pie'",
                                                            radioButtons("typeOfPiechart", 
                                                                         "Which Categorical Variable Should Be Used?",
                                                                         c("Mainroad" = "mainroad",
                                                                           "Guestroom" = "guestroom",
                                                                           "Basement" = "basement",
                                                                           "HotWaterHeating" = "hotwaterheating",
                                                                           "Airconditioning" = "airconditioning",
                                                                           "PreferredArea" = "prefarea",
                                                                           "FurnishingStatus" = "furnishingstatus"),
                                                                         selected = character(0))),
                                           conditionalPanel(condition = "input.typeOfGraph == 'scatter'",
                                                            radioButtons("yScatter", 
                                                                         "Which Numeric Variable Should Be on the Y Axis?",
                                                                         c("Price" = "price",
                                                                           "Area" = "area",
                                                                           "Bedrooms" = "bedrooms",
                                                                           "Bathrooms" = "bathrooms",
                                                                           "Stories" = "stories",
                                                                           "Parking" = "parking"),
                                                                         selected = character(0)),
                                                            radioButtons("xScatter", 
                                                                         "Which Numeric Variable Should Be on the X Axis?",
                                                                         c("Price" = "price",
                                                                           "Area" = "area",
                                                                           "Bedrooms" = "bedrooms",
                                                                           "Bathrooms" = "bathrooms",
                                                                           "Stories" = "stories",
                                                                           "Parking" = "parking"),
                                                                         selected = character(0)),
                                                            radioButtons("scatterSubset", 
                                                                         "Should the Values of the Numeric Variables be Subsetted?",
                                                                         c("Yes" = "yes",
                                                                           "No" = "no"),
                                                                         selected = "no"),
                                                            conditionalPanel(condition = "input.scatterSubset == 'yes'",
                                                                             numericInput("lowRangeScatter1",
                                                                                          "Choose a Minimum for the Numeric 
                                                                                          Variable on the Y Axis:",
                                                                                          min = 0, max = 15000000, value = 1),
                                                                             numericInput("highRangeScatter1",
                                                                                          "Choose a Maximum for the Numeric 
                                                                                          Variable on the Y Axis:",
                                                                                          min = 0, max = 15000000, value = 15),
                                                                             numericInput("lowRangeScatter2",
                                                                                          "Choose a Minimum for the Numeric 
                                                                                          Variable on the X Axis:",
                                                                                          min = 0, max = 15000000, value = 1),
                                                                             numericInput("highRangeScatter2",
                                                                                          "Choose a Maximum for the Numeric 
                                                                                          Variable on the X Axis:",
                                                                                          min = 0, max = 15000000, value = 15))),
                                           conditionalPanel(condition = "input.typeOfGraph == 'bar'",
                                                            radioButtons("typeOfBarplotNum", 
                                                                         "Which Numeric Variable Should Be Used?",
                                                                         c("Price" = "price",
                                                                           "Area" = "area",
                                                                           "Bedrooms" = "bedrooms",
                                                                           "Bathrooms" = "bathrooms",
                                                                           "Stories" = "stories",
                                                                           "Parking" = "parking"),
                                                                         selected = character(0)),
                                                            radioButtons("typeOfBarplotCat", 
                                                                         "Which Categorical Variable Should Be Used?",
                                                                         c("Mainroad" = "mainroad",
                                                                           "Guestroom" = "guestroom",
                                                                           "Basement" = "basement",
                                                                           "HotWaterHeating" = "hotwaterheating",
                                                                           "Airconditioning" = "airconditioning",
                                                                           "PreferredArea" = "prefarea",
                                                                           "FurnishingStatus" = "furnishingstatus"),
                                                                         selected = character(0)),
                                                            radioButtons("barplotSubset", 
                                                                         "Should the Values of the Numeric Variable be Subsetted?",
                                                                         c("Yes" = "yes",
                                                                           "No" = "no"),
                                                                         selected = "no"),
                                                            conditionalPanel(condition = "input.barplotSubset == 'yes'",
                                                                             numericInput("lowRangeBar",
                                                                                          "Choose a Minimum for the Numeric Variable:",
                                                                                          min = 0, max = 15000000, value = 1),
                                                                             numericInput("highRangeBar",
                                                                                          "Choose a Maximum for the Numeric Variable:",
                                                                                          min = 0, max = 15000000, value = 15))),
                                           conditionalPanel(condition = "input.typeOfGraph == 'box'",
                                                            radioButtons("typeOfBoxplotNum", 
                                                                         "Which Numeric Variable Should Be Used?",
                                                                         c("Price" = "price",
                                                                           "Area" = "area",
                                                                           "Bedrooms" = "bedrooms",
                                                                           "Bathrooms" = "bathrooms",
                                                                           "Stories" = "stories",
                                                                           "Parking" = "parking"),
                                                                         selected = character(0)),
                                                            radioButtons("typeOfBoxplotCat", 
                                                                         "Which Categorical Variable Should Be Used?",
                                                                         c("Mainroad" = "mainroad",
                                                                           "Guestroom" = "guestroom",
                                                                           "Basement" = "basement",
                                                                           "HotWaterHeating" = "hotwaterheating",
                                                                           "Airconditioning" = "airconditioning",
                                                                           "PreferredArea" = "prefarea",
                                                                           "FurnishingStatus" = "furnishingstatus"),
                                                                         selected = character(0)),
                                                            radioButtons("boxplotSubset", 
                                                                         "Should the Values of the Numeric Variable be Subsetted?",
                                                                         c("Yes" = "yes",
                                                                           "No" = "no"),
                                                                         selected = "no"),
                                                            conditionalPanel(condition = "input.boxplotSubset == 'yes'",
                                                                             numericInput("lowRangeBox",
                                                                                          "Choose a Minimum for the Numeric Variable:",
                                                                                          min = 0, max = 15000000, value = 1),
                                                                             numericInput("highRangeBox",
                                                                                          "Choose a Maximum for the Numeric Variable:",
                                                                                          min = 0, max = 15000000, value = 15))),
                                           br(),
                                           actionButton("runGraph", strong("Create Graph"),style = "color: #428bca;")
                                           #action button for creating the graph
                              ),
                              mainPanel(
                                #text header for explanation on how to make graphs
                                uiOutput("text_header5"),
                                br(),
                                #title for piecharts
                                uiOutput("subtext_header7"),
                                #Plot output
                                plotOutput("finalPlot")
                              ))
                   ),
                   tabPanel("Summaries", #Start of summaries tab for data exploration
                            sidebarLayout(
                              sidebarPanel(h2("Select a Summary"),
                                           #Options for numerical/categorical summaries
                                           radioButtons("typeOfSummary", 
                                                        "Which Type of Summary Would You Like to See?",
                                                        c("A Single or Multi-Level Contingency Table on One or More 
                                                          Categorical Variables" = "contingency",
                                                          "A Numerical Summary of a Single Numeric Variable" = "summary"),
                                                        selected = character(0)),
                                           #The following are conditional panels depending on what kind of summary was selected
                                           conditionalPanel(condition = "input.typeOfSummary == 'contingency'",
                                                            radioButtons("typeOfContingencyTable", 
                                                                         "How Many Levels Should the Contingency Table Have?",
                                                                         c("One" = "one",
                                                                           "Two" = "two",
                                                                           "Three" = "three"),
                                                                         selected = character(0)),
                                                            conditionalPanel(condition = "input.typeOfContingencyTable == 'one'",
                                                                             radioButtons("singleCatVar", 
                                                                                          "Which Categorical Variable Should Be Used?",
                                                                                          c("Mainroad" = "mainroad",
                                                                                            "Guestroom" = "guestroom",
                                                                                            "Basement" = "basement",
                                                                                            "HotWaterHeating" = "hotwaterheating",
                                                                                            "Airconditioning" = "airconditioning",
                                                                                            "PreferredArea" = "prefarea",
                                                                                            "FurnishingStatus" = "furnishingstatus"),
                                                                                          selected = character(0))),
                                                            conditionalPanel(condition = "input.typeOfContingencyTable == 'two'",
                                                                             radioButtons("doubleCatVar1", 
                                                                                          "Which Categorical Variable Should 
                                                                                          Be Used for the First Level?",
                                                                                          c("Mainroad" = "mainroad",
                                                                                            "Guestroom" = "guestroom",
                                                                                            "Basement" = "basement",
                                                                                            "HotWaterHeating" = "hotwaterheating",
                                                                                            "Airconditioning" = "airconditioning",
                                                                                            "PreferredArea" = "prefarea",
                                                                                            "FurnishingStatus" = "furnishingstatus"),
                                                                                          selected = character(0)),
                                                                             radioButtons("doubleCatVar2", 
                                                                                          "Which Categorical Variable Should 
                                                                                          Be Used for the Second Level?",
                                                                                          c("Mainroad" = "mainroad",
                                                                                            "Guestroom" = "guestroom",
                                                                                            "Basement" = "basement",
                                                                                            "HotWaterHeating" = "hotwaterheating",
                                                                                            "Airconditioning" = "airconditioning",
                                                                                            "PreferredArea" = "prefarea",
                                                                                            "FurnishingStatus" = "furnishingstatus"),
                                                                                          selected = character(0))),
                                                            conditionalPanel(condition = "input.typeOfContingencyTable == 'three'",
                                                                             radioButtons("tripleCatVar1", 
                                                                                          "Which Categorical Variable Should 
                                                                                          Be Used for the First Level?",
                                                                                          c("Mainroad" = "mainroad",
                                                                                            "Guestroom" = "guestroom",
                                                                                            "Basement" = "basement",
                                                                                            "HotWaterHeating" = "hotwaterheating",
                                                                                            "Airconditioning" = "airconditioning",
                                                                                            "PreferredArea" = "prefarea",
                                                                                            "FurnishingStatus" = "furnishingstatus"),
                                                                                          selected = character(0)),
                                                                             radioButtons("tripleCatVar2", 
                                                                                          "Which Categorical Variable Should 
                                                                                          Be Used for the Second Level?",
                                                                                          c("Mainroad" = "mainroad",
                                                                                            "Guestroom" = "guestroom",
                                                                                            "Basement" = "basement",
                                                                                            "HotWaterHeating" = "hotwaterheating",
                                                                                            "Airconditioning" = "airconditioning",
                                                                                            "PreferredArea" = "prefarea",
                                                                                            "FurnishingStatus" = "furnishingstatus"),
                                                                                          selected = character(0)),
                                                                             radioButtons("tripleCatVar3", 
                                                                                          "Which Categorical Variable Should 
                                                                                          Be Used for the Third Level?",
                                                                                          c("Mainroad" = "mainroad",
                                                                                            "Guestroom" = "guestroom",
                                                                                            "Basement" = "basement",
                                                                                            "HotWaterHeating" = "hotwaterheating",
                                                                                            "Airconditioning" = "airconditioning",
                                                                                            "PreferredArea" = "prefarea",
                                                                                            "FurnishingStatus" = "furnishingstatus"),
                                                                                          selected = character(0)))),
                                           conditionalPanel(condition = "input.typeOfSummary == 'summary'",
                                                            radioButtons("typeOfSummaryNum", 
                                                                         "Which Numeric Variable Should Be Used?",
                                                                         c("Price" = "price",
                                                                           "Area" = "area",
                                                                           "Bedrooms" = "bedrooms",
                                                                           "Bathrooms" = "bathrooms",
                                                                           "Stories" = "stories",
                                                                           "Parking" = "parking"),
                                                                         selected = character(0)),
                                                            radioButtons("summarySubset", 
                                                                         "Should the Values of the Numeric Variable be Subsetted?",
                                                                         c("Yes" = "yes",
                                                                           "No" = "no"),
                                                                         selected = "no"),
                                                            conditionalPanel(condition = "input.summarySubset == 'yes'",
                                                                             numericInput("lowRangeSum",
                                                                                          "Choose a Minimum for the Numeric Variable:",
                                                                                          min = 0, max = 15000000, value = 1),
                                                                             numericInput("highRangeSum",
                                                                                          "Choose a Maximum for the Numeric Variable:",
                                                                                          min = 0, max = 15000000, value = 15))),
                                           br(),
                                           actionButton("runSummary", strong("Create Summary"),style = "color: #428bca;")
                                           #action button for creating summaries
                              ),
                              mainPanel(
                                #Explanation into how to create summaries
                                uiOutput("text_header6"),
                                br(),
                                #header for contingency table
                                uiOutput("subtext_header8"),
                                #contingency table output
                                verbatimTextOutput("finalSummary1"),
                                #header for numerical summary
                                uiOutput("subtext_header9"),
                                #numerical summary table output
                                dataTableOutput("finalSummary2")
                              ))
                   ))),
                   tabPanel("Modeling", #Start of modeling page
                            tabsetPanel(
                              tabPanel("Modeling Info", #Start of modeling info tab
                                       sidebarLayout(
                                         sidebarPanel(
                                           h2(strong("Model Choice")),
                                           br(),
                                           #options for models to learn more about
                                           radioButtons("modelTypeInfo", 
                                                        "Which Model Would You Like to Learn More About?",
                                                        c("Multiple Linear Regression Model" = "aboutMLR",
                                                          "Bagged Tree Regression Model" = "aboutBT",
                                                          "Random Forest Model" = "aboutRF"),
                                                        selected = character(0))
                                         ),
                                         mainPanel(
                                           #header for whole main page
                                           uiOutput("text_header4"),
                                           br(),
                                           #header for what the model is
                                           uiOutput("subtext_header4"),
                                           #Explanation for what the model is
                                           uiOutput("paragraph1"),
                                           #mathjax output
                                           uiOutput("modelMath"),
                                           #header for advantages of model
                                           uiOutput("subtext_header5"),
                                           #explanation of advantages of model
                                           uiOutput("paragraph2"),
                                           #header for disadvantages of model
                                           uiOutput("subtext_header6"),
                                           #explanation of disadvantages of model
                                           uiOutput("paragraph3"),
                                           br(),
                                           br()
                                         )
                                       )
                              ),
                              tabPanel("Model Fitting", #start of model fitting tab
                                       sidebarLayout(
                                         sidebarPanel(
                                           #Training/testing split options
                                           numericInput("trainSplit",
                                                  label="Select Training Split Proportion",
                                                  value=.75, min=0, max=1,
                                                  step=.05),
                                           numericInput("testSplit",
                                                  label="Select Testing Split Proportion",
                                                  value=.25, min=0, max=1,
                                                  step=.05),
                                           #MLR options
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
                                           #Bagged tree options
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
                                           #random forest options
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
                                           actionButton("runModels", strong("Run Models"),style = "color: #428bca;")
                                           #action button for running the models
                                         ),
                                         mainPanel(
                                           #Overall header for model fitting page
                                           uiOutput("text_header1"),
                                           br(),
                                           #explanation header
                                           uiOutput("subtext_header1"),
                                           br(),
                                           #MLR summary descriptive header
                                           uiOutput("desc_header1"),
                                           #MLR summary output
                                           verbatimTextOutput("modelSummary1"),
                                           #MLR RMSE descriptive header
                                           uiOutput("desc_header2"),
                                           #MLR RMSE output
                                           verbatimTextOutput("RMSE1"),
                                           #Bagged tree summary header
                                           uiOutput("desc_header3"),
                                           #Bagged tree output
                                           verbatimTextOutput("modelSummary2"),
                                           #Random forest summary header
                                           uiOutput("desc_header4"),
                                           #random forest output
                                           verbatimTextOutput("modelSummary3"),
                                           br(),
                                           #plot header
                                           uiOutput("subtext_header2"),
                                           br(),
                                           #MLR plot header
                                           uiOutput("desc_header5"),
                                           #MLR plot
                                           plotOutput("plot1"),
                                           #BT plot header
                                           uiOutput("desc_header6"),
                                           #BT plot
                                           plotOutput("plot2"),
                                           #RF plot header
                                           uiOutput("desc_header7"),
                                           #RF plot
                                           plotOutput("plot3"),
                                           br(),
                                           #Comparison header
                                           uiOutput("subtext_header3"),
                                           #Table of comparison output
                                           tableOutput("testTable"),
                                           #Text about which model is the best
                                           uiOutput("desc_header8"),
                                           br(),
                                           br()
                                         )
                                       )
                              ),
                              tabPanel("Prediction", #start of prediction page
                                       sidebarLayout(
                                         sidebarPanel(
                                           h2(strong("Prediction Adjustments")),
                                           br(),
                                           #options for model to predict with
                                           radioButtons("predictModelType", 
                                                        "Which Model Would You Like to Make Predictions With?",
                                                        c("Multiple Linear Regression Model" = "predictWithMLR",
                                                          "Bagged Tree Regression Model" = "predictWithBT",
                                                          "Random Forest Model" = "predictWithRF"),
                                                        selected = character(0)),
                                           #panel for inserting house features
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
                                           actionButton("getPrediction", strong("Get Prediction"),style = "color: #428bca;")
                                           #action button for creating prediction
                                         ),
                                         mainPanel(
                                           #Explanation header for how to predict
                                           uiOutput("text_header2"),
                                           #prediction output
                                           uiOutput("text_header3")
                                           
                                         )
                                       )
                              )
                            )  
                   ),
                   tabPanel("Data", #start of data page
                            sidebarLayout(
                              sidebarPanel(
                                h2("Subset Options", align="center"),
                                br(),
                                h3("Subset Columns:"),
                                #subset column options
                                checkboxGroupInput("columnSubsets", 
                                                   "Which Columns Should be Included?",
                                                   c("Price" = "price",
                                                     "Area" = "area",
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
                                                   selected = c("price",
                                                                "area",
                                                                "bedrooms",
                                                                "bathrooms",
                                                                "stories",
                                                                "mainroad",
                                                                "guestroom",
                                                                "basement",
                                                                "hotwaterheating",
                                                                "airconditioning",
                                                                "parking",
                                                                "prefarea",
                                                                "furnishingstatus")),
                                
                                h3("Subset Rows:"),
                                #subset rows options
                                h4("Price Options:"),
                                sliderInput("priceRange",
                                            label = "Choose a range for the price variable", 
                                            min = 1000000, max = 15000000, value = c(1000000,15000000), dragRange=TRUE
                                ),
                                h5("For more a more precise price range, use the inputs below!"),
                                numericInput("lowPriceRange",
                                             "Choose a minimum for the price variable:",
                                             min = 1000000, max = 15000000, value = 1000000),
                                numericInput("highPriceRange",
                                             "Choose a maximum for the price variable:",
                                             min = 1000000, max = 15000000, value = 15000000),
                                h4("Area Options:"),
                                sliderInput("areaRange",
                                  label = "Choose a range for the area variable", 
                                  min = 0, max = 20000, value = c(0,20000), dragRange=TRUE
                                ),
                                h5("For more a more precise area range, use the inputs below!"),
                                numericInput("lowAreaRange",
                                             "Choose a minimum for the area variable:",
                                             min = 0, max = 20000, value = 0),
                                numericInput("highAreaRange",
                                             "Choose a maximum for the area variable:",
                                             min = 0, max = 20000, value = 20000),
                                h4("Options for Bedrooms:"),
                                checkboxGroupInput("bedroomSubset", 
                                                   "Select the interested number of bedrooms:",
                                                   c("One" = "1",
                                                     "Two" = "2",
                                                     "Three" = "3",
                                                     "Four" = "4",
                                                     "Five" = "5",
                                                     "Six" = "6"),
                                                   selected = c("1",
                                                                "2",
                                                                "3",
                                                                "4",
                                                                "5",
                                                                "6")),
                                h4("Options for Bathrooms:"),
                                checkboxGroupInput("bathroomSubset", 
                                                   "Select the interested number of bathrooms:",
                                                   c("One" = "1",
                                                     "Two" = "2",
                                                     "Three" = "3",
                                                     "Four" = "4"),
                                                   selected = c("1",
                                                                "2",
                                                                "3",
                                                                "4")),
                                h4("Options for Stories:"),
                                checkboxGroupInput("storiesSubset", 
                                                   "Select the interested number of stories:",
                                                   c("One" = "1",
                                                     "Two" = "2",
                                                     "Three" = "3",
                                                     "Four" = "4"),
                                                   selected = c("1",
                                                                "2",
                                                                "3",
                                                                "4")),
                                h4("Mainroad Options:"),
                                checkboxGroupInput("mainroadSubset", 
                                                   "Select the whether the house is connected to a mainroad:",
                                                   c("Yes" = "yes",
                                                     "No" = "no"),
                                                   selected = c("yes",
                                                                "no")),
                                h4("Guestroom Options:"),
                                checkboxGroupInput("guestroomSubset", 
                                                   "Select the whether the house has a guestoom:",
                                                   c("Yes" = "yes",
                                                     "No" = "no"),
                                                   selected = c("yes",
                                                                "no")),
                                h4("Basement Options:"),
                                checkboxGroupInput("basementSubset", 
                                                   "Select the whether the house has a basement:",
                                                   c("Yes" = "yes",
                                                     "No" = "no"),
                                                   selected = c("yes",
                                                                "no")),
                                h4("HotWaterHeating Options:"),
                                checkboxGroupInput("hotWaterHeatingSubset", 
                                                   "Select the whether the house has a hot water heater:",
                                                   c("Yes" = "yes",
                                                     "No" = "no"),
                                                   selected = c("yes",
                                                                "no")),
                                h4("AirConditioning Options:"),
                                checkboxGroupInput("airConditioningSubset", 
                                                   "Select the whether the house has air conditioning:",
                                                   c("Yes" = "yes",
                                                     "No" = "no"),
                                                   selected = c("yes",
                                                                "no")),
                                h4("Parking Options:"),
                                checkboxGroupInput("parkingSubset", 
                                                   "Select the interested number of parking spaces:",
                                                   c("Zero" = "0",
                                                     "One" = "1",
                                                     "Two" = "2",
                                                     "Three" = "3"),
                                                   selected = c("0",
                                                                "1",
                                                                "2",
                                                                "3")),
                                h4("PrefArea Options:"),
                                checkboxGroupInput("prefareaSubset", 
                                                   "Select the whether the house is in a preferred area:",
                                                   c("Yes" = "yes",
                                                     "No" = "no"),
                                                   selected = c("yes",
                                                                "no")),
                                h4("Furnishing Options:"),
                                checkboxGroupInput("furnishedSubset", 
                                                   "Select the interested furnished status:",
                                                   c("Furnished" = "furnished",
                                                     "Semi-Furnished" = "semi-furnished",
                                                     "Unfurnished" = "unfurnished"),
                                                   selected = c("furnished",
                                                                "semi-furnished",
                                                                "unfurnished")),
                                br(),
                                actionButton("subsetData", strong("Subset Data"),style = "color: #428bca;")
                                #action button for subsetting the data
                              ),
                              mainPanel(
                                #title for housing data
                                h1("The Housing Dataset", align="center"),
                                #explaination of page
                                h3("Please explore the data below or subset
                                   it with the panel on the left. Don't forget
                                   to click the submit button at the botton 
                                   after making adjustments!"),
                                br(),
                                #table output
                                dataTableOutput("dataTable"),
                                #download explanation
                                h3("Interested in saving the data? Click the 
                                    download button below!"),
                                #download button
                                downloadButton('downloadCSV',"Download", 
                                               style = "color: #428bca;"),
                                br()
                              )
                            )
                   ),
        fluid=TRUE)
) #end of UI
