

library(shiny)

shinyUI(navbarPage("Navigation Tabs",
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
                            sidebarLayout(
                              sidebarPanel(
                                
                              ),
                              mainPanel(
                                
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
                   )
        )
)


