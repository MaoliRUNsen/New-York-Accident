library(shiny)
library(leaflet)
library(tidyverse)
library(lubridate)
accidents <- read.csv("NYC accidents 2020.csv", stringsAsFactors = FALSE )
accidents= accidents %>% filter( BOROUGH !=  "") # removing NA values

accidents$CRASH.DATE = ymd(accidents$CRASH.DATE)


navbarPage("NYC Traffic accidents 2020", id="main",
           tabPanel("Data", DT::dataTableOutput("data")),
           tabPanel("Crashes in the New York City area", # Use a fluid Bootstrap layout
                    fluidPage(
                      # Give the page a title
                      titlePanel("Crashes in various regions of New York City"),
                      # Generate a row with a sidebar
                      sidebarLayout(
                        # Define the sidebar with one input
                        sidebarPanel(
                          selectInput("region", "BOROUGH:",
                                      choices=unique(accidents$BOROUGH)),
                          hr(),
                          helpText("Select New York City regions")
                        ),
                        # Create a spot for the barplot
                        mainPanel(
                          leafletOutput("map"),
                          plotOutput("bar")
                        )
                      )
                    )),

           tabPanel("Time period of accident", # Use a fluid Bootstrap layout
                    fluidPage(
                      # Give the page a title
                      titlePanel("Time period analysis of traffic accidents"),
                      # Generate a row with a sidebar
                      sidebarLayout(
                        # Define the sidebar with one input
                        sidebarPanel(
                          dateInput('startdate',
                                    label = 'Date input: yyyy-mm-dd',
                                    value = min(accidents$CRASH.DATE)
                          ),
                          helpText("Select startdate"),

                          dateInput('enddate',
                                    label = 'Date input: yyyy-mm-dd',
                                    value = max(accidents$CRASH.DATE)
                          ),
                          helpText("Select enddate")
                        ),
                        # Create a spot for the barplot
                        mainPanel(
                          plotOutput("line1"),
                          plotOutput("line2")
                        )
                      )
                    )),
           tabPanel("Vehicle Type", # Use a fluid Bootstrap layout
                    fluidPage(
                      # Give the page a title
                      titlePanel("Type of vehicle involved in the accident"),
                      # Generate a row with a sidebar
                      sidebarLayout(
                        # Define the sidebar with one input
                        sidebarPanel(
                          selectInput("Region", "BOROUGH:",
                                      choices=unique(accidents$BOROUGH)),
                          hr(),
                          helpText("Select New York City regions")
                        ),
                        # Create a spot for the barplot
                        mainPanel(
                          plotOutput("bar1"),
                          plotOutput("bar2")
                        )
                      )
                    )),


           tabPanel("Read Me",includeMarkdown("readme.md")))

