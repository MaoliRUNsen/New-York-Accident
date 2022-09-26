

library(shiny)
library(lubridate)
library(tidyverse)
library(leaflet)
library(DT)
Sys.setenv(LANGUAGE = "en")
shinyServer(function(input, output) {
  # Import Data and clean it
  Accidents <- read.csv("NYC Accidents 2020.csv", stringsAsFactors = FALSE )
  Accidents=Accidents %>%  filter(BOROUGH !=  "") # removing NA values
  # change data type
  Accidents$CRASH.DATE = ymd(Accidents$CRASH.DATE)
  Accidents$CRASH.TIME = hms(Accidents$CRASH.TIME)
  Accidents$hours = hour(Accidents$CRASH.TIME)
  # Judge whether it is day or night
  Accidents$isday  = ifelse( (Accidents$hours >=6) & (Accidents$hours <=18),"day","night" )
  # Extract the required data columns and remove NA sorting and other operations
  Accidents = Accidents %>%
    select(CRASH.DATE,CRASH.TIME,LATITUDE,LONGITUDE,VEHICLE.TYPE.CODE.1,BOROUGH,isday,hours) %>%
    arrange(CRASH.DATE) %>%
    filter(LONGITUDE != 0) %>%
    filter(VEHICLE.TYPE.CODE.1 != "") %>%
    drop_na()



  # create the leaflet map
  output$map <- renderLeaflet({
    # Mainly draw the location of traffic accidents in different areas
    data = Accidents %>% filter(BOROUGH == input$region)
    leaflet(data) %>%
      addCircles(lng = ~LONGITUDE, lat = ~LATITUDE) %>%
      addTiles() %>%
      addCircleMarkers(data = data, lat =  ~LATITUDE, lng =~LONGITUDE,
                       radius = 0.001,  fillOpacity = 0.01)
  })

  #create a data object to display data

  output$data <-DT::renderDataTable(datatable(Accidents))
  output$bar <- renderPlot({
    data =Accidents %>% filter(BOROUGH == input$region) %>%   count(hours)
    ggplot(data,aes(x = hours, y = n )) + geom_col() + ylab("Number of traffic accidents") +   geom_text(aes(label=n),size=4,vjust=-0.5,col = "red")+ ggtitle(paste("Traffic accidents in 2020"," in " ,input$region,sep = ""))
  })

  # The broken line chart of the time period of the accident is divided by date
  output$line1 <- renderPlot({
    data = Accidents %>% filter(CRASH.DATE >= input$startdate,CRASH.DATE <= input$enddate) %>%  select(CRASH.DATE,BOROUGH )
    data %>%
      group_by(CRASH.DATE,BOROUGH)  %>%
      summarise(n =n()) %>%
     ggplot(aes(x = CRASH.DATE, y = n ,group =BOROUGH,color  = BOROUGH ))+ geom_line() + ylab("Number of traffic accidents") +geom_point()+ggtitle(paste("from" ,input$startdate, "to", input$enddate, "traffic accidents line in different areas ", sep = " "))
  })


  # The broken line chart of the time period of the accident is divided by isday
  output$line2 <- renderPlot({
    data = Accidents %>% filter(CRASH.DATE >= input$startdate,CRASH.DATE <= input$enddate) %>%  select(CRASH.DATE,isday)
    data %>%
      group_by(CRASH.DATE,isday)  %>%
      summarise(n =n()) %>%
      ggplot(aes(x = CRASH.DATE, y = n ,group =isday,color  = isday ))+ geom_line() + ylab("Number of traffic accidents") +geom_point()+ggtitle(paste("from" ,input$startdate, "to", input$enddate, "traffic accidents line in day or night", sep = " "))
  })

  # Time comparison of different types of vehicle accidents in top10 in the entire dataset
  output$bar1 <- renderPlot({
    Accidents %>%
      group_by(VEHICLE.TYPE.CODE.1)  %>%
      summarise(n =n()) %>%
      arrange(desc(n))%>%
      head(10)%>%
      ggplot(aes(x = n, y = fct_reorder(VEHICLE.TYPE.CODE.1,n)  ))+ geom_col() + ylab("Type of vehicle involved in the accident") + ggtitle("Bar Chart of Crash Vehicle Types for the Entire Dataset")
  })

  # Time comparison of different types of vehicle accidents in top10
  output$bar2 <- renderPlot({
    Accidents %>%
      filter(BOROUGH == input$Region) %>%
      group_by(VEHICLE.TYPE.CODE.1)  %>%
      summarise(n =n()) %>%
      arrange(desc(n))%>%
      head(10)%>%
      ggplot(aes(x = n, y = fct_reorder(VEHICLE.TYPE.CODE.1,n)  ))+
      geom_col() + ylab("Type of vehicle involved in the accident") +
      ggtitle(paste("Bar Chart of Crash Vehicle Types for the ",input$Region, " Dataset"))
  })
})
