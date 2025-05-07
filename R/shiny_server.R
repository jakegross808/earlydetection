library(shiny)
library(sf)
library(leaflet)
library(dplyr)
library(tidyverse)
library(DT)
#library(mapview)

#load and prep dataframe of inat obs for all the parks
master_pacn_inat <- read.csv("data/master_pacn_inat_updated_names_jake.csv")

#just keep the observations for species that are only on inat - I could change this to all observations though if that is prefered 
master_pacn_inat1 <- master_pacn_inat[master_pacn_inat$new == TRUE, ]

#select what columns you want in the displayed datatable
master_pacn_inat1 <- master_pacn_inat1 %>%
  select(Scientific_name, ParkName, latitude, longitude, year, url, quality_grade)

#make the links clickable
master_pacn_inat1$url <- paste0("<a href='",master_pacn_inat1$url,"' target='_blank'>",master_pacn_inat1$url,"</a>")

#make column of each park's full name for the selection tool
master_pacn_inat1$fullParkName <- 
  ifelse(master_pacn_inat1$ParkName == 'AMME', 'American Memorial Park',
         ifelse(master_pacn_inat1$ParkName == 'HALE', 'Haleakalā National Park',
                ifelse(master_pacn_inat1$ParkName == 'HAVO', "Hawai'i Volcanoes National Park",
                       ifelse(master_pacn_inat1$ParkName == 'KALA', 'Kalaupapa National Historical Park',
                              ifelse(master_pacn_inat1$ParkName == 'KAHO', "Kaloko-Honokohau National Historical Park",
                                     ifelse(master_pacn_inat1$ParkName == 'NPSA', "National Park of American Samoa",
                                            ifelse(master_pacn_inat1$ParkName == 'PUHE', "Pu‘ukoholā Heiau National Historic Site",     
                                                   ifelse(master_pacn_inat1$ParkName == 'PUHO', "Pu‘uhonau o Hōnaunau National Historical Park",
                                                          'War in the Pacific National Historical Park'))))))))

#map prep (sorry still dont know which packages are actually used form here or not so im still keeping them all just in case haha)

#library(shiny)
#library(tmap)
#library(leaflet)
## Load the conflicted package and set preferences
#library(ggplot2)
#install.packages('maptools')
#library(maptools)
#install.packages('mapdata')
#library(mapdata)
#install.packages('ggthemes')
#library(ggthemes)
#library(tibble)
#library(viridis)

#library(conflicted)
#conflict_prefer("filter", "dplyr", quiet = TRUE)
#conflict_prefer("count", "dplyr", quiet = TRUE)
#conflict_prefer("select", "dplyr", quiet = TRUE)
#conflict_prefer("arrange", "dplyr", quiet = TRUE)

#make a column for the label on the map
master_pacn_inat1$label <- with(master_pacn_inat1, paste(Scientific_name, '-', year))


######## shiny app woot woot! #####
# Define the side panel UI and server
sideUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      inputId = ns("park"),
      label = "Select PACN I&M Park", 
      choices = unique(master_pacn_inat1$fullParkName),
      selected = unique(master_pacn_inat1$fullParkName)[1] 
      
    ),
    actionButton(ns("action"),"Submit")
  )
  
}

#change the dataframe based on input
sideServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # define a reactive and return it
      react<-eventReactive(input$action,{
        
        omited <-subset(master_pacn_inat1, master_pacn_inat1$fullParkName %in% isolate(input$park))
    
      })
      
      return(react)
      
    })
}


# Define the UI and server functions for the map
mapUI <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map"))
  )
}

#server for the map
mapServer <- function(id, npsmap) {
  moduleServer(
    id,
    function(input, output, session) {
      output$map<-renderLeaflet({
        leaflet() %>% 
          addTiles() %>%
          addCircleMarkers(data=npsmap(),
                           radius = 4,
                           color = 'orange',
                           stroke = FALSE,
                           fillOpacity = 1,
                           popup = ~as.character(url), 
                           label = (~label)
          )})})}

## making the datatable too
dtUI <- function(id) {
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("dt"))
  )
}

dtServer <- function(id, npsdt){
  {
    moduleServer(
      id,
      function(input, output, session) {
      output$dt  <- DT::renderDataTable({
    datatable(npsdt(),
              extensions = 'Buttons',
              options = list(dom = 'Blfrtip',
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), #so you can export it different ways
                             pageLength=1000, #number of rows you can view at a time
                             scrollX=TRUE, #scroll x axis
                             sScrollY = '75vh', scrollCollapse = TRUE),
              escape = FALSE)}

  )})}}


# Build ui & server and then run
ui <- fluidPage(
  titlePanel("PACN I&M iNaturalist Early Detection"),
  fluidRow(
    column(width=3, sideUI("side")),
    br(),
    fluidRow(
      column(width=8, mapUI("npsmap"))),
    br(),
    fluidRow(
      column(width=12, dtUI("npsdt")))
  ))


#this is the server that is actually used when the app is run. it incorperates the other servers and pulls everything together
server <- function(input, output, session) {
  
  # use the reactive in another module
  park_input <- sideServer("side")
  mapServer("npsmap", park_input)
  dtServer('npsdt', park_input)
  
}
shinyApp(ui, server)


## this source is where I got the backbone for the shiny app code. I also based the datatable part of the shiny app on it:
#https://stackoverflow.com/questions/70550397/create-a-shiny-module-that-creates-a-leaflet-map-in-shiny-app

