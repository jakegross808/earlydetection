library(tidyverse)
library(shiny)
library(sf)
library(leaflet)
library(dplyr)
library(tidyverse)
library(DT)


#load and prep dataframe of inat obs for all the parks
undocumented_pacn_plants <- read.csv("data/undocumented_pacn_plants.csv")
undocumented_pacn_obs <- read.csv("data/undocumented_pacn_obs.csv")

#make a column for the label on the map
head(format(as.Date(undocumented_pacn_obs$observed_on_string), "%b %Y"))

undocumented_pacn_obs2 <- undocumented_pacn_obs |>
  mutate(obs_date = format(as.Date(observed_on_string), "%b %Y")) |>
  mutate(label = paste(taxon.name, '-', obs_date))


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

