library(shiny)
library(raster)
options(shiny.maxRequestSize = 1024 * 1024 * 1024 * 1024 * 1024 ) # 1 TB

chelsa_ui <- fluidPage(
  titlePanel("CHELSA Data Process"),
  sidebarLayout(
    sidebarPanel(
      fileInput("chelsa_pr", "CHELSA PR", multiple = FALSE),
      fileInput("chelsa_tas", "CHELSA TAS", multiple = FALSE),
      fileInput("chelsa_tas_min", "CHELSA TAS MIN", multiple = FALSE),
      fileInput("chelsa_tas_max", "CHELSA TAS MAX ", multiple = FALSE),
      fileInput("chelsa_rad", "CHELSA RAD", multiple = FALSE),
      actionButton("run_process_chelsa", "Process CHELSA Data"),
      br(),
      actionLink("back_home", "Back to Home Page")
    ),
    mainPanel(
      fluidRow(
        column(width = 12, align = "center",
               img(src = "https://www.unwater.org/sites/default/files/styles/d02/public/app/uploads/2020/01/220x120_PARTNERS_IIASA.webp?itok=MjAJX7xP", height = 100)
        )
      ),
      br(),
      br(),
      h4("CHELSA Data Upload"),
      br(),
      HTML("This page deals with processing and preparing CHELSA climate data for use in areas where ICP climate data is unavailable.The processed data can then be utilized to replace missing ICP climate data, providing a reliable and accurate alternative."),
      br(),
      h4("Please upload all the required data files on the left to process."),
      h5("Learn more about CHELSA data and download required listed files :"),
      a("CHELSA Data Documentation", href = "https://chelsa-climate.org/"),
      br(),
      uiOutput("processing_message")
    )
  )
)
chelsa_climate <- function(chelsa_pr = NULL, chelsa_tas = NULL, chelsa_tas_min = NULL, chelsa_tas_max = NULL, chelsa_rad = NULL) {
  library(data.table)
  library(dplyr)
  library(lubridate)
  library(readxl)
  library(tidyverse)
  library(utils)
  library(tcltk)
  
  chelsa_clim <- list()
  
 
    # Load the CHELSA data into data frames
    c_pr <- data.table::fread(chelsa_pr$datapath, sep = ",", header = FALSE, data.table = TRUE, stringsAsFactors = FALSE)
    chelsa_pr <- as.data.frame(c_pr[, 2:5])
    colnames(chelsa_pr)[1:4] <- c("LAT", "LON", "DATE", "pr")
    rm(c_pr)
    chelsa_clim[["pr"]] <- chelsa_pr
    
    c_tas <- data.table::fread(chelsa_tas$datapath, sep = ",", header = FALSE, data.table = TRUE, stringsAsFactors = FALSE)
    chelsa_tas <- as.data.frame(c_tas[, 2:5])
    colnames(chelsa_tas)[1:4] <- c("LAT", "LON", "DATE", "tas")
    rm(c_tas)
    chelsa_clim[["tas"]] <- chelsa_tas
    
    c_tas_min <- data.table::fread(chelsa_tas_min$datapath, sep = ",", header = FALSE, data.table = TRUE, stringsAsFactors = FALSE)
    chelsa_tas_min <- as.data.frame(c_tas_min[, 2:5])
    colnames(chelsa_tas_min)[1:4] <- c("LAT", "LON", "DATE", "tas_min")
    rm(c_tas_min)
    chelsa_clim[["tas_min"]] <- chelsa_tas_min
    
    c_tas_max <- data.table::fread(chelsa_tas_max$datapath, sep = ",", header = FALSE, data.table = TRUE, stringsAsFactors = FALSE)
    chelsa_tas_max <- as.data.frame(c_tas_max[, 2:5])
    colnames(chelsa_tas_max)[1:4] <- c("LAT", "LON", "DATE", "tas_min")
    rm(c_tas_max)
    chelsa_clim[["tas_max"]] <- chelsa_tas_max
    
    c_rad <- data.table::fread(chelsa_rad$datapath, sep = ",", header = FALSE, data.table = TRUE, stringsAsFactors = FALSE)
    chelsa_rad <- as.data.frame(c_rad[, 2:5])
    colnames(chelsa_rad)[1:4] <- c("LAT", "LON", "DATE", "rad")
    rm(c_rad)
    chelsa_clim[["sr"]] <- chelsa_rad
  
  
  message("Climate Data processed!")
  
 
  
  return(chelsa_clim)
  
}

chelsa_server <- function(input, output, session, chelsa_clim) {
  chelsa_clim <- reactiveValues(chelsa_pr = NULL, chelsa_tas = NULL, chelsa_tas_min = NULL, chelsa_tas_max = NULL, chelsa_rad = NULL)
  
  chelsa_clim <- reactive({
    input$chelsa_pr
  })
  chelsa_clim <- reactive({
    input$chelsa_tas
  })
  chelsa_clim <- reactive({
    input$chelsa_tas_min
  })
  chelsa_clim <- reactive({
    input$chelsa_tas_max
  })
  chelsa_clim <- reactive({
    input$chelsa_rad
  })
  
  
  observeEvent(input$run_process_chelsa, {
    req(input$chelsa_pr)
    req(input$chelsa_tas)
    req(input$chelsa_tas_min)
    req(input$chelsa_tas_max)
    req(input$chelsa_rad)
    
    
    chelsa_clim <- chelsa_climate(chelsa_pr = input$chelsa_pr, chelsa_tas = input$chelsa_tas, chelsa_tas_min = input$chelsa_tas_min, chelsa_tas_max = input$chelsa_tas_max, chelsa_rad = input$chelsa_rad)
    
    # Add the processing message after processing is complete
    output$processing_message <- renderUI({
      HTML("<p>All files are processed! You can now return to the home page.</p>")
      
    })
  })
  
  observeEvent(input$back_home, {
    output$page <- renderUI(home_ui)
  })
}

shinyApp(ui = chelsa_ui, server = chelsa_server)







