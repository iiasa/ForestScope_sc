library(shiny)
library(raster)
options(shiny.maxRequestSize = 1024 * 1024 * 1024 * 1024 * 1024 ) # 1 TB

isric_ui <- fluidPage(
  titlePanel("ISRIC Soil Data process"),
  
  
  sidebarLayout(
    sidebarPanel(
      fileInput("awc_files", "Select WWP Files", multiple = TRUE),
      fileInput("clay_files", "Select Clay Files", multiple = TRUE),
      fileInput("silt_files", "Select Silt Files", multiple = TRUE),
      fileInput("sand_files", "Select Sand Files", multiple = TRUE),
      fileInput("soc_files", "Select SOC Files", multiple = TRUE),
      fileInput("nitrogen_files", "Select Nitrogen Files", multiple = TRUE),
      actionButton("run_process", "Process Soil Data"),
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
      h4("ISRIC soil data upload"),
      br(),
      HTML("<p>This page deals with the ISRIC data input and sorting them for the replacement of where ICP soil data is not available for Clay, Silt, Sand, SOC, Nitrogen, and AWC. The required files to be uploaded are available on the"),
      a(href = "https://www.isric.org/explore/soilgrids", "ISRIC database"),
      HTML("at 1km resolution.</p>"),
      br(),
      h4("To get started, please upload all the required on the left."),
      uiOutput("processing_message")
    )
  ))
# Function to read and process raster file
read_and_process_raster_file <- function(file, ex, is_awc = FALSE) {
  r <- raster(file)
  r <- crop(r, ex)
  return(r)}

# Function to process ISRIC soil data with Shiny file inputs
isric_soil_data_modified <- function(awc_files=NULL, clay_files=NULL, silt_files = NULL, sand_files = NULL, soc_files = NULL, nitrogen_files = NULL) {
  ex <- as(extent(-8 * 100000, 40 * 100000, 35 * 100000, 75 * 100000), "SpatialPolygons")
  exs <- as(extent(-8, 40, 35, 75), "SpatialPolygons")
  
  
  total_files <- length(awc_files$datapath)
  progress <- 0
  
  isric_clay_f <- lapply(seq_along(clay_files$datapath), function(index) {
    file <- clay_files$datapath[index]
    df <- read_and_process_raster_file(file, exs, is_awc = TRUE)
    progress <<- progress + (1 / length(clay_files$datapath))
    setProgress(progress, detail = paste("Clay - Processing file", index, "of", length(clay_files$datapath)))
    return(df)})
  
  progress <- 0
  isric_silt_f <- lapply(seq_along(silt_files$datapath), function(index) {
    file <- silt_files$datapath[index]
    df <- read_and_process_raster_file(file, exs, is_awc = TRUE)
    progress <<- progress + (1 / length(silt_files$datapath))
    setProgress(progress, detail = paste("silt - Processing file", index, "of", length(silt_files$datapath)))
    return(df)})
  
  progress <- 0
  isric_sand_f <- lapply(seq_along(sand_files$datapath), function(index) {
    file <- sand_files$datapath[index]
    df <- read_and_process_raster_file(file, exs, is_awc = TRUE)
    progress <<- progress + (1 / length(sand_files$datapath))
    setProgress(progress, detail = paste("sand - Processing file", index, "of", length(sand_files$datapath)))
    return(df)})
  
  progress <- 0
  isric_soc_f <- lapply(seq_along(soc_files$datapath), function(index) {
    file <- soc_files$datapath[index]
    df <- read_and_process_raster_file(file, exs, is_awc = TRUE)
    progress <<- progress + (1 / length(soc_files$datapath))
    setProgress(progress, detail = paste("soc - Processing file", index, "of", length(soc_files$datapath)))
    return(df)})
  
  progress <- 0
  isric_nitrogen_f <- lapply(seq_along(nitrogen_files$datapath), function(index) {
    file <- nitrogen_files$datapath[index]
    df <- read_and_process_raster_file(file, ex)
    progress <<- progress + (1 / length(nitrogen_files$datapath))
    setProgress(progress, detail = paste("nitrogen - Processing file", index, "of", length(nitrogen_files$datapath)))
    return(df)})
  
  progress <- 0
  isric_awc_f <- lapply(seq_along(awc_files$datapath), function(index) {
    file <- awc_files$datapath[index]
    df <- read_and_process_raster_file(file, exs, is_awc = TRUE)
    progress <<- progress + (1 / total_files)
    setProgress(progress, detail = paste("AWC - Processing file", index, "of", total_files))
    return(df)})
  
  cat("Processing the loaded files...")
  
  
  l1<-0.136601
  l2<-0.218493
  l3<-0.226531
  l4<-0.239226
  l5<-0.179149
  x<-as.data.frame(values(isric_clay_f[[1]][[names(isric_clay_f[[1]])]]) * l1)
  x[,2]<- (values(isric_clay_f[[2]][[names(isric_clay_f[[2]])]]) * l2)
  x[,3]<- (values(isric_clay_f[[3]][[names(isric_clay_f[[3]])]]) * l3)
  x[,4]<- (values(isric_clay_f[[4]][[names(isric_clay_f[[4]])]]) * l4)
  x[,5]<- (values(isric_clay_f[[5]][[names(isric_clay_f[[5]])]]) * l5)
  x_lon_lat <- as.data.frame(coordinates(isric_clay_f[[1]][[names(isric_clay_f[[1]])]]))
  colnames(x_lon_lat) <- c("x", "y")
  isric_clay_final  <- rowSums(x)
  
  x<-as.data.frame(values(isric_silt_f[[1]][[names(isric_silt_f[[1]])]]) * l1)
  x[,2]<- (values(isric_silt_f[[2]][[names(isric_silt_f[[2]])]]) * l2)
  x[,3]<- (values(isric_silt_f[[3]][[names(isric_silt_f[[3]])]]) * l3)
  x[,4]<- (values(isric_silt_f[[4]][[names(isric_silt_f[[4]])]]) * l4)
  x[,5]<- (values(isric_silt_f[[5]][[names(isric_silt_f[[5]])]]) * l5)
  isric_silt_final  <- rowSums(x)
  
  x<-as.data.frame(values(isric_sand_f[[1]][[names(isric_sand_f[[1]])]]) * l1)
  x[,2]<- (values(isric_sand_f[[2]][[names(isric_sand_f[[2]])]]) * l2)
  x[,3]<- (values(isric_sand_f[[3]][[names(isric_sand_f[[3]])]]) * l3)
  x[,4]<- (values(isric_sand_f[[4]][[names(isric_sand_f[[4]])]]) * l4)
  x[,5]<- (values(isric_sand_f[[5]][[names(isric_sand_f[[5]])]]) * l5)
  isric_sand_final  <- rowSums(x)
  
  x<-as.data.frame(values(isric_soc_f[[1]][[names(isric_soc_f[[1]])]]) * l1)
  x[,2]<- (values(isric_soc_f[[2]][[names(isric_soc_f[[2]])]]) * l2)
  x[,3]<- (values(isric_soc_f[[3]][[names(isric_soc_f[[3]])]]) * l3)
  x[,4]<- (values(isric_soc_f[[4]][[names(isric_soc_f[[4]])]]) * l4)
  x[,5]<- (values(isric_soc_f[[5]][[names(isric_soc_f[[5]])]]) * l5)
  isric_soc_final  <- rowSums(x)
  
  x<-as.data.frame(values(isric_nitrogen_f[[1]][[names(isric_nitrogen_f[[1]])]]) * l1)
  x[,2]<- (values(isric_nitrogen_f[[2]][[names(isric_nitrogen_f[[2]])]]) * l2)
  x[,3]<- (values(isric_nitrogen_f[[3]][[names(isric_nitrogen_f[[3]])]]) * l3)
  x[,4]<- (values(isric_nitrogen_f[[4]][[names(isric_nitrogen_f[[4]])]]) * l4)
  x[,5]<- (values(isric_nitrogen_f[[5]][[names(isric_nitrogen_f[[5]])]]) * l5)
  nitrogen_lon_lat <- as.data.frame(coordinates(isric_nitrogen_f[[1]][[names(isric_nitrogen_f[[1]])]]))/100000
  colnames(x_lon_lat) <- c("x", "y")
  isric_nitrogen_final  <- rowSums(x)/100
  
  d1 <- 50
  d2 <- 100
  d3 <- 150
  d4 <- 300
  d5 <- 400
  x_1 <- as.data.frame(values(isric_awc_f[[1]][[names(isric_awc_f[[1]])]]) * d1 * 1e-2)
  x_1[,2]<-values((isric_awc_f[[2]][[names(isric_awc_f[[2]])]]) * d2 * 1e-2)
  x_1[,3]<-values((isric_awc_f[[3]][[names(isric_awc_f[[3]])]]) * d3 * 1e-2)
  x_1[,4]<-values((isric_awc_f[[4]][[names(isric_awc_f[[4]])]]) * d4 * 1e-2)
  x_1[,5]<-values((isric_awc_f[[5]][[names(isric_awc_f[[5]])]]) * d5 * 1e-2)
  x_1_lon_lat <- as.data.frame(coordinates(isric_awc_f[[1]][[names(isric_awc_f[[1]])]]))
  colnames(x_1_lon_lat) <- c("x", "y")
  isric_awc<- rowSums(x_1)
  
  cat("\rProcessing completed.           \n")
  
  # Return the processed data
  isric_data_awc <- data.frame(isric_awc_lon = x_1_lon_lat[, 1], isric_awc_lat = x_1_lon_lat[, 2], isric_awc = isric_awc)
  isric_data_soil <- data.frame(isric_lon = x_lon_lat[,1], isric_lat = x_lon_lat[,2] , isric_clay = isric_clay_final, isric_silt = isric_silt_final, isric_sand = isric_sand_final,
                                isric_soc = isric_soc_final)
  
  isric_data_nitrogen<- data.frame(isric_nitrogen_lon = nitrogen_lon_lat[,1], isric_nitrogen_lat = nitrogen_lon_lat[,2], isric_nitrogen = isric_nitrogen_final)
  
  return(list(soil_data = isric_data_soil, soil_data_awc = isric_data_awc, soil_data_nitrogen = isric_data_nitrogen))
}

isric_server <- function(input, output, session) {
  result <- reactiveValues(awc_data = NULL)
  
  clay_files <- reactive({
    input$clay_files
  })
  silt_files <- reactive({
    input$silt_files
  })
  sand_files <- reactive({
    input$sand_files
  })
  soc_files <- reactive({
    input$soc_files
  })
  nitrogen_files <- reactive({
    input$nitrogen_files
  })
  
  observeEvent(input$run_process, {
    req(input$awc_files, input$clay_files, input$sand_files, input$silt_files, input$soc_files, input$nitrogen_files)
    
    withProgress(message = "Processing soil data! This may take a while...", value = 0, {
      data <- isric_soil_data_modified(awc_files = input$awc_files, clay_files = input$clay_files, sand_files = input$sand_files, silt_files = input$silt_files, soc_files = input$soc_files, nitrogen_files = input$nitrogen_files)
      result$awc_data <- data$isric_data_awc
      result$clay_data <- data$isric_data_soil
      result$nitrogen_data <- data$isric_data_nitrogen
      
      # Add the processing message after processing is complete
      output$processing_message <- renderUI({
        HTML("<p>All files are processed! You can now return to the home page.</p>")
      })
    })
  })
  
  observeEvent(input$back_home, {
    output$page <- renderUI(home_ui)
  })
}

shinyApp(ui = isric_ui, server = isric_server)







