# Load required packages
library(shiny)
library(raster)
library(dplyr)
library(leaflet)
library(data.table)
library(lubridate)
library(readxl)
library(tidyverse)
library(utils)
library(tcltk)
library(ggplot2)
library(soiltexture)
library(r3PG)
library(openxlsx)
library(shinyjs)


#######################

options(shiny.maxRequestSize = 1024 * 1024 * 1024 * 1024 * 1024 ) # 1 TB


# Define home page UI
home_ui <- fluidPage(
  navbarPage(
    tabPanel("",
             fluidRow(
               column(width = 12, align = "center",
                      img(src = "https://www.unwater.org/sites/default/files/styles/d02/public/app/uploads/2020/01/220x120_PARTNERS_IIASA.webp?itok=MjAJX7xP", height = 100)
               )
             ),
             br(),
             h4("Welcome to the ForestScope Data Analysis App"),
             br(),
             p("This app allows you to upload and process three types of data: ISRIC soil data, ICP data, and CHELSA data. Additionally, you can explore the data and run a model using the processed data."),
             br(),
             h4("To get started, please select one of the options below."),
             br(),
             fluidRow(
               column(width = 4, align = "center",
                      actionButton("upload_isric", icon = icon("file"), "Upload ISRIC Data")
               ),
               column(width = 4, align = "center",
                      actionButton("upload_icp", icon = icon("file"), "Upload ICP Data")
               ),
               column(width = 4, align = "center",
                      actionButton("upload_chelsa", icon = icon("file"), "Upload CHELSA Data")
               )
             ),
             br(),
             fluidRow(
               column(width = 4, align = "center",
                      actionButton("explore_data", icon = icon("search"), "Explore the data")
               ),
               column(width = 4, align = "center",
                      actionButton("run_model", icon = icon("cog"), "Run the Model")
               ),
               column(width = 4, align = "center",
                       actionButton("documentation", icon = icon("book"), "Documentation")),
             )
    )
  )
)




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
      actionButton("run_process_isric", "Process Soil Data"),
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
  x<-NA
  x<-as.data.frame(values(isric_clay_f[[1]][[names(isric_clay_f[[1]])]]) * l1)
  x[,2]<- (values(isric_clay_f[[2]][[names(isric_clay_f[[2]])]]) * l2)
  x[,3]<- (values(isric_clay_f[[3]][[names(isric_clay_f[[3]])]]) * l3)
  x[,4]<- (values(isric_clay_f[[4]][[names(isric_clay_f[[4]])]]) * l4)
  x[,5]<- (values(isric_clay_f[[5]][[names(isric_clay_f[[5]])]]) * l5)
  x_lon_lat <- as.data.frame(coordinates(isric_clay_f[[1]][[names(isric_clay_f[[1]])]]))
  colnames(x_lon_lat) <- c("x", "y")
  isric_clay_final  <- rowSums(x)
  
  x<-NA
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
  
  x<-NA
  x<-as.data.frame(values(isric_soc_f[[1]][[names(isric_soc_f[[1]])]]) * l1)
  x[,2]<- (values(isric_soc_f[[2]][[names(isric_soc_f[[2]])]]) * l2)
  x[,3]<- (values(isric_soc_f[[3]][[names(isric_soc_f[[3]])]]) * l3)
  x[,4]<- (values(isric_soc_f[[4]][[names(isric_soc_f[[4]])]]) * l4)
  x[,5]<- (values(isric_soc_f[[5]][[names(isric_soc_f[[5]])]]) * l5)
  isric_soc_final  <- rowSums(x)
  
  x<-NA
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
  
  x_1<-NA
  x_1 <- as.data.frame(values(isric_awc_f[[1]][[names(isric_awc_f[[1]])]]) * d1 * 1e-2)
  x_1[,2]<-values((isric_awc_f[[2]][[names(isric_awc_f[[2]])]]) * d2 * 1e-2)
  x_1[,3]<-values((isric_awc_f[[3]][[names(isric_awc_f[[3]])]]) * d3 * 1e-2)
  x_1[,4]<-values((isric_awc_f[[4]][[names(isric_awc_f[[4]])]]) * d4 * 1e-2)
  x_1[,5]<-values((isric_awc_f[[5]][[names(isric_awc_f[[5]])]]) * d5 * 1e-2)
  x_1_lon_lat <- NA
  x_1_lon_lat <- as.data.frame(coordinates(isric_awc_f[[1]][[names(isric_awc_f[[1]])]]))
  colnames(x_1_lon_lat) <- c("x", "y")
  isric_awc<- rowSums(x_1)
  
  cat("\rProcessing completed.           \n")
  
  # Return the processed data
  isric_data_awc <- data.frame(isric_awc_lon = x_1_lon_lat$x, isric_awc_lat = x_1_lon_lat$y, isric_awc = isric_awc)
  isric_data_soil <- data.frame(isric_lon = x_lon_lat[,1], isric_lat = x_lon_lat[,2] , isric_clay = isric_clay_final, isric_silt = isric_silt_final, isric_sand = isric_sand_final,
                                isric_soc = isric_soc_final)
  isric_data_nitrogen<- data.frame(isric_nitrogen_lon = nitrogen_lon_lat[,1], isric_nitrogen_lat = nitrogen_lon_lat[,2], isric_nitrogen = isric_nitrogen_final)
  return(list(soil_data = isric_data_soil, soil_data_awc = isric_data_awc, soil_data_nitrogen = isric_data_nitrogen))
}

# Define server logic for ISRIC data
isric_server <- function(input, output, session, isric_soil_data) {
  isric_soil_data <- reactiveValues(soil_data = NULL, soil_data_awc = NULL, soil_data_nitrogen = NULL)

  output$processing_message <- renderUI({
    if (!is.null(results$soil_data) && !is.null(results$soil_data_awc) && !is.null(results$soil_data_nitrogen)) {
      HTML("<p>Soil data processing completed successfully. Click 'Back to Home Page' to proceed with the next steps.</p>")
    } else {
      NULL
    }
  })
  
  observeEvent(input$back_home, {
    updateTabsetPanel(session, "navigation", selected = "home")
    
  })
  
  return(isric_soil_data)
  
}

icp_ui <- fluidPage(
  titlePanel("ICP Soil and Climate Data Process"),
  sidebarLayout(
    sidebarPanel(
      fileInput("icp_climate", "ICP Climate files", multiple = TRUE),
      h4("ICP Soil Data:"),
      fileInput("so_pfh", "ICP soil (so_pfh)", multiple = FALSE),
      fileInput("so_som", "ICP SOM (so_som)", multiple = FALSE),
      fileInput("ss_ssm", "ICP SSM (ss_ssm)", multiple = FALSE),
      h4("ICP Stand Data:"),
      fileInput("si_sta", "ICP Stand (si_sta)", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      fileInput("gr_ipm", "ICP Tree diameter (gr_ipm)", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      fileInput("gr_iev", "ICP Thinning (gr_iev)", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      fileInput("gr_inv", "ICP Inventory (gr_inv)", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      fileInput("lf_lfm", "ICP Litter (lf_lfm)", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      actionButton("run_process_icp", "Process ICP Data"),
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
      h4("ICP Soil and Climate Data Upload"),
      br(),
      HTML("This page deals with processing the International Cooperative Programme on Assessment and Monitoring of Air Pollution Effects on Forests (ICP) climate, soil, and stand data. The ICP is a network of scientists and experts who monitor the impacts of air pollution on forests and their ecosystems."),
      br(),
      h4("Please upload all the required data files on the left to process."),
      h5("Learn more about ICP and download required listed files :"),
      a("ICP Data Documentation", href = "https://icp-forests.org/documentation/Introduction/index.html"),
      br(),
      uiOutput("processing_message")
    )
  )
)
ICP_climate_soil <- function(icp_climate=NULL , chelsa_pr = NULL, chelsa_tas = NULL, chelsa_tas_min = NULL, chelsa_tas_max = NULL, chelsa_rad = NULL, so_pfh = NULL, so_som = NULL,
                             ss_ssm = NULL, si_sta= NULL, gr_ipm = NULL, gr_iev = NULL, gr_inv = NULL, lf_lfm = NULL) {

  
  country_code <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,72,80,95,96)
  country_names <- c("FR","BE","NL","DE","IT","UK","IE","DK","GR","PT","ES","LU","SE","AT","FI","CH","HU","RO","PL","SK","NO","LT","HR","CZ","EE","SI","MD","RU","BG","LV","BY","CY","CS","AD","TR","ME","cn","AZ")
  
  file.list <- (icp_climate$datapath)
  
  message("Reading climate data...")
  df.list <- lapply(file.list, read_excel)
  data_names <- c(1994:2018)
  
  clim_list <- list()
  for (i in 1:length(file.list)){
    clim_list[[i]] <- df.list[[i]]
  }
  
  clim_tot <- bind_rows(clim_list)
  
  
  country_site_num_list <- list()
  for (i in 1:length(country_code)){
    country_data <- filter(clim_tot, clim_tot$code_country == country_code[i])
    country_name <- country_names[i]
    country_site_num <- unique(country_data[, 8])
    country_site_num_list[[i]] <- list(country_name = country_name, site_num = country_site_num)
  }
  country_data_list <- list()
  country_data_list_tas <- list()
  country_data_list_pr <- list()
  country_data_list_sr <- list()
  
  for (i in 1:length(country_site_num_list)){
    country_data_list[[i]] <- list()
    country_data_list_tas[[i]] <- list()
    country_data_list_pr[[i]] <- list()
    country_data_list_sr[[i]] <- list()
    
    if(length(country_site_num_list[[i]]$site_num[[1]]) >0){
      for (j in 1:length(country_site_num_list[[i]]$site_num[[1]])){
        site_data <- filter(
          clim_tot,
          code_country == country_code[i],
          code_plot == country_site_num_list[[i]]$site_num[[1]][j]
        )
        site_data_at <- filter(site_data, code_variable == "AT")
        site_data_pr <- filter(site_data, code_variable == "PR")
        site_data_sr <- filter(site_data, code_variable == "SR")
        site_data_list_tas <- list(
          date_tmp = site_data_at$date_observation,
          tavg = site_data_at$daily_mean,
          tmin = site_data_at$daily_min,
          tmax = site_data_at$daily_max)
        site_data_list_pr <- list(
          date_pr = site_data_pr$date_observation,
          pr = site_data_pr$daily_mean)
        site_data_list_sr <- list(
          date_sr = site_data_sr$date_observation,
          sr = site_data_sr$daily_mean
        )
        country_data_list_tas[[i]][[paste0("site_num_",country_site_num_list[[i]]$site_num[[1]][j])]] <- site_data_list_tas
        country_data_list_pr[[i]][[paste0("site_num_",country_site_num_list[[i]]$site_num[[1]][j])]] <- site_data_list_pr
        country_data_list_sr[[i]][[paste0("site_num_",country_site_num_list[[i]]$site_num[[1]][j])]] <- site_data_list_sr
      }}}
  
  
  
  soil_data <- read_delim(so_pfh$datapath, show_col_types = FALSE)
  soil_som_data <- read_delim(so_som$datapath, show_col_types = FALSE)
  soil_ssm_data <- read_delim(ss_ssm$datapath, show_col_types = FALSE)
  message("Soil Data processed!")
  
  
  stand_data <- read_delim(si_sta$datapath, show_col_types = FALSE)
  
  stand_code<-c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,90,91,92,93,94,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,197,198,199,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,888)
  stand_name<-c("Not-known","Acer campestre","Acer monspessulanum","Acer opalus","Acer platanoides","Acer pseudoplatanus","Alnus cordata","Alnus glutinosa","Alnus incana","Alnus viridis","Betula pendula","Betula pubescens","Buxus sempervirens","Carpinus betulus","Carpinus orientalis","Castanea sativa","Corylus avellana","Eucalyptus sp.","Fagus moesiaca","Fagus orientalis","Fagus sylvatica","Fraxinus angustifolia","Fraxinus excelsior","Fraxinus ornus","Ilex aquifolium","Juglans nigra","Juglans regia","Malus domestica","Olea europaea","Ostrya carpinifolia","Platanus orientalis","Populus alba","Populus x canescens","Populus hybrides","Populus nigra","Populus tremula","Prunus avium","Prunus dulcis","Prunus padus","Prunus serotina","Pyrus communis","Quercus cerris","Quercus coccifera","Quercus faginea","Quercus frainetto","Quercus fruticosa","Quercus ilex","Quercus macrolepis","Quercus petraea","Quercus pubescens","Quercus pyrenaica","Quercus robur","Quercus rotundifolia","Quercus rubra","Quercus suber","Quercus trojana","Robinia pseudoacacia","Salix alba","Salix caprea","Salix cinerea","Salix eleagnos","Salix fragilis","Salix sp.","Sorbus aria","Sorbus aucuparia","Sorbus domestica","Sorbus torminalis","Tamarix africana","Tilia cordata","Tilia platyphyllos","Ulmus glabra","Ulmus laevis","Ulmus minor","Arbutus unedo","Arbutus andrachne","Ceratonia siliqua","Cercis siliquastrum","Erica arborea","Erica scoparia","Erica manipuliflora","Laurus nobilis","Myrtus communis","Phillyrea latifolia","Phillyrea angustifolia","Pistacia lentiscus","Pistacia terebinthus","Rhamnus oleoides","Rhamnus alaternus","Betula tortuosa","Crataegus monogyna","Ilex canariensis","Laurus canariensis","Myrica faya","Central Anatolian oaks","Quercus petraea_or_robur","Other broadleaves","Abies alba","Abies borisii-regis","Abies cephalonica","Abies grandis","Abies nordmanniana","Abies pinsapo","Abies procera","Cedrus atlantica","Cedrus deodara","Cupressus lusitanica","Cupressus sempervirens","Juniperus communis","Juniperus oxycedrus","Juniperus phoenicea","Juniperus sabina","Juniperus thurifera","Larix decidua","Larix kaempferi","Picea abies","Picea omorika","Picea sitchensis","Pinus brutia","Pinus canariensis","Pinus cembra","Pinus contorta","Pinus halepensis","Pinus heldreichii","Pinus mugo","Pinus nigra","Pinus pinaster","Pinus pinea","Pinus radiata","Pinus strobus","Pinus sylvestris","Pinus uncinata","Pseudotsuga menziesii","Taxus baccata","Thuya sp.","Tsuga sp.","Chamaecyparis lawsoniana","Cedrus brevifolia","Abies cilicica","Cedrus libani","Juniperus excelsa","Juniperus foetidissima","Picea orientalis","Abies amabilis","Abies concolor","Abies veitchii","Larix eurolepis","Picea glauca","Picea pungens","Pinus banksiana","Pinus peuce","Pinus rigida","Pinus wallichiana","Araucaria araucana","Calocedrus decurrens","Cryptomeria japonica","Metasequoia glyptostroboides","Sequoiadendron giganteum","Tamarix ramosissima","Taxodium distichum","Tsuga canadensis","Larix sp.","Abies sp.","Other conifers","Quercus hartwissiana","Quercus vulcanica","Quercus infectoria","Quercus macranthera","Quercus libani","Quercus brantii","Quercus ithaburensis","Quercus aucheri  ","Quercus pontica","Tilia sp.","Populus sp.","Betula sp.","Ulmus sp.","Betula x hybrida","Acer sp.","Alnus sp.","Crataegus sp.","Malus sylvestris","Aesculus hippocastanum","Acer negundo","Acer obtusatum","Acer saccharinum","Acer tataricum","Fraxinus americana","Fraxinus pennsylvanica","Laburnum alpinum","Laburnum anagyroides","Populus balsamifera","Populus deltoides","Prunus cerasifera","Prunus domestica","Prunus laurocerasus","Prunus mahaleb","Prunus spinosa","Quercus palustris","Quercus pedunculiflora","Salix purpurea","Salix triandra","Salix viminalis","Ailanthus altissima","Broussonetia papyrifera","Carya ovata","Corylus colurna","Crataegus laevigata","Liriodendron tulipifera","Mespilus germanica","Paulownia tomentosa","Petteria ramentacea","Phillyrea media","Platanus xacerifolia","Pyrus pyraster","Sorbus austriaca","Tilia tomentosa","All species")
  ss <- data.frame(stand_code,stand_name)
  
  
  ss_age_num_2 <-c(1,2,3,4,5,6,7,8,99,0) #stand age history groups
  ss_age_2<-c((1984-20),(1984-30),(1984-50),(1984-70),(1984-90),(1984-110),(1984-120),1984,NA,1984)
  ss_age_d_2 <- data.frame(ss_age_num_2,ss_age_2)
  
  ss_age_num_1 <-c(1,2,3,4,9,0) #stand age history groups
  ss_age_1<-c((1984-300),(1984-100),(1984-50),(1984-25),1984,1984)
  ss_age_d_1 <- data.frame(ss_age_num_1,ss_age_1)
  
  message("Stand Data processed!")
  
  
  tree_data_dia<-read_delim(gr_ipm$datapath, show_col_types = FALSE)
  
  
  message("Tree Data processed!")
  
  thinning_data <- read_delim(gr_iev$datapath, show_col_types = FALSE)
  
  
  inventory_data<- read_delim(gr_inv$datapath, show_col_types = FALSE)
  
  
  message("Thinninh Data processed!")
  
  litter_data<- read_delim(lf_lfm$datapath, show_col_types = FALSE)
  
  
  frosster_d<- read_excel("frosster_data.xlsx")
  
  sites_lat_lon <- read.table("ICP_lat_lon_all.txt", sep = ",", header = TRUE)
  
  country_data_list_lat_lon <- list()
  for (i in 1:length(country_site_num_list)) {
    country_code <- country_code[i]
    country_name <- country_names[i]
    site_num_list <- country_site_num_list[[i]]$site_num
    country_data_list_lat_lon[[i]] <- list(country_name = country_name)
    
    if(length(site_num_list$code_plot) >0){
      for (j in 1:length(site_num_list$code_plot)) {
        site_lat_lon <- filter(sites_lat_lon, code_country == country_code & code_plot == site_num_list$code_plot[j])
        site_lat <- unique(site_lat_lon$lat)[1]
        site_lon <- unique(site_lat_lon$lon)[1]
        site_data_list_lat_lon <- list(
          lat = site_lat,
          lon = site_lon
        )
        country_data_list_lat_lon[[i]][[paste0("site_num_", site_num_list$code_plot[j])]] <- site_data_list_lat_lon
      }}
  }
  
  return(list(country_code = country_code, country_names = country_names, lat_lon = country_data_list_lat_lon, ICP_temperature = country_data_list_tas, ICP_precipitation = country_data_list_pr, ICP_radiation = country_data_list_sr,
              soil_data = soil_data, soil_som_data = soil_som_data, soil_ssm_data = soil_ssm_data, stand_data = stand_data, ss_age_d_1=ss_age_d_1, ss_age_d_2=ss_age_d_2,
              tree_data_dia = tree_data_dia, thinning_data = thinning_data,inventory_data = inventory_data,
              litter_data = litter_data, frosster_d = frosster_d))
}

# Define server logic for ISRIC data
icp_server <- function(input, output, session, icp_data) {
  icp_data <- reactiveValues(country_code = NULL, country_names = NULL, lat_lon = NULL, ICP_temperature = NULL, ICP_precipitation = NULL, ICP_radiation = NULL,
                             soil_data = NULL, soil_som_data = NULL, soil_ssm_data = NULL, stand_data = NULL, ss_age_d_1=NULL, ss_age_d_2=NULL,
                             tree_data_dia = NULL, thinning_data = NULL,inventory_data = NULL,
                             litter_data = NULL, frosster_d = NULL)
  observeEvent(input$back_home, {
    updateTabsetPanel(session, "navigation", selected = "home")
    
  })
  icp_data$country_code <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,72,80,95,96)
  ss_age_num_2 <-c(1,2,3,4,5,6,7,8,99,0) #stand age history groups
  ss_age_2<-c((1984-20),(1984-30),(1984-50),(1984-70),(1984-90),(1984-110),(1984-120),1984,NA,1984)
  icp_data$ss_age_d_2 <- data.frame(ss_age_num_2,ss_age_2)
  ss_age_num_1 <-c(1,2,3,4,9,0) #stand age history groups
  ss_age_1<-c((1984-300),(1984-100),(1984-50),(1984-25),1984,1984)
  icp_data$ss_age_d_1 <- data.frame(ss_age_num_1,ss_age_1)
  
  
  return(icp_data)
  
}
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
  
  chelsa_clim_s <- list()
  
  
  # Load the CHELSA data into data frames
  c_pr <- data.table::fread(chelsa_pr$datapath, sep = ",", header = FALSE, data.table = TRUE, stringsAsFactors = FALSE)
  chelsa_pr <- as.data.frame(c_pr[, 2:5])
  colnames(chelsa_pr)[1:4] <- c("LAT", "LON", "DATE", "pr")
  rm(c_pr)
  chelsa_clim_s[["pr"]] <- chelsa_pr
  
  c_tas <- data.table::fread(chelsa_tas$datapath, sep = ",", header = FALSE, data.table = TRUE, stringsAsFactors = FALSE)
  chelsa_tas <- as.data.frame(c_tas[, 2:5])
  colnames(chelsa_tas)[1:4] <- c("LAT", "LON", "DATE", "tas")
  rm(c_tas)
  chelsa_clim_s[["tas"]] <- chelsa_tas
  
  c_tas_min <- data.table::fread(chelsa_tas_min$datapath, sep = ",", header = FALSE, data.table = TRUE, stringsAsFactors = FALSE)
  chelsa_tas_min <- as.data.frame(c_tas_min[, 2:5])
  colnames(chelsa_tas_min)[1:4] <- c("LAT", "LON", "DATE", "tas_min")
  rm(c_tas_min)
  chelsa_clim_s[["tas_min"]] <- chelsa_tas_min
  
  c_tas_max <- data.table::fread(chelsa_tas_max$datapath, sep = ",", header = FALSE, data.table = TRUE, stringsAsFactors = FALSE)
  chelsa_tas_max <- as.data.frame(c_tas_max[, 2:5])
  colnames(chelsa_tas_max)[1:4] <- c("LAT", "LON", "DATE", "tas_min")
  rm(c_tas_max)
  chelsa_clim_s[["tas_max"]] <- chelsa_tas_max
  
  c_rad <- data.table::fread(chelsa_rad$datapath, sep = ",", header = FALSE, data.table = TRUE, stringsAsFactors = FALSE)
  chelsa_rad <- as.data.frame(c_rad[, 2:5])
  colnames(chelsa_rad)[1:4] <- c("LAT", "LON", "DATE", "rad")
  rm(c_rad)
  chelsa_clim_s[["sr"]] <- chelsa_rad
  
  
  message("Climate Data processed!")
  return(list(chelsa_pr = chelsa_clim_s$pr, chelsa_tas = chelsa_clim_s$tas,chelsa_tas_min = chelsa_clim_s$tas_min, chelsa_tas_max = chelsa_clim_s$tas_max, chelsa_rad = chelsa_clim_s$sr))
  
}

chelsa_server <- function(input, output, session, chelsa_clim) {
  chelsa_clim <- reactiveValues(chelsa_pr = NULL, chelsa_tas = NULL, chelsa_tas_min = NULL, chelsa_tas_max = NULL, chelsa_rad = NULL)
  
  observeEvent(input$back_home, {
    output$page <- renderUI(home_ui)
  })
  
  return(chelsa_clim)
}

explore_ui<-shinyUI(fluidPage(
  titlePanel("ICP Data Browser"),
  mainPanel(
    h4("Select a site from ICP database:"),
    leafletOutput("map", height = "500px"),
    hr(),
    p("You need to upload all the required data before selecting site."), 
    p("Following point selection, you can run the model simulation on the selected site."),
    actionLink("back_home", "Back to Home Page"),
    br(),
    actionButton("reset_explore", "Reset"),
    h4("Selected Point Information"),
    downloadButton("download", "Download"),
    verbatimTextOutput("selected_point"),
    h4("Plot for Selected Point:"),
    plotOutput("plot"),
    h4("Data Table for Selected Point:"),
    DT::dataTableOutput("table1"),
    DT::dataTableOutput("table2"),
    DT::dataTableOutput("table3"),
    br(),
    br(),
    br()
  )
))
table <- read.table("latlon_table.txt",sep=",", header=TRUE)
points<- NA
points <- data.frame(
  id = 1:length(table$CountryCode),
  con<-table$CountryName,
  lat = table$Lat,
  lon = table$Lon,
  site= table$Site
)
points$lat[points$lat>85]<-NA
points$lon[points$lon>85]<-NA




# Define IIASA_ICP_process function
# Define IIASA_ICP_process function
IIASA_ICP_process <- function(isric_soil_d = NULL, icp_data = NULL, chelsa_clim = NULL, country= NULL, site_number= NULL, lat = NULL, lon = NULL) {
  
  select_country <- country
  ss_select <- site_number
  ###  CLIMATE DATA  ###
  
  country_code_index <- which(icp_data$country_names == select_country)
  x <- icp_data$ICP_temperature[[country_code_index]]
  
  for (i in 1:length(x)){
    if(names(x[i]) == ss_select){
      x_s<- x[i]
      site_date_tmp <- x_s[[1]][[1]]
      site_tavg <- x_s[[1]][[2]]
      site_tmin <- x_s[[1]][[3]]
      site_tmax <- x_s[[1]][[4]]}}

  x<-icp_data$ICP_precipitation[[country_code_index]]
  for (i in 1:length(x)){
    if(names(x[i]) == ss_select){
      x_s<- x[i]
      site_date_pr <- x_s[[1]][[1]]
      site_pr <- x_s[[1]][[2]]}}
  
  x<-icp_data$ICP_radiation[[country_code_index]]
  for (i in 1:length(x)){
    if(names(x[i]) == ss_select){
      x_s<- x[i]
      site_date_sr <- x_s[[1]][[1]]
      site_sr <- x_s[[1]][[2]]}}
  
  x<-icp_data$lat_lon[[country_code_index]]
  site_lat <- lat
  site_lon <- lon
  
  x_tmp <- data.frame(date = (site_date_tmp))
  x_tmp  <- x_tmp %>%
    mutate(date = ymd(date)) %>%
    mutate(year = ifelse(is.na(date), NA, year(date)),
           month = ifelse(is.na(date), NA, month(date)),
           day = ifelse(is.na(date), NA, day(date)))
  
  x_pr <- data.frame(date = (site_date_pr))
  x_pr  = x_pr %>%
    mutate(date = ymd(date)) %>%
    mutate(year = ifelse(is.na(date), NA, year(date)),
           month = ifelse(is.na(date), NA, month(date)),
           day = ifelse(is.na(date), NA, day(date)))
  
  x_sr <- data.frame(date = (site_date_sr))
  x_sr  = x_sr %>%
    mutate(date = ymd(date)) %>%
    mutate(year = ifelse(is.na(date), NA, year(date)),
           month = ifelse(is.na(date), NA, month(date)),
           day = ifelse(is.na(date), NA, day(date)))
  
  tmin<-NA
  tmax<-NA
  tave<-NA
  prcpi<-NA
  sradi<-NA
  frost_day<-NA
  start_date<- (paste(x_tmp$year[1],x_tmp$mo[1],x_tmp$day[1],sep = "/", collapse=NULL))
  datess <- seq(as.Date(start_date), by = "days", length.out =length(x_tmp$day))
  tmin<- aggregate(as.numeric(site_tmin)~x_tmp$month + x_tmp$year, datess,mean)
  tmax<- aggregate(as.numeric(site_tmax)~x_tmp$month + x_tmp$year, datess,mean)
  tave<- aggregate(site_tavg~x_tmp$month + x_tmp$year, datess,mean)
  
  start_date_pr<- (paste(x_pr$year[1],x_pr$mo[1],x_pr$day[1],sep = "/", collapse=NULL))
  if(length(x_pr$date) >0 & (length(site_pr) == length(x_pr$date))){
    datess_pr <- seq(as.Date(start_date_pr), by = "days", length.out =length(x_pr$day))
    prcpi<- aggregate(site_pr~x_pr$month + x_pr$year, datess_pr,sum)}else{ tmin <- tmin %>% filter(tmin$`x_tmp$year`<2017)
    tmax <- tmax %>% filter(tmax$`x_tmp$year`<2017)
    tave <- tave %>% filter(tave$`x_tmp$year`<2017)
    
    lat_min <- (abs(chelsa_clim$pr$LON- as.numeric(unique(site_lon)))==min(abs(chelsa_clim$pr$LON- as.numeric(unique(site_lon)))))
    lon_min <- (abs(chelsa_clim$pr$LAT- as.numeric(unique(site_lat)))==min(abs(chelsa_clim$pr$LAT- as.numeric(unique(site_lat)))))
    ch_pr_in <- chelsa_clim$pr %>% filter( lat_min & lon_min)
    
    if(x_tmp$mo[max(length(x_tmp$year))] <10){
      dx<-as.numeric(paste0(x_tmp$year[max(length(x_tmp$year))],"0",x_tmp$mo[max(length(x_tmp$year))]))}else{
        dx<-as.numeric(paste0(x_tmp$year[max(length(x_tmp$year))],x_tmp$mo[max(length(x_tmp$year))]))
      }
    if(x_tmp$mo[1] <10){
      dm<-as.numeric(paste0(x_tmp$year[1],"0",x_tmp$mo[1]))}else{
        dm<-as.numeric(paste0(x_tmp$year[1],x_tmp$mo[1]))
      }
    ch_pr_x <- ch_pr_in %>% filter(ch_pr_in$DATE >= dm & ch_pr_in$DATE <= dx)
    
    ch_pr_x2 <- ch_pr_x %>% filter( substr(ch_pr_x$DATE, 1, 4) %in% x_tmp$year & sub("^0", "", substr(ch_pr_x$DATE, 5, 6)) %in% x_tmp$month)
    prcpi<- tmax
    prcpi$site_pr<- ch_pr_x2$prcpi
    colnames(prcpi)<- c("x_pr$month","x_pr$year","site_prcpi")
    }
  
  
  start_date_sr<- (paste(x_sr$year[1],x_sr$mo[1],x_sr$day[1],sep = "/", collapse=NULL))
  if(length(x_sr$date) >0 & (length(site_date_sr) == length(x_sr$date))){
    datess_sr <- seq(as.Date(start_date_sr), by = "days", length.out =length(x_sr$day))
    sradi<- aggregate((site_sr/10)~x_sr$month + x_sr$year, datess_sr,mean)}else{
      tmin <- tmin %>% filter(tmin$`x_tmp$year`<2017)
      tmax <- tmax %>% filter(tmax$`x_tmp$year`<2017)
      tave <- tave %>% filter(tave$`x_tmp$year`<2017)
      prcpi <- prcpi %>% filter(prcpi$`x_pr$year`<2017)
      
      
      lat_min <- (abs(chelsa_clim$sr$LON- as.numeric(unique(site_lon)))==min(abs(chelsa_clim$sr$LON- as.numeric(unique(site_lon)))))
      lon_min <- (abs(chelsa_clim$sr$LAT- as.numeric(unique(site_lat)))==min(abs(chelsa_clim$sr$LAT- as.numeric(unique(site_lat)))))
      ch_rad_in <- chelsa_clim$sr %>% filter( lat_min & lon_min)
      
      if(x_tmp$mo[max(length(x_tmp$year))] <10){
        dx<-as.numeric(paste0(x_tmp$year[max(length(x_tmp$year))],"0",x_tmp$mo[max(length(x_tmp$year))]))}else{
          dx<-as.numeric(paste0(x_tmp$year[max(length(x_tmp$year))],x_tmp$mo[max(length(x_tmp$year))]))
        }
      if(x_tmp$mo[1] <10){
        dm<-as.numeric(paste0(x_tmp$year[1],"0",x_tmp$mo[1]))}else{
          dm<-as.numeric(paste0(x_tmp$year[1],x_tmp$mo[1]))
        }
      
      
      ch_rad_x <- ch_rad_in %>% filter(ch_rad_in$DATE >= dm & ch_rad_in$DATE <= dx)
      
      ch_rad_x2 <- ch_rad_x %>% filter( substr(ch_rad_x$DATE, 1, 4) %in% x_tmp$year & sub("^0", "", substr(ch_rad_x$DATE, 5, 6)) %in% x_tmp$month)
      sradi<- tmin
      sradi$`as.numeric(site_tmin)`<- ch_rad_x2$rad*0.1
      colnames(sradi)<- c("x_sr$month","x_sr$year","site_srad")
    }
  
  
  
  #calculate forst day
  frost <- array(NA,c(length(x_tmp$year)))
  x_site_tmin <- site_tmin
  x_site_tmin[is.na(x_site_tmin)]<-0
  for(i in 1:length(site_tmin)){
    if(x_site_tmin[i] < -3){frost[i]=1} else {frost[i]=0}}
  frost_day <- aggregate(frost~x_tmp$month + x_tmp$year, datess,sum)
  
  
  
  # CO2 (from ISIMIP)
  site_co2 <- read.table("data/co2_data.txt",sep=",")
  
  first_year <-x_tmp$year[length(1)]
  last_year  <- x_tmp$year[length(x_tmp$day)]
  co2 <- array(NA,c(last_year - first_year))
  co2 <- site_co2[which(site_co2$year <= last_year),]
  co2 <- co2[which(site_co2$year >= first_year), ]
  co2<-na.omit(co2)
  
  cc_data <- tmin
  colnames(cc_data)[3] <- "site$co2"
  colnames(cc_data)[2] <- "site$year"
  
  cc_data[,3] <- 0
  yrx0 = first_year - 1
  for (j in 1:length(co2$year)){
    yrx = yrx0 + j
    for (i in 1:length(cc_data$`x_tmp$month`)) {
      if(cc_data[i,]$`site$year` == yrx){
        xx<- which(co2$year== yrx)
        cc_data[i,]$`site$co2`= co2[xx,]$co2_ppm
      }
    }
  }
  co2_data <- tmin
  colnames(co2_data)[3] <- "site$co2"
  co2_data[,3] <- cc_data$`site$co2`
  
  
  
  if(is.na(site_lat )){
    ndep_data <- tmin
    colnames(ndep_data)[3] <- "site$nox"
    colnames(ndep_data)[2] <- "site$year"
    ndep_data[,3] <- 0}else{
      # #calculate N deposition from EMEP
      ndep_data <- tmin
      colnames(ndep_data)[3] <- "site$nox"
      colnames(ndep_data)[2] <- "site$year"
      
      ndep_data[,3] <- 0
      # yrx0 = first_year - 1
      # #for (j in 1:length(ndep_data$`site$year`)){
      # #  yrx = yrx0 + j
      # #  for (i in 1:length(ndep_data$`x_tmp$month`)) {
      # #    if(ndep_data[i,]$`site$year` == yrx){
      # #      ndep_data[i,]$`site$nox`= (mean((unique(ifelse((nox_data$longitude==site_lon)&(nox_data$latitude==site_lat)&(nox_data$year==yrx),nox_data$value,0)))[2:24])*0.00001/12) +
      # #  (mean((unique(ifelse((NH3_data$longitude==site_lon)&(NH3_data$latitude==site_lat)&(NH3_data$year==yrx),NH3_data$value,0)))[2:24])*0.00001/12)
      # #    }
      # #  }
      #}
    }
  
  
  
  ##### check to have same period for all input data #####
  if(length(tmin$`x_tmp$month` != length(sradi$`x_sr$month`))){
    tmin3 <-tmin
    tmin3$`x_tmp$month`<-paste(tmin$`x_tmp$month`,tmin$`x_tmp$year`)
    tt <-sradi
    tt$`x_tmp$month` <- paste(sradi$`x_sr$month`,sradi$`x_sr$year`)
    tmin <- tmin[which( tmin3$`x_tmp$month` %in% tt$`x_tmp$month`),]}
  if(length(tmax$`x_tmp$month` != length(sradi$`x_sr$month`))){
    tmax3 <-tmax
    tmax3$`x_tmp$month`<-paste(tmax$`x_tmp$month`,tmax$`x_tmp$year`)
    tt <-sradi
    tt$`x_tmp$month` <- paste(sradi$`x_sr$month`,sradi$`x_sr$year`)
    tmax <- tmax[which( tmax3$`x_tmp$month` %in% tt$`x_tmp$month`),]}
  if(length(tave$`x_tmp$month` != length(sradi$`x_sr$month`))){
    tave3 <-tave
    tave3$`x_tmp$month`<-paste(tave$`x_tmp$month`,tave$`x_tmp$year`)
    tt <-sradi
    tt$`x_tmp$month` <- paste(sradi$`x_sr$month`,sradi$`x_sr$year`)
    tave <- tave[which( tave3$`x_tmp$month` %in% tt$`x_tmp$month`),]}
  if(length(prcpi$`x_pr$month` != length(sradi$`x_sr$month`))){
    prcpi3 <- prcpi
    prcpi3$`x_pr$month`<-paste(prcpi$`x_pr$month`,prcpi$`x_pr$year`)
    tt <-sradi
    tt$`x_tmp$month` <- paste(sradi$`x_sr$month`,sradi$`x_sr$year`)
    prcpi <- prcpi[which( prcpi3$`x_pr$month` %in% tt$`x_tmp$month`),]}
  if(length(tmin$`x_tmp$month` != length(prcpi$`x_pr$month`))){
    tmin3 <-tmin
    tmin3$`x_tmp$month`<-paste(tmin$`x_tmp$month`,tmin$`x_tmp$year`)
    tt <-prcpi
    tt$`x_tmp$month` <- paste(prcpi$`x_pr$month`,prcpi$`x_pr$year`)
    tmin <- tmin[which( tmin3$`x_tmp$month` %in% tt$`x_tmp$month`),]}
  if(length(tmax$`x_tmp$month` != length(prcpi$`x_pr$month`))){
    tmax3 <-tmax
    tmax3$`x_tmp$month`<-paste(tmax$`x_tmp$month`,tmax$`x_tmp$year`)
    tt <-prcpi
    tt$`x_tmp$month` <- paste(prcpi$`x_pr$month`,prcpi$`x_pr$year`)
    tmax <- tmax[which( tmax3$`x_tmp$month` %in% tt$`x_tmp$month`),]}
  if(length(sradi$`x_sr$month` != length(prcpi$`x_pr$month`))){
    sradi3 <- sradi
    sradi3$`x_sr$month`<-paste(sradi$`x_sr$month`,sradi$`x_sr$year`)
    tt <-prcpi
    tt$`x_tmp$month` <- paste(prcpi$`x_pr$month`,prcpi$`x_pr$year`)
    sradi <- sradi[which( sradi3$`x_sr$month` %in% tt$`x_tmp$month`),]}
  if(length(prcpi$`x_pr$month` != length(tmin$`x_tmp$month`))){
    prcpi3 <- prcpi
    prcpi3$`x_pr$month`<-paste(prcpi$`x_pr$month`,prcpi$`x_pr$year`)
    tt <-tmin
    tt$`x_tmp$month` <- paste(tmin$`x_tmp$month`,tmin$`x_tmp$year`)
    prcpi <- prcpi[which( prcpi3$`x_pr$month` %in% tt$`x_tmp$month`),]}
  if(length(tave$`x_tmp$month` != length(prcpi$`x_pr$month`))){
    tave3 <-tave
    tave3$`x_tmp$month`<-paste(tave$`x_tmp$month`,tave$`x_tmp$year`)
    tt <-prcpi
    tt$`x_tmp$month` <- paste(prcpi$`x_pr$month`,prcpi$`x_pr$year`)
    tave <- tave[which( tave3$`x_tmp$month` %in% tt$`x_tmp$month`),]}
  if(length(sradi$`x_sr$month` != length(tmin$`x_tmp$month`))){
    sradi3 <- sradi
    sradi3$`x_sr$month`<-paste(sradi$`x_sr$month`,sradi$`x_sr$year`)
    tt <-tmin
    tt$`x_tmp$month` <- paste(tmin$`x_tmp$month`,tmin$`x_tmp$year`)
    sradi <- sradi[which( sradi3$`x_sr$month` %in% tt$`x_tmp$month`),]}
  if(length(frost_day$`x_tmp$month` != length(tmin$`x_tmp$month`))){
    frost_day3 <-frost_day
    frost_day3$`x_tmp$month`<-paste(frost_day$`x_tmp$month`,frost_day$`x_tmp$year`)
    tt <-tmin
    tt$`x_tmp$month` <- paste(tmin$`x_tmp$month`,tmin$`x_tmp$year`)
    frost_day <- frost_day[which( frost_day3$`x_tmp$month` %in% tt$`x_tmp$month`),]}
  if(length(co2_data$`x_tmp$month` != length(tmin$`x_tmp$month`))){
    co2_data3 <-co2_data
    co2_data3$`x_tmp$month`<-paste(co2_data$`x_tmp$month`,co2_data$`x_tmp$year`)
    tt <-tmin
    tt$`x_tmp$month` <- paste(tmin$`x_tmp$month`,tmin$`x_tmp$year`)
    co2_data <- co2_data[which( co2_data3$`x_tmp$month` %in% tt$`x_tmp$month`),]}
  if(length(ndep_data$`x_tmp$month` != length(sradi$`x_sr$month`))){
    ndep_data3 <-ndep_data
    ndep_data3$`x_tmp$month`<-paste(ndep_data$`x_tmp$month`,ndep_data$`site$year`)
    tt <-sradi
    tt$`x_tmp$month` <- paste(sradi$`x_sr$month`,sradi$`x_sr$year`)
    ndep_data <- ndep_data[which( ndep_data3$`x_tmp$month` %in% tt$`x_tmp$month`),]}
  #### end of period check ####
  d13catm_m <- array(0, length(tmin[,3]))
  
  
  ### FORM THE CLIMATE DATA FOR MODEL ###
  data <- data.frame(year = tmin[,2], month = tmin[,1], tmp_min = tmin[,3],tmp_max = tmax[,3],tmp_ave = tave[,3], prcp = prcpi[,3],srad = sradi[,3], frost_days = frost_day[,3],co2=co2_data[,3], d13catm = d13catm_m, n_depo=ndep_data[,3])
  d_climate <- as_tibble(data)
  
  
  ######## end of climate data preparation #####
  
  
  ######## Start soil data preparation #####
  
  
  ### finding soil class ####
  require(soiltexture)
  cs_select <- country_code_index
  ss_select <-as.numeric(as.numeric(gsub("site_num_", "", ss_select)))
  
  clay<- unique(ifelse((cs_select==icp_data$soil_data$code_country)&(as.numeric(ss_select)==as.numeric(icp_data$soil_data$code_plot)),icp_data$soil_data$horizon_clay,NA))
  clay <- mean(as.numeric(na.omit(clay)))
  
  #if clay not available in ICP data, then take it from ISRIC
  if(length(clay)==0 | clay== "NaN" ){
    lat_min <- (abs(isric_soil_d$soil_data$isric_lat- as.numeric(unique(site_lat)))==min(abs(isric_soil_d$soil_data$isric_lat- as.numeric(unique(site_lat)))))
    lon_min <- (abs(isric_soil_d$soil_data$isric_lon- as.numeric(unique(site_lon)))==min(abs(isric_soil_d$soil_data$isric_lon- as.numeric(unique(site_lon)))))
    x_clay <- isric_soil_d$soil_data %>% filter( lat_min & lon_min)
    clay <-mean(x_clay$isric_clay.CLYPPT_M_sl2_1km_ll,na.rm=T)
  }
  
  silt<- unique(ifelse((cs_select==icp_data$soil_data$code_country)&(as.numeric(ss_select)==as.numeric(icp_data$soil_data$code_plot)),icp_data$soil_data$horizon_silt,NA))
  silt <- mean(as.numeric(na.omit(silt)))
  
  #if silt not available in ICP data, then take it from ISRIC
  if(length(silt)==0 | silt== "NaN"  ){
    lat_min <- (abs(isric_soil_d$soil_data$isric_lat- as.numeric(unique(site_lat)))==min(abs(isric_soil_d$soil_data$isric_lat- as.numeric(unique(site_lat)))))
    lon_min <- (abs(isric_soil_d$soil_data$isric_lon- as.numeric(unique(site_lon)))==min(abs(isric_soil_d$soil_data$isric_lon- as.numeric(unique(site_lon)))))
    x_silt <- isric_soil_d$soil_data %>% filter( lat_min & lon_min)
    silt <-mean(x_silt$isric_silt.SLTPPT_M_sl2_1km_ll,na.rm=T)
  }
  
  sand<- unique(ifelse((cs_select==icp_data$soil_data$code_country)&(as.numeric(ss_select)==as.numeric(icp_data$soil_data$code_plot)),icp_data$soil_data$horizon_sand,NA))
  sand <- mean(as.numeric(na.omit(sand)))
  
  #if sand not available in ICP data, then take it from ISRIC
  if(length(sand)==0 | sand== "NaN" ){
    lat_min <- (abs(isric_soil_d$soil_data$isric_lat- as.numeric(unique(site_lat)))==min(abs(isric_soil_d$soil_data$isric_lat- as.numeric(unique(site_lat)))))
    lon_min <- (abs(isric_soil_d$soil_data$isric_lon- as.numeric(unique(site_lon)))==min(abs(isric_soil_d$soil_data$isric_lon- as.numeric(unique(site_lon)))))
    x_sand <- isric_soil_d$soil_data %>% filter( lat_min & lon_min)
    sand <-mean(x_sand$isric_sand.SNDPPT_M_sl2_1km_ll,na.rm=T)
  }
  
  clay <- as.numeric(na.omit(clay))
  sand <- as.numeric(na.omit(sand))
  silt <- as.numeric(na.omit(silt))
  
  if((length(clay) < length(silt)) | length(clay) < length(sand)){
    sand <- sand[1:length(clay)]
    silt <- silt[1:length(clay)]}
  
  if((length(silt) < length(clay)) | length(silt) < length(sand)){
    clay <- clay[1:length(silt)]
    sand <- sand[1:length(silt)]}
  
  if((length(sand) < length(clay)) | length(sand) < length(silt)){
    clay <- clay[1:length(sand)]
    silt <- silt[1:length(sand)]}
  
  
  
  for (i in 1:length(clay)){
    if(sum(c(clay[i],silt[i],sand[i]))< 100){
      if(sand[i]>clay[i] && sand[i]>silt[i]){
        sand[i] <-sand[i]+(100-sum(c(silt[i],clay[i],sand[i])))}
      else if(silt[i]>clay[i] && silt[i]>sand[i]){
        silt[i] <-silt[i]+(100-sum(c(silt[i],clay[i],sand[i])))}
      else if (clay[i]>sand[i] && clay[i]>silt[i]){
        clay[i] <-clay[i]+(100-sum(c(silt[i],clay[i],sand[i])))}
    }
  }
  for (i in 1:length(clay)){
    if(sum(c(clay[i],silt[i],sand[i]))> 100){
      if(sand[i]>clay[i] && sand[i]>silt[i]){
        sand[i] <-sand[i]+(100-sum(c(silt[i],clay[i],sand[i])))}
      else if(silt[i]>clay[i] && silt[i]>sand[i]){
        silt[i] <-silt[i]+(100-sum(c(silt[i],clay[i],sand[i])))}
      else if (clay[i]>sand[i] && clay[i]>silt[i]){
        clay[i] <-clay[i]+(100-sum(c(silt[i],clay[i],sand[i])))}
    }
  }
  msoil <- data.frame(clay,silt,sand)
  
  df <- as.data.frame(msoil)
  row_sub <- apply(df, 1, function(row) all(row !=0))
  df <- df[row_sub,]
  names(df) <- c('CLAY','SILT','SAND')
  df_classes <- TT.points.in.classes(tri.data = df, class.sys   = "HYPRES.TT", PiC.type = 't')
  df_classes <- as.factor(df_classes)
  cls_m<-as.character(names(which.max(table(df_classes))))
  if(cls_m == "C"){class_soil=1}
  if(cls_m == "M"){class_soil=2}
  if(cls_m == "MF"){class_soil=3}
  if(cls_m == "F"){class_soil=4}
  if(cls_m == "VF"){class_soil=5}
  
  x<-NA
  cs_int<- mean(unique(ifelse((cs_select==icp_data$soil_som_data$code_country)&(as.numeric(ss_select)==as.numeric(icp_data$soil_som_data$code_plot)),icp_data$soil_som_data$organic_carbon_total,NA)),na.rm=TRUE)
  #if soc not available in ICP data, then take it from ISRIC
  if(length(cs_int)==0 | cs_int=="NaN"){
    lat_min <- (abs(isric_soil_d$soil_data$isric_lat- as.numeric(unique(site_lat)))==min(abs(isric_soil_d$soil_data$isric_lat- as.numeric(unique(site_lat)))))
    lon_min <- (abs(isric_soil_d$soil_data$isric_lon- as.numeric(unique(site_lon)))==min(abs(isric_soil_d$soil_data$isric_lon- as.numeric(unique(site_lon)))))
    x_soc <- isric_soil_d$soil_data %>% filter( lat_min & lon_min)
    cs_int <-mean(x_soc$isric_soc.OCSTHA_M_sd1_1km_ll,na.rm=T)*10
  }
  
  
  x<-NA
  ns_int<- mean(unique(ifelse((cs_select==icp_data$soil_som_data$code_country)&(as.numeric(ss_select)==as.numeric(icp_data$soil_som_data$code_plot)),icp_data$soil_som_data$n_total,NA)),na.rm=TRUE)
  if(is.na(ns_int)){
    lat_min <- (abs(isric_soil_d$soil_data_nitrogen$x - as.numeric(unique(site_lon)))==min(abs(isric_soil_d$soil_data_nitrogen$x- as.numeric(unique(site_lon)))))
    lon_min <- (abs(isric_soil_d$soil_data_nitrogen$y- as.numeric(unique(site_lat)))==min(abs(isric_soil_d$soil_data_nitrogen$y- as.numeric(unique(site_lat)))))
    x <- isric_soil_d$soil_data_nitrogen %>% filter( lat_min & lon_min)
    ns_int<-mean(x$nitrogen,na.rm=T)*10
  }
  
  ps_int<- mean(unique(ifelse((cs_select==icp_data$soil_ssm_data$code_country)&(as.numeric(ss_select)==as.numeric(icp_data$soil_ssm_data$code_plot)),icp_data$soil_ssm_data$p,NA)),na.rm=TRUE)
  if(is.na(ps_int)){
    ps_int<- ns_int/10 #N:P can be change
  }
  
  x<-NA
  aw_s_int<- mean(as.numeric(unique(ifelse((cs_select==icp_data$soil_som_data$code_country)&(as.numeric(ss_select)==as.numeric(icp_data$soil_som_data$code_plot)),icp_data$soil_som_data$moisture_content,NA))),na.rm=T)
  if(is.na(aw_s_int) | aw_s_int <100){
    lat_min <- (abs(isric_soil_d$soil_data_awc$isric_awc_lat- as.numeric(unique(site_lat)))==min(abs(isric_soil_d$soil_data_awc$isric_awc_lat- as.numeric(unique(site_lat)))))
    lon_min <- (abs(isric_soil_d$soil_data_awc$isric_awc_lon- as.numeric(unique(site_lon)))==min(abs(isric_soil_d$soil_data_awc$isric_awc_lon- as.numeric(unique(site_lon)))))
    x <- isric_soil_d$soil_data_awc %>% filter( lat_min & lon_min)
    aw_s_int<-x$isrci_awc}
  
  cn_bio <- 14.3
  cn_hum <- 14.3
  cp_bio <- 2000.0
  cp_hum <- 2000.0
  lit_cn <- 300.0
  lit_cp <- 3000.0
  cn_veg <- 24
  cp_veg <- 240
  
  
  ####### SITE info #######
  lat <- site_lat
  lon <- site_lon
  cls <- class_soil
  asw <- 999
  as_min <- 0
  as_max <- aw_s_int
  xl<-tmin$`x_tmp$month`[max(length(tmin$`x_tmp$month`))]
  xs<-tmin$`x_tmp$month`[1]
  
  from_i <-(paste(min(unique(tmin$`x_tmp$year`)),xs,sep = "-", collapse=NULL))
  to_i  <- (paste(max(unique(tmin$`x_tmp$year`)),xl,sep = "-", collapse=NULL))
  soil_carbon_i <- cs_int
  if(ns_int != "NaN"){
    soil_nitrogen_i <- ns_int}else{soil_nitrogen_i = (cs_int / cn_bio)}
  if(ps_int != "NaN"){
    soil_phosphorus_i <- ps_int}else{soil_phosphorus_i = cs_int / cp_bio}
  
  data <- data.frame( latitude = lat, altitude = lon, soil_class = cls, asw_i = asw, asw_min = as_min, asw_max = as_max, from = from_i, to = to_i, soil_carbon = soil_carbon_i, soil_nitrogen = soil_nitrogen_i, soil_phosphorus = soil_phosphorus_i)
  d_site <- as_tibble(data)
  d_site_full <- as.tibble(data.frame( latitude = lat, altitude = lon, clay= clay, silt = silt, sand = sand, soil_class = cls, asw_i = asw, asw_min = as_min, asw_max = as_max, from = from_i, to = to_i, soil_carbon = soil_carbon_i, soil_nitrogen = soil_nitrogen_i, soil_phosphorus = soil_phosphorus_i))
  
  
  ######## End of soil data preparation #####
  
  ######## Start species data preparation #####
  
  spx<- as.numeric(na.omit((unique(ifelse((cs_select==icp_data$stand_data$code_country)&(as.numeric(ss_select)==as.numeric(icp_data$stand_data$code_plot)),icp_data$stand_data$code_tree_species,NA)))))
  stand_code<-c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,90,91,92,93,94,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,197,198,199,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,888)
  stand_name<-c("Not-known","Acer campestre","Acer monspessulanum","Acer opalus","Acer platanoides","Acer pseudoplatanus","Alnus cordata","Alnus glutinosa","Alnus incana","Alnus viridis","Betula pendula","Betula pubescens","Buxus sempervirens","Carpinus betulus","Carpinus orientalis","Castanea sativa","Corylus avellana","Eucalyptus sp.","Fagus moesiaca","Fagus orientalis","Fagus sylvatica","Fraxinus angustifolia","Fraxinus excelsior","Fraxinus ornus","Ilex aquifolium","Juglans nigra","Juglans regia","Malus domestica","Olea europaea","Ostrya carpinifolia","Platanus orientalis","Populus alba","Populus x canescens","Populus hybrides","Populus nigra","Populus tremula","Prunus avium","Prunus dulcis","Prunus padus","Prunus serotina","Pyrus communis","Quercus cerris","Quercus coccifera","Quercus faginea","Quercus frainetto","Quercus fruticosa","Quercus ilex","Quercus macrolepis","Quercus petraea","Quercus pubescens","Quercus pyrenaica","Quercus robur","Quercus rotundifolia","Quercus rubra","Quercus suber","Quercus trojana","Robinia pseudoacacia","Salix alba","Salix caprea","Salix cinerea","Salix eleagnos","Salix fragilis","Salix sp.","Sorbus aria","Sorbus aucuparia","Sorbus domestica","Sorbus torminalis","Tamarix africana","Tilia cordata","Tilia platyphyllos","Ulmus glabra","Ulmus laevis","Ulmus minor","Arbutus unedo","Arbutus andrachne","Ceratonia siliqua","Cercis siliquastrum","Erica arborea","Erica scoparia","Erica manipuliflora","Laurus nobilis","Myrtus communis","Phillyrea latifolia","Phillyrea angustifolia","Pistacia lentiscus","Pistacia terebinthus","Rhamnus oleoides","Rhamnus alaternus","Betula tortuosa","Crataegus monogyna","Ilex canariensis","Laurus canariensis","Myrica faya","Central Anatolian oaks","Quercus petraea_or_robur","Other broadleaves","Abies alba","Abies borisii-regis","Abies cephalonica","Abies grandis","Abies nordmanniana","Abies pinsapo","Abies procera","Cedrus atlantica","Cedrus deodara","Cupressus lusitanica","Cupressus sempervirens","Juniperus communis","Juniperus oxycedrus","Juniperus phoenicea","Juniperus sabina","Juniperus thurifera","Larix decidua","Larix kaempferi","Picea abies","Picea omorika","Picea sitchensis","Pinus brutia","Pinus canariensis","Pinus cembra","Pinus contorta","Pinus halepensis","Pinus heldreichii","Pinus mugo","Pinus nigra","Pinus pinaster","Pinus pinea","Pinus radiata","Pinus strobus","Pinus sylvestris","Pinus uncinata","Pseudotsuga menziesii","Taxus baccata","Thuya sp.","Tsuga sp.","Chamaecyparis lawsoniana","Cedrus brevifolia","Abies cilicica","Cedrus libani","Juniperus excelsa","Juniperus foetidissima","Picea orientalis","Abies amabilis","Abies concolor","Abies veitchii","Larix eurolepis","Picea glauca","Picea pungens","Pinus banksiana","Pinus peuce","Pinus rigida","Pinus wallichiana","Araucaria araucana","Calocedrus decurrens","Cryptomeria japonica","Metasequoia glyptostroboides","Sequoiadendron giganteum","Tamarix ramosissima","Taxodium distichum","Tsuga canadensis","Larix sp.","Abies sp.","Other conifers","Quercus hartwissiana","Quercus vulcanica","Quercus infectoria","Quercus macranthera","Quercus libani","Quercus brantii","Quercus ithaburensis","Quercus aucheri  ","Quercus pontica","Tilia sp.","Populus sp.","Betula sp.","Ulmus sp.","Betula x hybrida","Acer sp.","Alnus sp.","Crataegus sp.","Malus sylvestris","Aesculus hippocastanum","Acer negundo","Acer obtusatum","Acer saccharinum","Acer tataricum","Fraxinus americana","Fraxinus pennsylvanica","Laburnum alpinum","Laburnum anagyroides","Populus balsamifera","Populus deltoides","Prunus cerasifera","Prunus domestica","Prunus laurocerasus","Prunus mahaleb","Prunus spinosa","Quercus palustris","Quercus pedunculiflora","Salix purpurea","Salix triandra","Salix viminalis","Ailanthus altissima","Broussonetia papyrifera","Carya ovata","Corylus colurna","Crataegus laevigata","Liriodendron tulipifera","Mespilus germanica","Paulownia tomentosa","Petteria ramentacea","Phillyrea media","Platanus xacerifolia","Pyrus pyraster","Sorbus austriaca","Tilia tomentosa","All species")
  ss <- data.frame(stand_code,stand_name)
  sp<- ss$stand_name[which(ss$stand_code==spx)]
  subset_data <- icp_data$stand_data[(cs_select == icp_data$stand_data$code_country) & (as.numeric(ss_select) == as.numeric(icp_data$stand_data$code_plot)), "code_stand_history"]
  unique_data <- unique(na.omit(subset_data))
  plx <-NA
  plx[1:length(sp)] <- as.numeric(unique_data)
  na_idx <- which(is.na(plx))
  plx[na_idx] <- 9
  pl<-NA
  if (plx %in% icp_data$ss_age_d_2$ss_age_num) {
    pl[1:length(sp)] <- icp_data$ss_age_d_2$ss_age_2[which(icp_data$ss_age_d_2$ss_age_num_2 == plx[1:length(sp)])]
  } else {
    pl[1:length(sp)] <- icp_data$ss_age_d_1$ss_age_1[which(icp_data$ss_age_d_1$ss_age_num_1 == plx[1:length(sp)])]
  }
  
  #species fertility
  fer<- array(NA,length(sp))
  fer[1:length(sp)] <- NA
  
  tree_no<- NA
  st_n<- array(NA,length(sp))
  bio_s<- array(NA,length(sp))
  root_s<- array(NA,length(sp))
  foliage_s<- array(NA,length(sp))
  deadwood_c<- array(NA,length(sp))
  deadwood_n<- array(NA,length(sp))
  litter_c  <- array(NA,length(sp))
  litter_n	<- array(NA,length(sp))
  n_sap	    <- array(NA,length(sp))
  sap_wf	  <- array(NA,length(sp))
  sap_wr	  <- array(NA,length(sp))
  sap_ws    <- array(NA,length(sp))
  diam<-NA
  
  #species stem n
  spl <- sp
  
  
  
  
  if(!is.na(spl) && length(spl)>0){
    fer <- NA
    ####Frosster beta s for biomass estimation
    beta0_s <- as.numeric(substr(filter(icp_data$frosster_d, icp_data$frosster_d$Species==spl,icp_data$frosster_d$Equation==3,icp_data$frosster_d$Component=="Stem mass")$`ln(β0)`, 1, 4))
    beta0_r <- as.numeric(substr(filter(icp_data$frosster_d, icp_data$frosster_d$Species==spl,icp_data$frosster_d$Equation==3,icp_data$frosster_d$Component=="Root mass")$`ln(β0)`, 1, 4))
    beta0_f <- as.numeric(substr(filter(icp_data$frosster_d, icp_data$frosster_d$Species==spl,icp_data$frosster_d$Equation==3,icp_data$frosster_d$Component=="Foliage mass")$`ln(β0)`, 1, 4))
    beta0_d <- as.numeric(substr(filter(icp_data$frosster_d, icp_data$frosster_d$Species==spl,icp_data$frosster_d$Equation==3,icp_data$frosster_d$Component=="Dead branch")$`ln(β0)`, 1, 4))
    if(length(beta0_s)<1){beta0_s <- as.numeric(substr(filter(icp_data$frosster_d, icp_data$frosster_d$Species=="All species combined",icp_data$frosster_d$Equation==3,icp_data$frosster_d$Component=="Stem mass")$`ln(β0)`, 1, 4))}
    if(length(beta0_r)<1){beta0_r <- as.numeric(substr(filter(icp_data$frosster_d, icp_data$frosster_d$Species=="All species combined",icp_data$frosster_d$Equation==3,icp_data$frosster_d$Component=="Root mass")$`ln(β0)`, 1, 4))}
    if(length(beta0_f)<1){beta0_f <- as.numeric(substr(filter(icp_data$frosster_d, icp_data$frosster_d$Species=="All species combined",icp_data$frosster_d$Equation==3,icp_data$frosster_d$Component=="Foliage mass")$`ln(β0)`, 1, 4))}
    if(length(beta0_d)<1){beta0_d <- as.numeric(substr(filter(icp_data$frosster_d, icp_data$frosster_d$Species=="All species combined",icp_data$frosster_d$Equation==3,icp_data$frosster_d$Component=="Dead branch")$`ln(β0)`, 1, 4))}
    
    beta1_s <- as.numeric(substr(filter(icp_data$frosster_d, icp_data$frosster_d$Species==spl,icp_data$frosster_d$Equation==3,icp_data$frosster_d$Component=="Stem mass")$`β for ln(d)`, 1, 4))
    beta1_r <- as.numeric(substr(filter(icp_data$frosster_d, icp_data$frosster_d$Species==spl,icp_data$frosster_d$Equation==3,icp_data$frosster_d$Component=="Root mass")$`β for ln(d)`, 1, 4))
    beta1_f <- as.numeric(substr(filter(icp_data$frosster_d, icp_data$frosster_d$Species==spl,icp_data$frosster_d$Equation==3,icp_data$frosster_d$Component=="Foliage mass")$`β for ln(d)`, 1, 4))
    beta1_d <- as.numeric(substr(filter(icp_data$frosster_d, icp_data$frosster_d$Species==spl,icp_data$frosster_d$Equation==3,icp_data$frosster_d$Component=="Dead branch")$`β for ln(d)`, 1, 4))
    if(length(beta1_s)<1){beta1_s <- as.numeric(substr(filter(icp_data$frosster_d, icp_data$frosster_d$Species=="All species combined",icp_data$frosster_d$Equation==3,icp_data$frosster_d$Component=="Stem mass")$`β for ln(d)`, 1, 4))}
    if(length(beta1_r)<1){beta1_r <- as.numeric(substr(filter(icp_data$frosster_d, icp_data$frosster_d$Species=="All species combined",icp_data$frosster_d$Equation==3,icp_data$frosster_d$Component=="Root mass")$`β for ln(d)`, 1, 4))}
    if(length(beta1_f)<1){beta1_f <- as.numeric(substr(filter(icp_data$frosster_d, icp_data$frosster_d$Species=="All species combined",icp_data$frosster_d$Equation==3,icp_data$frosster_d$Component=="Foliage mass")$`β for ln(d)`, 1, 4))}
    if(length(beta1_d)<1){beta1_d <- as.numeric(substr(filter(icp_data$frosster_d, icp_data$frosster_d$Species=="All species combined",icp_data$frosster_d$Equation==3,icp_data$frosster_d$Component=="Dead branch")$`β for ln(d)`, 1, 4))}
    
    
    tt<-filter(icp_data$tree_data_dia, icp_data$tree_data_dia$code_country==cs_select & as.numeric(icp_data$tree_data_dia$code_plot)==as.numeric(ss_select))
    
    
    
    if(length(tt$survey_year) > 0 && any(complete.cases(tt$diameter)) && any(complete.cases(tt$height))){
      tree_no<- median(tt$tree_number[tt$survey_year==min(tt$survey_year)])
      st_n[1:length(st_n)] = max(tree_no)
      fer <- 0.5
      
      
      diam<-as.numeric(na.omit((unique(ifelse((cs_select==icp_data$tree_data_dia$code_country)&(as.numeric(ss_select)==as.numeric(icp_data$tree_data_dia$code_plot)),icp_data$tree_data_dia$diameter,NA)))))
      height<-as.numeric(na.omit((unique(ifelse((cs_select==icp_data$tree_data_dia$code_country)&(as.numeric(ss_select)==as.numeric(icp_data$tree_data_dia$code_plot)),icp_data$tree_data_dia$height,NA)))))
      bio_s[1:length(bio_s)] = exp(beta0_s+beta1_s*log(mean(diam))+0.087*log(mean(height)))
      root_s[1:length(root_s)]  = exp(beta0_r+beta1_r*log(mean(diam))+0.087*log(mean(height)))
      foliage_s[1:length(foliage_s)] = exp(beta0_f+beta1_f*log(mean(diam))+0.087*log(mean(height)))
      if(length(exp(beta0_d+beta1_d*log(mean(diam))))>0){
        deadwood_c[1:length(deadwood_c)] = exp(beta0_d+beta1_d*log(mean(diam))+0.087*log(mean(height)))}}else{
          if(length(tt$survey_year)>0){
            #Second method of calculation
            tt<-filter(icp_data$thinning_data, icp_data$thinning_data$code_country==cs_select & as.numeric(icp_data$thinning_data$code_plot)==as.numeric(ss_select))
            tt_int<-filter(tt, tt$survey_year == min(tt$survey_year))
            tree_no<- median(tt_int$trees_number)
            diam <-mean(c(as.numeric(tt_int$basal_area_1),as.numeric(tt_int$basal_area_2),as.numeric(tt_int$basal_area_5),as.numeric(tt_int$basal_area_10),as.numeric(tt_int$basal_area_15),as.numeric(tt_int$basal_area_20),as.numeric(tt_int$basal_area_25)),na.rm=T)
            st_n[1:length(st_n)] = max(tree_no)
            bio_s[1:length(bio_s)] = exp(beta0_s+beta1_s*log(mean(diam)))
            root_s[1:length(root_s)] = exp(beta0_r+beta1_r*log(mean(diam)))
            foliage_s[i] = exp(beta0_f+beta1_f*log(mean(diam)))
            if(length(exp(beta0_d+beta1_d*log(mean(diam))))>0){
              deadwood_c[1:length(deadwood_c)] = exp(beta0_d+beta1_d*log(mean(diam)))}
            
          }
        }
    
  }
  
  if(is.na(deadwood_c[1:length(sp)])){deadwood_c[1:length(sp)]<-0}
  deadwood_n[1:length(sp)]	<- 0 # N/A
  litter_c[1:length(sp)]	<- 0 # N/A
  litter_n[1:length(sp)]	<- 0 # N/A
  n_sap[1:length(sp)]   	<- 10000 # N/A
  sap_wf[1:length(sp)]   	<- 10 # N/A
  sap_wr[1:length(sp)]  	<- 10 # N/A
  sap_ws[1:length(sp)]    <- 10 # N/A
  
  if(length(sp)>0){
    fer <- 0.5
    
    data <-data.frame( species=sp,	planted=paste0(pl,"-01"),	fertility=fer,	stems_n=st_n,	biom_stem=bio_s,	biom_root=root_s,	biom_foliage=foliage_s, deadwood_carbon=deadwood_c,	deadwood_nitrogen=deadwood_n,	litter_carbon=litter_c,	litter_nitrogen=litter_n,	n_sapling=n_sap,	sapling_wf=sap_wf,	sapling_wr=sap_wr,	sapling_ws=sap_ws)}else{
      data <-data.frame( species=NA,	planted=NA,	fertility=NA,	stems_n=NA,	biom_stem=NA,	biom_root=NA,	biom_foliage=NA, deadwood_carbon=NA,	deadwood_nitrogen=NA,	litter_carbon=NA,	litter_nitrogen=NA,	n_sapling=NA,	sapling_wf=NA,	sapling_wr=NA,	sapling_ws=NA)  
    }
  d_species <- as_tibble(data)
  
  if(exists("tt")) {
    cols <- intersect(colnames(tt), c("survey_year", "code_country", "code_plot", "tree_number", "code_tree_species", "diameter", "diameter_caliper", "bark", "height", "tree_volume", "crown_base_height", "crown_width", "code_removal", "other_obs", "partner_code", "q_flag", "change_date", "code_line", "code_diameter_qc", "code_diameter2_qc", "code_bark_qc", "code_height_qc", "code_volume_qc", "code_base_height_qc", "code_crown_width_qc", "gr_plot_id", "line_nr"))
    d_species_full <- as_tibble(tt[, cols])
  } else {
    d_species_full <- as_tibble(data.frame( survey_year = NA, code_country = NA, code_plot = NA, tree_number = NA, code_tree_species = NA, diameter = NA, diameter_caliper = NA, bark = NA, height = NA, tree_volume = NA, crown_base_height = NA, crown_width = NA, code_removal = NA, other_obs = NA, partner_code = NA, q_flag = NA, change_date = NA, code_line = NA, code_dimameter_qc = NA, code_dimameter2_qc = NA, code_bark_qc = NA, code_height_qc = NA, code_volume_qc = NA, code_base_height_qc = NA, code_crown_width_qc = NA, gr_plot_id = NA, line_nr = NA))  
  }
  
  
  ######## End of species data preparation #####
  
  age_i <- NA
  age_init <-NA
  ss_n<- array(NA,length(sp))
  s<- array(NA,length(sp))
  r<- array(NA,length(sp))
  f<- array(NA,length(sp))
  b<- array(NA,length(sp))
  dd<-NA
  V<-NA
  if(!is.na(spl) && length(spl)>0 && length(unique(tt$survey_year))>0){
    ######## Start thinning data preparation #####
    age_i <- 0
    age_init <-0
    t_int_s <- 0 
    #First age method
    years_s <- unique(tt$survey_year)
    if(length(years_s)>0){
      for(i in 1:length(sp)){
        if ("tree_number" %in% names(tt)) {
          if(length((filter(tt,tt$survey_year==years_s[1])$tree_number))>0){
            t_int_s<- mean(filter(tt,tt$survey_year==years_s[1])$tree_number,na.rm=T)}}
        if ("trees_number" %in% names(tt)) {
          if(length((filter(tt,tt$survey_year==years_s[1])$tree_number))>0){
            t_int_s<-mean(filter(tt,tt$survey_year==years_s[1])$tree_number,na.rm=T)}}
        if(t_int_s < tree_no){age_init <- years_s[1] - pl[1:length(sp)]}}
      if(length(years_s) >3){
        if(length((filter(tt,tt$survey_year==years_s[1])$tree_number))>0){
          for(i in 2:length(years_s)){
            if ("tree_number" %in% names(tt)) {
              t_d<-mean(filter(tt,tt$survey_year==years_s[i])$tree_number)}
            if ("trees_number" %in% names(tt)) {
              t_d<-mean(filter(tt,tt$survey_year==years_s[i])$tree_number)}
            if(!is.na(t_d)){
              if(t_d < t_int_s){age_init <- age_init + years_s[1] - pl[1:length(sp)]}}}}else{
                if(length((filter(tt,tt$survey_year==years_s[1])$tree_number))>0){
                  
                  if(length(years_s)==2){
                    if ("tree_number" %in% names(tt)) {
                      t_d<-mean(filter(tt,tt$survey_year==years_s[2])$tree_number)}
                    if ("trees_number" %in% names(tt)) {
                      t_d<-mean(filter(tt,tt$survey_year==years_s[2])$tree_number)}
                    if(!is.na(t_d)){
                      if(t_d < t_int_s){age_init <- age_init + years_s[1] - pl[1:length(sp)]}}
                  }}}
      }
      
      age_i <- age_init}
    
    
    #Second age method
    if(is.na(age_i)){
      
      t_0_5X <- (as.numeric(na.omit((unique(ifelse((as.numeric(cs_select)==icp_data$thinning_data$code_country)&(as.numeric(ss_select)==as.numeric(icp_data$thinning_data$code_plot)),icp_data$thinning_data$thinning_0_5,NA))))))
      if(length(t_0_5X)>0){t_0_5 <- max(t_0_5X,na.rm=T)}
      t_5_10X <- (as.numeric(na.omit((unique(ifelse((as.numeric(cs_select)==icp_data$thinning_data$code_country)&(as.numeric(ss_select)==as.numeric(icp_data$thinning_data$code_plot)),icp_data$thinning_data$thinning_5_10,NA))))))
      if(length(t_5_10X)>0){t_5_10 <- max(t_5_10X,na.rm=T)}
      t_10_15X <- (as.numeric(na.omit((unique(ifelse((as.numeric(cs_select)==icp_data$thinning_data$code_country)&(as.numeric(ss_select)==as.numeric(icp_data$thinning_data$code_plot)),icp_data$thinning_data$thinning_10_15,NA))))))
      if(length(t_10_15X)>0){t_10_15 <- max(t_10_15X,na.rm=T)}
      t_15_20X <- (as.numeric(na.omit((unique(ifelse((as.numeric(cs_select)==icp_data$thinning_data$code_country)&(as.numeric(ss_select)==as.numeric(icp_data$thinning_data$code_plot)),icp_data$thinning_data$thinning_15_20,NA))))))
      if(length(t_15_20X)>0){t_15_20 <- max(t_15_20X,na.rm=T)}
      t_20_25X <- (as.numeric(na.omit((unique(ifelse((as.numeric(cs_select)==icp_data$thinning_data$code_country)&(as.numeric(ss_select)==as.numeric(icp_data$thinning_data$code_plot)),icp_data$thinning_data$thinning_20_25,NA))))))
      if(length(t_20_25X)>0){t_20_25 <- max(t_20_25X,na.rm=T)}
      age_i= NA
      if(length(t_0_5X)>0){if(t_0_5 == 1){age_i <- 5}}
      if(length(t_5_10X)>0){if(t_5_10 == 1){age_i <- 10}}
      if(length(t_10_15X)>0){if(t_10_15 == 1){age_i <- 15}}
      if(length(t_15_20X)>0){if(t_15_20 == 1){age_i <- 20}}
      if(length(t_20_25X)>0){if(t_20_25 == 1){age_i <- 25}}}
    if(is.na(age_i)){age_i <-0}
    
    tt_in<-filter(icp_data$inventory_data, icp_data$inventory_data$code_country==cs_select & as.numeric(icp_data$inventory_data$code_plot)==as.numeric(ss_select))
    
    s_r <- mean(tt_in$stemvol_removed/tt_in$stemvol_remain,na.rm=T)
    ss_n<- array(NA,length(sp))
    s<- array(NA,length(sp))
    r<- array(NA,length(sp))
    f<- array(NA,length(sp))
    b<- array(NA,length(sp))
    
    s_r_rate <- root_s/bio_s
    s_f_rate <- foliage_s/bio_s
    
    
    ss_n[1:length(sp)]<- -999
    if(length(s_r)>0){
      if(!is.na(s_r) & s_r>0){
        s[1:length(sp)]   <-max(as.numeric(na.omit(s_r[s_r>0])))
        r[1:length(sp)]   <- s * s_r_rate
        f[1:length(sp)]   <- s * s_f_rate}}
    if(is.na(s)){s<-1}
    if(is.na(r)){r<-1}
    if(is.na(f)){f<-1}
    
    dd<-NA
    
    tt_int<-filter(tt, tt$survey_year == max(tt$survey_year))
    if ("basal_area_1" %in% names(tt_int)) {
      dd <-mean(c(as.numeric(tt_int$basal_area_1),as.numeric(tt_int$basal_area_2),as.numeric(tt_int$basal_area_5),as.numeric(tt_int$basal_area_10),as.numeric(tt_int$basal_area_15),as.numeric(tt_int$basal_area_20),as.numeric(tt_int$basal_area_25)),na.rm=T)
      V <-mean(c(as.numeric(tt_int$volume_1),as.numeric(tt_int$volume_2),as.numeric(tt_int$volume_5),as.numeric(tt_int$volume_10),as.numeric(tt_int$volume_15),as.numeric(tt_int$volume_20),as.numeric(tt_int$volume_25)),na.rm=T)*1e-2
      b<- array(NA,length(sp))
      for(i in 1:length(sp)){
        b[i] <-mean(diam,na.rm=T)}}
    if ("diameter" %in% names(tt_int)) {
      dd <-mean(as.numeric(tt_int$diameter),na.rm=T)
      V <-mean(as.numeric(tt_int$tree_volume),na.rm=T)*1e-2
      b<- array(NA,length(sp))
      for(i in 1:length(sp)){
        b[i] <-mean(diam,na.rm=T)}}}
  
  if(length(sp)>0){
    data <- data.frame(species=sp,age=age_i,stems_n=ss_n,stem=s,root=r,foliage=f,BA=dd,Vol=V)}else{
      data <- data.frame(species=NA,age=NA,stems_n=NA,stem=NA,root=NA,foliage=NA,BA=NA,Vol=NA)}
  
  d_thinning <- as_tibble(data)
  
  if(exists("tt_in")){
  d_thinning_full <- as_tibble(data.frame(survery_year = tt_in$survey_year, code_country = tt_in$code_country, code_plot = tt_in$code_plot, latitude = tt_in$latitude, longitude = tt_in$longitude, code_altitude = tt_in$code_altitude, date_sampling = tt_in$date_sampling, stemvol_remain = tt_in$stemvol_remain, stemvol_dead = tt_in$stemvol_dead, stemvol_removed = tt_in$stemvol_removed, other_obs = tt_in$other_obs, partner_code = tt_in$partner_code, q_flag = tt_in$q_flag, change_date = tt_in$change_date, code_line = tt_in$code_line, code_tree_species = tt_in$code_tree_species, trees_remain = tt_in$trees_remain, trees_dead = tt_in$trees_dead, trees_removed = tt_in$trees_removed, diameter_basal_area_tree = tt_in$diameter_basal_area_tree, height_basal_area_tree = tt_in$height_basal_area_tree, top_height_absolute = tt_in$top_height_absolute, trees_top_height = tt_in$trees_top_height, top_height_relative = tt_in$top_height_relative, percentage_top_height_relative = tt_in$percentage_top_height_relative, gr_plot_id = tt_in$gr_plot_id, line_nr = tt_in$line_nr))}else{
    d_thinning_full <- as_tibble(data.frame(survery_year = NA, code_country = NA, code_plot = NA, latitude = NA, longitude = NA, code_altitude = NA, date_sampling = NA, stemvol_remain = NA, stemvol_dead = NA, stemvol_removed = NA, other_obs = NA, partner_code = NA, q_flag = NA, change_date = NA, code_line = NA, code_tree_species = NA, trees_remain = NA, trees_dead = NA, trees_removed = NA, diameter_basal_area_tree = NA, height_basal_area_tree = NA, top_height_absolute =NA, trees_top_height = NA, top_height_relative = NA, percentage_top_height_relative = NA, gr_plot_id = NA, line_nr = NA))
  }
  
  
  ######## End of thinning data preparation #####
  
  return(list(d_climate = d_climate, d_site = d_site, d_site_full = d_site_full, d_species = d_species, d_species_full = d_species_full, d_thinning = d_thinning, d_thinning_full = d_thinning_full, d_parameters = d_parameters, d_sizeDist = d_sizeDist))
  
}


explore_server <- shinyServer(function(input, output, session, extracted_data) {
  # Initialize reactive values
  reset_outputs <- function() {
    selected_point <- NULL
    marker <- NULL
    extracted_data <- NULL
    extracted_data_es <- NULL
    output$select_point <- reactiveVal(NULL)
    output$plot <- renderPlot({
      return(NULL)
    })
    output$selected_point <- renderText({
      return(NULL)
    })
    output$table1 <- DT::renderDT({
      return(NULL)
    })
    output$table2 <- DT::renderDT({
      return(NULL)
    })
    output$table3 <- DT::renderDT({
      return(NULL)
    })
    map_marker_click <- reactiveVal(NULL)
  }
  
  rv <- reactiveValues(map_marker_click = NULL)
  session_data <- reactiveVal(list(map_marker_click = NULL))
  extracted_data <- reactiveValues(
    d_climate = NULL, 
    d_site = NULL, 
    d_site_full = NULL, 
    d_species = NULL, 
    d_species_full = NULL, 
    d_thinning = NULL, 
    d_thinning_full = NULL, 
    d_parameters = NULL, 
    d_sizeDist = NULL
  )


  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      setView(lng = 14, lat = 50, zoom = 3.2) %>%
      addCircleMarkers(
        data = points,
        lng = ~lon,
        lat = ~lat,
        radius = 5,
        color = "black",
        labelOptions = labelOptions(noHide = TRUE),
        layerId = ~id
      )
  })
  
  
  return(extracted_data)
})

model_ui <- fluidPage(
  navbarPage(
    tabPanel("",
             fluidRow(
               shinyjs::useShinyjs(),
               column(width = 12, align = "center",
                      img(src = "https://www.unwater.org/sites/default/files/styles/d02/public/app/uploads/2020/01/220x120_PARTNERS_IIASA.webp?itok=MjAJX7xP", height = 100)
               )
             ),
             br(),
             h4("ForestScope r3PG Modified Model Simulation"),
             br(),
             p("This app utilizes a modified version of the ",
               a("r3PG vegetation model", href = "https://github.com/trotsiuk/r3PG"),
               " provided by ",
               a("IIASA", href = "https://www.iiasa.ac.at/"),
               ". The original r3PG model is a process-based model developed for simulating forest growth and stand dynamics. This modified version extends the original model to include the simulation of carbon, nitrogen, and phosphorus pools and fluxes in both aboveground and belowground components."),
             br(),
             p("To get started, make sure you selected the site and the site has all the required data (e.g. climate, soil, species and thinning)."),
             br(),
             
             fluidRow(
               column(width = 3, align = "center",
                      selectInput("carbonModel",
                                  "Carbon Model:",
                                  choices = c( "RothC", "ICBM", "YASSO")),
                      selectInput("limitation",
                                  "Limitation:",
                                  choices = c("No - Limitation", "N-P limitation")),
                      br(),
                      actionButton("run_model_r3pg", icon = icon("cog"), "Run MODEL"),
                      br(),
                      actionLink("back_home", "Back to Home Page")
               ),
               conditionalPanel(
                 condition = "input.run_model_r3pg > 0",
                 actionButton("output_climate", "Climate"),
                 actionButton("output_stand", "Stand"),
                 actionButton("output_canopy", "Canopy"),
                 actionButton("output_stocks", "Stocks"),
                 actionButton("output_modifiers", "Modifiers"),
                 actionButton("output_production", "Production"),
                 actionButton("output_wateruse", "Water Use"),
                 actionButton("output_mortality", "Mortality Management"),
                 actionButton("output_wooddelta", "Wood Delta"),
                 actionButton("output_weibull", "Weibull"),
                 actionButton("output_soil", "Soil")
               ),
               column(width = 8, align = "center",
                      plotOutput("output_plot")
               )
             )
    )
  )
)

model_server <- function(input, output, session, model_outputs) {
  model_outputs <- reactiveValues(climate = NULL, stand = NULL, canopy = NULL, stocks = NULL, modifiers = NULL, production = NULL, water_use = NULL, mortality_management = NULL, wood_delta = NULL, weibull = NULL, soil = NULL)
  
  return(model_outputs)
}

documentation_ui <- fluidPage(
  titlePanel("ForestScope Package Documentation"),
  
  mainPanel(
    fluidRow(
      column(
        width = 12,
        align = "center",
        img(
          src = "https://www.unwater.org/sites/default/files/styles/d02/public/app/uploads/2020/01/220x120_PARTNERS_IIASA.webp?itok=MjAJX7xP",
          height = 100
        )
      )
    ),
    
    HTML("
      <div style='text-align:left;'>
        <h3>Introduction to the ForestScope Package</h3>
        <p>
          The ForestScope package is a tool for processing and analyzing soil, climate, and stand management data related to the International Co-operative Programme (ICP) on the Assessment and Monitoring of Air Pollution Effects on Forests. The ICP is a global network of research stations that collect data on forest ecosystems and their response to air pollution. The ForestScope package provides functions that help researchers and analysts work with this data more effectively.
        </p>

        <p>
          The package includes functions for processing both ISRIC soil data and ICP climate, soil, stand, and management parameters. These functions allow users to extract specific information about ICP sites, including soil characteristics and properties, and to get a comprehensive overview of the various environmental factors affecting these sites. Additionally, the package includes a wrapper function that streamlines the data processing and analysis workflow for users working with ICP data. The package also allows for processing of the CHELSA climate data, which can replace the ICP climate data when it is not available.
        </p>

        <p>
          The ForestScope package was developed by Dr. Andre Nakhavali, a researcher at the International Institute for Applied Systems Analysis (IIASA).
        </p>
      </div>
    "),
    
    fluidRow(
      column(
        width = 12,
        align = "left",
        tags$a(
          href = "mailto:nakhavali@iiasa.ac.at",
          class = "btn btn-primary",
          icon("envelope"),
          "Contact"
        )
      )
    ),
    
    HTML("
      <div style='text-align:left;'>
        <h3>Main Functions</h3>
        <ol>
          <li>
            <strong>isric_soil_data:</strong> This function processes ISRIC soil data and extracts information specific to ICP sites. It allows users to retrieve soil characteristics and properties for the selected sites.
          </li>
          <li>
            <strong>chelsa_climate_data:</strong>  This function processes CHELSA soil data and extracts alternative climate variables for ICP sites. It enables users to retrieve climate variables for selected sites where site measurements are not available. 
          </li>
          <li>
            <strong>ICP_climate_soil:</strong> This function processes ICP climate, soil, stand, and management parameters. It provides a comprehensive overview of the various environmental factors affecting ICP sites and helps users understand the relationships between these factors.
          </li>
          <li>
            <strong>IIASA_ICP_process:</strong> This function acts as a wrapper, calling the aforementioned functions (isric_soil_data and ICP_climate_soil) and inquiring about the site to be processed. It streamlines the data processing and analysis workflow for users working with ICP data.
          </li>
        </ol>
      </div>
    "),
    actionLink("back_home", "Back to Home Page"),
    br(),
    br()
  )
)



# Define server logic for ISRIC data
documentation_server <- function(input, output, session) {
  observeEvent(input$back_home, {
    output$page <- renderUI(home_ui)
  })
}
ui <- fluidPage(
  
  uiOutput("page"),
  
  
)
server <- function(input, output, session, isric_soil_data, chelsa_clim, icp_data, extracted_data, model_outputs) {
  
  isric_soil_data <- callModule(isric_server, id = "isric")
  icp_data <- callModule(icp_server, id = "icp")
  chelsa_clim <- callModule(chelsa_server, id = "chelsa")
  extracted_data <- callModule(explore_server, id = "explore")
  model_outputs <- callModule(model_server, id = "model")
  state <- reactiveVal("home")

  
  observeEvent(input$run_process_isric, {
    req(input$awc_files)
    req(input$clay_files)
    req(input$silt_files)
    req(input$sand_files)
    req(input$soc_files)
    req(input$nitrogen_files)
    withProgress(message = "Processing Soil Data", value = 0, {
      isric_data <- NA
      isric_data <- isric_soil_data_modified(input$awc_files, input$clay_files, input$silt_files, input$sand_files, input$soc_files, input$nitrogen_files)
      isric_soil_data_re<- NA
      isric_soil_data_re<- list(isric_data$soil_data, isric_data$soil_data_awc, isric_data$soil_data_nitrogen)
      callModule(isric_server, id = "isric", isric_soil_data = isric_soil_data_re)
    isric_soil_data$soil_data <- isric_data$soil_data
    isric_soil_data$soil_data_awc <- isric_data$soil_data_awc
    isric_soil_data$soil_data_nitrogen <- isric_data$soil_data_nitrogen
    })
  })
  
  observeEvent(input$run_process_icp, {
    req(input$icp_climate)
    req(input$so_pfh)
    req(input$so_som)
    req(input$ss_ssm)
    req(input$si_sta)
    req(input$gr_ipm)
    req(input$gr_iev)
    req(input$gr_inv)
    req(input$lf_lfm)
    icp_data_es <- NA
    icp_data_es <- ICP_climate_soil(icp_climate=input$icp_climate, so_pfh = input$so_pfh, so_som = input$so_som, ss_ssm = input$ss_ssm,
                                 si_sta= input$si_sta, gr_ipm = input$gr_ipm, gr_iev = input$gr_iev, gr_inv = input$gr_inv, lf_lfm = input$lf_lfm)
    
    callModule(icp_server, id = "icp", icp_data = icp_data_es)

    icp_data$country_names <- icp_data_es$country_names 
    icp_data$lat_lon <- icp_data_es$lat_lon
    icp_data$soil_data <- icp_data_es$soil_data
    icp_data$soil_som_data <- icp_data_es$soil_som_data
    icp_data$soil_ssm_data <- icp_data_es$soil_ssm_data
    icp_data$ICP_temperature <- icp_data_es$ICP_temperature
    icp_data$ICP_precipitation <- icp_data_es$ICP_precipitation
    icp_data$ICP_radiation <- icp_data_es$ICP_radiation
    icp_data$stand_data <- icp_data_es$stand_data
    icp_data$tree_data_dia  <- icp_data_es$tree_data_dia 
    icp_data$thinning_data <- icp_data_es$thinning_data
    icp_data$inventory_data <- icp_data_es$inventory_data
    icp_data$litter_data <- icp_data_es$litter_data
    icp_data$frosster_d <- icp_data_es$frosster_d
    icp_data$s_age_d_1 <- icp_data_es$ss_age_d_1
    icp_data$s_age_d_2 <- icp_data_es$ss_age_d_2
    
  })
  
  
  observeEvent(input$run_process_chelsa, {
    req(input$chelsa_pr)
    req(input$chelsa_tas)
    req(input$chelsa_tas_min)
    req(input$chelsa_tas_max)
    req(input$chelsa_rad)
    
    chelsa_clim_es <-NA
    chelsa_clim_es <- chelsa_climate(chelsa_pr = input$chelsa_pr, chelsa_tas = input$chelsa_tas, chelsa_tas_min = input$chelsa_tas_min, chelsa_tas_max = input$chelsa_tas_max, chelsa_rad = input$chelsa_rad)
    
    callModule(chelsa_server, id = "icp", chelsa_clim = chelsa_clim_es)

    chelsa_clim$chelsa_pr <- chelsa_clim_es$chelsa_pr
    chelsa_clim$chelsa_tas <- chelsa_clim_es$chelsa_tas
    chelsa_clim$chelsa_tas_min <- chelsa_clim_es$chelsa_tas_min
    chelsa_clim$chelsa_tas_max <- chelsa_clim_es$chelsa_tas_max
    chelsa_clim$chelsa_rad <- chelsa_clim_es$chelsa_rad
  })
  
  reset_outputs <- function() {
    selected_point <- NULL
    marker <- NULL
    extracted_data <- NULL
    extracted_data_es <- NULL
    output$select_point <- reactiveVal(NULL)
    output$plot <- renderPlot({
      return(NULL)
    })
    output$selected_point <- renderText({
      return(NULL)
    })
    output$table1 <- DT::renderDT({
      return(NULL)
    })
    output$table2 <- DT::renderDT({
      return(NULL)
    })
    output$table3 <- DT::renderDT({
      return(NULL)
    })
    map_marker_click <- reactiveVal(NULL)
  }
  
 
  observeEvent(input$reset_explore, {
    
    reset_outputs()
    
  })
 
  
  observeEvent(input$map_marker_click, {
    reset_outputs()  
     isric_soil_data <- isric_soil_dx
     icp_data <- icp_data_dx
     chelsa_clim <- chelsa_clim_dx
    # 
     # req(isric_soil_data$soil_data)
     # req(isric_soil_data$soil_data_awc)
     # req(isric_soil_data$soil_data_nitrogen)
     # req(icp_data$country_code)
     # req(icp_data$country_names)
     # req(icp_data$lat_lon)
     # req(icp_data$ICP_temperature)
     # req(icp_data$ICP_precipitation)
     # req(icp_data$ICP_radiation)
     # req(icp_data$tree_data_dia)
     # req(icp_data$thinning_data)
     # req(icp_data$inventory_data)
     # req(icp_data$litter_data)
     # req(icp_data$frosster_d)
     # req(chelsa_clim$chelsa_pr)
     # req(chelsa_clim$chelsa_tas)
     # req(chelsa_clim$chelsa_tas_min)
     # req(chelsa_clim$chelsa_tas_max)
     # req(chelsa_clim$chelsa_rad)
    extracted_data_es <-NA
    marker <- input$map_marker_click
    selected_point <- points[points$id == marker$id, ]
    output$selected_point <- renderText({
      paste("Site Number:", as.numeric(gsub("site_num_", "", selected_point$site)), "\nCountry:", selected_point$con, "\nLatitude:", selected_point$lat, "\nLongitude:", selected_point$lon)
    })
    
    extracted_data_es<-IIASA_ICP_process(isric_soil_d = isric_soil_data, icp_data = icp_data, chelsa_clim = chelsa_clim, country= selected_point$con, site_number= selected_point$site, lat = selected_point$lat, lon = selected_point$lon)
    output$plot <- renderPlot({
      par(mfrow=c(3,3))
      plot(extracted_data_es$d_climate$tmp_min, ylab="Tmin", xlab="Time", typ="l", lwd=1.2, col="brown", cex.lab=1.5, cex.axis=1.3)
      plot(extracted_data_es$d_climate$tmp_max, ylab="Tmax", xlab="Time", typ="l", lwd=1.2, col="brown", cex.lab=1.5, cex.axis=1.3)
      plot(extracted_data_es$d_climate$tmp_ave, ylab="Tavg", xlab="Time", typ="l", lwd=1.2, col="brown", cex.lab=1.5, cex.axis=1.3)
      plot(extracted_data_es$d_climate$prcp, ylab="Precipitation", xlab="Time", typ="l", lwd=1.2, col="blue", cex.lab=1.5, cex.axis=1.3)
      plot(extracted_data_es$d_climate$srad, ylab="Srad", xlab="Time", typ="l", lwd=1.2, col="red", cex.lab=1.5, cex.axis=1.3)
      plot(extracted_data_es$d_climate$frost_days, ylab="Frost days", xlab="Time", typ="l", lwd=1.2, col="grey", cex.lab=1.5, cex.axis=1.3)
      plot(extracted_data_es$d_climate$co2, ylab="CO2", xlab="Time", typ="l", lwd=1.2, col="black", cex.lab=1.5, cex.axis=1.3)
    })
    
    output$table1 <- DT::renderDT({
      DT::datatable(extracted_data$d_site, options = list(paging = FALSE, searching = FALSE))
    })
    output$table2 <- DT::renderDT({
      DT::datatable(extracted_data$d_species, options = list(paging = FALSE,searching = FALSE))
    })
    output$table3 <- DT::renderDT({
      DT::datatable(extracted_data$d_thinning, options = list(paging = FALSE, searching = FALSE))
    })
    
    output$download <- downloadHandler(
      filename = function() {
        "processed_data.xlsx"
      },
      content = function(file) {
        wb <- createWorkbook()
        
        # Add a sheet for d_climate
        addWorksheet(wb, "Climate")
        writeData(wb, sheet = "Climate", extracted_data$d_climate)

        # Add a sheet for d_site_full
        addWorksheet(wb, "Site")
        writeData(wb, sheet = "Site", extracted_data$d_site_full)
      
        # Add a sheet for d_species_full
        addWorksheet(wb, "Species")
        writeData(wb, sheet = "Species", extracted_data$d_species_full)
        
        # Add a sheet for d_thinning_full
        addWorksheet(wb, "Thinning")
        writeData(wb, sheet = "Thinning", extracted_data$d_thinning_full)
        
        # Add a sheet for d_site
        addWorksheet(wb, "d_site")
        writeData(wb, sheet = "d_site", extracted_data$d_site)
        
        # Add a sheet for d_species
        addWorksheet(wb, "d_species")
        writeData(wb, sheet = "d_species", extracted_data$d_species)
        
        # Add a sheet for d_thinning
        addWorksheet(wb, "d_thinning")
        writeData(wb, sheet = "d_thinning", extracted_data$d_thinning)
        
        # Save the workbook to the specified file
        saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    
    
    callModule(explore_server, id = "explore", extracted_data = extracted_data_es)
    extracted_data$d_climate <- extracted_data_es$d_climate
    extracted_data$d_site <- extracted_data_es$d_site
    extracted_data$d_site_full <- extracted_data_es$d_site_full
    extracted_data$d_species <- extracted_data_es$d_species
    extracted_data$d_species_full <- extracted_data_es$d_species_full
    extracted_data$d_thinning <- extracted_data_es$d_thinning
    extracted_data$d_thinning_full <- extracted_data_es$d_thinning_full
    extracted_data$d_parameters <- extracted_data_es$d_parameters
    extracted_data$d_sizeDist <- extracted_data_es$d_sizeDist
  })
  

  validate_inputs <- function(climate, site, species, thinning, parameters, sizeDist) {
    missing_params <- c()
    
    if (any(is.na(climate))) missing_params <- c(missing_params, "d_climate")
    if (any(is.na(site))) missing_params <- c(missing_params, "d_site")
    if (any(is.na(species))) missing_params <- c(missing_params, "d_species")
    if (any(is.na(thinning))) missing_params <- c(missing_params, "d_thinning")
    if (any(is.na(parameters))) missing_params <- c(missing_params, "d_parameters")
    if (any(is.na(sizeDist))) missing_params <- c(missing_params, "d_sizeDist")
    
    if (length(missing_params) > 0) {
      return(paste("Missing values detected in the following parameters: ", paste(missing_params, collapse = ", "), ". Please fill in the missing values before running the model."))
    } else {
      return(NULL)
    }
  }  
    
 
  
    observeEvent(input$run_model_r3pg, {
      
      
      req(input$carbonModel)
      req(input$limitation)
      req(extracted_data)
      soil_m <- as.numeric(switch(input$carbonModel,
                       "RothC" = 3,
                       "ICBM" = 1,
                       "YASSO" = 2))
      
      nut_l <- as.numeric(ifelse(input$limitation == "No - Limitation",1, 0))
      d_climate <- extracted_data$d_climate
      d_site <- extracted_data$d_site
      d_species <- extracted_data$d_species 
      d_thinning <- extracted_data$d_thinning
      d_parameters <- extracted_data$d_parameters
      d_sizeDist <- extracted_data$d_sizeDit 
      validation_error <- validate_inputs(d_climate, d_site, d_species, d_thinning, d_parameters, d_sizeDist)
      if (!is.null(validation_error)) {
        # Display the error message in the UI using showNotification
        showNotification(
          validation_error,
          type = "error",
          duration = NULL,
          closeButton = TRUE
        )
      } else {
      
      ss_e<-c("Not-known","Acer campestre","Acer monspessulanum","Acer opalus","Acer platanoides","Acer pseudoplatanus","Alnus cordata","Alnus glutinosa","Alnus incana","Alnus viridis","Betula pendula","Betula pubescens","Buxus sempervirens","Carpinus betulus","Carpinus orientalis","Castanea sativa","Corylus avellana","Eucalyptus sp.","Fagus moesiaca","Fagus orientalis","Fagus sylvatica","Fraxinus angustifolia","Fraxinus excelsior","Fraxinus ornus","Ilex aquifolium","Juglans nigra","Juglans regia","Malus domestica","Olea europaea","Ostrya carpinifolia","Platanus orientalis","Populus alba","Populus x canescens","Populus hybrides","Populus nigra","Populus tremula","Prunus avium","Prunus dulcis","Prunus padus","Prunus serotina","Pyrus communis","Quercus cerris","Quercus coccifera","Quercus faginea","Quercus frainetto","Quercus fruticosa","Quercus ilex","Quercus macrolepis","Quercus petraea","Quercus pubescens","Quercus pyrenaica","Quercus robur","Quercus rotundifolia","Quercus rubra","Quercus suber","Quercus trojana","Robinia pseudoacacia","Salix alba","Salix caprea","Salix cinerea","Salix eleagnos","Salix fragilis","Salix sp.","Sorbus aria","Sorbus aucuparia","Sorbus domestica","Sorbus torminalis","Tamarix africana","Tilia cordata","Tilia platyphyllos","Ulmus glabra","Ulmus laevis","Ulmus minor","Arbutus unedo","Arbutus andrachne","Ceratonia siliqua","Cercis siliquastrum","Erica arborea","Erica scoparia","Erica manipuliflora","Laurus nobilis","Myrtus communis","Phillyrea latifolia","Phillyrea angustifolia","Pistacia lentiscus","Pistacia terebinthus","Rhamnus oleoides","Rhamnus alaternus","Betula tortuosa","Crataegus monogyna","Ilex canariensis","Laurus canariensis","Myrica faya","Central Anatolian oaks","Quercus petraea_or_robur","Other broadleaves","Abies alba","Abies borisii-regis","Abies cephalonica","Abies grandis","Abies nordmanniana","Abies pinsapo","Abies procera","Cedrus atlantica","Cedrus deodara","Cupressus lusitanica","Cupressus sempervirens","Juniperus communis","Juniperus oxycedrus","Juniperus phoenicea","Juniperus sabina","Juniperus thurifera","Larix decidua","Larix kaempferi","Picea abies","Picea omorika","Picea sitchensis","Pinus brutia","Pinus canariensis","Pinus cembra","Pinus contorta","Pinus halepensis","Pinus heldreichii","Pinus mugo","Pinus nigra","Pinus pinaster","Pinus pinea","Pinus radiata","Pinus strobus","Pinus sylvestris","Pinus uncinata","Pseudotsuga menziesii","Taxus baccata","Thuya sp.","Tsuga sp.","Chamaecyparis lawsoniana","Cedrus brevifolia","Abies cilicica","Cedrus libani","Juniperus excelsa","Juniperus foetidissima","Picea orientalis","Abies amabilis","Abies concolor","Abies veitchii","Larix eurolepis","Picea glauca","Picea pungens","Pinus banksiana","Pinus peuce","Pinus rigida","Pinus wallichiana","Araucaria araucana","Calocedrus decurrens","Cryptomeria japonica","Metasequoia glyptostroboides","Sequoiadendron giganteum","Tamarix ramosissima","Taxodium distichum","Tsuga canadensis","Larix sp.","Abies sp.","Other conifers","Quercus hartwissiana","Quercus vulcanica","Quercus infectoria","Quercus macranthera","Quercus libani","Quercus brantii","Quercus ithaburensis","Quercus aucheri  ","Quercus pontica","Tilia sp.","Populus sp.","Betula sp.","Ulmus sp.","Betula x hybrida","Acer sp.","Alnus sp.","Crataegus sp.","Malus sylvestris","Aesculus hippocastanum","Acer negundo","Acer obtusatum","Acer saccharinum","Acer tataricum","Fraxinus americana","Fraxinus pennsylvanica","Laburnum alpinum","Laburnum anagyroides","Populus balsamifera","Populus deltoides","Prunus cerasifera","Prunus domestica","Prunus laurocerasus","Prunus mahaleb","Prunus spinosa","Quercus palustris","Quercus pedunculiflora","Salix purpurea","Salix triandra","Salix viminalis","Ailanthus altissima","Broussonetia papyrifera","Carya ovata","Corylus colurna","Crataegus laevigata","Liriodendron tulipifera","Mespilus germanica","Paulownia tomentosa","Petteria ramentacea","Phillyrea media","Platanus xacerifolia","Pyrus pyraster","Sorbus austriaca","Tilia tomentosa","All species")
      family_ss_e<-c("Unknown", "Sapindaceae", "Sapindaceae", "Sapindaceae", "Sapindaceae", "Sapindaceae", "Betulaceae", "Betulaceae", "Betulaceae", "Betulaceae", "Betulaceae", "Betulaceae", "Buxaceae", "Betulaceae", "Betulaceae", "Fagaceae", "Betulaceae", "Myrtaceae", "Fagaceae", "Fagaceae", "Fagaceae", "Oleaceae", "Oleaceae", "Oleaceae", "Aquifoliaceae", "Juglandaceae", "Juglandaceae", "Rosaceae", "Oleaceae", "Betulaceae", "Platanaceae", "Salicaceae", "Salicaceae", "Salicaceae", "Salicaceae", "Salicaceae", "Rosaceae", "Rosaceae", "Rosaceae", "Rosaceae", "Rosaceae", "Rosaceae", "Fagaceae", "Rhamnaceae", "Fagaceae", "Fagaceae", "Fagaceae", "Fagaceae", "Fagaceae", "Fagaceae", "Fagaceae", "Fagaceae", "Fagaceae", "Fagaceae", "Fagaceae", "Fagaceae", "Fagaceae", "Fagaceae", "Fabaceae", "Salicaceae", "Salicaceae", "Salicaceae", "Salicaceae", "Salicaceae", "Rosaceae", "Rosaceae", "Rosaceae", "Rosaceae", "Tamaricaceae", "Malvaceae", "Tiliaceae", "Ulmaceae", "Ulmaceae", "Ulmaceae", "Ericaceae", "Ericaceae", "Fabaceae", "Fabaceae", "Ericaceae", "Lauraceae", "Myrtaceae", "Oleaceae", "Oleaceae", "Anacardiaceae", "Anacardiaceae", "Rhamnaceae", "Rhamnaceae", "Betulaceae", "Rosaceae", "Aquifoliaceae", "Lauraceae", "Myricaceae", "Fagaceae", "Fagaceae", "Other", "Pinaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Cupressaceae", "Cupressaceae", "Cupressaceae", "Cupressaceae", "Cupressaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Taxaceae", "Cupressaceae", "Pinaceae", "Cupressaceae", "Pinaceae", "Pinaceae", "Cupressaceae", "Cupressaceae", "Cupressaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Araucariaceae", "Cupressaceae", "Cupressaceae", "Cupressaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Pinaceae", "Fagaceae", "Fagaceae", "Fagaceae", "Fagaceae", "Fagaceae", "Fagaceae", "Fagaceae", "Fagaceae", "Tiliaceae", "Salicaceae", "Betulaceae", "Ulmaceae", "Betulaceae", "Aceraceae", "Betulaceae", "Rosaceae", "Rosaceae", "Hippocastanaceae", "Aceraceae", "Aceraceae", "Oleaceae", "Oleaceae", "Fabaceae", "Salicaceae", "Salicaceae", "Salicaceae", "Simaroubaceae", "Moraceae", "Juglandaceae", "Betulaceae", "Rosaceae", "Magnoliaceae", "Rosaceae", "Rosaceae", "Rosaceae", "Rosaceae", "Rosaceae", "Fagaceae", "Fagaceae", "Salicaceae", "Salicaceae", "Salicaceae", "Simaroubaceae", "Moraceae", "Juglandaceae", "Betulaceae", "Rosaceae", "Magnoliaceae", "Rosaceae", "Rosaceae", "Rosaceae", "Rosaceae", "Rosaceae", "Fagaceae", "Fagaceae", "Tiliaceae", "All families")
      
      sp <- d_species$species
      ss_e_families <- as.data.frame(ss_e)
      ss_e_families[,2]<- family_ss_e
      colss <- c("Eucalyptus globulus",	"Abies alba",	"Alnus glutinosa",	"Betula pendula",	"Carpinus betulus",	"Castanea sativa",	"Fagus sylvatica",	"Fagus sylvatica2",	"Fraxinus excelsior",	"Larix decidua",	"Acer pseudoplatanus",	"Pinus pinaster",	"Quercus pubescence",	"Picea abies",	"Picea abies2",	"Pinus sylvestris",	"Pinus sylvestris2",	"Populus tremula",	"Pseudotsuga menzisii")
      if (sp %in% colss) {sp_re <- sp
      }else{
        sx_re<-ss_e_families$V2[ss_e_families$ss_e == sp]
        colss_re <- c("Fagus sylvatica", "Pinus sylvestris", "Hardwoods", "Acer platanoides", "Populus tremula","Eucalyptus globulus", "Betulaceae","Oleaceae")
        colss_type <- c("Fagaceae","Pinaceae","Hardwoods","Sapindaceae", "Salicaceae", "Myrtaceae","Betula pendula","Fraxinus excelsior")
        sp_re <- colss_re[colss_type==sx_re]
        d_species$species <- sp_re
        d_thinning$species <- sp_re
      }

      model_outputs_es <-NA
 
      model_outputs_es <- run_3PG(
        site = d_site,
        species = d_species,
        climate = d_climate,
        thinning = d_thinning,
        parameters = d_parameters,
        size_dist = d_sizeDist,
        settings = list(light_model = 2, transp_model = 2, phys_model = 2,
                        calculate_d13c = 0, soil_model = soil_m, nut_lim = nut_l),
        check_input = TRUE, df_out = FALSE)

      callModule(model_server, id = "model", model_outputs = model_outputs_es)
      
      model_outputs$climate <- (model_outputs_es[,,1,])
      model_outputs$stand <- model_outputs_es[,,2,]
      model_outputs$canopy <- model_outputs_es[,,3,]
      model_outputs$stocks <- model_outputs_es[,,4,]
      model_outputs$modifiers <- model_outputs_es[,,5,]
      model_outputs$production <- model_outputs_es[,,6,]
      model_outputs$water_use <- model_outputs_es[,,7,]
      model_outputs$mortality_management <- model_outputs_es[,,8,]
      model_outputs$wood_delta <- model_outputs_es[,,9,]
      model_outputs$weibull <- model_outputs_es[,,10,]
      model_outputs$soil <- (model_outputs_es[,,11,])
      }
      
      
    })
    
    observeEvent(input$output_climate, {
      req(model_outputs$climate)
      output$output_plot <- renderPlot({
        isolate({
          par(mfrow=c(3,4))
          plot(model_outputs$climate[,1],main='Tmp_min',xlab='Time', ylab='C',typ='l',lwd=2)
          plot(model_outputs$climate[,2],main='Tmp_max',xlab='Time', ylab='C',typ='l',lwd=2)
          plot(model_outputs$climate[,3],main='Tmp_avg',xlab='Time', ylab='C',typ='l',lwd=2)
          plot(model_outputs$climate[,4],main='forst_days',xlab='Time', ylab='d month-1',typ='l',lwd=2)
          plot(model_outputs$climate[,5],main='solar_rad',xlab='Time', ylab='MJ m-2 d-1',typ='l',lwd=2)
          plot(model_outputs$climate[,6],main='day_length',xlab='Time', ylab=' s d-1',typ='l',lwd=2)
          plot(model_outputs$climate[,7],main='prcp',xlab='Time', ylab=' mm month-1',typ='l',lwd=2)
          plot(model_outputs$climate[,8],main='vpd_day',xlab='Time', ylab=' mbar',typ='l',lwd=2)
          plot(model_outputs$climate[,9],main='co2',xlab='Time', ylab='ppm',typ='l',lwd=2)
          plot(model_outputs$climate[,10],main='delta13c',xlab='Time', ylab='mm month-1',typ='l',lwd=2)
        })}) })
    
    observeEvent(input$output_stand, {
      req(model_outputs$stand)
      output$output_plot <- renderPlot({
        isolate({
          par(mfrow=c(3,5))
          plot(model_outputs$stand[,1],main='age',xlab='Time', ylab='years',typ='l',lwd=2)
          plot(model_outputs$stand[,2],main='stems_n',xlab='Time', ylab='trees ha-1',typ='l',lwd=2)
          plot(model_outputs$stand[,3],main='basal_area',xlab='Time', ylab='m2 ha-1',typ='l',lwd=2)
          plot(model_outputs$stand[,4],main='basal_area_prop',xlab='Time', ylab='%',typ='l',lwd=2)
          plot(model_outputs$stand[,5],main='dbh',xlab='Time', ylab='cm',typ='l',lwd=2)
          plot(model_outputs$stand[,6],main='height',xlab='Time', ylab='m',typ='l',lwd=2)
          plot(model_outputs$stand[,7],main='height_rel',xlab='Time', ylab='',typ='l',lwd=2)
          plot(model_outputs$stand[,8],main='crown_length',xlab='Time', ylab='m',typ='l',lwd=2)
          plot(model_outputs$stand[,9],main='crown_width',xlab='Time', ylab='m',typ='l',lwd=2)
          plot(model_outputs$stand[,10],main='volume',xlab='Time', ylab='m3 ha-1',typ='l',lwd=2)
          plot(model_outputs$stand[,11],main='volume_mai',xlab='Time', ylab='m3 ha-1 yr-1',typ='l',lwd=2)
          plot(model_outputs$stand[,12],main='volume_change',xlab='Time', ylab='m3 ha-1',typ='l',lwd=2)
          plot(model_outputs$stand[,13],main='volume_cum',xlab='Time', ylab='m3 ha-1',typ='l',lwd=2)
          plot(model_outputs$stand[,14],main='harvesting',xlab='Time', ylab='m3 ha-1',typ='l',lwd=2)
          plot(model_outputs$stand[,15],main='residues',xlab='Time', ylab='m3 ha-1',typ='l',lwd=2)
        })}) })
          
    observeEvent(input$output_canopy, {
      req(model_outputs$canopy)
      output$output_plot <- renderPlot({
        isolate({
          par(mfrow=c(3,4))
          plot(model_outputs$canopy[,1],main='layer_id',xlab='Time', ylab='',typ='l',lwd=2)
          plot(model_outputs$canopy[,2],main='sla',xlab='Time', ylab='m2 kg-1',typ='l',lwd=2)
          plot(model_outputs$canopy[,3],main='lai',xlab='Time', ylab='m2 m-2',typ='l',lwd=2)
          plot(model_outputs$canopy[,4],main='lai_above',xlab='Time', ylab='m2 m-2',typ='l',lwd=2)
          plot(model_outputs$canopy[,5],main='lai_sa_ratio',xlab='Time', ylab='',typ='l',lwd=2)
          plot(model_outputs$canopy[,6],main='canopy_vol_frac',xlab='Time', ylab='proportion',typ='l',lwd=2)
          plot(model_outputs$canopy[,7],main='canopy_cover',xlab='Time', ylab='',typ='l',lwd=2)
          plot(model_outputs$canopy[,8],main='lambda_v',xlab='Time', ylab='',typ='l',lwd=2)
          plot(model_outputs$canopy[,9],main='lambda_h',xlab='Time', ylab='',typ='l',lwd=2)
          plot(model_outputs$canopy[,10],main='aero_resist',xlab='Time', ylab='s m-1',typ='l',lwd=2)
          plot(model_outputs$canopy[,11],main='vpd_sp',xlab='Time', ylab='mbar',typ='l',lwd=2)})}) })
         
    observeEvent(input$output_stocks, {
      req(model_outputs$stocks)
      output$output_plot <- renderPlot({
        isolate({
          par(mfrow=c(3,4))
          plot(model_outputs$stocks[,1],main='biom_stem',xlab='Time', ylab='tDM ha-1',typ='l',lwd=2)
          plot(model_outputs$stocks[,2],main='biom_root',xlab='Time', ylab='tDM ha-1',typ='l',lwd=2)
          plot(model_outputs$stocks[,3],main='biom_foliage',xlab='Time', ylab='tDM ha-1',typ='l',lwd=2)
          plot(model_outputs$stocks[,4],main='biom_tree',xlab='Time', ylab='kgDM/tree',typ='l',lwd=2)
          plot(model_outputs$stocks[,5],main='wood_density',xlab='Time', ylab='tDM m-3',typ='l',lwd=2)
          plot(model_outputs$stocks[,6],main='fracBB',xlab='Time', ylab='',typ='l',lwd=2)
          plot(model_outputs$stocks[,7],main='biom_loss_foliage',xlab='Time', ylab='tDM ha-1',typ='l',lwd=2)
          plot(model_outputs$stocks[,8],main='biom_loss_root',xlab='Time', ylab='tDM ha-1',typ='l',lwd=2)
          plot(model_outputs$stocks[,9],main='biom_incr_foliage',xlab='Time', ylab='tDM ha-1',typ='l',lwd=2)
          plot(model_outputs$stocks[,10],main='biom_incr_root',xlab='Time', ylab='tDM ha-1',typ='l',lwd=2)
          plot(model_outputs$stocks[,11],main='biom_incr_stem',xlab='Time', ylab='tDM ha-1',typ='l',lwd=2)})}) })
    
    observeEvent(input$output_modifiers, {
      req(model_outputs$modifiers)
      output$output_plot <- renderPlot({
        isolate({
          par(mfrow=c(3,4))
          plot(model_outputs$modifiers[,1],main='f_age',xlab='Time', ylab='',typ='l',lwd=2)
          plot(model_outputs$modifiers[,2],main='f_vpd',xlab='Time', ylab='',typ='l',lwd=2)
          plot(model_outputs$modifiers[,3],main='f_tmp',xlab='Time', ylab='',typ='l',lwd=2)
          plot(model_outputs$modifiers[,4],main='f_tmp_gc',xlab='Time', ylab='',typ='l',lwd=2)
          plot(model_outputs$modifiers[,5],main='f_frost',xlab='Time', ylab='',typ='l',lwd=2)
          plot(model_outputs$modifiers[,6],main='f_sw',xlab='Time', ylab='',typ='l',lwd=2)
          plot(model_outputs$modifiers[,7],main='f_nutr',xlab='Time', ylab='',typ='l',lwd=2)
          plot(model_outputs$modifiers[,8],main='f_calpha',xlab='Time', ylab='',typ='l',lwd=2)
          plot(model_outputs$modifiers[,9],main='f_cg',xlab='Time', ylab='',typ='l',lwd=2)
          plot(model_outputs$modifiers[,10],main='f_phys',xlab='Time', ylab='',typ='l',lwd=2)
          plot(model_outputs$modifiers[,11],main='gammaF',xlab='Time', ylab='',typ='l',lwd=2)
          plot(model_outputs$modifiers[,12],main='f_transp_scale',xlab='Time', ylab='',typ='l',lwd=2)})}) })
    
    observeEvent(input$output_production, {
      req(model_outputs$production)
      output$output_plot <- renderPlot({
        isolate({
          par(mfrow=c(3,4))
          plot(model_outputs$production[,1],main='gpp',xlab='Time', ylab='tDM ha-1',typ='l',lwd=2)
          plot(model_outputs$production[,2],main='npp',xlab='Time', ylab='tDM ha-1',typ='l',lwd=2)
          plot(model_outputs$production[,3],main='apar',xlab='Time', ylab='MJ m-2 month-1',typ='l',lwd=2)
          plot(model_outputs$production[,4],main='fi',xlab='Time', ylab='',typ='l',lwd=2)
          plot(model_outputs$production[,5],main='alpha_c',xlab='Time', ylab='mol mol-1',typ='l',lwd=2)
          plot(model_outputs$production[,6],main='epsilon_gpp',xlab='Time', ylab='gDM MJ-1',typ='l',lwd=2)
          plot(model_outputs$production[,7],main='epsilon_npp',xlab='Time', ylab='gDM MJ-1',typ='l',lwd=2)
          plot(model_outputs$production[,8],main='npp_frac_stem',xlab='Time', ylab='gDM MJ-1',typ='l',lwd=2)
          plot(model_outputs$production[,9],main='npp_frac_foliage',xlab='Time', ylab='',typ='l',lwd=2)
          plot(model_outputs$production[,10],main='npp_frac_root',xlab='Time', ylab='',typ='l',lwd=2)
          plot(model_outputs$production[,11],main='pFS',xlab='Time', ylab='',typ='l',lwd=2)
          plot(model_outputs$production[,12],main='biom_foliage_debt',xlab='Time', ylab='tDM ha-1',typ='l',lwd=2)})}) })
    
    
    observeEvent(input$output_wateruse, {
      req(model_outputs$water_use)
      output$output_plot <- renderPlot({
        isolate({
          par(mfrow=c(3,5))
          plot(model_outputs$water_use[,1],main='conduct_canopy',xlab='Time', ylab='m s-1',typ='l',lwd=2)
          plot(model_outputs$water_use[,2],main='conduct_soil',xlab='Time', ylab='m s-1',typ='l',lwd=2)
          plot(model_outputs$water_use[,3],main='evapotra_soil',xlab='Time', ylab='mm (= kg per m2)',typ='l',lwd=2)
          plot(model_outputs$water_use[,4],main='prcp_interc',xlab='Time', ylab='mm',typ='l',lwd=2)
          plot(model_outputs$water_use[,5],main='prcp_interc_fract',xlab='Time', ylab='',typ='l',lwd=2)
          plot(model_outputs$water_use[,6],main='prcp_runoff',xlab='Time', ylab='  mm',typ='l',lwd=2)
          plot(model_outputs$water_use[,7],main='irrig_supl',xlab='Time', ylab='mm',typ='l',lwd=2)
          plot(model_outputs$water_use[,8],main='wue',xlab='Time', ylab='gDM mm-1',typ='l',lwd=2)
          plot(model_outputs$water_use[,9],main='wue_transp',xlab='Time', ylab='gDM mm-1',typ='l',lwd=2)
          plot(model_outputs$water_use[,10],main='evapo_transp',xlab='Time', ylab='mm (= kg per m2',typ='l',lwd=2)
          plot(model_outputs$water_use[,11],main='transp_veg',xlab='Time', ylab='mm (= kg per m2',typ='l',lwd=2)
          plot(model_outputs$water_use[,12],main='asw',xlab='Time', ylab='mm',typ='l',lwd=2)
          plot(model_outputs$water_use[,13],main='water_runoff_polled',xlab='Time', ylab='mm',typ='l',lwd=2)})})})
    
    observeEvent(input$output_mortality, {
      req(model_outputs$mortality_management)
      output$output_plot <- renderPlot({
        isolate({
          par(mfrow=c(3,5))
          plot(model_outputs$mortality_management[,1],main='wSmax',xlab='Time', ylab='kg tree-1',typ='l',lwd=2)
          plot(model_outputs$mortality_management[,2],main='gammaN',xlab='Time', ylab='month-1',typ='l',lwd=2)
          plot(model_outputs$mortality_management[,3],main='Self thinning',xlab='Time', ylab='trees ha-1',typ='l',lwd=2)
          plot(model_outputs$mortality_management[,4],main='Stem mortality',xlab='Time', ylab='trees ha-1',typ='l',lwd=2)
          plot(model_outputs$mortality_management[,5],main='',xlab='Time', ylab='',typ='l',lwd=2)
          plot(model_outputs$mortality_management[,6],main='Latest extracted volume',xlab='Time', ylab='m3 ha-1',typ='l',lwd=2)
          plot(model_outputs$mortality_management[,7],main='',xlab='Time', ylab='',typ='l',lwd=2)
          plot(model_outputs$mortality_management[,8],main='CropTrees per ha',xlab='Time', ylab='trees ha-1',typ='l',lwd=2)
          plot(model_outputs$mortality_management[,9],main='CropTree Mean DBH',xlab='Time', ylab='cm',typ='l',lwd=2)
          plot(model_outputs$mortality_management[,10],main='CropTree Basal area',xlab='Time', ylab='m2 ha-1',typ='l',lwd=2)
          plot(model_outputs$mortality_management[,11],main='CropTree volume',xlab='Time', ylab='m2 ha-1',typ='l',lwd=2)
          plot(model_outputs$mortality_management[,12],main='CropTree Height',xlab='Time', ylab='m',typ='l',lwd=2)
          plot(model_outputs$mortality_management[,13],main='CropTree Stem DM',xlab='Time', ylab='tDM ha-1',typ='l',lwd=2)})}) })
   
     observeEvent(input$output_wooddelta, {
      req(model_outputs$wood_delta)
      output$output_plot <- renderPlot({
        isolate({
          par(mfrow=c(3,5))
          plot(model_outputs$wood_delta[,1],main='Gc_mol',xlab='Time', ylab='mol/m2 s',typ='l',lwd=2)
          plot(model_outputs$wood_delta[,2],main='Gw_mol',xlab='Time', ylab='mol/m2 s',typ='l',lwd=2)
          plot(model_outputs$wood_delta[,3],main='D13CNewPS',xlab='Time', ylab='',typ='l',lwd=2)
          plot(model_outputs$wood_delta[,4],main='D13CTissue',xlab='Time', ylab='',typ='l',lwd=2)
          plot(model_outputs$wood_delta[,5],main='InterCi',xlab='Time', ylab='ppm',typ='l',lwd=2)})}) })
     
    observeEvent(input$output_weibull, {
      req(model_outputs$weibull)
      output$output_plot <- renderPlot({
        isolate({
          par(mfrow=c(3,5))
          plot(model_outputs$weibull[,1],main='Dweibullscale',xlab='Time', ylab='',typ='l',lwd=2)
          plot(model_outputs$weibull[,2],main='Dweibullshape',xlab='Time', ylab='',typ='l',lwd=2)
          plot(model_outputs$weibull[,3],main='Dweibulllocation',xlab='Time', ylab='',typ='l',lwd=2)
          plot(model_outputs$weibull[,4],main='wsweibullscale',xlab='Time', ylab='',typ='l',lwd=2)
          plot(model_outputs$weibull[,5],main='wsweibullshape',xlab='Time', ylab='',typ='l',lwd=2)
          plot(model_outputs$weibull[,6],main='wsweibulllocation',xlab='Time', ylab='',typ='l',lwd=2)
          plot(model_outputs$weibull[,7],main='CVdbhDistribution',xlab='Time', ylab='proportion',typ='l',lwd=2)
          plot(model_outputs$weibull[,8],main='CVwsDistribution',xlab='Time', ylab='proportion',typ='l',lwd=2)
          plot(model_outputs$weibull[,9],main='wsrelBias',xlab='Time', ylab='proportion',typ='l',lwd=2)
          plot(model_outputs$weibull[,10],main='DrelBiaspFS',xlab='Time', ylab='proportion',typ='l',lwd=2)
          plot(model_outputs$weibull[,11],main='DrelBiasheight',xlab='Time', ylab='proportion',typ='l',lwd=2)
          plot(model_outputs$weibull[,12],main='DrelBiasBasArea',xlab='Time', ylab='proportion',typ='l',lwd=2)
          plot(model_outputs$weibull[,13],main='DrelBiasLCL',xlab='Time', ylab='proportion',typ='l',lwd=2)
          plot(model_outputs$weibull[,14],main='DrelBiasCrowndiameter',xlab='Time', ylab='proportion',typ='l',lwd=2)})}) })
    
    observeEvent(input$output_soil, {
      req(model_outputs$soil)

    output$output_plot <- renderPlot({
      isolate({
        soil_m <- as.numeric(switch(input$carbonModel,
                                    "RothC" = 3,
                                    "ICBM" = 1,
                                    "YASSO" = 2))
        if(soil_m == 3){
          par(mfrow=c(3,5))
          plot(model_outputs$soil[,1],main='Labile carbon pool',xlab='Time', ylab='t ha-1',typ='l',lwd=2)
          plot(model_outputs$soil[,2],main='Recalcitrant carbon pool',xlab='Time', ylab='t ha-1',typ='l',lwd=2)
          plot(model_outputs$soil[,3],main='Mineralized N',xlab='Time', ylab='t ha-1',typ='l',lwd=2)
          plot(model_outputs$soil[,4],main='Immobilized N',xlab='Time', ylab='t ha-1',typ="l")
          plot(model_outputs$soil[,5],main='Mineralized P',xlab='Time', ylab='t ha-1',typ='l',lwd=2)
          plot(model_outputs$soil[,6],main='Immobilized P',xlab='Time', ylab='t ha-1',typ='l',lwd=2)
          plot(model_outputs$soil[,7],main='Outgassed N',xlab='Time', ylab='t ha-1',typ='l',lwd=2)
          plot(model_outputs$soil[,8],main='Leached N',xlab='Time', ylab='t ha-1',typ='l',lwd=2)
          plot(model_outputs$soil[,9],main='Leached P',xlab='Time', ylab='t ha-1',typ='l',lwd=2)
          plot(model_outputs$soil[,10],main='Total soil respiration',xlab='Time', ylab='t ha-1',typ='l',lwd=2)
          plot(model_outputs$soil[,11],main='Total Carbon',xlab='Time', ylab='t ha-1',typ='l',lwd=2)
          plot(model_outputs$soil[,12],main='Total Nitrogen',xlab='Time', ylab='t ha-1',typ='l',lwd=2)
          plot(model_outputs$soil[,13],main='Total Phosphorus',xlab='Time', ylab='t ha-1',typ='l',lwd=2)
          plot(model_outputs$soil[,14],main='Inorganic Nitrogen',xlab='Time', ylab='t ha-1',typ='l',lwd=2)
          plot(model_outputs$soil[,15],main='Inogranic P',xlab='Time', ylab='t ha-1',typ='l',lwd=2)}
        if(soil_m == 2){
          par(mfrow=c(3,4))
          plot(model_outputs$soil[,1],main='Yl_C',xlab='Time', ylab='t ha-1',typ='l',lwd=2)
          plot(model_outputs$soil[,2],main='Yr_C',xlab='Time', ylab='t ha-1',typ='l',lwd=2)
          plot(model_outputs$soil[,3],main='O_C',xlab='Time', ylab='t ha-1',typ='l',lwd=2)
          plot(model_outputs$soil[,4],main='O_N',xlab='Time', ylab='t ha-1',typ="l")
          plot(model_outputs$soil[,5],main='Litter_emissions',xlab='Time', ylab='t ha-1',typ='l',lwd=2)
          plot(model_outputs$soil[,6],main='Humus_Emissions',xlab='Time', ylab='t ha-1',typ='l',lwd=2)
          plot(model_outputs$soil[,7],main='TotalCarbo',xlab='Time', ylab='t ha-1',typ='l',lwd=2)
          plot(model_outputs$soil[,8],main='TotalNitro',xlab='Time', ylab='t ha-1',typ='l',lwd=2)
          plot(model_outputs$soil[,9],main='Nav',xlab='Time', ylab='t ha-1',typ='l',lwd=2)
          plot(model_outputs$soil[,10],main='Un',xlab='Time', ylab='t ha-1',typ='l',lwd=2)}
        if(soil_m == 1){
          par(mfrow=c(3,4))
          plot(model_outputs$soil[,1],main='Yl_C',xlab='Time', ylab='t ha-1',typ='l',lwd=2)
          plot(model_outputs$soil[,2],main='Yr_C',xlab='Time', ylab='t ha-1',typ='l',lwd=2)
          plot(model_outputs$soil[,3],main='O_C',xlab='Time', ylab='t ha-1',typ='l',lwd=2)
          plot(model_outputs$soil[,4],main='O_N',xlab='Time', ylab='t ha-1',typ="l")
          plot(model_outputs$soil[,5],main='Litter_emissions',xlab='Time', ylab='t ha-1',typ='l',lwd=2)
          plot(model_outputs$soil[,6],main='Humus_Emissions',xlab='Time', ylab='t ha-1',typ='l',lwd=2)
          plot(model_outputs$soil[,7],main='TotalCarbo',xlab='Time', ylab='t ha-1',typ='l',lwd=2)
          plot(model_outputs$soil[,8],main='TotalNitro',xlab='Time', ylab='t ha-1',typ='l',lwd=2)
          plot(model_outputs$soil[,9],main='Nav',xlab='Time', ylab='t ha-1',typ='l',lwd=2)
          plot(model_outputs$soil[,10],main='Un',xlab='Time', ylab='t ha-1',typ='l',lwd=2)}
      })
    })})
    
  observeEvent(input$back_home, {
    state("home")
  })
  
  observeEvent(input$upload_isric, {
    state("isric")
  })
  
  
  observeEvent(input$upload_icp, {
    state("icp")
  })
  
  observeEvent(input$upload_chelsa, {
    state("chelsa")
  })
  
  observeEvent(input$explore_data, {
    state("explore")
    reset_outputs() 
    output$map <- renderLeaflet({
      reset_outputs()  
      leaflet<-NULL
      leaflet() %>%
        addProviderTiles("OpenStreetMap.Mapnik") %>%
        setView(lng = 14, lat = 50, zoom = 3.2) %>%
        addCircleMarkers(
          data = points,
          lng = ~lon,
          lat = ~lat,
          radius = 5,
          color = "black",
          labelOptions = labelOptions(noHide = TRUE),
          layerId = ~id
        )
    })
    })
  
  observeEvent(input$run_model, {    
    state("model")
  })
  
  observeEvent(input$documentation, {
    state("documentation")
  })
  
  output$page <- renderUI({
    switch(state(),
           "home" = home_ui,
           "isric" = isric_ui,
           "icp" = icp_ui,
           "chelsa" = chelsa_ui,
           "explore" = explore_ui,
           "model" = model_ui,
           "documentation" = documentation_ui)
  })
  
  output$state <- renderText(state())
  
  
  callModule(isric_server, id= "isric", isric_soil_data = isric_soil_data)
  callModule(icp_server, id = "icp", icp_data = icp_data)
  callModule(chelsa_server, id = "chelsa", chelsa_clim = chelsa_clim) 
  callModule(explore_server, id = "explore", extracted_data = extracted_data) 
  callModule(model_server, id = "model", model_outputs = model_outputs) 

}

shinyApp(ui = ui, server = server)
