#' Prepare the ICP climate and soil database
#' This function reads the ISRIC soil data from ICP files that should be provided and processes the data to create processed dataframes for climate, soil, stand and thinning data from ICP database.
#' @param ICP_temperature: Temperature parameters from ICP
#' @param ICP_precipitation: Precipitation parameter from ICP
#' @param ICP_radation: Radiation parameter from ICP
#' @param country_names: Country names
#' @param country_codes: Country codes
#' @param country_lat_lon: Country lat and lon
#' @param chelsa_clim: Climate parameters from CHELSA
#' @param soil_data: ICP soil data
#' @param soil_som_data:  ICP soil SOM data
#' @param soil_ssm_data: ICP soil SOM data
#' @param stand_data: ICP Stand data
#' @param ss_age_d_1: ICP stand age data Method 1
#' @param ss_age_d_2: ICP stand age data Method 2
#' @param tree_data_dia: ICP tree diameter Method 1
#' @param thinning_data: ICP thinning data
#' @param inventory_data: ICP tree inventory data
#' @param litter_data: ICP litter data
#' @param frosster_d: Frosster et al. parameters
#'
#' @return A list containing processed dataframes for soil and climate data from ICP database
#'
#' @import data.table
#' @import dplyr
#' @import lubridate
#' @import readxl
#' @import stringr
#' @import utils
#' @import tcltk
#' @examples
#' ICP_climate_soil(icp_climate, chelsa_pr, chelsa_tas, chelsa_tas_min, chelsa_tas_max, chelsa_rad, so_pfh, so_som,ss_ssm, si_sta, gr_ipm, gr_iev, gr_inv, lf_lfm)
#' @export
#'
#'
ICP_climate_soil <- function(icp_climate = NULL, chelsa_pr = NULL, chelsa_tas = NULL, chelsa_tas_min = NULL, chelsa_tas_max = NULL, chelsa_rad = NULL, so_pfh = NULL, so_som = NULL,
                    ss_ssm = NULL, si_sta= NULL, gr_ipm = NULL, gr_iev = NULL, gr_inv = NULL, lf_lfm = NULL) {
library(data.table)
library(dplyr)
library(lubridate)
library(readxl)
library(tidyverse)
library(utils)
library(tcltk)



  # Create a function to display the directory selection dialog
  chooseDirectory <- function() {
    directory <- choose.dir()
    return(directory)
  }

  # Prompt user to select files
  select_files <- function(prompt) {
    file_paths <- tk_choose.files(default = "", caption = prompt, multi = TRUE)
    return(as.character(file_paths))
  }

  country_codes <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,72,80,95,96)
  country_names <- c("FR","BE","NL","DE","IT","UK","IE","DK","GR","PT","ES","LU","SE","AT","FI","CH","HU","RO","PL","SK","NO","LT","HR","CZ","EE","SI","MD","RU","BG","LV","BY","CY","CS","AD","TR","ME","cn","AZ")

  if(is.null(icp_climate)){
  cat("Please select the file path for the ICP climate data files :\n")
  file_path <- chooseDirectory()
  file.list <- list.files(path = file_path, pattern = "*.xlsx", full.names = TRUE)}else{
    file.list = icp_climate
  }
  message("Reading climate data...")
  df.list <- lapply(file.list, read_excel)
  data_names <- c(1994:2018)

  clim_list <- list()
  for (i in 1:length(file.list)){
    clim_list[[i]] <- df.list[[i]]
  }

  clim_tot <- bind_rows(clim_list)

  country_site_num_list <- list()
  for (i in 1:length(country_codes)){
    country_data <- filter(clim_tot, clim_tot$code_country == country_codes[i])
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
        code_country == country_codes[i],
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

  ans <- tclvalue(tkmessageBox(title = "Select Climate Data", message = "Select alternative climate data (CHELSA)?", type = "yesno"))

  chelsa_clim <- list()

    if(is.null(chelsa_pr) || is.null(chelsa_tas) || is.null(chelsa_tas_min) || is.null(chelsa_tas_max) || is.null(chelsa_rad)){
  if (ans == "yes") {
    # Alternative climate data from Chelsa

    cat("Please select the CHELSA data files for the following variables:\n")

    cat("CHELSA monthly Precipitation file\n")
    file_pr <- select_files("CHELSA monthly Precipitation file")

    cat("CHELSA monthly Temperature file\n")
    file_tas <- select_files("CHELSA monthly Temperature file")

    cat("CHELSA monthly min-Temperature file\n")
    file_tas_min <- select_files("CHELSA monthly min-Temperature file")

    cat("CHELSA monthly max-Temperature file\n")
    file_tas_max <- select_files("CHELSA monthly max-Temperature file")

    cat("CHELSA monthly Radiation file\n")
    file_rad <- select_files("CHELSA monthly Radiation file")
  }else{
    file_pr <- chelsa_pr
    file_tas <- chelsa_tas
    file_tas_min <- chelsa_tas_min
    file_tas_max <- chelsa_tas_max
    file_rad <- chelsa_rad
}

    # Load the CHELSA data into data frames
    c_pr <- data.table::fread(file_pr, sep = ",", header = FALSE, data.table = TRUE, stringsAsFactors = FALSE)
    chelsa_pr <- as.data.frame(c_pr[, 2:5])
    colnames(chelsa_pr)[1:4] <- c("LAT", "LON", "DATE", "pr")
    rm(c_pr)
    chelsa_clim[["pr"]] <- chelsa_pr

    c_tas <- data.table::fread(file_tas, sep = ",", header = FALSE, data.table = TRUE, stringsAsFactors = FALSE)
    chelsa_tas <- as.data.frame(c_tas[, 2:5])
    colnames(chelsa_tas)[1:4] <- c("LAT", "LON", "DATE", "tas")
    rm(c_tas)
    chelsa_clim[["tas"]] <- chelsa_tas

    c_tas_min <- data.table::fread(file_tas_min, sep = ",", header = FALSE, data.table = TRUE, stringsAsFactors = FALSE)
    chelsa_tas_min <- as.data.frame(c_tas_min[, 2:5])
    colnames(chelsa_tas_min)[1:4] <- c("LAT", "LON", "DATE", "tas_min")
    rm(c_tas_min)
    chelsa_clim[["tas_min"]] <- chelsa_tas_min

    c_tas_max <- data.table::fread(file_tas_max, sep = ",", header = FALSE, data.table = TRUE, stringsAsFactors = FALSE)
    chelsa_tas_max <- as.data.frame(c_tas_max[, 2:5])
    colnames(chelsa_tas_max)[1:4] <- c("LAT", "LON", "DATE", "tas_min")
    rm(c_tas_max)
    chelsa_clim[["tas_max"]] <- chelsa_tas_max

    c_rad <- data.table::fread(file_rad, sep = ",", header = FALSE, data.table = TRUE, stringsAsFactors = FALSE)
    chelsa_rad <- as.data.frame(c_rad[, 2:5])
    colnames(chelsa_rad)[1:4] <- c("LAT", "LON", "DATE", "rad")
    rm(c_rad)
    chelsa_clim[["sr"]] <- chelsa_rad
  }

  message("Climate Data processed!")

  if(is.null(so_pfh)){
  cat("Select ICP soil data file (so_pfh)\n")
  soil_data_x <- select_files("ICP soil data file (so_pfh)")
  soil_data <- read_delim(soil_data_x, show_col_types = FALSE)}else{
  soil_data <- read_delim(so_pfh, show_col_types = FALSE)
  }

  if(is.null(so_som)){
  cat("Select ICP soil SOM data file (so_som)\n")
  soil_som_data_x <- select_files("ICP soil SOM data file (so_som)")
  soil_som_data <- read_delim(soil_som_data_x, show_col_types = FALSE)
  }

  if(is.null(ss_ssm)){
  cat("Select ICP soil SSM data file (ss_ssm)\n")
  soil_ssm_data_x <- select_files("ICP soil SSM data file (ss_ssm)")
  soil_ssm_data<- read_delim(soil_ssm_data_x, show_col_types = FALSE)
  }

  message("Soil Data processed!")

  if(is.null(si_sta)){
  cat("Select ICP stand data file (si_sta)\n")
  stand_data_x <- select_files("ICP stand data file (si_sta)")
  stand_data <- read_delim(stand_data_x, show_col_types = FALSE)}else{
    stand_data <- read_delim(si_sta, show_col_types = FALSE)
  }

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

  if(is.null(gr_ipm)){
  cat("Select ICP stand data file (gr_ipm)\n")
  tree_data_dia_x <- select_files("ICP Growth data file (gr_ipm)")
  tree_data_dia<-read_delim(tree_data_dia_x, show_col_types = FALSE)}else{
  tree_data_dia<-read_delim(gr_ipm, show_col_types = FALSE)
  }

  message("Tree Data processed!")

  if(is.null(gr_iev)){
  cat("Select ICP thinning and basal data file (gr_iev)\n")
  thinning_data_x <-  select_files("ICP thinning and basal data file (gr_iev)")
  thinning_data <- read_delim(thinning_data_x, show_col_types = FALSE)}else{
  thinning_data <- read_delim(gr_iev, show_col_types = FALSE)
  }

  if(is.null(gr_inv)){
  cat("Select ICP growth inventory data file (gr_inv) \n")
  inventory_data_x <-  select_files("ICP growth inventory data file (gr_inv) ")
  inventory_data<- read_delim(inventory_data_x, show_col_types = FALSE)}else{
  inventory_data<- read_delim(gr_inv, show_col_types = FALSE)
  }

  message("Thinninh Data processed!")

  if(is.null(lf_lfm)){
  cat("Select ICP litter data file (lf_lfm)\n")
  litter_data_x <-  select_files("ICP litter data file (lf_lfm)")
  litter_data<- read_delim(litter_data_x, show_col_types = FALSE)}else{
  litter_data<- read_delim(lf_lfm, show_col_types = FALSE)
  }

  frosster_d<- read_excel('data/frosster_data.xlsx')

  sites_lat_lon <- read.table("data/ICP_lat_lon_all.txt", sep = ",", header = TRUE)

  country_data_list_lat_lon <- list()
  for (i in 1:length(country_site_num_list)) {
    country_code <- country_codes[i]
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


  if(length(chelsa_clim) > 0) {
  return(list(country_code = country_codes, country_names = country_names, lat_lon = country_data_list_lat_lon, ICP_temperature = country_data_list_tas, ICP_precipitation = country_data_list_pr, ICP_radiation = country_data_list_sr,
              soil_data = soil_data, soil_som_data = soil_som_data, soil_ssm_data = soil_ssm_data, stand_data = stand_data,  ss_age_d_1=ss_age_d_1, ss_age_d_2=ss_age_d_2,
              tree_data_dia = tree_data_dia, thinning_data = thinning_data,inventory_data = inventory_data,
              litter_data = litter_data, frosster_d = frosster_d, chelsa_clim = chelsa_clim))}else{
                return(list(country_code = country_codes, country_names = country_names, lat_lon = country_data_list_lat_lon, ICP_temperature = country_data_list_tas, ICP_precipitation = country_data_list_pr, ICP_radiation = country_data_list_sr,
                            soil_data = soil_data, soil_som_data = soil_som_data, soil_ssm_data = soil_ssm_data, stand_data = stand_data, ss_age_d_1=ss_age_d_1, ss_age_d_2=ss_age_d_2,
                            tree_data_dia = tree_data_dia, thinning_data = thinning_data,inventory_data = inventory_data,
                            litter_data = litter_data, frosster_d = frosster_d))
  }
}

