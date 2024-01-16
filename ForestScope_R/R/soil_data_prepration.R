#' Process ISRIC soil data to create processed dataframes for soil data and available water capacity (AWC).
#'
#' This function reads the ISRIC soil data from raster files and processes the data to create processed dataframes for soil data and available water capacity (AWC).
#'
#' @param soil_data A null object (unused).
#' @param isric_data_awc A dataframe containing longitude, latitude, and available water capacity (AWC) data for each point (must contain columns named "longitude", "latitude", and "awc").
#' @param isric_data_soil A dataframe containing longitude, latitude, and soil attribute data (clay, silt, sand, and  SOC) for each point (must contain columns named "longitude", "latitude", "clay", "silt", "sand", and "soc").
#' @param isric_data_nitrogen A dataframe containing longitude, latitude, and soil attribute data (Nitrogen) for each point (must contain columns named "longitude", "latitude", "nitrogen").
#' @return A list containing two processed dataframes: "soil_data" and "awc_data". The "soil_data" dataframe contains processed soil attribute data (clay, silt, sand, SOC, and Nitrogen) for each point. The "awc_data" dataframe contains processed available water capacity (AWC) data for each point.
#'
#' @importFrom raster crop as.data.frame
#' @import ncdf4
#' @import dplyr
#' @import readxl
#' @import tidyverse
#' @import lubridate
#' @import Metrics
#' @import tcltk
#'
#' @examples
#' isric_soil_data(awc_files, clay_files, silt_files, sand_files, soc_files, nitrogen_files)
#'
#' @export
#'
isric_soil_data <- function(awc_files = NULL, clay_files = NULL, silt_files = NULL, sand_files = NULL, soc_files = NULL, nitrogen_files = NULL)  {
  library(raster)
  library(ncdf4)
  library(dplyr)
  library(readxl)
  library(tidyverse)
  library(lubridate)
  library(Metrics)
  library(tcltk)


  ex <- as(extent(-8 * 100000, 40 * 100000, 35 * 100000, 75 * 100000), "SpatialPolygons")
  exs <- as(extent(-8, 40, 35, 75), "SpatialPolygons")
  l1<-0.136601
  l2<-0.218493
  l3<-0.226531
  l4<-0.239226
  l5<-0.179149

  # Function for reading raster files and processing data
  process_isric_data <- function(file_path, ex, is_awc = FALSE) {
    raster_file <- raster(file_path)
    cropped_raster <- if (is_awc) {
      crop(raster_file, exs)
    } else {
      crop(raster_file, ex)
    }
    df <- raster::as.data.frame(cropped_raster, xy = TRUE)
    return(df)
  }


  # Prompt user to select files
  select_files <- function(prompt) {
    file_paths <- tk_choose.files(default = "", caption = prompt, multi = TRUE)
    return(as.character(file_paths))
  }

  # Prompt user to select files if not already defined
  if (is.null(clay_files) || is.null(silt_files) || is.null(sand_files) || is.null(soc_files) || is.null(nitrogen_files) || is.null(awc_files)) {
  cat("Select the ISRIC soil clay files\n")
  clay_files <- select_files("Select Clay Files")

  cat("Select the ISRIC soil silt files\n")
  silt_files <- select_files("Select Silt Files")

  cat("Select the ISRIC soil sand files\n")
  sand_files <- select_files("Select Sand Files")

  cat("Select the ISRIC soil soc files\n")
  soc_files <- select_files("Select SOC Files")

  cat("Select the ISRIC soil nitrogen files\n")
  nitrogen_files <- select_files("Select Nitrogen Files")

  cat("Select the ISRIC soil AWC files\n")
  awc_files <- select_files("Select AWC Files")}

  # Initialize progress bar
  total_steps <- length(clay_files) + length(silt_files) + length(sand_files) +
    length(soc_files) + length(nitrogen_files) + length(awc_files)
  progress_bar <- txtProgressBar(min = 0, max = total_steps, style = 3)

  # Read and process raster files with progress bar updates
  process_data_with_progress <- function(files, ex, is_awc = FALSE) {
    data <- lapply(files, function(file) {
      df <- process_isric_data(file, ex, is_awc)
      setTxtProgressBar(progress_bar, getTxtProgressBar(progress_bar) + 1)
      return(df)
    })
    return(data)
  }
  isric_clay_f <- process_data_with_progress(clay_files, exs, is_awc = TRUE)
  isric_silt_f <- process_data_with_progress(silt_files, exs, is_awc = TRUE)
  isric_sand_f <- process_data_with_progress(sand_files, exs, is_awc = TRUE)
  isric_soc_f <- process_data_with_progress(soc_files,exs, is_awc = TRUE)
  isric_nitrogen_f <- process_data_with_progress(nitrogen_files, ex)
  isric_awc_f <- process_data_with_progress(awc_files, exs, is_awc = TRUE)

  # Close progress bar
  close(progress_bar)

  # Further processing and calculations
  cat("Processing the loaded files...")


  isric_clay <- isric_clay_f[[1]]
  isric_clay[,1]<- isric_clay_f[[1]][,1]
  isric_clay[,2]<- isric_clay_f[[1]][,2]
  x<-NA
  x2<-NA
  x<-(as.data.frame(isric_clay_f[[1]][,3]))*l1
  x[,2]<- (isric_clay_f[[1]][,3])*l2
  x[,3]<- (isric_clay_f[[1]][,3])*l3
  x[,4]<- (isric_clay_f[[1]][,3])*l4
  x[,5]<- (isric_clay_f[[1]][,3])*l5
  x2 <- rowSums(x)
  isric_clay_final  <- x2

  isric_silt <- isric_silt_f[[1]]
  isric_silt[,1]<- isric_silt_f[[1]][,1]
  isric_silt[,2]<- isric_silt_f[[1]][,2]
  x<-NA
  x2<-NA
  x<-(as.data.frame(isric_silt_f[[1]][,3]))*l1
  x[,2]<- (isric_silt_f[[1]][,3])*l2
  x[,3]<- (isric_silt_f[[1]][,3])*l3
  x[,4]<- (isric_silt_f[[1]][,3])*l4
  x[,5]<- (isric_silt_f[[1]][,3])*l5
  x2 <- rowSums(x)
  isric_silt_final  <- x2

  isric_sand <- isric_sand_f[[1]]
  isric_sand[,1]<- isric_sand_f[[1]][,1]
  isric_sand[,2]<- isric_sand_f[[1]][,2]
  x<-NA
  x2<-NA
  x<-(as.data.frame(isric_sand_f[[1]][,3]))*l1
  x[,2]<- (isric_sand_f[[1]][,3])*l2
  x[,3]<- (isric_sand_f[[1]][,3])*l3
  x[,4]<- (isric_sand_f[[1]][,3])*l4
  x[,5]<- (isric_sand_f[[1]][,3])*l5
  x2 <- rowSums(x)
  isric_sand_final  <- x2

  isric_soc <- isric_soc_f[[1]]
  isric_soc[,1]<- isric_soc_f[[1]][,1]
  isric_soc[,2]<- isric_soc_f[[1]][,2]
  x<-NA
  x2<-NA
  x<-(as.data.frame(isric_soc_f[[1]][,3]))*l1
  x[,2]<- (isric_soc_f[[1]][,3])*l2
  x[,3]<- (isric_soc_f[[1]][,3])*l3
  x[,4]<- (isric_soc_f[[1]][,3])*l4
  x[,5]<- (isric_soc_f[[1]][,3])*l5
  x2 <- rowSums(x)
  isric_soc_final  <- x2


  isric_nitrogen <- isric_nitrogen_f[[1]]
  isric_nitrogen[,1]<- isric_nitrogen_f[[1]][,1]/100000
  isric_nitrogen[,2]<- isric_nitrogen_f[[1]][,2]/100000
  x<-NA
  x2<-NA
  x<-(as.data.frame(isric_nitrogen_f[[1]][,3]))*l1
  x[,2]<- (isric_nitrogen_f[[1]][,3])*l2
  x[,3]<- (isric_nitrogen_f[[1]][,3])*l3
  x[,4]<- (isric_nitrogen_f[[1]][,3])*l4
  x[,5]<- (isric_nitrogen_f[[1]][,3])*l5
  x2 <- rowSums(x)
  isric_nitrogen_final  <- x2/100

  d1<-50
  d2<- 100
  d3<- 150
  d4<- 300
  d5<- 400
  isric_wv_h3 <- list(isric_awc_f[[1]], isric_awc_f[[2]], isric_awc_f[[3]], isric_awc_f[[4]], isric_awc_f[[5]])
  x_1<-as.data.frame(isric_awc_f[[1]]$WWP_M_sl2_1km_ll * d1 *1e-2)
  x_1[,2]<-(isric_awc_f[[2]]$WWP_M_sl3_1km_ll *d2 *1e-2)
  x_1[,3]<-(isric_awc_f[[3]]$WWP_M_sl4_1km_ll * d3 *1e-2)
  x_1[,4]<-(isric_awc_f[[4]]$WWP_M_sl5_1km_ll * d4 *1e-2)
  x_1[,5]<-(isric_awc_f[[5]]$WWP_M_sl6_1km_ll * d5 *1e-2)
  x_1_lon_lat <- as.data.frame(isric_awc_f[[1]]$x)
  x_1_lon_lat[,2] <- isric_awc_f[[1]]$y
  isric_awc<- rowSums(x_1)

  # Return the processed data
  isric_data_awc <- data.frame(isric_awc_lon = x_1_lon_lat[,1], isric_awc_lat = x_1_lon_lat[,2], isrci_awc = isric_awc)
  isric_data_soil <- data.frame(isric_lon = isric_clay[,1], isric_lat = isric_clay[,2] , isric_clay = isric_clay, isric_silt = isric_silt, isric_sand = isric_sand,
                                isric_soc = isric_soc)
  isric_data_nitrogen<- data.frame(isric_nitrogen_lon = isric_nitrogen[,1], isric_nitrogen_lat = isric_nitrogen[,2], isric_nitrogen = isric_nitrogen)

  return(list(soil_data = isric_data_soil, soil_data_awc = isric_data_awc, soil_data_nitrogen = isric_nitrogen))
  cat("\rProcessing completed.           \n")
}


