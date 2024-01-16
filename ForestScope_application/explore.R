
# 
# isric_soil_dx <- isric_soil_data()
#  icp_data_dx <- ICP_climate_soil()
#isric_soil_d <- isric_soil_d
#icp_data_d <- icp_data_dx

 # chelsa_clim_dx <- icp_data$chelsa_clim
#chelsa_clim_d <- chelsa_clim_dx

# Define IIASA_ICP_process function
IIASA_ICP_process <- function(isric_soil_d = NULL, icp_data = NULL, chelsa_clim = NULL, country= NULL, site_number= NULL, lat = NULL, lon = NULL) {
  library(shiny)
  library(leaflet)
  library(ggplot2)
  library(soiltexture)
    select_country <- country
    ss_select <- site_number
  ###  CLIMATE DATA  ###

  country_code_index <- which(icp_data$country_names == select_country)
  x <- icp_data$ICP_temperature[[country_code_index]]
  
  for (i in 1:length(x)){
    if(names(x[i]) == ss_select){
      x_s<- x[i]
      site_date_tmp <- x_s[[1]][[1]]
      site_tmin <- x_s[[1]][[2]]
      site_tmax <- x_s[[1]][[3]]
      site_tavg <- x_s[[1]][[4]]}}
  
    
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
  

  ######## End of thinning data preparation #####
  
  return(list(d_climate = d_climate, d_site = d_site, d_species = d_species, d_thinning = d_thinning))
  
}



explore_ui<-shinyUI(fluidPage(
  titlePanel("Europe Map with Points"),
  mainPanel(
    leafletOutput("map", height = "600px"),
    hr(),
    h4("Selected Point Information"),
    verbatimTextOutput("selected_point"),
    h4("Plot for Selected Point"),
    plotOutput("plot"),
    h4("Data Table for Selected Point"),
    DT::dataTableOutput("table1"),
    DT::dataTableOutput("table2"),
    DT::dataTableOutput("table3"),
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

explore_server<-shinyServer(function(input, output, session, extracted_data) {
  extracted_data <- reactiveValues(d_climate = NULL, d_site = NULL, d_species = NULL, d_thinning = NULL, d_parameters = NULL, d_sizeDist = NULL)
  
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
  
  observeEvent(input$map_marker_click, {
    marker <- input$map_marker_click
    selected_point <- points[points$id == marker$id, ]
    output$selected_point <- renderText({
      paste("Site Number:", as.numeric(gsub("site_num_", "", selected_point$site)), "\nCountry:", selected_point$con, "\nLatitude:", selected_point$lat, "\nLongitude:", selected_point$lon)
    })
    
    extracted_data<-IIASA_ICP_process(isric_soil_d = isric_soil_d, icp_data = icp_data, chelsa_clim = chelsa_clim, country= selected_point$con, site_number= selected_point$site, lat = selected_point$lat, lon = selected_point$lon)
    output$plot <- renderPlot({
      par(mfrow=c(3,3))
      plot(extracted_data$d_climate$tmp_min, ylab="Tmin", xlab="Time",typ="l",lwd=1.2)
      plot(extracted_data$d_climate$tmp_max, ylab="Tmax", xlab="Time",typ="l",lwd=1.2)
      plot(extracted_data$d_climate$tmp_ave, ylab="Tavg", xlab="Time",typ="l")
      plot(extracted_data$d_climate$prcp, ylab="Precipitaion", xlab="Time",typ="l",lwd=1.2)
      plot(extracted_data$d_climate$srad, ylab="Srad", xlab="Time",typ="l",lwd=1.2)
      plot(extracted_data$d_climate$frost_days, ylab="Frost days", xlab="Time",typ="l",lwd=1.2)
      plot(extracted_data$d_climate$co2,ylab="CO2", xlab="Time",typ="l",lwd=1.2)
    })
    
    output$table1 <- DT::renderDT({
      DT::datatable(extracted_data$soil, options = list(paging = FALSE, searching = FALSE))
    })
    output$table2 <- DT::renderDT({
      DT::datatable(extracted_data$species, options = list(paging = FALSE,searching = FALSE))
    })
    output$table3 <- DT::renderDT({
      DT::datatable(extracted_data$thinning, options = list(paging = FALSE, searching = FALSE))
    })
    
  })
})

shinyApp(ui = explore_ui, server = explore_server)

