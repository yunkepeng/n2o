library(terra)
library(stringr)
library(rgdal)
library(raster)
library(dplyr)
library(readr)
library(ncdf4)
#read all field-data that includes n2o

cropland <- read.csv("~/data/n2o_Yunke/final_forcing/EF_database.csv")
cropland <- unique(cropland[,c("lon","lat","z","start_yr","end_yr")])
cropland$pft <- "cropland"

summary(cropland) #values range from 1980 to 2016


#1. N fertilisation
cropland$nfer <- NA
for (i in 1:nrow(cropland)) {
  print(i)
  site_data <- SpatialPoints(cropland[i,c("lon","lat")])
  start_yr <- cropland[i,c("start_yr")]
  end_yr <- cropland[i,c("end_yr")]
  
  #now, select bands from all these months
  start_year <- 171- (2020-start_yr)
  
  end_year <- 171- (2020-end_yr)
  
  empty_data <- NA
  for (a in start_year:end_year) {
    a1 <- (raster::extract(raster("~/data/LPX/data/nfert_NMIP2022_1850-2021_per_landuse.nc",
                                  band=a,varname="NH4CROP"), site_data, sp = TRUE)%>%
             as_tibble())[,1]
    a2 <- (raster::extract(raster("~/data/LPX/data/nfert_NMIP2022_1850-2021_per_landuse.nc",
                                  band=a,varname="NO3CROP"), site_data, sp = TRUE)%>%
             as_tibble())[,1]
    a_all <- (a1 + a2)*10  #convert unit from g/m2 to kg/ha
    empty_data <- rbind(empty_data, a_all)
  } 
  cropland[i,c("nfer")] <- colMeans(empty_data,na.rm=T)
}

#2. N2O
cropland$n2o_sh1 <- NA
cropland$n2o_sh3 <- NA

for (i in 1:nrow(cropland)) {
  print(i)
  site_data <- SpatialPoints(cropland[i,c("lon","lat")])
  start_yr <- cropland[i,c("start_yr")]
  end_yr <- cropland[i,c("end_yr")]
  
  #now, select bands from all these months
  start_month <- 2052-12*(2020-start_yr+1)+1
  
  end_month <- 2052-12*(2020-end_yr)
  
  empty_data <- NA
  empty_data2 <- NA
  
  for (a in start_month:end_month) {
    a1 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH1_fN2Opft_lu.nc",
                                  band=a,level=2), site_data, sp = TRUE)%>%
             as_tibble())[,1]
    empty_data <- rbind(empty_data, a1)
    
    a2 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH3_fN2Opft_lu.nc",
                                  band=a,level=2), site_data, sp = TRUE)%>%
             as_tibble())[,1]
    empty_data2 <- rbind(empty_data2, a2)
    
  } 
  cropland[i,c("n2o_sh1")] <- colMeans(empty_data,na.rm=T)*1000000000*3600 #convert from kg_m2_s to ug_m2_h
  cropland[i,c("n2o_sh3")] <- colMeans(empty_data2,na.rm=T)*1000000000*3600 #convert from kg_m2_s to ug_m2_h
}

csvfile <- paste("~/data/n2o_Yunke/final_forcing/EF_database_with_LPX.csv")
write_csv(cropland, path = csvfile)

#read data and finish relevant analysis
#convert values from ug/m2/h to kg/ha/yr -> so consistent with nfer
pred_EF <- read.csv("~/data/n2o_Yunke/final_forcing/EF_database_with_LPX.csv")
pred_EF$pred_EF <- (pred_EF$n2o_sh1-pred_EF$n2o_sh3)*(8760/100000)/pred_EF$nfer

pred_EF[sapply(pred_EF, is.nan)] <- NA
pred_EF[sapply(pred_EF, is.infinite)] <- NA

pred_EF <- pred_EF[,c("lon","lat","z","start_yr","end_yr","pft","pred_EF")]

cropland_EF <- read.csv("~/data/n2o_Yunke/final_forcing/EF_database.csv")

cropland_EF_compare <- merge(cropland_EF,pred_EF,
                             by=c("lon","lat","z","start_yr","end_yr","pft"),all.x=TRUE)

summary(cropland_EF_compare)
cropland_EF_compare$obs_EF <- cropland_EF_compare$EF/100

analyse_modobs2(cropland_EF_compare,"pred_EF","obs_EF", type = "points",relative=TRUE)$gg
