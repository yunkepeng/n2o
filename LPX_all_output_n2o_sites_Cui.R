library(terra)
library(stringr)
library(rgdal)
library(raster)
library(dplyr)
library(readr)
library(ncdf4)
#read all field-data that includes n2o

cropland <- read.csv("~/data/n2o_Yunke/forcing/lpx_sites_EF_Cui.csv")
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
    a1 <- (raster::extract(raster("~/data/LPX/data/nfert_NMIP2022_1850-2021.nc",
                                  band=a,varname="NH4CROP"), site_data, sp = TRUE)%>%
             as_tibble())[,1]
    a2 <- (raster::extract(raster("~/data/LPX/data/nfert_NMIP2022_1850-2021.nc",
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

csvfile <- paste("~/data/n2o_Yunke/forcing/LPX_years_cropland_Cui.csv")
write_csv(cropland, path = csvfile)

