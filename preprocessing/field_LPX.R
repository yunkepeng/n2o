library(terra)
library(stringr)
library(rgdal)
library(raster)
library(dplyr)
library(readr)
library(ncdf4)
#read all field-data from data-driven model
field_data <- read.csv("~/data/n2o_Yunke/final_forcing/n2o_siteinfo.csv")
field_data$treatment <- "field"

#rbind into a dataframe
df_all <- unique(field_data[,c("lon","lat","z","pft")])

forest <- subset(df_all,pft=="forest")
grassland <- subset(df_all,pft=="grassland")
cropland <- subset(df_all,pft=="cropland")

#1. N2O
a1 <- raster("/Users/yunpeng/data/LPX/data/LPX-Bern_SH6_fN2Opft_lu.nc",band=2052,level=1)
plot(a1*1000000000*3600)
(2020-1980+1)
2052-12*41+1
(2020-2016)
2052-12*4
length(c(1561:2004))/12

nc_open("/Users/yunpeng/data/LPX/data/LPX-Bern_SH6_fN2Opft_lu.nc")
#(1:Natural; 2:Cropland; 3:Pasture; 4:Urban)

empty_data1 <- data.frame(matrix(NA)) 
empty_data2 <- data.frame(matrix(NA)) 
empty_data3 <- data.frame(matrix(NA)) 

for (i in c(1561:2004)) {
  print(i)
  site_data <- SpatialPoints(forest[,c("lon","lat")])
  pft1 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_fN2Opft_lu.nc",
                                  band=i,level=1), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  empty_data1[1:nrow(forest),(i-1560)]<- pft1
  
  site_data <- SpatialPoints(grassland[,c("lon","lat")])
  pft1 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_fN2Opft_lu.nc",
                                  band=i,level=3), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(grassland, by = c("lon", "lat")))[,1]
  empty_data2[1:nrow(grassland),(i-1560)]<- pft1
  
  site_data <- SpatialPoints(cropland[,c("lon","lat")])
  pft1 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_fN2Opft_lu.nc",
                                  band=i,level=2), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(cropland, by = c("lon", "lat")))[,1]
  empty_data3[1:nrow(cropland),(i-1560)]<- pft1
  
} 

forest_n2o <- data.frame(matrix(NA)) 
grassland_n2o <- data.frame(matrix(NA)) 
cropland_n2o <- data.frame(matrix(NA)) 

for (i in c(1:37)) {

  forest_n2o[1:nrow(forest),i] <- rowMeans(empty_data1[1:nrow(forest),c((i*12-11):(i*12))],na.rm=T)*1000000000*3600 #convert from kg_m2_s to ug_m2_h
  
  grassland_n2o[1:nrow(grassland),i] <- rowMeans(empty_data2[1:nrow(grassland),c((i*12-11):(i*12))],na.rm=T)*1000000000*3600 #convert from kg_m2_s to ug_m2_h
  
  cropland_n2o[1:nrow(cropland),i] <- rowMeans(empty_data3[1:nrow(cropland),c((i*12-11):(i*12))],na.rm=T)*1000000000*3600 #convert from kg_m2_s to ug_m2_h
  
} 

forest_annual_n2o <- as.data.frame(cbind(forest,forest_n2o))
grassland_annual_n2o <- as.data.frame(cbind(grassland,grassland_n2o))
cropland_annual_n2o <- as.data.frame(cbind(cropland,cropland_n2o))

all_annual_n2o <- as.data.frame(rbind(forest_annual_n2o,grassland_annual_n2o,cropland_annual_n2o))

names(all_annual_n2o) <- c("lon","lat","z","pft",paste0("year",c(1980:2016)))
csvfile <- paste("~/data/n2o_Yunke/final_forcing/LPX_annual_n2o.csv")
write_csv(all_annual_n2o, path = csvfile)

#fapar

empty_data1 <- data.frame(matrix(NA)) 
empty_data2 <- data.frame(matrix(NA)) 
empty_data3 <- data.frame(matrix(NA)) 

for (i in c(1561:2004)) {
  print(i)
  site_data <- SpatialPoints(forest[,c("lon","lat")])
  pft1 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_fAPARpft_lu.nc",
                                  band=i,level=1), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  empty_data1[1:nrow(forest),(i-1560)]<- pft1
  
  site_data <- SpatialPoints(grassland[,c("lon","lat")])
  pft1 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_fAPARpft_lu.nc",
                                  band=i,level=3), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(grassland, by = c("lon", "lat")))[,1]
  empty_data2[1:nrow(grassland),(i-1560)]<- pft1
  
  site_data <- SpatialPoints(cropland[,c("lon","lat")])
  pft1 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_fAPARpft_lu.nc",
                                  band=i,level=2), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(cropland, by = c("lon", "lat")))[,1]
  empty_data3[1:nrow(cropland),(i-1560)]<- pft1
  
} 

#rbind into one
all_fapar <- as.data.frame(rbind(empty_data1,empty_data2,empty_data3))
all_site <- as.data.frame(rbind(forest,grassland,cropland))

annual_minfapar <- data.frame(matrix(NA))
annual_meanfapar <- data.frame(matrix(NA))
annual_maxfapar <- data.frame(matrix(NA))

for (i in c(1:37)) {
  annual_minfapar[1:nrow(all_fapar),i] <- apply(all_fapar[1:nrow(all_fapar),c((i*12-11):(i*12))], 1, FUN = min, na.rm = TRUE)
  annual_meanfapar[1:nrow(all_fapar),i] <- apply(all_fapar[1:nrow(all_fapar),c((i*12-11):(i*12))], 1, FUN = mean, na.rm = TRUE)
  annual_maxfapar[1:nrow(all_fapar),i] <- apply(all_fapar[1:nrow(all_fapar),c((i*12-11):(i*12))], 1, FUN = max, na.rm = TRUE)
  print(i)
} 

final_minfapar <- as.data.frame(cbind(all_site,annual_minfapar))
final_meanfapar <- as.data.frame(cbind(all_site,annual_meanfapar))
final_maxfapar <- as.data.frame(cbind(all_site,annual_maxfapar))

names(final_minfapar) <- c("lon","lat","z","pft",paste0("year",c(1980:2016)))
names(final_meanfapar) <- c("lon","lat","z","pft",paste0("year",c(1980:2016)))
names(final_maxfapar) <- c("lon","lat","z","pft",paste0("year",c(1980:2016)))

final_minfapar[sapply(final_minfapar, is.nan)] <- NA
final_minfapar[sapply(final_minfapar, is.infinite)] <- NA
final_meanfapar[sapply(final_meanfapar, is.nan)] <- NA
final_meanfapar[sapply(final_meanfapar, is.infinite)] <- NA
final_maxfapar[sapply(final_maxfapar, is.nan)] <- NA
final_maxfapar[sapply(final_maxfapar, is.infinite)] <- NA

csvfile <- paste("~/data/n2o_Yunke/final_forcing/LPX_annual_actual_minfapar.csv")
write_csv(final_minfapar, path = csvfile)
csvfile <- paste("~/data/n2o_Yunke/final_forcing/LPX_annual_actual_meanfapar.csv")
write_csv(final_meanfapar, path = csvfile)
csvfile <- paste("~/data/n2o_Yunke/final_forcing/LPX_annual_actual_maxfapar.csv")
write_csv(final_maxfapar, path = csvfile)

#2. moisture
forest <- subset(df_all,pft=="forest")
grassland <- subset(df_all,pft=="grassland")
cropland <- subset(df_all,pft=="cropland")

nc_open("~/data/LPX/data/LPX-Bern_SH6_msl.nc")
#use first layer as it is 0-10cm

empty_data1 <- data.frame(matrix(NA)) 
empty_data2 <- data.frame(matrix(NA)) 
empty_data3 <- data.frame(matrix(NA)) 

for (i in c(1561:2004)) {
  print(i)
  site_data <- SpatialPoints(forest[,c("lon","lat")])
  pft1 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_msl.nc",
                                  band=i,level=1), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  empty_data1[1:nrow(forest),(i-1560)]<- pft1
  
  site_data <- SpatialPoints(grassland[,c("lon","lat")])
  pft1 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_msl.nc",
                                  band=i,level=1), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(grassland, by = c("lon", "lat")))[,1]
  empty_data2[1:nrow(grassland),(i-1560)]<- pft1
  
  site_data <- SpatialPoints(cropland[,c("lon","lat")])
  pft1 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_msl.nc",
                                  band=i,level=1), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(cropland, by = c("lon", "lat")))[,1]
  empty_data3[1:nrow(cropland),(i-1560)]<- pft1
  
} 

forest_moisture <- data.frame(matrix(NA)) 
grassland_moisture <- data.frame(matrix(NA)) 
cropland_moisture <- data.frame(matrix(NA)) 

#below divided by 100 to unitless factor because depth = 0.1m, then 1000kg water = 1m3 water
for (i in c(1:37)) {
  
  forest_moisture[1:nrow(forest),i] <- rowMeans(empty_data1[1:nrow(forest),c((i*12-11):(i*12))],na.rm=T)/100 #convert to unitless
  
  grassland_moisture[1:nrow(grassland),i] <- rowMeans(empty_data2[1:nrow(grassland),c((i*12-11):(i*12))],na.rm=T)/100 #convert to unitless
  
  cropland_moisture[1:nrow(cropland),i] <- rowMeans(empty_data3[1:nrow(cropland),c((i*12-11):(i*12))],na.rm=T)/100 #convert to unitless
  
} 

forest_annual_moisture <- as.data.frame(cbind(forest,forest_moisture))
grassland_annual_moisture <- as.data.frame(cbind(grassland,grassland_moisture))
cropland_annual_moisture <- as.data.frame(cbind(cropland,cropland_moisture))

all_annual_moisture <- as.data.frame(rbind(forest_annual_moisture,grassland_annual_moisture,cropland_annual_moisture))

names(all_annual_moisture) <- c("lon","lat","z","pft",paste0("year",c(1980:2016)))
csvfile <- paste("~/data/n2o_Yunke/final_forcing/LPX_annual_moisture.csv")
write_csv(all_annual_moisture, path = csvfile)

#3. N fertilisation
forest <- subset(df_all,pft=="forest")
grassland <- subset(df_all,pft=="grassland")
cropland <- subset(df_all,pft=="cropland")

a1 <- raster("~/data/LPX/data/nfert_NMIP2022_1850-2021_per_landuse.nc",band=171)
plot(a1) # unit in gN/m2
#NH4CROP, NO3CROP, NH4PAST, NO3PAST
#band: 1-171
#Nfertilisation is during 1980-2016
2020-2016+1
171-5+1
2020-1980+1
171-41+1
length(c(131:167))

empty_data2 <- data.frame(matrix(NA)) 
empty_data3 <- data.frame(matrix(NA)) 

for (i in c(131:167)) {
  print(i)
  site_data <- SpatialPoints(grassland[,c("lon","lat")]) # only select lon and lat
  pft1 <- (raster::extract(raster("~/data/LPX/data/nfert_NMIP2022_1850-2021_per_landuse.nc",
                                  band=i,varname="NH4PAST"), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(grassland, by = c("lon", "lat")))[,1]
  pft2 <- (raster::extract(raster("~/data/LPX/data/nfert_NMIP2022_1850-2021_per_landuse.nc",
                                  band=i,varname="NO3PAST"), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(grassland, by = c("lon", "lat")))[,1]
  pft_all <- pft1+pft2
  empty_data2[1:nrow(grassland),(i-130)]<- pft_all
  
  site_data <- SpatialPoints(cropland[,c("lon","lat")]) # only select lon and lat
  pft1 <- (raster::extract(raster("~/data/LPX/data/nfert_NMIP2022_1850-2021_per_landuse.nc",
                                  band=i,varname="NH4CROP"), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(cropland, by = c("lon", "lat")))[,1]
  pft2 <- (raster::extract(raster("~/data/LPX/data/nfert_NMIP2022_1850-2021_per_landuse.nc",
                                  band=i,varname="NO3CROP"), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(cropland, by = c("lon", "lat")))[,1]
  pft_all <- pft1+pft2
  empty_data3[1:nrow(cropland),(i-130)]<- pft_all
  
} 

grassland_nfer <- as.data.frame(cbind(grassland,empty_data2*10)) #convert unit from g/m2 to kg/ha
cropland_nfer <- as.data.frame(cbind(cropland,empty_data3*10)) #convert unit from g/m2 to kg/ha
names(grassland_nfer) <- c("lon","lat","z","pft",paste0("year",c(1980:2016)))
names(cropland_nfer) <- c("lon","lat","z","pft",paste0("year",c(1980:2016)))

all_nfer <- as.data.frame(rbind(grassland_nfer,cropland_nfer))

csvfile <- paste("~/data/n2o_Yunke/final_forcing/LPX_annual_nfer.csv")
write_csv(all_nfer, path = csvfile)

#4. temperature
df_all_sites <- unique(df_all[,c("lon","lat")])

empty_data <- data.frame(matrix(NA)) 

for (i in c(131:167)) {
  print(i)
  site_data <- SpatialPoints(df_all_sites[,c("lon","lat")]) # only select lon and lat
  pft1 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH1_tas.nc",
                                  band=i,level=1), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(df_all_sites, by = c("lon", "lat")))[,1]
  pft2 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH1_tas.nc",
                                  band=i,level=2), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(df_all_sites, by = c("lon", "lat")))[,1]
  pft3 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH1_tas.nc",
                                  band=i,level=3), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(df_all_sites, by = c("lon", "lat")))[,1]
  pft4 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH1_tas.nc",
                                  band=i,level=4), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(df_all_sites, by = c("lon", "lat")))[,1]
  pft5 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH1_tas.nc",
                                  band=i,level=5), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(df_all_sites, by = c("lon", "lat")))[,1]
  pft6 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH1_tas.nc",
                                  band=i,level=6), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(df_all_sites, by = c("lon", "lat")))[,1]
  pft7 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH1_tas.nc",
                                  band=i,level=7), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(df_all_sites, by = c("lon", "lat")))[,1]
  pft8 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH1_tas.nc",
                                  band=i,level=8), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(df_all_sites, by = c("lon", "lat")))[,1]
  pft9 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH1_tas.nc",
                                  band=i,level=9), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(df_all_sites, by = c("lon", "lat")))[,1]
  pft10 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH1_tas.nc",
                                   band=i,level=10), site_data, sp = TRUE) %>%
              as_tibble() %>% right_join(df_all_sites, by = c("lon", "lat")))[,1]
  pft11 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH1_tas.nc",
                                   band=i,level=11), site_data, sp = TRUE) %>%
              as_tibble() %>% right_join(df_all_sites, by = c("lon", "lat")))[,1]
  pft12 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH1_tas.nc",
                                   band=i,level=12), site_data, sp = TRUE) %>%
              as_tibble() %>% right_join(df_all_sites, by = c("lon", "lat")))[,1]
  pft_all <- as.data.frame(cbind(pft1,pft2,pft3,pft4,pft5,pft6,pft7,pft8,pft9,pft10,pft11,pft12))
  pft_all[pft_all<=0] <- NA
  empty_data[1:nrow(df_all_sites),c( ((i-130)*12-11) : ((i-130)*12) )]<- pft_all
} 
monthly_temperature <- empty_data
dim(monthly_temperature)

#5. ppfd
monthly_ppfd <- data.frame(matrix(NA)) 

for (i in c(1561:2004)) {
  print(i)
  site_data <- SpatialPoints(df_all_sites[,c("lon","lat")]) # only select lon and lat
  pft1 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_rsds.nc",
                                  band=i), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(df_all_sites, by = c("lon", "lat")))[,1]
  monthly_ppfd[1:nrow(df_all_sites),(i-1560)]<- pft1
} 
#PPFD (umol/m2/s) = SWdown (w/m2) * 4.6 * 0.5 
monthly_ppfd_final <- monthly_ppfd*4.6*0.5
monthly_ppfd_final_growing <- monthly_ppfd_final + monthly_temperature- monthly_temperature

annual_temperature <- data.frame(matrix(NA)) 
annual_ppfd <- data.frame(matrix(NA)) 


#now, average T into annual, sum-ups ppfd into annnual
days <- rep(c(31,28,31,30,31,30,31,31,30,31,30,31),ncol(annual_ppfd))
monthly_ppfd_final_final <- monthly_ppfd_final_growing*86400*days/1000000 #convert values from umol/m2/s to mol/month

for (i in c(1:37)) {
  annual_temperature[1:nrow(monthly_temperature),i] <- rowMeans(monthly_temperature[1:nrow(monthly_temperature),c((i*12-11):(i*12))],na.rm=T)
  annual_ppfd[1:nrow(monthly_ppfd_final_final),i] <- rowSums(monthly_ppfd_final_final[1:nrow(monthly_ppfd_final_final),c((i*12-11):(i*12))],na.rm=T) 
} 

output_T <- as.data.frame(cbind(df_all_sites,annual_temperature))
output_ppfd <- as.data.frame(cbind(df_all_sites,annual_ppfd))

df_output_T <- merge(df_all,output_T,by=c("lon","lat"),all.x=TRUE)
df_output_PPFD <- merge(df_all,output_ppfd,by=c("lon","lat"),all.x=TRUE)

names(df_output_T) <- c("lon","lat","z","pft",paste0("year",c(1980:2016)))
names(df_output_PPFD) <- c("lon","lat","z","pft",paste0("year",c(1980:2016)))

csvfile <- paste("~/data/n2o_Yunke/final_forcing/LPX_annual_T.csv")
write_csv(df_output_T, path = csvfile)

csvfile <- paste("~/data/n2o_Yunke/final_forcing/LPX_annual_PPFD.csv")
write_csv(df_output_PPFD, path = csvfile)

#6. forest cover
forest <- subset(df_all,pft=="forest")

#extract SH6's forest cover
empty_data <- data.frame(matrix(NA)) 

for (i in c(1561:2004)) {
  print(i)
  site_data <- SpatialPoints(forest[,c("lon","lat")]) # only select lon and lat
  pft1 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_landCoverFrac.nc",
                                  band=i,level=1), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  pft2 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_landCoverFrac.nc",
                                  band=i,level=2), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  pft3 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_landCoverFrac.nc",
                                  band=i,level=3), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  pft4 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_landCoverFrac.nc",
                                  band=i,level=4), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  pft5 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_landCoverFrac.nc",
                                  band=i,level=5), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  pft6 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_landCoverFrac.nc",
                                  band=i,level=6), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  pft7 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_landCoverFrac.nc",
                                  band=i,level=7), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  pft8 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_landCoverFrac.nc",
                                  band=i,level=8), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  pft9 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_landCoverFrac.nc",
                                  band=i,level=9), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  pft10 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_landCoverFrac.nc",
                                   band=i,level=10), site_data, sp = TRUE) %>%
              as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  forest_percentage <- (pft1+pft2+pft3+pft4+pft5+pft6+pft7+pft8)/(pft1+pft2+pft3+pft4+pft5+pft6+pft7+pft8+pft9+pft10)
  empty_data[1:nrow(forest),i-1560]<- forest_percentage
} 

forest_cover <- rowMeans(empty_data,na.rm=T)
final_forest_cover <- as.data.frame(cbind(forest,forest_cover))
csvfile <- paste("~/data/n2o_Yunke/final_forcing/forestcover_site.csv")
write_csv(final_forest_cover, path = csvfile)

