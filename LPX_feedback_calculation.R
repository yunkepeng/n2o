library(terra)
library(stringr)
library(rgdal)
library(raster)
library(dplyr)
library(readr)
library(ncdf4)

#first - get land cover map in 2006 (actually, any year should be fine - as we are using fixed land use)

lc1 <- raster::as.data.frame(raster("~/data/LPX/data/LPX-Bern_SH6_landCoverFrac_lu.nc",band=1873,level=1),xy=TRUE)[,3]
lc2 <- raster::as.data.frame(raster("~/data/LPX/data/LPX-Bern_SH6_landCoverFrac_lu.nc",band=1873,level=2),xy=TRUE)[,3]
lc3 <- raster::as.data.frame(raster("~/data/LPX/data/LPX-Bern_SH6_landCoverFrac_lu.nc",band=1873,level=3),xy=TRUE)[,3]
lc4 <- raster::as.data.frame(raster("~/data/LPX/data/LPX-Bern_SH6_landCoverFrac_lu.nc",band=1873,level=4),xy=TRUE)[,3]

lc_df <- as.data.frame(cbind(lc1,lc2,lc3,lc4))
summary(rowSums(lc_df))

#calculate average N2O when dT = 0 
for (i in c(1:100)) {
  raster1 <- raster("~/data/LPX/data/step_experiment/co2_380_dT_0/LPX-Bern_co2_380_dT_0_fN2Opft_lu_annual.nc",
                    band=i,level=1)
  df1 <- raster::as.data.frame(raster1,xy=TRUE)
  
  raster2 <- raster("~/data/LPX/data/step_experiment/co2_380_dT_0/LPX-Bern_co2_380_dT_0_fN2Opft_lu_annual.nc",
                    band=i,level=2)
  df2 <- raster::as.data.frame(raster2,xy=TRUE)
  
  raster3 <- raster("~/data/LPX/data/step_experiment/co2_380_dT_0/LPX-Bern_co2_380_dT_0_fN2Opft_lu_annual.nc",
                    band=i,level=3)
  df3 <- raster::as.data.frame(raster3,xy=TRUE)
  
  raster4 <- raster("~/data/LPX/data/step_experiment/co2_380_dT_0/LPX-Bern_co2_380_dT_0_fN2Opft_lu_annual.nc",
                    band=i,level=4)
  df4 <- raster::as.data.frame(raster4,xy=TRUE)
  
  n2o_df <- as.data.frame(cbind(df1[,3],df2[,3],df3[,3],df4[,3]))
  
} 


forest_1a <- rowMeans(empty_data1a,na.rm=T)*1000000000*3600 #convert from kg_m2_s to ug_m2_h
forest_1b <- rowMeans(empty_data1b,na.rm=T)*1000000000*3600 
forest_1c <- rowMeans(empty_data1c,na.rm=T)*1000000000*3600 
forest_1d <- rowMeans(empty_data1d,na.rm=T)*1000000000*3600 
forest_1e <- rowMeans(empty_data1e,na.rm=T)*1000000000*3600 
forest_1f <- rowMeans(empty_data1f,na.rm=T)*1000000000*3600 
forest_1g <- rowMeans(empty_data1g,na.rm=T)*1000000000*3600 

grassland_1a <- rowMeans(empty_data2a,na.rm=T)*1000000000*3600 #convert from kg_m2_s to ug_m2_h
grassland_1b <- rowMeans(empty_data2b,na.rm=T)*1000000000*3600 
grassland_1c <- rowMeans(empty_data2c,na.rm=T)*1000000000*3600 
grassland_1d <- rowMeans(empty_data2d,na.rm=T)*1000000000*3600 
grassland_1e <- rowMeans(empty_data2e,na.rm=T)*1000000000*3600 
grassland_1f <- rowMeans(empty_data2f,na.rm=T)*1000000000*3600 
grassland_1g <- rowMeans(empty_data2g,na.rm=T)*1000000000*3600 

cropland_1a <- rowMeans(empty_data3a,na.rm=T)*1000000000*3600 #convert from kg_m2_s to ug_m2_h
cropland_1b <- rowMeans(empty_data3b,na.rm=T)*1000000000*3600 
cropland_1c <- rowMeans(empty_data3c,na.rm=T)*1000000000*3600 
cropland_1d <- rowMeans(empty_data3d,na.rm=T)*1000000000*3600 
cropland_1e <- rowMeans(empty_data3e,na.rm=T)*1000000000*3600 
cropland_1f <- rowMeans(empty_data3f,na.rm=T)*1000000000*3600 
cropland_1g <- rowMeans(empty_data3g,na.rm=T)*1000000000*3600 

forest_annual_n2o <- as.data.frame(cbind(forest,forest_1a,forest_1b,forest_1c,forest_1d,forest_1e,forest_1f,forest_1g))
grassland_annual_n2o <- as.data.frame(cbind(grassland,grassland_1a,grassland_1b,grassland_1c,grassland_1d,grassland_1e,grassland_1f,grassland_1g))
cropland_annual_n2o <- as.data.frame(cbind(cropland,cropland_1a,cropland_1b,cropland_1c,cropland_1d,cropland_1e,cropland_1f,cropland_1g))
names(forest_annual_n2o) <- c("lon","lat","z","pft","dT0_C380","dT0.39_C380","dT3.95_C380",
                              "dT7.5_C380","dT0_C416","dT0_C582","dT0_C813")
names(grassland_annual_n2o) <- c("lon","lat","z","pft","dT0_C380","dT0.39_C380","dT3.95_C380",
                                 "dT7.5_C380","dT0_C416","dT0_C582","dT0_C813")
names(cropland_annual_n2o) <- c("lon","lat","z","pft","dT0_C380","dT0.39_C380","dT3.95_C380",
                                "dT7.5_C380","dT0_C416","dT0_C582","dT0_C813")

all_annual_n2o <- as.data.frame(rbind(forest_annual_n2o,grassland_annual_n2o,cropland_annual_n2o))

csvfile <- paste("~/data/n2o_Yunke/forcing/eCO2_LPX_annual_n2o.csv")
write_csv(all_annual_n2o, path = csvfile)


#3. N fertilisation
forest <- subset(df_all,pft=="forest")
grassland <- subset(df_all,pft=="grassland")
cropland <- subset(df_all,pft=="cropland")

#a1 <- raster("~/data/LPX/data/nfert_NMIP2022_1850-2021.nc",band=171)
#plot(a1) # unit in gN/m2
#NH4CROP, NO3CROP, NH4PAST, NO3PAST
#band: 1-171

empty_data2 <- data.frame(matrix(NA)) 
empty_data3 <- data.frame(matrix(NA)) 

for (i in c(1:171)) {
  print(i)
  site_data <- SpatialPoints(grassland[,c("lon","lat")]) # only select lon and lat
  pft1 <- (raster::extract(raster("~/data/LPX/data/nfert_NMIP2022_1850-2021.nc",
                                  band=i,varname="NH4PAST"), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(grassland, by = c("lon", "lat")))[,1]
  pft2 <- (raster::extract(raster("~/data/LPX/data/nfert_NMIP2022_1850-2021.nc",
                                  band=i,varname="NO3PAST"), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(grassland, by = c("lon", "lat")))[,1]
  pft_all <- pft1+pft2
  empty_data2[1:nrow(grassland),(i)]<- pft_all
  
  site_data <- SpatialPoints(cropland[,c("lon","lat")]) # only select lon and lat
  pft1 <- (raster::extract(raster("~/data/LPX/data/nfert_NMIP2022_1850-2021.nc",
                                  band=i,varname="NH4CROP"), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(cropland, by = c("lon", "lat")))[,1]
  pft2 <- (raster::extract(raster("~/data/LPX/data/nfert_NMIP2022_1850-2021.nc",
                                  band=i,varname="NO3CROP"), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(cropland, by = c("lon", "lat")))[,1]
  pft_all <- pft1+pft2
  empty_data3[1:nrow(cropland),(i)]<- pft_all
  
} 

grassland_nfer <- as.data.frame(cbind(grassland,empty_data2*10)) #convert unit from g/m2 to kg/ha
cropland_nfer <- as.data.frame(cbind(cropland,empty_data3*10)) #convert unit from g/m2 to kg/ha
names(grassland_nfer) <- c("lon","lat","z","pft",paste0("year",c(1850:2020)))
names(cropland_nfer) <- c("lon","lat","z","pft",paste0("year",c(1850:2020)))

all_nfer <- as.data.frame(rbind(grassland_nfer,cropland_nfer))

csvfile <- paste("~/data/n2o_Yunke/forcing/eCO2_LPX_annual_nfer.csv")
write_csv(all_nfer, path = csvfile)

#4. temperature
df_all_sites <- unique(df_all[,c("lon","lat")])

empty_data <- data.frame(matrix(NA)) 

for (i in c(1:171)) {
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
  empty_data[1:nrow(df_all_sites),c( ((i)*12-11) : ((i)*12) )]<- pft_all
} 
monthly_temperature <- empty_data
dim(monthly_temperature)

#5. ppfd
monthly_ppfd <- data.frame(matrix(NA)) 

for (i in c(1:2052)) {
  print(i)
  site_data <- SpatialPoints(df_all_sites[,c("lon","lat")]) # only select lon and lat
  pft1 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_rsds.nc",
                                  band=i), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(df_all_sites, by = c("lon", "lat")))[,1]
  monthly_ppfd[1:nrow(df_all_sites),(i)]<- pft1
} 
#PPFD (umol/m2/s) = SWdown (w/m2) * 4.6 * 0.5 
monthly_ppfd_final <- monthly_ppfd*4.6*0.5
monthly_ppfd_final_growing <- monthly_ppfd_final + monthly_temperature- monthly_temperature

annual_temperature <- data.frame(matrix(NA)) 
annual_ppfd <- data.frame(matrix(NA)) 


#now, average T into annual, sum-ups ppfd into annnual
days <- rep(c(31,28,31,30,31,30,31,31,30,31,30,31),ncol(annual_ppfd))
monthly_ppfd_final_final <- monthly_ppfd_final_growing*86400*days/1000000 #convert values from umol/m2/s to mol/month

for (i in c(1:171)) {
  annual_temperature[1:nrow(monthly_temperature),i] <- rowMeans(monthly_temperature[1:nrow(monthly_temperature),c((i*12-11):(i*12))],na.rm=T)
  annual_ppfd[1:nrow(monthly_ppfd_final_final),i] <- rowSums(monthly_ppfd_final_final[1:nrow(monthly_ppfd_final_final),c((i*12-11):(i*12))],na.rm=T) 
} 

output_T <- as.data.frame(cbind(df_all_sites,annual_temperature))
output_ppfd <- as.data.frame(cbind(df_all_sites,annual_ppfd))

df_output_T <- merge(df_all,output_T,by=c("lon","lat"),all.x=TRUE)
df_output_PPFD <- merge(df_all,output_ppfd,by=c("lon","lat"),all.x=TRUE)

names(df_output_T) <- c("lon","lat","z","pft",paste0("year",c(1850:2020)))
names(df_output_PPFD) <- c("lon","lat","z","pft",paste0("year",c(1850:2020)))

csvfile <- paste("~/data/n2o_Yunke/forcing/eCO2_LPX_annual_T.csv")
write_csv(df_output_T, path = csvfile)

csvfile <- paste("~/data/n2o_Yunke/forcing/eCO2_LPX_annual_PPFD.csv")
write_csv(df_output_PPFD, path = csvfile)
