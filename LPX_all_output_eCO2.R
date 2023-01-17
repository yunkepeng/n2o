library(terra)
library(stringr)
library(rgdal)
library(raster)
library(dplyr)
library(readr)
library(ncdf4)
#read all experimental data
df1 <- read_csv("~/data/n2o_wang_oikos/n2o_tables1.csv")
summary(df1)
names(df1) <- c("ref","location","n_amb","n_elv","n_site","n2o_amb",
                "n2o_elv","co2_amb","co2_elv","dco2","duration","pft","logr",
                "weight","group","method","species","Nfer","other","latitude","longitude","lat","lon","comments")

co2_site <- na.omit(unique(df1[,c("lon","lat","pft")]))
co2_site$treatment <- "co2"
#merge with elevation
climates_soil <- read.csv("~/data/n2o_Yunke/forcing/co2_siteinfo_predictors.csv")
co2_elv <- na.omit(unique(climates_soil[,c("lon","lat","z")]))
co2_site_z <- merge(co2_site,co2_elv,by=c("lon","lat"),all.x=TRUE)
co2_site_z$pft[co2_site_z$pft=="Forest"]<- "forest"
co2_site_z$pft[co2_site_z$pft=="Grassland"]<- "grassland"
co2_site_z$pft[co2_site_z$pft=="Cropland"]<- "cropland"

df2 <- read_csv("~/data/n2o_wang_oikos/n2o_tables2.csv")
summary(df2)
names(df2) <- c("ref","location","n_amb","n_elv","n_site","n2o_amb",
                "n2o_elv","dT","duration","pft","logr",
                "weight","group","species","Nfer","other","latitude","longitude","lat","lon","comments")
warming_site <- na.omit(unique(df2[,c("lon","lat","pft")]))
warming_site$treatment <- "warming"
warming_site_z <- merge(warming_site,co2_elv,by=c("lon","lat"),all.x=TRUE)
warming_site_z$pft[warming_site_z$pft=="Forest"]<- "forest"
warming_site_z$pft[warming_site_z$pft=="Grassland"]<- "grassland"
warming_site_z$pft[warming_site_z$pft=="Cropland"]<- "cropland"

warming_site_z <- warming_site_z[,c("lon","lat","z","pft")]
co2_site_z <- co2_site_z[,c("lon","lat","z","pft")]

df_all_bind <- as.data.frame(rbind(co2_site_z,warming_site_z))

df_all <- unique(df_all_bind)
#by checking these papers - it seems for 6 sites (they have same coordinates, but different pfts as recorded in the same paper either oikos's experimental paper or liao's field paper - just keep it)
dim(unique(df_all[,c("lon","lat")]))
dim(unique(df_all[,c("lon","lat","z")]))
dim(unique(df_all[,c("lon","lat","z","pft")]))

forest <- subset(df_all,pft=="forest")
grassland <- subset(df_all,pft=="grassland")
cropland <- subset(df_all,pft=="cropland")

#1. N2O
#a1 <- raster("/Users/yunpeng/data/LPX/data/LPX-Bern_SH6_fN2Opft_lu.nc",band=2052,level=1)
#(1:Natural; 2:Cropland; 3:Pasture; 4:Urban)

empty_data1 <- data.frame(matrix(NA)) 
empty_data2 <- data.frame(matrix(NA)) 
empty_data3 <- data.frame(matrix(NA)) 

for (i in c(1:2052)) {
  print(i)
  site_data <- SpatialPoints(forest[,c("lon","lat")])
  pft1 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_fN2Opft_lu.nc",
                                  band=i,level=1), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  empty_data1[1:nrow(forest),(i)]<- pft1
  
  site_data <- SpatialPoints(grassland[,c("lon","lat")])
  pft1 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_fN2Opft_lu.nc",
                                  band=i,level=3), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(grassland, by = c("lon", "lat")))[,1]
  empty_data2[1:nrow(grassland),(i)]<- pft1
  
  site_data <- SpatialPoints(cropland[,c("lon","lat")])
  pft1 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_fN2Opft_lu.nc",
                                  band=i,level=2), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(cropland, by = c("lon", "lat")))[,1]
  empty_data3[1:nrow(cropland),(i)]<- pft1
  
} 

forest_n2o <- data.frame(matrix(NA)) 
grassland_n2o <- data.frame(matrix(NA)) 
cropland_n2o <- data.frame(matrix(NA)) 

for (i in c(1:171)) {
  
  forest_n2o[1:nrow(forest),i] <- rowMeans(empty_data1[1:nrow(forest),c((i*12-11):(i*12))],na.rm=T)*1000000000*3600 #convert from kg_m2_s to ug_m2_h
  
  grassland_n2o[1:nrow(grassland),i] <- rowMeans(empty_data2[1:nrow(grassland),c((i*12-11):(i*12))],na.rm=T)*1000000000*3600 #convert from kg_m2_s to ug_m2_h
  
  cropland_n2o[1:nrow(cropland),i] <- rowMeans(empty_data3[1:nrow(cropland),c((i*12-11):(i*12))],na.rm=T)*1000000000*3600 #convert from kg_m2_s to ug_m2_h
  
} 

forest_annual_n2o <- as.data.frame(cbind(forest,forest_n2o))
grassland_annual_n2o <- as.data.frame(cbind(grassland,grassland_n2o))
cropland_annual_n2o <- as.data.frame(cbind(cropland,cropland_n2o))

all_annual_n2o <- as.data.frame(rbind(forest_annual_n2o,grassland_annual_n2o,cropland_annual_n2o))

names(all_annual_n2o) <- c("lon","lat","z","pft",paste0("year",c(1850:2020)))
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
