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

#read all field-data from data-driven model
field_data <- read.csv("/Users/yunpeng/data/n2o_Yunke/forcing/lpx_sites_field.csv")
field_data$treatment <- "field"

#rbind into a dataframe
field_site_z <- field_data[,c("lon","lat","z","pft")]
co2_site_z <- co2_site_z[,c("lon","lat","z","pft")]
warming_site_z <- warming_site_z[,c("lon","lat","z","pft")]

df_all_bind <- as.data.frame(rbind(field_site_z,co2_site_z,warming_site_z))

df_all <- unique(df_all_bind)
#by checking these papers - it seems for 6 sites (they have same coordinates, but different pfts as recorded in the same paper either oikos's experimental paper or liao's field paper - just keep it)
dim(unique(df_all[,c("lon","lat")]))
dim(unique(df_all[,c("lon","lat","z")]))
dim(unique(df_all[,c("lon","lat","z","pft")]))

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
csvfile <- paste("~/data/n2o_Yunke/forcing/LPX_annual_n2o.csv")
write_csv(all_annual_n2o, path = csvfile)


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
csvfile <- paste("~/data/n2o_Yunke/forcing/LPX_annual_moisture.csv")
write_csv(all_annual_moisture, path = csvfile)

#3. N fertilisation
forest <- subset(df_all,pft=="forest")
grassland <- subset(df_all,pft=="grassland")
cropland <- subset(df_all,pft=="cropland")

a1 <- raster("~/data/LPX/data/nfert_NMIP2022_1850-2021.nc",band=171)
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
  pft1 <- (raster::extract(raster("~/data/LPX/data/nfert_NMIP2022_1850-2021.nc",
                                  band=i,varname="NH4PAST"), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(grassland, by = c("lon", "lat")))[,1]
  pft2 <- (raster::extract(raster("~/data/LPX/data/nfert_NMIP2022_1850-2021.nc",
                                  band=i,varname="NO3PAST"), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(grassland, by = c("lon", "lat")))[,1]
  pft_all <- pft1+pft2
  empty_data2[1:nrow(grassland),(i-130)]<- pft_all
  
  site_data <- SpatialPoints(cropland[,c("lon","lat")]) # only select lon and lat
  pft1 <- (raster::extract(raster("~/data/LPX/data/nfert_NMIP2022_1850-2021.nc",
                                  band=i,varname="NH4CROP"), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(cropland, by = c("lon", "lat")))[,1]
  pft2 <- (raster::extract(raster("~/data/LPX/data/nfert_NMIP2022_1850-2021.nc",
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

csvfile <- paste("~/data/n2o_Yunke/forcing/LPX_annual_nfer.csv")
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

csvfile <- paste("~/data/n2o_Yunke/forcing/LPX_annual_T.csv")
write_csv(df_output_T, path = csvfile)

csvfile <- paste("~/data/n2o_Yunke/forcing/LPX_annual_PPFD.csv")
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
#all values are higher than 80% (just one point less than 80%) - we can define threshold value as 80%
#only one site's forest cover <80% - removing them
csvfile <- paste("~/data/n2o_Yunke/forcing/forestcover_site.csv")
write_csv(final_forest_cover, path = csvfile)

#8. LAI: for max, mean and min fapar
forest <- subset(df_all,pft=="forest")
cropland <- subset(df_all,pft=="cropland")
grassland <- subset(df_all,pft=="grassland")

#LAI is pft-based
empty_data1 <- data.frame(matrix(NA)) #forest
empty_data2 <- data.frame(matrix(NA)) #cropland
empty_data3 <- data.frame(matrix(NA)) #grassland

#this analysis requries both lai and pft fractional cover
for (i in c(1561:2004)) {
  print(i)
  #forest
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
  pft_forest_all <- as.data.frame(cbind(pft1,pft2,pft3,pft4,pft5,pft6,pft7,pft8,pft9,pft10))
  
  lai1 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_laipft.nc",
                                  band=i,level=1), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  lai2 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_laipft.nc",
                                  band=i,level=2), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  lai3 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_laipft.nc",
                                  band=i,level=3), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  lai4 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_laipft.nc",
                                  band=i,level=4), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  lai5 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_laipft.nc",
                                  band=i,level=5), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  lai6 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_laipft.nc",
                                  band=i,level=6), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  lai7 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_laipft.nc",
                                  band=i,level=7), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  lai8 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_laipft.nc",
                                  band=i,level=8), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  lai9 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_laipft.nc",
                                  band=i,level=9), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  lai10 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_laipft.nc",
                                  band=i,level=10), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  
  lai_forest_all <- as.data.frame(cbind(lai1,lai2,lai3,lai4,lai5,lai6,lai7,lai8,lai9,lai10))

  lai_forest_monthly <- rowSums(lai_forest_all*pft_forest_all,na.rm=T)
  empty_data1[1:nrow(forest),i-1560]<- lai_forest_monthly
  
  #cropland
  site_data <- SpatialPoints(cropland[,c("lon","lat")]) # only select lon and lat
  pft11 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_landCoverFrac.nc",
                                  band=i,level=11), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(cropland, by = c("lon", "lat")))[,1]
  pft12 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_landCoverFrac.nc",
                                  band=i,level=12), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(cropland, by = c("lon", "lat")))[,1]
  pft_cropland_all <- as.data.frame(cbind(pft11,pft12))
  
  lai11 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_laipft.nc",
                                  band=i,level=11), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(cropland, by = c("lon", "lat")))[,1]
  lai12 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_laipft.nc",
                                  band=i,level=12), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(cropland, by = c("lon", "lat")))[,1]
 
  lai_cropland_all <- as.data.frame(cbind(lai11,lai12))
  
  lai_cropland_monthly <- rowSums(lai_cropland_all*pft_cropland_all,na.rm=T)
  empty_data2[1:nrow(cropland),i-1560]<- lai_cropland_monthly
  
  #grassland
  site_data <- SpatialPoints(grassland[,c("lon","lat")]) # only select lon and lat
  pft13 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_landCoverFrac.nc",
                                   band=i,level=13), site_data, sp = TRUE) %>%
              as_tibble() %>% right_join(grassland, by = c("lon", "lat")))[,1]
  pft14 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_landCoverFrac.nc",
                                   band=i,level=14), site_data, sp = TRUE) %>%
              as_tibble() %>% right_join(grassland, by = c("lon", "lat")))[,1]
  pft_grassland_all <- as.data.frame(cbind(pft13,pft14))
  
  lai13 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_laipft.nc",
                                   band=i,level=13), site_data, sp = TRUE) %>%
              as_tibble() %>% right_join(grassland, by = c("lon", "lat")))[,1]
  lai14 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_laipft.nc",
                                   band=i,level=14), site_data, sp = TRUE) %>%
              as_tibble() %>% right_join(grassland, by = c("lon", "lat")))[,1]
  
  lai_grassland_all <- as.data.frame(cbind(lai13,lai14))
  
  lai_grassland_monthly <- rowSums(lai_grassland_all*pft_grassland_all,na.rm=T)
  empty_data3[1:nrow(grassland),i-1560]<- lai_grassland_monthly
} 

#temporaily saving
csvfile <- paste("~/data/n2o_Yunke/forcing/lai_emptydata1.csv")
write_csv(empty_data1, path = csvfile)
csvfile <- paste("~/data/n2o_Yunke/forcing/lai_emptydata2.csv")
write_csv(empty_data2, path = csvfile)
csvfile <- paste("~/data/n2o_Yunke/forcing/lai_emptydata3.csv")
write_csv(empty_data3, path = csvfile)

#rbind into one
all_lai <- as.data.frame(rbind(empty_data1,empty_data2,empty_data3))
all_site <- as.data.frame(rbind(forest,cropland,grassland))

all_fapar <- 1-exp(-0.5 * all_lai)

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

csvfile <- paste("~/data/n2o_Yunke/forcing/LPX_annual_minfapar.csv")
write_csv(final_minfapar, path = csvfile)
csvfile <- paste("~/data/n2o_Yunke/forcing/LPX_annual_meanfapar.csv")
write_csv(final_meanfapar, path = csvfile)
csvfile <- paste("~/data/n2o_Yunke/forcing/LPX_annual_maxfapar.csv")
write_csv(final_maxfapar, path = csvfile)

#finally - annual vpd