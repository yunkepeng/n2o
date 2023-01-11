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

#first, n2o - have a look at map
a1 <- raster("/Users/yunpeng/data/LPX/data/LPX-Bern_SH6_fN2Opft_lu.nc",band=2052,level=1)
plot(a1*1000000000*730.49)
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

  forest_n2o[1:nrow(forest),i] <- rowMeans(empty_data1[1:nrow(forest),c((i*12-11):(i*12))],na.rm=T)*1000000000*730.49 #convert from kg_m2_month to ug_m2_h
  
  grassland_n2o[1:nrow(grassland),i] <- rowMeans(empty_data2[1:nrow(grassland),c((i*12-11):(i*12))],na.rm=T)*1000000000*730.49 #convert from kg_m2_month to ug_m2_h
  
  cropland_n2o[1:nrow(cropland),i] <- rowMeans(empty_data3[1:nrow(cropland),c((i*12-11):(i*12))],na.rm=T)*1000000000*730.49 #convert from kg_m2_month to ug_m2_h
  
} 

forest_annual_n2o <- as.data.frame(cbind(forest,forest_n2o))
grassland_annual_n2o <- as.data.frame(cbind(grassland,grassland_n2o))
cropland_annual_n2o <- as.data.frame(cbind(cropland,cropland_n2o))

all_annual_n2o <- as.data.frame(rbind(forest_annual_n2o,grassland_annual_n2o,cropland_annual_n2o))

names(all_annual_n2o) <- c("lon","lat","z","pft",paste0("year",c(1980:2016)))
csvfile <- paste("~/data/n2o_Yunke/forcing/LPX_annual_n2o.csv")
write_csv(all_annual_n2o, path = csvfile)


#forest
forest0 <- unique(subset(field_data,pft=="forest")[,c("lon","lat")])
forest1 <- unique(subset(co2_site_z,pft=="forest")[,c("lon","lat")])
forest2 <- unique(subset(warming_site_z,pft=="forest")[,c("lon","lat")])

forest <- as.data.frame(unique(rbind(forest0,forest1,forest2)))

#extract SH6's forest cover
#bands from 1-2052 are (months from 1850 to 2020)
#level from 1-15 are different pfts
(2020-1978+1)
2052-12*43+1
#we want pft 1-8/ pft 1-10, from the months (1537 to 2052) which are from the year 1978 to 2020

empty_data <- data.frame(matrix(NA)) 
seq_number <- seq(1537,2052,12) #only select one month of each year, since all others month of the year is the same! #already checked by doing experiment 

for (i in seq_number) {
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
  empty_data[1:nrow(forest),((i-1)/12-127)]<- forest_percentage
} 
forest_cover <- rowMeans(empty_data,na.rm=T)
hist(forest_cover)
final_forest_cover <- as.data.frame(cbind(forest,forest_cover))
#all values are higher than 82% (just one point less than 85%) - we can define threshold value as 81%
csvfile <- paste("~/data/n2o_Yunke/forcing/forestcover_site.csv")
write_csv(final_forest_cover, path = csvfile)
#only one site's forest cover <81% - removing them


#2. get moisture
#nc_open("/Volumes/My Passport/data/LPX/data/msl/LPX-Bern_SH6_msl.nc")
#raster("/Volumes/My Passport/data/LPX/data/msl/LPX-Bern_SH6_msl.nc")
#use first layer as it is 0-10cm
forest <- unique(subset(field_data,pft=="forest")[,c("lon","lat")])
empty_data <- data.frame(matrix(NA)) 

for (i in c(1537:2052)) {
  print(i)
  site_data <- SpatialPoints(forest[,c("lon","lat")]) # only select lon and lat
  pft1 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_msl.nc",
                                  band=i,level=1), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  empty_data[1:nrow(forest),(i-1536)]<- pft1
} 

moisture <- rowMeans(empty_data,na.rm=T)/100
forest$moisture <- moisture

#temperature
empty_data <- data.frame(matrix(NA)) 
(2020-1978+1) #43 years
length(c(129:171))

for (i in c(129:171)) {
  print(i)
  site_data <- SpatialPoints(forest[,c("lon","lat")]) # only select lon and lat
  pft1 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH1_tas.nc",
                                  band=i,level=1), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  pft2 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH1_tas.nc",
                                  band=i,level=2), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  pft3 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH1_tas.nc",
                                  band=i,level=3), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  pft4 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH1_tas.nc",
                                  band=i,level=4), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  pft5 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH1_tas.nc",
                                  band=i,level=5), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  pft6 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH1_tas.nc",
                                  band=i,level=6), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  pft7 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH1_tas.nc",
                                  band=i,level=7), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  pft8 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH1_tas.nc",
                                  band=i,level=8), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  pft9 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH1_tas.nc",
                                  band=i,level=9), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  pft10 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH1_tas.nc",
                                  band=i,level=10), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  pft11 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH1_tas.nc",
                                  band=i,level=11), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  pft12 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH1_tas.nc",
                                  band=i,level=12), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  pft_all <- as.data.frame(cbind(pft1,pft2,pft3,pft4,pft5,pft6,pft7,pft8,pft9,pft10,pft11,pft12))
  pft_all[pft_all<=0] <- NA
  temperature_averge <- rowMeans(pft_all,na.rm=T)
  empty_data[1:nrow(forest),(i-128)]<- temperature_averge
} 

growth_temperature <- rowMeans(empty_data,na.rm=T)
forest$growth_temperature <- growth_temperature

#forest n2o
a1 <- raster("/Users/yunpeng/data/LPX/data/LPX-Bern_SH6_fN2Opft_lu.nc",band=2052,level=1)
plot(a1*1000000000*730.49)
for (i in c(1537:2052)) {
  print(i)
  site_data <- SpatialPoints(forest[,c("lon","lat")]) # only select lon and lat
  pft1 <- (raster::extract(raster("/Users/yunpeng/data/LPX/data/LPX-Bern_SH6_fN2Opft_lu.nc",
                                  band=i,level=1), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest, by = c("lon", "lat")))[,1]
  empty_data[1:nrow(forest),(i-1536)]<- pft1
} 

forest_n2o <- rowMeans(empty_data,na.rm=T)*1000000000*730.49 #convert from kg_m2_month to ug_m2_h
forest$forest_n2o <- forest_n2o

forest_all <- merge(forest,final_forest_cover,by=c("lon","lat"),all.x=TRUE)

csvfile <- paste("~/data/n2o_Yunke/forcing/forest_field_lpx_output.csv")
write_csv(forest_all, path = csvfile)

#secondily, grassland
grassland <- unique(subset(field_data,pft=="grassland")[,c("lon","lat")])
dim(grassland)

raster("/Users/yunpeng/data/LPX/data/nfert_NMIP2022_1850-2021.nc")
#NH4CROP, NO3CROP, NH4PAST, NO3PAST
#band: 1-171
#Nfertilisation is during 1961-2020
#our measurement year is 1978-2020
#we take 1961-1990 as first 30 years
#we take 1991-2020 as last 30 years
#2020-1961+1 60 years
171-60+1
empty_data <- data.frame(matrix(NA)) 

for (i in c(112:171)) {
  print(i)
  site_data <- SpatialPoints(grassland[,c("lon","lat")]) # only select lon and lat
  pft1 <- (raster::extract(raster("~/data/LPX/data/nfert_NMIP2022_1850-2021.nc",
                                  band=i,varname="NH4PAST"), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(grassland, by = c("lon", "lat")))[,1]
  pft2 <- (raster::extract(raster("~/data/LPX/data/nfert_NMIP2022_1850-2021.nc",
                                  band=i,varname="NO3PAST"), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(grassland, by = c("lon", "lat")))[,1]
  pft_all <- pft1+pft2
  empty_data[1:nrow(grassland),(i-111)]<- pft_all
} 
dim(empty_data)
grassland$nfer_1 <- rowMeans(empty_data[,c(1:30)],na.rm=T)
grassland$nfer_2 <- rowMeans(empty_data[,c(31:60)],na.rm=T)

#cropland n2o
nc_open("/Users/yunpeng/data/LPX/data/LPX-Bern_SH6_fN2Opft_lu.nc")
#3. grassland (pasture)

empty_data <- data.frame(matrix(NA)) 
2052-60*12+1

for (i in c(1333:2052)) {
  print(i)
  site_data <- SpatialPoints(grassland[,c("lon","lat")]) # only select lon and lat
  pft1 <- (raster::extract(raster("/Users/yunpeng/data/LPX/data/LPX-Bern_SH6_fN2Opft_lu.nc",
                                  band=i,level=3), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(grassland, by = c("lon", "lat")))[,1]
  empty_data[1:nrow(grassland),(i-1332)]<- pft1
} 
n2o_grassland <- empty_data
#convert from kg_m2_month to ug_m2_h
grassland$n2o_1 <- rowMeans(n2o_grassland[,c(1:360)],na.rm=T)*1000000000*730.49
grassland$n2o_2 <- rowMeans(n2o_grassland[,c(361:720)],na.rm=T)*1000000000*730.49

#now, min fapar
#lai(pft-based) - can just use level= 1 (no difference between levels). But need to get measurement year. And then get fapar
#get lai, only level-1 is fine (pft-1 is the same as all other pfts, as tested)
raster("/Users/yunpeng/data/LPX/data/LPX-Bern_SH6_laipft.nc")
empty_data <- data.frame(matrix(NA)) 
a1 <- raster("/Users/yunpeng/data/LPX/data/LPX-Bern_SH6_laipft.nc")
plot(a1,band=2052,level=4)

for (i in c(1333:2052)) {
  print(i)
  site_data <- SpatialPoints(grassland[,c("lon","lat")]) # only select lon and lat
  pft1 <- (raster::extract(raster("/Users/yunpeng/data/LPX/data/LPX-Bern_SH6_laipft.nc",
                                  band=i,level=1), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(grassland, by = c("lon", "lat")))[,1]
  empty_data[1:nrow(grassland),(i-1332)]<- pft1
} 
fapar <- 1-exp(-0.5 * empty_data)

grassland$min_fapar_1 <- apply(fapar[,c(1:360)], 1, FUN = min, na.rm = TRUE)
grassland$min_fapar_2 <-  apply(fapar[,c(361:720)], 1, FUN = min, na.rm = TRUE)
grassland[sapply(grassland, is.infinite)] <- NA
grassland[sapply(grassland, is.nan)] <- NA

csvfile <- paste("~/data/n2o_Yunke/forcing/grassland_field_lpx_output.csv")
write_csv(grassland, path = csvfile)

for (i in c(1572:2052)) {
  print(i)
  site_data <- SpatialPoints(cropland[,c("lon","lat")]) # only select lon and lat
  pft1 <- (raster::extract(raster("/Volumes/My Passport/data/LPX/data/rsds/LPX-Bern_SH6_rsds.nc",
                                  band=i,level=1), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(cropland, by = c("lon", "lat")))[,1]
  empty_data[1:nrow(cropland),(i-1571)]<- pft1
} 

radiation_w_m2 <- rowMeans(empty_data,na.rm=T)
cropland$radiation_w_m2 <- radiation_w_m2
#growing season???

#lai(pft-based) - can just use level= 1 (no difference between levels). But need to get measurement year. And then get fapar
#get lai, only level-1 is fine (pft-1 is the same as all other pfts, as tested)

raster("/Volumes/My Passport/data/LPX/data/lai/LPX-Bern_SH6_laipft.nc")
nc_open("/Volumes/My Passport/data/LPX/data/lai/LPX-Bern_SH6_laipft.nc")

cropland <- unique(subset(field_data,pft=="cropland")[,c("lon","lat")])
grassland <- unique(subset(field_data,pft=="grassland")[,c("lon","lat")])
grassland_cropland <- as.data.frame(rbind(grassland,cropland))

empty_data <- data.frame(matrix(NA)) 

for (i in c(1572:2052)) {
  print(i)
  site_data <- SpatialPoints(cropland[,c("lon","lat")]) # only select lon and lat
  pft1 <- (raster::extract(raster("/Volumes/My Passport/data/LPX/data/rsds/LPX-Bern_SH6_rsds.nc",
                                  band=i,level=1), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(cropland, by = c("lon", "lat")))[,1]
  empty_data[1:nrow(cropland),(i-1571)]<- pft1
} 


