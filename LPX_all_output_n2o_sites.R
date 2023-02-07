library(terra)
library(stringr)
library(rgdal)
library(raster)
library(dplyr)
library(readr)
library(ncdf4)
#read all field-data that includes n2o
df_all <- read.csv("/Users/yunpeng/data/n2o_Yunke/forcing/lpx_sites_field_all_n2o.csv")
summary(df_all) #values range from 1980 to 2016

df_all$n2o <- NA

unique(df_all$pft)
forest <- subset(df_all,pft=="forest")
grassland <- subset(df_all,pft=="grassland")
cropland <- subset(df_all,pft=="cropland")


#1. forest cover --> all getting forest from 1980-2016
empty_data <- data.frame(matrix(NA)) 

forest_unique <- unique(forest[,c("lon","lat")])
#extract SH6's forest cover
#1980-2016
for (i in c(1561:2004)) {
  print(i)
  site_data <- SpatialPoints(forest_unique[,c("lon","lat")]) # only select lon and lat
  pft1 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_landCoverFrac.nc",
                                  band=i,level=1), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest_unique, by = c("lon", "lat")))[,1]
  pft2 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_landCoverFrac.nc",
                                  band=i,level=2), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest_unique, by = c("lon", "lat")))[,1]
  pft3 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_landCoverFrac.nc",
                                  band=i,level=3), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest_unique, by = c("lon", "lat")))[,1]
  pft4 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_landCoverFrac.nc",
                                  band=i,level=4), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest_unique, by = c("lon", "lat")))[,1]
  pft5 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_landCoverFrac.nc",
                                  band=i,level=5), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest_unique, by = c("lon", "lat")))[,1]
  pft6 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_landCoverFrac.nc",
                                  band=i,level=6), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest_unique, by = c("lon", "lat")))[,1]
  pft7 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_landCoverFrac.nc",
                                  band=i,level=7), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest_unique, by = c("lon", "lat")))[,1]
  pft8 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_landCoverFrac.nc",
                                  band=i,level=8), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest_unique, by = c("lon", "lat")))[,1]
  pft9 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_landCoverFrac.nc",
                                  band=i,level=9), site_data, sp = TRUE) %>%
             as_tibble() %>% right_join(forest_unique, by = c("lon", "lat")))[,1]
  pft10 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_landCoverFrac.nc",
                                   band=i,level=10), site_data, sp = TRUE) %>%
              as_tibble() %>% right_join(forest_unique, by = c("lon", "lat")))[,1]
  forest_percentage <- (pft1+pft2+pft3+pft4+pft5+pft6+pft7+pft8)/(pft1+pft2+pft3+pft4+pft5+pft6+pft7+pft8+pft9+pft10)
  empty_data[1:nrow(forest_unique),i-1560]<- forest_percentage
} 

forest_cover <- rowMeans(empty_data,na.rm=T)
final_forest_cover <- as.data.frame(cbind(forest_unique,forest_cover))

csvfile <- paste("~/data/n2o_Yunke/forcing/forestcover_site_alln2o.csv")
write_csv(final_forest_cover, path = csvfile)




#2. ingest forest N2O
#1-2052 level is monthly data from 1850 to 2020
#(1:Natural; 2:Cropland; 3:Pasture; 4:Urban)

for (i in 1:nrow(forest)) {
  print(i)
  site_data <- SpatialPoints(forest[i,c("lon","lat")])
  start_yr <- forest[i,c("start_yr")]
  end_yr <- forest[i,c("end_yr")]
  
  #now, select bands from all these months
  start_month <- 2052-12*(2020-start_yr+1)+1
  
  end_month <- 2052-12*(2020-end_yr)
  
  empty_data <- NA
  for (a in start_month:end_month) {
  a1 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_fN2Opft_lu.nc",
                                 band=a,level=1), site_data, sp = TRUE)%>%
    as_tibble())[,1]
  empty_data <- rbind(empty_data, a1)
  } 
  forest[i,c("n2o")] <- colMeans(empty_data,na.rm=T)*1000000000*3600 #convert from kg_m2_s to ug_m2_h
}
  
csvfile <- paste("~/data/n2o_Yunke/forcing/LPX_years_forest_n2o.csv")
write_csv(forest, path = csvfile)

for (i in 1:nrow(cropland)) {
  print(i)
  site_data <- SpatialPoints(cropland[i,c("lon","lat")])
  start_yr <- cropland[i,c("start_yr")]
  end_yr <- cropland[i,c("end_yr")]
  
  #now, select bands from all these months
  start_month <- 2052-12*(2020-start_yr+1)+1
  
  end_month <- 2052-12*(2020-end_yr)
  
  empty_data <- NA
  for (a in start_month:end_month) {
    a1 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_fN2Opft_lu.nc",
                                  band=a,level=2), site_data, sp = TRUE)%>%
             as_tibble())[,1]
    empty_data <- rbind(empty_data, a1)
  } 
  cropland[i,c("n2o")] <- colMeans(empty_data,na.rm=T)*1000000000*3600 #convert from kg_m2_s to ug_m2_h
}

csvfile <- paste("~/data/n2o_Yunke/forcing/LPX_years_cropland_n2o.csv")
write_csv(cropland, path = csvfile)

for (i in 1:nrow(grassland)) {
  print(i)
  site_data <- SpatialPoints(grassland[i,c("lon","lat")])
  start_yr <- grassland[i,c("start_yr")]
  end_yr <- grassland[i,c("end_yr")]
  
  #now, select bands from all these months
  start_month <- 2052-12*(2020-start_yr+1)+1
  
  end_month <- 2052-12*(2020-end_yr)
  
  empty_data <- NA
  for (a in start_month:end_month) {
    a1 <- (raster::extract(raster("~/data/LPX/data/LPX-Bern_SH6_fN2Opft_lu.nc",
                                  band=a,level=3), site_data, sp = TRUE)%>%
             as_tibble())[,1]
    empty_data <- rbind(empty_data, a1)
  } 
  grassland[i,c("n2o")] <- colMeans(empty_data,na.rm=T)*1000000000*3600 #convert from kg_m2_s to ug_m2_h
}

csvfile <- paste("~/data/n2o_Yunke/forcing/LPX_years_grassland_n2o.csv")
write_csv(grassland, path = csvfile)

final <- as.data.frame(rbind(forest,grassland,cropland))
csvfile <- paste("~/data/n2o_Yunke/forcing/LPX_years_all_n2o.csv")
write_csv(final, path = csvfile)