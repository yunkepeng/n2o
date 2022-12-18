library(ncdf4)
library(dplyr)

devtools::load_all("/Users/yunpeng/yunkepeng/latest_packages/rbeni/") 

#n fertilisation process
library(ncdf4)
library(dplyr)

devtools::load_all("/Users/yunpeng/yunkepeng/latest_packages/rbeni/") 
ncin <- nc_open("/Volumes/My Passport/data/LPX/LPX_NMIP/NMIP/nfert_NMIP2022_1850-2021.nc")
ncin
lon <- ncvar_get(ncin,"LON")
nlon <- dim(lon) 

lat<-ncvar_get(ncin,"LAT")
nlat <- dim(lat)
lonlat <- expand.grid(lon, lat)

NH4CROP <- ncvar_get(ncin,"NH4CROP")
NO3CROP <- ncvar_get(ncin,"NO3CROP")
NH4PAST <- ncvar_get(ncin,"NH4PAST")
NO3PAST <- ncvar_get(ncin,"NO3PAST")
nc_close(ncin)

NH4CROP <- as.vector(NH4CROP)
NO3CROP <- as.vector(NO3CROP)
NH4PAST <- as.vector(NH4PAST)
NO3PAST <- as.vector(NO3PAST)

length(NO3CROP)
720*360*171
#using year from 1978-2020 (which covers measurement year)

NH4CROP2 <- matrix(NH4CROP, nrow = nlon * nlat, ncol = 171) 
rm(NH4CROP)
NO3CROP2 <- matrix(NO3CROP, nrow = nlon * nlat, ncol = 171) 
rm(NO3CROP)
NH4PAST2 <- matrix(NH4PAST, nrow = nlon * nlat, ncol = 171) 
rm(NH4PAST)
NO3PAST2 <- matrix(NO3PAST, nrow = nlon * nlat, ncol = 171) 
rm(NO3PAST)

#171 is the year 2020, 129 is the year 1978
cropland_nfer <- rowMeans( (NH4CROP2[,c(129:171)]+NO3CROP2[,c(129:171)]),na.rm = TRUE)

grassland_nfer <- rowMeans( (NH4PAST2[,c(129:171)]+NO3PAST2[,c(129:171)]),na.rm = TRUE)

cropland_map <- as.data.frame(cbind(lonlat,cropland_nfer))
grassland_map <- as.data.frame(cbind(lonlat,grassland_nfer))

names(cropland_map) <- c("lon","lat","nfer")
names(grassland_map) <- c("lon","lat","nfer")

cropland_map$nfer[cropland_map$nfer == "NaN"] <- NA
grassland_map$nfer[grassland_map$nfer == "NaN"] <- NA

plot_map3(cropland_map) # unit in gN/m2/yr, after checking with /Users/yunpeng/data/LPX/document/1520-0477-bams-d-17-0212.1 (1).pdf

cropland_nc <- list(df_to_grid(cropland_map,varnam = "nfer", 
                               lonnam = "lon", latnam = "lat"))
names(cropland_nc) <- "nfer"
varams = "nfer"
test <- list(lon,lat,cropland_nc,varams)
names(test) <- c("lon","lat","vars","varams")
write_nc2(test,varnams = "nfer",long_name = "nfer",units = "/",
          path = "~/data/LPX/output/nfer_cropland_1978_2020.nc")

grassland_nc <- list(df_to_grid(grassland_map,varnam = "nfer", 
                                lonnam = "lon", latnam = "lat"))
names(grassland_nc) <- "nfer"
varams = "nfer"
test <- list(lon,lat,grassland_nc,varams)
names(test) <- c("lon","lat","vars","varams")
write_nc2(test,varnams = "nfer",long_name = "nfer",units = "/",
          path = "~/data/LPX/output/nfer_grassland_1978_2020.nc")


#n2o (SH0, 1, 3, 6, 10)
###SH0
ncin <- nc_open("/Volumes/My Passport/data/LPX/LPX_NMIP/NMIP/SH0/LPX-Bern_SH0_fN2Opft_lu.nc")
ncin
lon <- ncvar_get(ncin,"longitude")
nlon <- dim(lon) 

lat<-ncvar_get(ncin,"latitude")
nlat <- dim(lat)
lonlat <- expand.grid(lon, lat)

n2o <- ncvar_get(ncin,"fN2Opft_lu")
nc_close(ncin)

n2o_long <- as.vector(n2o)
length(n2o_long)
720*360*4*2052
#using year from 1978-2020 (which covers measurement year)

n2o_long2 <- matrix(n2o_long, nrow = nlon * nlat, ncol = 2052*4) # 2052 is 2052 months * 4-pft
rm(n2o)
rm(n2o_long)
#2052*4 is (2020-1850+1)*12*4


#8208 is 2020/12-urban
#create a function to extract years
#no: 1 = natural (forest), 2=cropland, 3= pasture (grassland), 4=urban
#veg change first, then time
output_nc <- function(obj,no,start_year,end_year){
  all_months <- seq(no,8208,4)
  first_time <- (start_year-1849)*12-11
  last_time <- (end_year-1849)*12
  selected_column<- all_months[first_time:last_time] 
  return(obj[,selected_column])
}

#the sequence 
forest_all <- output_nc(n2o_long2,1,1978,2020)
cropland_all <- output_nc(n2o_long2,2,1978,2020)
grassland_all <- output_nc(n2o_long2,3,1978,2020)
urban_all <- output_nc(n2o_long2,4,1978,2020)

forest_mean <- rowMeans(forest_all,na.rm = TRUE)*31556952*1000 #convert from kg/m2/s to g/m2/yr
cropland_mean <- rowMeans(cropland_all,na.rm = TRUE)*31556952*1000 #convert from kg/m2/s to g/m2/yr
grassland_mean <- rowMeans(grassland_all,na.rm = TRUE)*31556952*1000 #convert from kg/m2/s to g/m2/yr
urban_mean <- rowMeans(urban_all,na.rm = TRUE)*31556952*1000 #convert from kg/m2/s to g/m2/yr


forest_map <- as.data.frame(cbind(lonlat,forest_mean))
cropland_map <- as.data.frame(cbind(lonlat,cropland_mean))
grassland_map <- as.data.frame(cbind(lonlat,grassland_mean))
urban_map <- as.data.frame(cbind(lonlat,urban_mean))

names(forest_map) <- c("lon","lat","n2o")
names(cropland_map) <- c("lon","lat","n2o")
names(grassland_map) <- c("lon","lat","n2o")
names(urban_map) <- c("lon","lat","n2o")

forest_map$n2o[forest_map$n2o == "NaN"] <- NA
cropland_map$n2o[cropland_map$n2o == "NaN"] <- NA
grassland_map$n2o[grassland_map$n2o == "NaN"] <- NA
urban_map$n2o[urban_map$n2o == "NaN"] <- NA

forest_nc <- list(df_to_grid(forest_map,varnam = "n2o", 
                             lonnam = "lon", latnam = "lat"))
names(forest_nc) <- "n2o"
varams = "n2o"
test <- list(lon,lat,forest_nc,varams)
names(test) <- c("lon","lat","vars","varams")
write_nc2(test,varnams = "n2o",long_name = "n2o",units = "g_m2_yr",
          path = "~/data/LPX/output/forest_SH0_n2o_1978_2020.nc")

cropland_nc <- list(df_to_grid(cropland_map,varnam = "n2o", 
                             lonnam = "lon", latnam = "lat"))
names(cropland_nc) <- "n2o"
varams = "n2o"
test <- list(lon,lat,cropland_nc,varams)
names(test) <- c("lon","lat","vars","varams")
write_nc2(test,varnams = "n2o",long_name = "n2o",units = "g_m2_yr",
          path = "~/data/LPX/output/cropland_SH0_n2o_1978_2020.nc")

grassland_nc <- list(df_to_grid(grassland_map,varnam = "n2o", 
                               lonnam = "lon", latnam = "lat"))
names(grassland_nc) <- "n2o"
varams = "n2o"
test <- list(lon,lat,grassland_nc,varams)
names(test) <- c("lon","lat","vars","varams")
write_nc2(test,varnams = "n2o",long_name = "n2o",units = "g_m2_yr",
          path = "~/data/LPX/output/grassland_SH0_n2o_1978_2020.nc")

urban_nc <- list(df_to_grid(urban_map,varnam = "n2o", 
                                lonnam = "lon", latnam = "lat"))
names(urban_nc) <- "n2o"
varams = "n2o"
test <- list(lon,lat,urban_nc,varams)
names(test) <- c("lon","lat","vars","varams")
write_nc2(test,varnams = "n2o",long_name = "n2o",units = "g_m2_yr",
          path = "~/data/LPX/output/urban_SH0_n2o_1978_2020.nc")

###SH1
ncin <- nc_open("/Volumes/My Passport/data/LPX/LPX_NMIP/NMIP/SH1/LPX-Bern_SH1_fN2Opft_lu.nc")
ncin
lon <- ncvar_get(ncin,"longitude")
nlon <- dim(lon) 

lat<-ncvar_get(ncin,"latitude")
nlat <- dim(lat)
lonlat <- expand.grid(lon, lat)

n2o <- ncvar_get(ncin,"fN2Opft_lu")
nc_close(ncin)

n2o_long <- as.vector(n2o)
length(n2o_long)
720*360*4*2052
#using year from 1978-2020 (which covers measurement year)

n2o_long2 <- matrix(n2o_long, nrow = nlon * nlat, ncol = 2052*4) # 2052 is 2052 months * 4-pft
rm(n2o)
rm(n2o_long)
#2052*4 is (2020-1850+1)*12*4

#the sequence 
forest_all <- output_nc(n2o_long2,1,1978,2020)
cropland_all <- output_nc(n2o_long2,2,1978,2020)
grassland_all <- output_nc(n2o_long2,3,1978,2020)
urban_all <- output_nc(n2o_long2,4,1978,2020)

forest_mean <- rowMeans(forest_all,na.rm = TRUE)*31556952*1000 #convert from kg/m2/s to g/m2/yr
cropland_mean <- rowMeans(cropland_all,na.rm = TRUE)*31556952*1000 #convert from kg/m2/s to g/m2/yr
grassland_mean <- rowMeans(grassland_all,na.rm = TRUE)*31556952*1000 #convert from kg/m2/s to g/m2/yr
urban_mean <- rowMeans(urban_all,na.rm = TRUE)*31556952*1000 #convert from kg/m2/s to g/m2/yr


forest_map <- as.data.frame(cbind(lonlat,forest_mean))
cropland_map <- as.data.frame(cbind(lonlat,cropland_mean))
grassland_map <- as.data.frame(cbind(lonlat,grassland_mean))
urban_map <- as.data.frame(cbind(lonlat,urban_mean))

names(forest_map) <- c("lon","lat","n2o")
names(cropland_map) <- c("lon","lat","n2o")
names(grassland_map) <- c("lon","lat","n2o")
names(urban_map) <- c("lon","lat","n2o")

forest_map$n2o[forest_map$n2o == "NaN"] <- NA
cropland_map$n2o[cropland_map$n2o == "NaN"] <- NA
grassland_map$n2o[grassland_map$n2o == "NaN"] <- NA
urban_map$n2o[urban_map$n2o == "NaN"] <- NA

forest_nc <- list(df_to_grid(forest_map,varnam = "n2o", 
                             lonnam = "lon", latnam = "lat"))
names(forest_nc) <- "n2o"
varams = "n2o"
test <- list(lon,lat,forest_nc,varams)
names(test) <- c("lon","lat","vars","varams")
write_nc2(test,varnams = "n2o",long_name = "n2o",units = "g_m2_yr",
          path = "~/data/LPX/output/forest_SH1_n2o_1978_2020.nc")

cropland_nc <- list(df_to_grid(cropland_map,varnam = "n2o", 
                               lonnam = "lon", latnam = "lat"))
names(cropland_nc) <- "n2o"
varams = "n2o"
test <- list(lon,lat,cropland_nc,varams)
names(test) <- c("lon","lat","vars","varams")
write_nc2(test,varnams = "n2o",long_name = "n2o",units = "g_m2_yr",
          path = "~/data/LPX/output/cropland_SH1_n2o_1978_2020.nc")

grassland_nc <- list(df_to_grid(grassland_map,varnam = "n2o", 
                                lonnam = "lon", latnam = "lat"))
names(grassland_nc) <- "n2o"
varams = "n2o"
test <- list(lon,lat,grassland_nc,varams)
names(test) <- c("lon","lat","vars","varams")
write_nc2(test,varnams = "n2o",long_name = "n2o",units = "g_m2_yr",
          path = "~/data/LPX/output/grassland_SH1_n2o_1978_2020.nc")

urban_nc <- list(df_to_grid(urban_map,varnam = "n2o", 
                            lonnam = "lon", latnam = "lat"))
names(urban_nc) <- "n2o"
varams = "n2o"
test <- list(lon,lat,urban_nc,varams)
names(test) <- c("lon","lat","vars","varams")
write_nc2(test,varnams = "n2o",long_name = "n2o",units = "g_m2_yr",
          path = "~/data/LPX/output/urban_SH1_n2o_1978_2020.nc")


###SH3
ncin <- nc_open("/Volumes/My Passport/data/LPX/LPX_NMIP/NMIP/SH3/LPX-Bern_SH3_fN2Opft_lu.nc")
ncin
lon <- ncvar_get(ncin,"longitude")
nlon <- dim(lon) 

lat<-ncvar_get(ncin,"latitude")
nlat <- dim(lat)
lonlat <- expand.grid(lon, lat)

n2o <- ncvar_get(ncin,"fN2Opft_lu")
nc_close(ncin)

n2o_long <- as.vector(n2o)
length(n2o_long)
720*360*4*2052
#using year from 1978-2020 (which covers measurement year)

n2o_long2 <- matrix(n2o_long, nrow = nlon * nlat, ncol = 2052*4) # 2052 is 2052 months * 4-pft
rm(n2o)
rm(n2o_long)
#2052*4 is (2020-1850+1)*12*4

#the sequence 
forest_all <- output_nc(n2o_long2,1,1978,2020)
cropland_all <- output_nc(n2o_long2,2,1978,2020)
grassland_all <- output_nc(n2o_long2,3,1978,2020)
urban_all <- output_nc(n2o_long2,4,1978,2020)

forest_mean <- rowMeans(forest_all,na.rm = TRUE)*31556952*1000 #convert from kg/m2/s to g/m2/yr
cropland_mean <- rowMeans(cropland_all,na.rm = TRUE)*31556952*1000 #convert from kg/m2/s to g/m2/yr
grassland_mean <- rowMeans(grassland_all,na.rm = TRUE)*31556952*1000 #convert from kg/m2/s to g/m2/yr
urban_mean <- rowMeans(urban_all,na.rm = TRUE)*31556952*1000 #convert from kg/m2/s to g/m2/yr


forest_map <- as.data.frame(cbind(lonlat,forest_mean))
cropland_map <- as.data.frame(cbind(lonlat,cropland_mean))
grassland_map <- as.data.frame(cbind(lonlat,grassland_mean))
urban_map <- as.data.frame(cbind(lonlat,urban_mean))

names(forest_map) <- c("lon","lat","n2o")
names(cropland_map) <- c("lon","lat","n2o")
names(grassland_map) <- c("lon","lat","n2o")
names(urban_map) <- c("lon","lat","n2o")

forest_map$n2o[forest_map$n2o == "NaN"] <- NA
cropland_map$n2o[cropland_map$n2o == "NaN"] <- NA
grassland_map$n2o[grassland_map$n2o == "NaN"] <- NA
urban_map$n2o[urban_map$n2o == "NaN"] <- NA

forest_nc <- list(df_to_grid(forest_map,varnam = "n2o", 
                             lonnam = "lon", latnam = "lat"))
names(forest_nc) <- "n2o"
varams = "n2o"
test <- list(lon,lat,forest_nc,varams)
names(test) <- c("lon","lat","vars","varams")
write_nc2(test,varnams = "n2o",long_name = "n2o",units = "g_m2_yr",
          path = "~/data/LPX/output/forest_SH3_n2o_1978_2020.nc")

cropland_nc <- list(df_to_grid(cropland_map,varnam = "n2o", 
                               lonnam = "lon", latnam = "lat"))
names(cropland_nc) <- "n2o"
varams = "n2o"
test <- list(lon,lat,cropland_nc,varams)
names(test) <- c("lon","lat","vars","varams")
write_nc2(test,varnams = "n2o",long_name = "n2o",units = "g_m2_yr",
          path = "~/data/LPX/output/cropland_SH3_n2o_1978_2020.nc")

grassland_nc <- list(df_to_grid(grassland_map,varnam = "n2o", 
                                lonnam = "lon", latnam = "lat"))
names(grassland_nc) <- "n2o"
varams = "n2o"
test <- list(lon,lat,grassland_nc,varams)
names(test) <- c("lon","lat","vars","varams")
write_nc2(test,varnams = "n2o",long_name = "n2o",units = "g_m2_yr",
          path = "~/data/LPX/output/grassland_SH3_n2o_1978_2020.nc")

urban_nc <- list(df_to_grid(urban_map,varnam = "n2o", 
                            lonnam = "lon", latnam = "lat"))
names(urban_nc) <- "n2o"
varams = "n2o"
test <- list(lon,lat,urban_nc,varams)
names(test) <- c("lon","lat","vars","varams")
write_nc2(test,varnams = "n2o",long_name = "n2o",units = "g_m2_yr",
          path = "~/data/LPX/output/urban_SH3_n2o_1978_2020.nc")


###SH6
ncin <- nc_open("/Volumes/My Passport/data/LPX/LPX_NMIP/NMIP/SH6/LPX-Bern_SH6_fN2Opft_lu.nc")
ncin
lon <- ncvar_get(ncin,"longitude")
nlon <- dim(lon) 

lat<-ncvar_get(ncin,"latitude")
nlat <- dim(lat)
lonlat <- expand.grid(lon, lat)

n2o <- ncvar_get(ncin,"fN2Opft_lu")
nc_close(ncin)

n2o_long <- as.vector(n2o)
length(n2o_long)
720*360*4*2052
#using year from 1978-2020 (which covers measurement year)

n2o_long2 <- matrix(n2o_long, nrow = nlon * nlat, ncol = 2052*4) # 2052 is 2052 months * 4-pft
rm(n2o)
rm(n2o_long)
#2052*4 is (2020-1850+1)*12*4

#the sequence 
forest_all <- output_nc(n2o_long2,1,1978,2020)
cropland_all <- output_nc(n2o_long2,2,1978,2020)
grassland_all <- output_nc(n2o_long2,3,1978,2020)
urban_all <- output_nc(n2o_long2,4,1978,2020)

forest_mean <- rowMeans(forest_all,na.rm = TRUE)*31556952*1000 #convert from kg/m2/s to g/m2/yr
cropland_mean <- rowMeans(cropland_all,na.rm = TRUE)*31556952*1000 #convert from kg/m2/s to g/m2/yr
grassland_mean <- rowMeans(grassland_all,na.rm = TRUE)*31556952*1000 #convert from kg/m2/s to g/m2/yr
urban_mean <- rowMeans(urban_all,na.rm = TRUE)*31556952*1000 #convert from kg/m2/s to g/m2/yr


forest_map <- as.data.frame(cbind(lonlat,forest_mean))
cropland_map <- as.data.frame(cbind(lonlat,cropland_mean))
grassland_map <- as.data.frame(cbind(lonlat,grassland_mean))
urban_map <- as.data.frame(cbind(lonlat,urban_mean))

names(forest_map) <- c("lon","lat","n2o")
names(cropland_map) <- c("lon","lat","n2o")
names(grassland_map) <- c("lon","lat","n2o")
names(urban_map) <- c("lon","lat","n2o")

forest_map$n2o[forest_map$n2o == "NaN"] <- NA
cropland_map$n2o[cropland_map$n2o == "NaN"] <- NA
grassland_map$n2o[grassland_map$n2o == "NaN"] <- NA
urban_map$n2o[urban_map$n2o == "NaN"] <- NA

forest_nc <- list(df_to_grid(forest_map,varnam = "n2o", 
                             lonnam = "lon", latnam = "lat"))
names(forest_nc) <- "n2o"
varams = "n2o"
test <- list(lon,lat,forest_nc,varams)
names(test) <- c("lon","lat","vars","varams")
write_nc2(test,varnams = "n2o",long_name = "n2o",units = "g_m2_yr",
          path = "~/data/LPX/output/forest_SH6_n2o_1978_2020.nc")

cropland_nc <- list(df_to_grid(cropland_map,varnam = "n2o", 
                               lonnam = "lon", latnam = "lat"))
names(cropland_nc) <- "n2o"
varams = "n2o"
test <- list(lon,lat,cropland_nc,varams)
names(test) <- c("lon","lat","vars","varams")
write_nc2(test,varnams = "n2o",long_name = "n2o",units = "g_m2_yr",
          path = "~/data/LPX/output/cropland_SH6_n2o_1978_2020.nc")

grassland_nc <- list(df_to_grid(grassland_map,varnam = "n2o", 
                                lonnam = "lon", latnam = "lat"))
names(grassland_nc) <- "n2o"
varams = "n2o"
test <- list(lon,lat,grassland_nc,varams)
names(test) <- c("lon","lat","vars","varams")
write_nc2(test,varnams = "n2o",long_name = "n2o",units = "g_m2_yr",
          path = "~/data/LPX/output/grassland_SH6_n2o_1978_2020.nc")

urban_nc <- list(df_to_grid(urban_map,varnam = "n2o", 
                            lonnam = "lon", latnam = "lat"))
names(urban_nc) <- "n2o"
varams = "n2o"
test <- list(lon,lat,urban_nc,varams)
names(test) <- c("lon","lat","vars","varams")
write_nc2(test,varnams = "n2o",long_name = "n2o",units = "g_m2_yr",
          path = "~/data/LPX/output/urban_SH6_n2o_1978_2020.nc")


###SH10
ncin <- nc_open("/Volumes/My Passport/data/LPX/LPX_NMIP/NMIP/SH10/LPX-Bern_SH10_fN2Opft_lu.nc")
ncin
lon <- ncvar_get(ncin,"longitude")
nlon <- dim(lon) 

lat<-ncvar_get(ncin,"latitude")
nlat <- dim(lat)
lonlat <- expand.grid(lon, lat)

n2o <- ncvar_get(ncin,"fN2Opft_lu")
nc_close(ncin)

n2o_long <- as.vector(n2o)
length(n2o_long)
720*360*4*2052
#using year from 1978-2020 (which covers measurement year)

n2o_long2 <- matrix(n2o_long, nrow = nlon * nlat, ncol = 2052*4) # 2052 is 2052 months * 4-pft
rm(n2o)
rm(n2o_long)
#2052*4 is (2020-1850+1)*12*4

#the sequence 
forest_all <- output_nc(n2o_long2,1,1978,2020)
cropland_all <- output_nc(n2o_long2,2,1978,2020)
grassland_all <- output_nc(n2o_long2,3,1978,2020)
urban_all <- output_nc(n2o_long2,4,1978,2020)

forest_mean <- rowMeans(forest_all,na.rm = TRUE)*31556952*1000 #convert from kg/m2/s to g/m2/yr
cropland_mean <- rowMeans(cropland_all,na.rm = TRUE)*31556952*1000 #convert from kg/m2/s to g/m2/yr
grassland_mean <- rowMeans(grassland_all,na.rm = TRUE)*31556952*1000 #convert from kg/m2/s to g/m2/yr
urban_mean <- rowMeans(urban_all,na.rm = TRUE)*31556952*1000 #convert from kg/m2/s to g/m2/yr


forest_map <- as.data.frame(cbind(lonlat,forest_mean))
cropland_map <- as.data.frame(cbind(lonlat,cropland_mean))
grassland_map <- as.data.frame(cbind(lonlat,grassland_mean))
urban_map <- as.data.frame(cbind(lonlat,urban_mean))

names(forest_map) <- c("lon","lat","n2o")
names(cropland_map) <- c("lon","lat","n2o")
names(grassland_map) <- c("lon","lat","n2o")
names(urban_map) <- c("lon","lat","n2o")

forest_map$n2o[forest_map$n2o == "NaN"] <- NA
cropland_map$n2o[cropland_map$n2o == "NaN"] <- NA
grassland_map$n2o[grassland_map$n2o == "NaN"] <- NA
urban_map$n2o[urban_map$n2o == "NaN"] <- NA

forest_nc <- list(df_to_grid(forest_map,varnam = "n2o", 
                             lonnam = "lon", latnam = "lat"))
names(forest_nc) <- "n2o"
varams = "n2o"
test <- list(lon,lat,forest_nc,varams)
names(test) <- c("lon","lat","vars","varams")
write_nc2(test,varnams = "n2o",long_name = "n2o",units = "g_m2_yr",
          path = "~/data/LPX/output/forest_SH10_n2o_1978_2020.nc")

cropland_nc <- list(df_to_grid(cropland_map,varnam = "n2o", 
                               lonnam = "lon", latnam = "lat"))
names(cropland_nc) <- "n2o"
varams = "n2o"
test <- list(lon,lat,cropland_nc,varams)
names(test) <- c("lon","lat","vars","varams")
write_nc2(test,varnams = "n2o",long_name = "n2o",units = "g_m2_yr",
          path = "~/data/LPX/output/cropland_SH10_n2o_1978_2020.nc")

grassland_nc <- list(df_to_grid(grassland_map,varnam = "n2o", 
                                lonnam = "lon", latnam = "lat"))
names(grassland_nc) <- "n2o"
varams = "n2o"
test <- list(lon,lat,grassland_nc,varams)
names(test) <- c("lon","lat","vars","varams")
write_nc2(test,varnams = "n2o",long_name = "n2o",units = "g_m2_yr",
          path = "~/data/LPX/output/grassland_SH10_n2o_1978_2020.nc")

urban_nc <- list(df_to_grid(urban_map,varnam = "n2o", 
                            lonnam = "lon", latnam = "lat"))
names(urban_nc) <- "n2o"
varams = "n2o"
test <- list(lon,lat,urban_nc,varams)
names(test) <- c("lon","lat","vars","varams")
write_nc2(test,varnams = "n2o",long_name = "n2o",units = "g_m2_yr",
          path = "~/data/LPX/output/urban_SH10_n2o_1978_2020.nc")


#csoil
###SH0
ncin <- nc_open("/Volumes/My Passport/data/LPX/LPX_NMIP/NMIP/SH0/LPX-Bern_SH0_cSoil.nc")
ncin
lon <- ncvar_get(ncin,"longitude")
nlon <- dim(lon) 

lat<-ncvar_get(ncin,"latitude")
nlat <- dim(lat)
lonlat <- expand.grid(lon, lat)

cSoil <- ncvar_get(ncin,"cSoil")
nc_close(ncin)

cSoil_long <- as.vector(cSoil)
length(cSoil_long)
720*360*171 # years
#using year from 1978-2020 (which covers measurement year)

cSoil_long2 <- matrix(cSoil_long, nrow = nlon * nlat, ncol = 171) # 2052 is 2052 months * 4-pft
rm(cSoil)
rm(cSoil_long)

#171 is the year 2021, 129 is the year 1978
Csoil_mean <- rowMeans(cSoil_long2[,c(129:171)],na.rm = TRUE) #convert from kg/m2/s to g/m2/yr
summary(Csoil_mean) # Csoil

Csoil_map <- as.data.frame(cbind(lonlat,Csoil_mean))

names(Csoil_map) <- c("lon","lat","csoil")

Csoil_map$csoil[Csoil_map$csoil == "NaN"] <- NA

csoil_nc <- list(df_to_grid(Csoil_map,varnam = "csoil", 
                            lonnam = "lon", latnam = "lat"))
names(csoil_nc) <- "csoil"
varams = "csoil"
test <- list(lon,lat,csoil_nc,varams)
names(test) <- c("lon","lat","vars","varams")
write_nc2(test,varnams = "csoil",long_name = "csoil",units = "kg/m2",
          path = "~/data/LPX/output/csoil_SH0_1978_2020.nc")
plot_map3(Csoil_map)

###SH1
ncin <- nc_open("/Volumes/My Passport/data/LPX/LPX_NMIP/NMIP/SH1/LPX-Bern_SH1_cSoil.nc")
ncin
lon <- ncvar_get(ncin,"longitude")
nlon <- dim(lon) 

lat<-ncvar_get(ncin,"latitude")
nlat <- dim(lat)
lonlat <- expand.grid(lon, lat)

cSoil <- ncvar_get(ncin,"cSoil")
nc_close(ncin)

cSoil_long <- as.vector(cSoil)
length(cSoil_long)
720*360*171 # years
#using year from 1978-2020 (which covers measurement year)

cSoil_long2 <- matrix(cSoil_long, nrow = nlon * nlat, ncol = 171) # 2052 is 2052 months * 4-pft
rm(cSoil)
rm(cSoil_long)

#171 is the year 2021, 129 is the year 1978
Csoil_mean <- rowMeans(cSoil_long2[,c(129:171)],na.rm = TRUE) #convert from kg/m2/s to g/m2/yr
summary(Csoil_mean) # Csoil

Csoil_map <- as.data.frame(cbind(lonlat,Csoil_mean))

names(Csoil_map) <- c("lon","lat","csoil")

Csoil_map$csoil[Csoil_map$csoil == "NaN"] <- NA

csoil_nc <- list(df_to_grid(Csoil_map,varnam = "csoil", 
                            lonnam = "lon", latnam = "lat"))
names(csoil_nc) <- "csoil"
varams = "csoil"
test <- list(lon,lat,csoil_nc,varams)
names(test) <- c("lon","lat","vars","varams")
write_nc2(test,varnams = "csoil",long_name = "csoil",units = "kg/m2",
          path = "~/data/LPX/output/csoil_SH1_1978_2020.nc")


###SH3
ncin <- nc_open("/Volumes/My Passport/data/LPX/LPX_NMIP/NMIP/SH3/LPX-Bern_SH3_cSoil.nc")
ncin
lon <- ncvar_get(ncin,"longitude")
nlon <- dim(lon) 

lat<-ncvar_get(ncin,"latitude")
nlat <- dim(lat)
lonlat <- expand.grid(lon, lat)

cSoil <- ncvar_get(ncin,"cSoil")
nc_close(ncin)

cSoil_long <- as.vector(cSoil)
length(cSoil_long)
720*360*171 # years
#using year from 1978-2020 (which covers measurement year)

cSoil_long2 <- matrix(cSoil_long, nrow = nlon * nlat, ncol = 171) # 2052 is 2052 months * 4-pft
rm(cSoil)
rm(cSoil_long)

#171 is the year 2021, 129 is the year 1978
Csoil_mean <- rowMeans(cSoil_long2[,c(129:171)],na.rm = TRUE) #convert from kg/m2/s to g/m2/yr
summary(Csoil_mean) # Csoil

Csoil_map <- as.data.frame(cbind(lonlat,Csoil_mean))

names(Csoil_map) <- c("lon","lat","csoil")

Csoil_map$csoil[Csoil_map$csoil == "NaN"] <- NA

csoil_nc <- list(df_to_grid(Csoil_map,varnam = "csoil", 
                            lonnam = "lon", latnam = "lat"))
names(csoil_nc) <- "csoil"
varams = "csoil"
test <- list(lon,lat,csoil_nc,varams)
names(test) <- c("lon","lat","vars","varams")
write_nc2(test,varnams = "csoil",long_name = "csoil",units = "kg/m2",
          path = "~/data/LPX/output/csoil_SH3_1978_2020.nc")


###SH6
ncin <- nc_open("/Volumes/My Passport/data/LPX/LPX_NMIP/NMIP/SH6/LPX-Bern_SH6_cSoil.nc")
ncin
lon <- ncvar_get(ncin,"longitude")
nlon <- dim(lon) 

lat<-ncvar_get(ncin,"latitude")
nlat <- dim(lat)
lonlat <- expand.grid(lon, lat)

cSoil <- ncvar_get(ncin,"cSoil")
nc_close(ncin)

cSoil_long <- as.vector(cSoil)
length(cSoil_long)
720*360*171 # years
#using year from 1978-2020 (which covers measurement year)

cSoil_long2 <- matrix(cSoil_long, nrow = nlon * nlat, ncol = 171) # 2052 is 2052 months * 4-pft
rm(cSoil)
rm(cSoil_long)

#171 is the year 2021, 129 is the year 1978
Csoil_mean <- rowMeans(cSoil_long2[,c(129:171)],na.rm = TRUE) #convert from kg/m2/s to g/m2/yr
summary(Csoil_mean) # Csoil

Csoil_map <- as.data.frame(cbind(lonlat,Csoil_mean))

names(Csoil_map) <- c("lon","lat","csoil")

Csoil_map$csoil[Csoil_map$csoil == "NaN"] <- NA

csoil_nc <- list(df_to_grid(Csoil_map,varnam = "csoil", 
                            lonnam = "lon", latnam = "lat"))
names(csoil_nc) <- "csoil"
varams = "csoil"
test <- list(lon,lat,csoil_nc,varams)
names(test) <- c("lon","lat","vars","varams")
write_nc2(test,varnams = "csoil",long_name = "csoil",units = "kg/m2",
          path = "~/data/LPX/output/csoil_SH6_1978_2020.nc")

###SH10
ncin <- nc_open("/Volumes/My Passport/data/LPX/LPX_NMIP/NMIP/SH10/LPX-Bern_SH10_cSoil.nc")
ncin
lon <- ncvar_get(ncin,"longitude")
nlon <- dim(lon) 

lat<-ncvar_get(ncin,"latitude")
nlat <- dim(lat)
lonlat <- expand.grid(lon, lat)

cSoil <- ncvar_get(ncin,"cSoil")
nc_close(ncin)

cSoil_long <- as.vector(cSoil)
length(cSoil_long)
720*360*171 # years
#using year from 1978-2020 (which covers measurement year)

cSoil_long2 <- matrix(cSoil_long, nrow = nlon * nlat, ncol = 171) # 2052 is 2052 months * 4-pft
rm(cSoil)
rm(cSoil_long)

#171 is the year 2021, 129 is the year 1978
Csoil_mean <- rowMeans(cSoil_long2[,c(129:171)],na.rm = TRUE) #convert from kg/m2/s to g/m2/yr
summary(Csoil_mean) # Csoil

Csoil_map <- as.data.frame(cbind(lonlat,Csoil_mean))

names(Csoil_map) <- c("lon","lat","csoil")

Csoil_map$csoil[Csoil_map$csoil == "NaN"] <- NA

csoil_nc <- list(df_to_grid(Csoil_map,varnam = "csoil", 
                            lonnam = "lon", latnam = "lat"))
names(csoil_nc) <- "csoil"
varams = "csoil"
test <- list(lon,lat,csoil_nc,varams)
names(test) <- c("lon","lat","vars","varams")
write_nc2(test,varnams = "csoil",long_name = "csoil",units = "kg/m2",
          path = "~/data/LPX/output/csoil_SH10_1978_2020.nc")