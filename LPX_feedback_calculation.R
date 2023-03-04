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

#calculate area in land
lat <- raster::as.data.frame(raster("~/data/LPX/data/LPX-Bern_SH6_landCoverFrac_lu.nc",band=1873,level=1),xy=TRUE)[,2]
source("~/yunkepeng/CNuptake_MS/R/calc_area.R")
area_m2 <- calc_area(lat,0.5,0.5)

#calculate total N2O when dT = 0 
empty_data <- data.frame(matrix(NA)) 
for (i in c(1:100)) {
  print(i)
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
  
  n2o_df <- as.data.frame(cbind(df1[,3],df2[,3],df3[,3],df4[,3]))*31556952 #convert from kg_m2_s to kg_m2_yr
  n2o_total <- rowSums(n2o_df*lc_df,na.rm=T) #weighted-sum n2o values
  n2o_final <- sum(n2o_total*area_m2,na.rm=T)/1000000000 #weighted-sum, then value is from kg/m2/yr to kg/yr --> further convert to Tg.yr
  empty_data[i,1]<- n2o_final
} 

#when dT = 0.39
for (i in c(1:100)) {
  print(i)
  raster1 <- raster("/Users/yunpeng/data/LPX/data/step_experiment/co2_380_dT_0.39/LPX-Bern_co2_380_dT_0.39_fN2Opft_lu_annual.nc",
                    band=i,level=1)
  df1 <- raster::as.data.frame(raster1,xy=TRUE)
  
  raster2 <- raster("/Users/yunpeng/data/LPX/data/step_experiment/co2_380_dT_0.39/LPX-Bern_co2_380_dT_0.39_fN2Opft_lu_annual.nc",
                    band=i,level=2)
  df2 <- raster::as.data.frame(raster2,xy=TRUE)
  
  raster3 <- raster("/Users/yunpeng/data/LPX/data/step_experiment/co2_380_dT_0.39/LPX-Bern_co2_380_dT_0.39_fN2Opft_lu_annual.nc",
                    band=i,level=3)
  df3 <- raster::as.data.frame(raster3,xy=TRUE)
  
  raster4 <- raster("/Users/yunpeng/data/LPX/data/step_experiment/co2_380_dT_0.39/LPX-Bern_co2_380_dT_0.39_fN2Opft_lu_annual.nc",
                    band=i,level=4)
  df4 <- raster::as.data.frame(raster4,xy=TRUE)
  
  n2o_df <- as.data.frame(cbind(df1[,3],df2[,3],df3[,3],df4[,3]))*31556952 #convert from kg_m2_s to kg_m2_yr
  n2o_total <- rowSums(n2o_df*lc_df,na.rm=T) #weighted-sum n2o values
  n2o_final <- sum(n2o_total*area_m2,na.rm=T)/1000000000 #weighted-sum, then value is from kg/m2/yr to kg/yr --> further convert to Tg.yr
  empty_data[i,2]<- n2o_final
} 

#when dT = 3.95
for (i in c(1:100)) {
  print(i)
  raster1 <- raster("/Users/yunpeng/data/LPX/data/step_experiment/co2_380_dT_3.95/LPX-Bern_co2_380_dT_3.95_fN2Opft_lu_annual.nc",
                    band=i,level=1)
  df1 <- raster::as.data.frame(raster1,xy=TRUE)
  
  raster2 <- raster("/Users/yunpeng/data/LPX/data/step_experiment/co2_380_dT_3.95/LPX-Bern_co2_380_dT_3.95_fN2Opft_lu_annual.nc",
                    band=i,level=2)
  df2 <- raster::as.data.frame(raster2,xy=TRUE)
  
  raster3 <- raster("/Users/yunpeng/data/LPX/data/step_experiment/co2_380_dT_3.95/LPX-Bern_co2_380_dT_3.95_fN2Opft_lu_annual.nc",
                    band=i,level=3)
  df3 <- raster::as.data.frame(raster3,xy=TRUE)
  
  raster4 <- raster("/Users/yunpeng/data/LPX/data/step_experiment/co2_380_dT_3.95/LPX-Bern_co2_380_dT_3.95_fN2Opft_lu_annual.nc",
                    band=i,level=4)
  df4 <- raster::as.data.frame(raster4,xy=TRUE)
  
  n2o_df <- as.data.frame(cbind(df1[,3],df2[,3],df3[,3],df4[,3]))*31556952 #convert from kg_m2_s to kg_m2_yr
  n2o_total <- rowSums(n2o_df*lc_df,na.rm=T) #weighted-sum n2o values
  n2o_final <- sum(n2o_total*area_m2,na.rm=T)/1000000000 #weighted-sum, then value is from kg/m2/yr to kg/yr --> further convert to Tg.yr
  empty_data[i,3]<- n2o_final
} 

#when dT = 7.50
for (i in c(1:100)) {
  print(i)
  raster1 <- raster("/Users/yunpeng/data/LPX/data/step_experiment/co2_380_dT_7.5/LPX-Bern_co2_380_dT_7.5_fN2Opft_lu_annual.nc",
                    band=i,level=1)
  df1 <- raster::as.data.frame(raster1,xy=TRUE)
  
  raster2 <- raster("/Users/yunpeng/data/LPX/data/step_experiment/co2_380_dT_7.5/LPX-Bern_co2_380_dT_7.5_fN2Opft_lu_annual.nc",
                    band=i,level=2)
  df2 <- raster::as.data.frame(raster2,xy=TRUE)
  
  raster3 <- raster("/Users/yunpeng/data/LPX/data/step_experiment/co2_380_dT_7.5/LPX-Bern_co2_380_dT_7.5_fN2Opft_lu_annual.nc",
                    band=i,level=3)
  df3 <- raster::as.data.frame(raster3,xy=TRUE)
  
  raster4 <- raster("/Users/yunpeng/data/LPX/data/step_experiment/co2_380_dT_7.5/LPX-Bern_co2_380_dT_7.5_fN2Opft_lu_annual.nc",
                    band=i,level=4)
  df4 <- raster::as.data.frame(raster4,xy=TRUE)
  
  n2o_df <- as.data.frame(cbind(df1[,3],df2[,3],df3[,3],df4[,3]))*31556952 #convert from kg_m2_s to kg_m2_yr
  n2o_total <- rowSums(n2o_df*lc_df,na.rm=T) #weighted-sum n2o values
  n2o_final <- sum(n2o_total*area_m2,na.rm=T)/1000000000 #weighted-sum, then value is from kg/m2/yr to kg/yr --> further convert to Tg.yr
  empty_data[i,4]<- n2o_final
} 

#when co2 = 416
for (i in c(1:100)) {
  print(i)
  raster1 <- raster("/Users/yunpeng/data/LPX/data/step_experiment/co2_416_dT_0/LPX-Bern_co2_416_dT_0_fN2Opft_lu_annual.nc",
                    band=i,level=1)
  df1 <- raster::as.data.frame(raster1,xy=TRUE)
  
  raster2 <- raster("/Users/yunpeng/data/LPX/data/step_experiment/co2_416_dT_0/LPX-Bern_co2_416_dT_0_fN2Opft_lu_annual.nc",
                    band=i,level=2)
  df2 <- raster::as.data.frame(raster2,xy=TRUE)
  
  raster3 <- raster("/Users/yunpeng/data/LPX/data/step_experiment/co2_416_dT_0/LPX-Bern_co2_416_dT_0_fN2Opft_lu_annual.nc",
                    band=i,level=3)
  df3 <- raster::as.data.frame(raster3,xy=TRUE)
  
  raster4 <- raster("/Users/yunpeng/data/LPX/data/step_experiment/co2_416_dT_0/LPX-Bern_co2_416_dT_0_fN2Opft_lu_annual.nc",
                    band=i,level=4)
  df4 <- raster::as.data.frame(raster4,xy=TRUE)
  
  n2o_df <- as.data.frame(cbind(df1[,3],df2[,3],df3[,3],df4[,3]))*31556952 #convert from kg_m2_s to kg_m2_yr
  n2o_total <- rowSums(n2o_df*lc_df,na.rm=T) #weighted-sum n2o values
  n2o_final <- sum(n2o_total*area_m2,na.rm=T)/1000000000 #weighted-sum, then value is from kg/m2/yr to kg/yr --> further convert to Tg.yr
  empty_data[i,5]<- n2o_final
} 

#when co2 = 582
for (i in c(1:100)) {
  print(i)
  raster1 <- raster("/Users/yunpeng/data/LPX/data/step_experiment/co2_582_dT_0/LPX-Bern_co2_582_dT_0_fN2Opft_lu_annual.nc",
                    band=i,level=1)
  df1 <- raster::as.data.frame(raster1,xy=TRUE)
  
  raster2 <- raster("/Users/yunpeng/data/LPX/data/step_experiment/co2_582_dT_0/LPX-Bern_co2_582_dT_0_fN2Opft_lu_annual.nc",
                    band=i,level=2)
  df2 <- raster::as.data.frame(raster2,xy=TRUE)
  
  raster3 <- raster("/Users/yunpeng/data/LPX/data/step_experiment/co2_582_dT_0/LPX-Bern_co2_582_dT_0_fN2Opft_lu_annual.nc",
                    band=i,level=3)
  df3 <- raster::as.data.frame(raster3,xy=TRUE)
  
  raster4 <- raster("/Users/yunpeng/data/LPX/data/step_experiment/co2_582_dT_0/LPX-Bern_co2_582_dT_0_fN2Opft_lu_annual.nc",
                    band=i,level=4)
  df4 <- raster::as.data.frame(raster4,xy=TRUE)
  
  n2o_df <- as.data.frame(cbind(df1[,3],df2[,3],df3[,3],df4[,3]))*31556952 #convert from kg_m2_s to kg_m2_yr
  n2o_total <- rowSums(n2o_df*lc_df,na.rm=T) #weighted-sum n2o values
  n2o_final <- sum(n2o_total*area_m2,na.rm=T)/1000000000 #weighted-sum, then value is from kg/m2/yr to kg/yr --> further convert to Tg.yr
  empty_data[i,6]<- n2o_final
} 

#when co2 = 813
for (i in c(1:100)) {
  print(i)
  raster1 <- raster("/Users/yunpeng/data/LPX/data/step_experiment/co2_813_dT_0/LPX-Bern_co2_813_dT_0_fN2Opft_lu_annual.nc",
                    band=i,level=1)
  df1 <- raster::as.data.frame(raster1,xy=TRUE)
  
  raster2 <- raster("/Users/yunpeng/data/LPX/data/step_experiment/co2_813_dT_0/LPX-Bern_co2_813_dT_0_fN2Opft_lu_annual.nc",
                    band=i,level=2)
  df2 <- raster::as.data.frame(raster2,xy=TRUE)
  
  raster3 <- raster("/Users/yunpeng/data/LPX/data/step_experiment/co2_813_dT_0/LPX-Bern_co2_813_dT_0_fN2Opft_lu_annual.nc",
                    band=i,level=3)
  df3 <- raster::as.data.frame(raster3,xy=TRUE)
  
  raster4 <- raster("/Users/yunpeng/data/LPX/data/step_experiment/co2_813_dT_0/LPX-Bern_co2_813_dT_0_fN2Opft_lu_annual.nc",
                    band=i,level=4)
  df4 <- raster::as.data.frame(raster4,xy=TRUE)
  
  n2o_df <- as.data.frame(cbind(df1[,3],df2[,3],df3[,3],df4[,3]))*31556952 #convert from kg_m2_s to kg_m2_yr
  n2o_total <- rowSums(n2o_df*lc_df,na.rm=T) #weighted-sum n2o values
  n2o_final <- sum(n2o_total*area_m2,na.rm=T)/1000000000 #weighted-sum, then value is from kg/m2/yr to kg/yr --> further convert to Tg.yr
  empty_data[i,7]<- n2o_final
} 

names(empty_data) <- c("dT_0","dT_0.39","dT_3.95","dT_7.5",
                       "eCO2_416","eCO2_582","eCO2_813")

csvfile <- paste("~/data/n2o_Yunke/forcing/eCO2_warming_LPX_total_n2o.csv")
write_csv(empty_data, path = csvfile)

