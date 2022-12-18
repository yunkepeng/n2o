library(tidyverse)
library(dplyr)
library(maps)
library(rworldmap)
library(tidyverse) 
library(ncmeta)
library(viridis)
library(ggthemes)
library(LSD)
library(yardstick)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(gplots)
library(tidyselect)
library(extrafont)
devtools::load_all("/Users/yunpeng/yunkepeng/latest_packages/rbeni/") 
#library(rbeni)
library(raster)
library(maps)
library(rworldmap)
library(cowplot)
library(ncdf4)
library(scales)
library(spgwr)
devtools::load_all("/Users/yunpeng/yunkepeng/latest_packages/ingestr/")
library(hwsdr)
library(ncdf4)

#ingest 
#input data
site_record2 <- read.csv("~/data/n2o_Yunke/forcing/siteinfo.csv")
gwr_sites <- unique(site_record2[,c("lon","lat","z")])

#now, extracting site values from Tg, alpha, c/n.....
elev_nc <- read_nc_onefile("~/data/watch_wfdei/WFDEI-elevation.nc")
elev <- as.data.frame(nc_to_df(elev_nc, varnam = "elevation"))

nfer_grassland <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/LPX/output/nfer_grassland_1978_2020.nc"),
  varnam = "nfer"))

nfer_cropland <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/LPX/output/nfer_cropland_1978_2020.nc"),
  varnam = "nfer"))

n2o_forest_SH0 <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/LPX/output/forest_SH0_n2o_1978_2020.nc"),
  varnam = "n2o"))

n2o_forest_SH1 <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/LPX/output/forest_SH1_n2o_1978_2020.nc"),
  varnam = "n2o"))

n2o_forest_SH3 <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/LPX/output/forest_SH3_n2o_1978_2020.nc"),
  varnam = "n2o"))

n2o_forest_SH6 <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/LPX/output/forest_SH6_n2o_1978_2020.nc"),
  varnam = "n2o"))

n2o_forest_SH10 <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/LPX/output/forest_SH10_n2o_1978_2020.nc"),
  varnam = "n2o"))

n2o_grassland_SH0 <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/LPX/output/grassland_SH0_n2o_1978_2020.nc"),
  varnam = "n2o"))

n2o_grassland_SH1 <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/LPX/output/grassland_SH1_n2o_1978_2020.nc"),
  varnam = "n2o"))

n2o_grassland_SH3 <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/LPX/output/grassland_SH3_n2o_1978_2020.nc"),
  varnam = "n2o"))

n2o_grassland_SH6 <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/LPX/output/grassland_SH6_n2o_1978_2020.nc"),
  varnam = "n2o"))

n2o_grassland_SH10 <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/LPX/output/grassland_SH10_n2o_1978_2020.nc"),
  varnam = "n2o"))

n2o_cropland_SH0 <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/LPX/output/cropland_SH0_n2o_1978_2020.nc"),
  varnam = "n2o"))

n2o_cropland_SH1 <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/LPX/output/cropland_SH1_n2o_1978_2020.nc"),
  varnam = "n2o"))

n2o_cropland_SH3 <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/LPX/output/cropland_SH3_n2o_1978_2020.nc"),
  varnam = "n2o"))

n2o_cropland_SH6 <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/LPX/output/cropland_SH6_n2o_1978_2020.nc"),
  varnam = "n2o"))

n2o_cropland_SH10 <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/LPX/output/cropland_SH10_n2o_1978_2020.nc"),
  varnam = "n2o"))

csoil_SH0 <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/LPX/output/csoil_SH0_1978_2020.nc"),
  varnam = "csoil"))

csoil_SH1 <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/LPX/output/csoil_SH1_1978_2020.nc"),
  varnam = "csoil"))

csoil_SH3 <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/LPX/output/csoil_SH3_1978_2020.nc"),
  varnam = "csoil"))

csoil_SH6 <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/LPX/output/csoil_SH6_1978_2020.nc"),
  varnam = "csoil"))

csoil_SH10 <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/LPX/output/csoil_SH10_1978_2020.nc"),
  varnam = "csoil"))

#cbind all predictors, and its lon, lat, z
all_predictors <- cbind(elev,nfer_grassland$nfer,nfer_cropland$nfer,
                        n2o_forest_SH0$n2o,n2o_forest_SH1$n2o,n2o_forest_SH3$n2o,n2o_forest_SH6$n2o,n2o_forest_SH10$n2o,
                        n2o_grassland_SH0$n2o,n2o_grassland_SH1$n2o,n2o_grassland_SH3$n2o,n2o_grassland_SH6$n2o,n2o_grassland_SH10$n2o,
                        n2o_cropland_SH0$n2o,n2o_cropland_SH1$n2o,n2o_cropland_SH3$n2o,n2o_cropland_SH6$n2o,n2o_cropland_SH10$n2o,
                        csoil_SH0$csoil,csoil_SH1$csoil,csoil_SH3$csoil,csoil_SH6$csoil,csoil_SH10$csoil)

names(all_predictors) <- c("lon","lat","z","nfer_grassland","nfer_cropland",
                           "n2o_forest_SH0","n2o_forest_SH1","n2o_forest_SH3","n2o_forest_SH6","n2o_forest_SH10",
                           "n2o_grassland_SH0","n2o_grassland_SH1","n2o_grassland_SH3","n2o_grassland_SH6","n2o_grassland_SH10",
                           "n2o_cropland_SH0","n2o_cropland_SH1","n2o_cropland_SH3","n2o_cropland_SH6","n2o_cropland_SH10",
                           "csoil_SH0","csoil_SH1","csoil_SH3","csoil_SH6","csoil_SH10")

nfer_grassland <- all_predictors[,c("lon","lat","z","nfer_grassland")]
nfer_cropland <- all_predictors[,c("lon","lat","z","nfer_cropland")]
n2o_forest_SH0 <- all_predictors[,c("lon","lat","z","n2o_forest_SH0")]
n2o_forest_SH1 <- all_predictors[,c("lon","lat","z","n2o_forest_SH1")]
n2o_forest_SH3 <- all_predictors[,c("lon","lat","z","n2o_forest_SH3")]
n2o_forest_SH6 <- all_predictors[,c("lon","lat","z","n2o_forest_SH6")]
n2o_forest_SH10 <- all_predictors[,c("lon","lat","z","n2o_forest_SH10")]
n2o_grassland_SH0 <- all_predictors[,c("lon","lat","z","n2o_grassland_SH0")]
n2o_grassland_SH1 <- all_predictors[,c("lon","lat","z","n2o_grassland_SH1")]
n2o_grassland_SH3 <- all_predictors[,c("lon","lat","z","n2o_grassland_SH3")]
n2o_grassland_SH6 <- all_predictors[,c("lon","lat","z","n2o_grassland_SH6")]
n2o_grassland_SH10 <- all_predictors[,c("lon","lat","z","n2o_grassland_SH10")]
n2o_cropland_SH0 <- all_predictors[,c("lon","lat","z","n2o_cropland_SH0")]
n2o_cropland_SH1 <- all_predictors[,c("lon","lat","z","n2o_cropland_SH1")]
n2o_cropland_SH3 <- all_predictors[,c("lon","lat","z","n2o_cropland_SH3")]
n2o_cropland_SH6 <- all_predictors[,c("lon","lat","z","n2o_cropland_SH6")]
n2o_cropland_SH10 <- all_predictors[,c("lon","lat","z","n2o_cropland_SH10")]
csoil_SH0 <- all_predictors[,c("lon","lat","z","csoil_SH0")]
csoil_SH1 <- all_predictors[,c("lon","lat","z","csoil_SH1")]
csoil_SH3 <- all_predictors[,c("lon","lat","z","csoil_SH3")]
csoil_SH6 <- all_predictors[,c("lon","lat","z","csoil_SH6")]
csoil_SH10 <- all_predictors[,c("lon","lat","z","csoil_SH10")]

#now, apply gwr to extract site predictors' value
NPP_Forest <- gwr_sites

#forest model (~moisture+Tg), SH10
NPP_Forest$n2o_forest_SH10 <- NA
#grassland model (~N addition + fapar), SH0,1,3,6
NPP_Forest$nfer_grassland <- NA
NPP_Forest$n2o_grassland_SH0 <- NA
NPP_Forest$n2o_grassland_SH1 <- NA
NPP_Forest$n2o_grassland_SH3 <- NA
NPP_Forest$n2o_grassland_SH6 <- NA
#cropland model (~N addition + climates +fapar), SH1, 3, 6, 10
NPP_Forest$nfer_cropland <- NA
NPP_Forest$n2o_cropland_SH1 <- NA
NPP_Forest$n2o_cropland_SH3 <- NA
NPP_Forest$n2o_cropland_SH6 <- NA
NPP_Forest$n2o_cropland_SH10 <- NA
NPP_Forest$csoil_SH1 <- NA
NPP_Forest$csoil_SH3 <- NA
NPP_Forest$csoil_SH6 <- NA
NPP_Forest$csoil_SH10 <- NA

a <- 1.5 # which degree (distance) of grid when interpolating gwr from global grids
#Extract Tg, PPFD, vpd, alpha,fAPAR,age,CNrt,LMA, max-vcmax25
for (i in 1:nrow(NPP_Forest)) {
  print(i)
  tryCatch({
    #n2o_forest_SH10
    n2o_forest_SH10_global <- na.omit(n2o_forest_SH10)
    NRE_part <- subset(n2o_forest_SH10_global,lon>(NPP_Forest[i,"lon"]-a)&lon<(NPP_Forest[i,"lon"]+a)&
                         lat>(NPP_Forest[i,"lat"]-a)&lat<(NPP_Forest[i,"lat"]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NPP_Forest[i,c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NPP_Forest[i,c("n2o_forest_SH10")] <- (gwr(n2o_forest_SH10 ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #nfer_grassland
    nfer_grassland_global <- na.omit(nfer_grassland)
    NRE_part <- subset(nfer_grassland_global,lon>(NPP_Forest[i,"lon"]-a)&lon<(NPP_Forest[i,"lon"]+a)&
                         lat>(NPP_Forest[i,"lat"]-a)&lat<(NPP_Forest[i,"lat"]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NPP_Forest[i,c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NPP_Forest[i,c("nfer_grassland")] <- (gwr(nfer_grassland ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #n2o_grassland_SH0
    n2o_grassland_SH0_global <- na.omit(n2o_grassland_SH0)
    NRE_part <- subset(n2o_grassland_SH0_global,lon>(NPP_Forest[i,"lon"]-a)&lon<(NPP_Forest[i,"lon"]+a)&
                         lat>(NPP_Forest[i,"lat"]-a)&lat<(NPP_Forest[i,"lat"]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NPP_Forest[i,c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NPP_Forest[i,c("n2o_grassland_SH0")] <- (gwr(n2o_grassland_SH0 ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #n2o_grassland_SH1
    n2o_grassland_SH1_global <- na.omit(n2o_grassland_SH1)
    NRE_part <- subset(n2o_grassland_SH1_global,lon>(NPP_Forest[i,"lon"]-a)&lon<(NPP_Forest[i,"lon"]+a)&
                         lat>(NPP_Forest[i,"lat"]-a)&lat<(NPP_Forest[i,"lat"]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NPP_Forest[i,c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NPP_Forest[i,c("n2o_grassland_SH1")] <- (gwr(n2o_grassland_SH1 ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #n2o_grassland_SH3
    n2o_grassland_SH3_global <- na.omit(n2o_grassland_SH3)
    NRE_part <- subset(n2o_grassland_SH3_global,lon>(NPP_Forest[i,"lon"]-a)&lon<(NPP_Forest[i,"lon"]+a)&
                         lat>(NPP_Forest[i,"lat"]-a)&lat<(NPP_Forest[i,"lat"]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NPP_Forest[i,c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NPP_Forest[i,c("n2o_grassland_SH3")] <- (gwr(n2o_grassland_SH3 ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #n2o_grassland_SH6
    n2o_grassland_SH6_global <- na.omit(n2o_grassland_SH6)
    NRE_part <- subset(n2o_grassland_SH6_global,lon>(NPP_Forest[i,"lon"]-a)&lon<(NPP_Forest[i,"lon"]+a)&
                         lat>(NPP_Forest[i,"lat"]-a)&lat<(NPP_Forest[i,"lat"]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NPP_Forest[i,c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NPP_Forest[i,c("n2o_grassland_SH6")] <- (gwr(n2o_grassland_SH6 ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #nfer_cropland
    nfer_cropland_global <- na.omit(nfer_cropland)
    NRE_part <- subset(nfer_cropland_global,lon>(NPP_Forest[i,"lon"]-a)&lon<(NPP_Forest[i,"lon"]+a)&
                         lat>(NPP_Forest[i,"lat"]-a)&lat<(NPP_Forest[i,"lat"]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NPP_Forest[i,c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NPP_Forest[i,c("nfer_cropland")] <- (gwr(nfer_cropland ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #n2o_cropland_SH1
    n2o_cropland_SH1_global <- na.omit(n2o_cropland_SH1)
    NRE_part <- subset(n2o_cropland_SH1_global,lon>(NPP_Forest[i,"lon"]-a)&lon<(NPP_Forest[i,"lon"]+a)&
                         lat>(NPP_Forest[i,"lat"]-a)&lat<(NPP_Forest[i,"lat"]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NPP_Forest[i,c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NPP_Forest[i,c("n2o_cropland_SH1")] <- (gwr(n2o_cropland_SH1 ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #n2o_cropland_SH3
    n2o_cropland_SH3_global <- na.omit(n2o_cropland_SH3)
    NRE_part <- subset(n2o_cropland_SH3_global,lon>(NPP_Forest[i,"lon"]-a)&lon<(NPP_Forest[i,"lon"]+a)&
                         lat>(NPP_Forest[i,"lat"]-a)&lat<(NPP_Forest[i,"lat"]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NPP_Forest[i,c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NPP_Forest[i,c("n2o_cropland_SH3")] <- (gwr(n2o_cropland_SH3 ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #n2o_cropland_SH6
    n2o_cropland_SH6_global <- na.omit(n2o_cropland_SH6)
    NRE_part <- subset(n2o_cropland_SH6_global,lon>(NPP_Forest[i,"lon"]-a)&lon<(NPP_Forest[i,"lon"]+a)&
                         lat>(NPP_Forest[i,"lat"]-a)&lat<(NPP_Forest[i,"lat"]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NPP_Forest[i,c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NPP_Forest[i,c("n2o_cropland_SH6")] <- (gwr(n2o_cropland_SH6 ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #n2o_cropland_SH10
    n2o_cropland_SH10_global <- na.omit(n2o_cropland_SH10)
    NRE_part <- subset(n2o_cropland_SH10_global,lon>(NPP_Forest[i,"lon"]-a)&lon<(NPP_Forest[i,"lon"]+a)&
                         lat>(NPP_Forest[i,"lat"]-a)&lat<(NPP_Forest[i,"lat"]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NPP_Forest[i,c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NPP_Forest[i,c("n2o_cropland_SH10")] <- (gwr(n2o_cropland_SH10 ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #csoil_SH1
    csoil_SH1_global <- na.omit(csoil_SH1)
    NRE_part <- subset(csoil_SH1_global,lon>(NPP_Forest[i,"lon"]-a)&lon<(NPP_Forest[i,"lon"]+a)&
                         lat>(NPP_Forest[i,"lat"]-a)&lat<(NPP_Forest[i,"lat"]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NPP_Forest[i,c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NPP_Forest[i,c("csoil_SH1")] <- (gwr(csoil_SH1 ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #csoil_SH3
    csoil_SH3_global <- na.omit(csoil_SH3)
    NRE_part <- subset(csoil_SH3_global,lon>(NPP_Forest[i,"lon"]-a)&lon<(NPP_Forest[i,"lon"]+a)&
                         lat>(NPP_Forest[i,"lat"]-a)&lat<(NPP_Forest[i,"lat"]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NPP_Forest[i,c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NPP_Forest[i,c("csoil_SH3")] <- (gwr(csoil_SH3 ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #csoil_SH6
    csoil_SH6_global <- na.omit(csoil_SH6)
    NRE_part <- subset(csoil_SH6_global,lon>(NPP_Forest[i,"lon"]-a)&lon<(NPP_Forest[i,"lon"]+a)&
                         lat>(NPP_Forest[i,"lat"]-a)&lat<(NPP_Forest[i,"lat"]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NPP_Forest[i,c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NPP_Forest[i,c("csoil_SH6")] <- (gwr(csoil_SH6 ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #csoil_SH10
    csoil_SH10_global <- na.omit(csoil_SH10)
    NRE_part <- subset(csoil_SH10_global,lon>(NPP_Forest[i,"lon"]-a)&lon<(NPP_Forest[i,"lon"]+a)&
                         lat>(NPP_Forest[i,"lat"]-a)&lat<(NPP_Forest[i,"lat"]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NPP_Forest[i,c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NPP_Forest[i,c("csoil_SH10")] <- (gwr(csoil_SH10 ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    
  }, error=function(e){})} 

summary(NPP_Forest)


csvfile <- paste("~/data/n2o_Yunke/forcing/siteinfo_lpx_values.csv")
write_csv(NPP_Forest, path = csvfile)

#checked - values of n2o, soc and Nfer all in gN/m2/yr
