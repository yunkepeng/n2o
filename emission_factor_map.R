library(terra)
library(stringr)
library(rgdal)
library(raster)
library(dplyr)
library(readr)
library(ncdf4)
devtools::load_all("/Users/yunpeng/yunkepeng/latest_packages/rbeni/") # using beni's latest package.

ref_raster <- raster("~/data/LPX/data/LPX-Bern_SH1_tas.nc")

maize <- raster("~/data/n2o_cui_naturefood/map/MAIZE_EF_MEAN_CI.NC",
             varname = "N2O emission factor")
rice <- raster("~/data/n2o_cui_naturefood/map/RICE_EF_MEAN_CI.NC",
                varname = "N2O emission factor")
wheat <- raster("~/data/n2o_cui_naturefood/map/WHEAT_EF_MEAN_CI.NC",
                varname = "N2O emission factor")
other <- raster("~/data/n2o_cui_naturefood/map/OTHER CROPS_EF_MEAN_CI.NC",
                varname = "N2O emission factor")

#pre-processing, flip coordinates (so lon,lat change consistently), then 
#set the same extent, crs (projection) and same lon/lat
a2 <- t(flip(maize, direction='y' ))
extent(a2) <- extent(ref_raster)
crs(a2) <- crs(ref_raster)
a3 <- projectRaster(a2,ref_raster)
df_maize <- raster::as.data.frame(a3, xy = TRUE)

a2 <- t(flip(rice, direction='y' ))
extent(a2) <- extent(ref_raster)
crs(a2) <- crs(ref_raster)
a3 <- projectRaster(a2,ref_raster)
df_rice <- raster::as.data.frame(a3, xy = TRUE)

a2 <- t(flip(wheat, direction='y' ))
extent(a2) <- extent(ref_raster)
crs(a2) <- crs(ref_raster)
a3 <- projectRaster(a2,ref_raster)
df_wheat <- raster::as.data.frame(a3, xy = TRUE)

a2 <- t(flip(other, direction='y' ))
extent(a2) <- extent(ref_raster)
crs(a2) <- crs(ref_raster)
a3 <- projectRaster(a2,ref_raster)
df_other <- raster::as.data.frame(a3, xy = TRUE)

summary(df_other)
all_pft <- as.data.frame(cbind(df_maize$layer,df_rice$layer,
                               df_wheat$layer,df_other$layer))
dim(all_pft)
mean_cropland_cui <- rowMeans(all_pft,na.rm=T)

cui_final <- as.data.frame(cbind(df_maize[,c(1:2)],mean_cropland_cui))
names(cui_final) <- c("lon","lat","EF_Cui")
summary(cui_final)
plot_map3(cui_final)

#now, using LPX output (SH1 - SH3) - all others changed; but Nfer change or not
#Cui et al. Nature Food: The global patterns of crop-specific N2O EFs and mitigation potentials were predicted using four LME models in 2000 at 5-arc-minute spatial resolution.
#so, only look at the year 2000



#1. N2O
(2020-2000+1)
2052-12*21+1
(2020-2000)
2052-12*20
length(c(1801:1812))/12

nc_open("/Users/yunpeng/data/LPX/data/LPX-Bern_SH1_fN2Opft_lu.nc")
#(1:Natural; 2:Cropland; 3:Pasture; 4:Urban)
#SH1
cropland1 <- raster::as.data.frame(raster("~/data/LPX/data/LPX-Bern_SH1_fN2Opft_lu.nc",band=1801,level=2), xy = TRUE)[,3]
cropland2 <- raster::as.data.frame(raster("~/data/LPX/data/LPX-Bern_SH1_fN2Opft_lu.nc",band=1802,level=2), xy = TRUE)[,3]
cropland3 <- raster::as.data.frame(raster("~/data/LPX/data/LPX-Bern_SH1_fN2Opft_lu.nc",band=1803,level=2), xy = TRUE)[,3]
cropland4 <- raster::as.data.frame(raster("~/data/LPX/data/LPX-Bern_SH1_fN2Opft_lu.nc",band=1804,level=2), xy = TRUE)[,3]
cropland5 <- raster::as.data.frame(raster("~/data/LPX/data/LPX-Bern_SH1_fN2Opft_lu.nc",band=1805,level=2), xy = TRUE)[,3]
cropland6 <- raster::as.data.frame(raster("~/data/LPX/data/LPX-Bern_SH1_fN2Opft_lu.nc",band=1806,level=2), xy = TRUE)[,3]
cropland7 <- raster::as.data.frame(raster("~/data/LPX/data/LPX-Bern_SH1_fN2Opft_lu.nc",band=1807,level=2), xy = TRUE)[,3]
cropland8 <- raster::as.data.frame(raster("~/data/LPX/data/LPX-Bern_SH1_fN2Opft_lu.nc",band=1808,level=2), xy = TRUE)[,3]
cropland9 <- raster::as.data.frame(raster("~/data/LPX/data/LPX-Bern_SH1_fN2Opft_lu.nc",band=1809,level=2), xy = TRUE)[,3]
cropland10 <- raster::as.data.frame(raster("~/data/LPX/data/LPX-Bern_SH1_fN2Opft_lu.nc",band=1810,level=2), xy = TRUE)[,3]
cropland11 <- raster::as.data.frame(raster("~/data/LPX/data/LPX-Bern_SH1_fN2Opft_lu.nc",band=1811,level=2), xy = TRUE)[,3]
cropland12 <- raster::as.data.frame(raster("~/data/LPX/data/LPX-Bern_SH1_fN2Opft_lu.nc",band=1812,level=2), xy = TRUE)[,3]

#SH3
cropland1a <- raster::as.data.frame(raster("~/data/LPX/data/LPX-Bern_SH3_fN2Opft_lu.nc",band=1801,level=2), xy = TRUE)[,3]
cropland2a <- raster::as.data.frame(raster("~/data/LPX/data/LPX-Bern_SH3_fN2Opft_lu.nc",band=1802,level=2), xy = TRUE)[,3]
cropland3a <- raster::as.data.frame(raster("~/data/LPX/data/LPX-Bern_SH3_fN2Opft_lu.nc",band=1803,level=2), xy = TRUE)[,3]
cropland4a <- raster::as.data.frame(raster("~/data/LPX/data/LPX-Bern_SH3_fN2Opft_lu.nc",band=1804,level=2), xy = TRUE)[,3]
cropland5a <- raster::as.data.frame(raster("~/data/LPX/data/LPX-Bern_SH3_fN2Opft_lu.nc",band=1805,level=2), xy = TRUE)[,3]
cropland6a <- raster::as.data.frame(raster("~/data/LPX/data/LPX-Bern_SH3_fN2Opft_lu.nc",band=1806,level=2), xy = TRUE)[,3]
cropland7a <- raster::as.data.frame(raster("~/data/LPX/data/LPX-Bern_SH3_fN2Opft_lu.nc",band=1807,level=2), xy = TRUE)[,3]
cropland8a <- raster::as.data.frame(raster("~/data/LPX/data/LPX-Bern_SH3_fN2Opft_lu.nc",band=1808,level=2), xy = TRUE)[,3]
cropland9a <- raster::as.data.frame(raster("~/data/LPX/data/LPX-Bern_SH3_fN2Opft_lu.nc",band=1809,level=2), xy = TRUE)[,3]
cropland10a <- raster::as.data.frame(raster("~/data/LPX/data/LPX-Bern_SH3_fN2Opft_lu.nc",band=1810,level=2), xy = TRUE)[,3]
cropland11a <- raster::as.data.frame(raster("~/data/LPX/data/LPX-Bern_SH3_fN2Opft_lu.nc",band=1811,level=2), xy = TRUE)[,3]
cropland12a <- raster::as.data.frame(raster("~/data/LPX/data/LPX-Bern_SH3_fN2Opft_lu.nc",band=1812,level=2), xy = TRUE)[,3]

lpx_n2o <- rowMeans(as.data.frame(cbind(cropland1-cropland1a,
                                          cropland2-cropland2a,
                                          cropland3-cropland3a,
                                          cropland4-cropland4a,
                                          cropland5-cropland5a,
                                          cropland6-cropland6a,
                                          cropland7-cropland7a,
                                          cropland8-cropland8a,
                                          cropland9-cropland9a,
                                          cropland10-cropland10a,
                                          cropland11-cropland11a,
                                          cropland12-cropland12a),na.rm=T))

#N fertilisation's map
2020-2000+1
171-21+1
#band =151 is the year 2000
cropland1a <- raster::as.data.frame(raster("~/data/LPX/data/nfert_NMIP2022_1850-2021.nc",band=151,varname="NH4CROP"), xy = TRUE)[,3]
cropland2a <- raster::as.data.frame(raster("~/data/LPX/data/nfert_NMIP2022_1850-2021.nc",band=151,varname="NO3CROP"), xy = TRUE)[,3]
summary(cropland1a)
summary(cropland2a)

lpx_nfer <- cropland1a+cropland2a #g/m2/yr

#here *31556952*1000: converted n2o from kg/m2/s to g/m2/yr
emission_factor <- lpx_n2o*31556952*1000/lpx_nfer
summary(emission_factor)
emission_factor[sapply(emission_factor, is.nan)] <- NA
emission_factor[sapply(emission_factor, is.infinite)] <- NA
summary(emission_factor)

#convert to dataframe with lon and lat
ref_raster_df <- raster::as.data.frame(ref_raster, xy = TRUE)
lpx_final <- as.data.frame(cbind(ref_raster_df[,c(1:2)],emission_factor))
names(lpx_final) <- c("lon","lat","EF_LPX")
summary(lpx_final)
#a few grids show some extreme values - just remove them
dim(subset(lpx_final,EF_LPX>1))
dim(subset(lpx_final,EF_LPX<0))
lpx_final$EF_LPX[lpx_final$EF_LPX>1] <- NA
lpx_final$EF_LPX[lpx_final$EF_LPX<0] <- NA
summary(lpx_final)
#here *100: convert value to percentage
lpx_final$EF_LPX <- lpx_final$EF_LPX*100
plot_map3(lpx_final)  
summary(lpx_final)

summary(cui_final)
#now, compare
final <- as.data.frame(cbind(lpx_final$EF_LPX,cui_final$EF_Cui))
names(final) <- c("LPX","Cui")

analyse_modobs2(final,"LPX","Cui", type = "points",relative=TRUE)$gg 

analyse_modobs2(subset(final,LPX<5),"LPX","Cui", type = "points",relative=TRUE)$gg 
