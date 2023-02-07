library(raster)
devtools::load_all("/Users/yunpeng/yunkepeng/latest_packages/rbeni/") 
devtools::load_all("/Users/yunpeng/yunkepeng/latest_packages/ingestr/")
library(tidyr)

#NH4CROP, NO3CROP, NH4PAST, NO3PAST
#band: 1-171
#Nfertilisation is during 1980-2016
2020-2016+1
171-5+1
2020-1980+1
171-41+1
length(c(131:167))

empty_data <- data.frame(matrix(NA)) 

#this map is the area under per grid (so can be directly sum up)
for (i in c(131:167)) {
  a1 <- raster("~/data/LPX/data/nfert_NMIP2022_1850-2021.nc",varname="NH4CROP",band=i)
  a2 <- raster("~/data/LPX/data/nfert_NMIP2022_1850-2021.nc",varname="NO3CROP",band=i)
  a3 <- raster("~/data/LPX/data/nfert_NMIP2022_1850-2021.nc",varname="NH4PAST",band=i)
  a4 <- raster("~/data/LPX/data/nfert_NMIP2022_1850-2021.nc",varname="NO3PAST",band=i)
  
  aa1 <- as.data.frame(a1, xy=TRUE)
  aa2 <- as.data.frame(a2, xy=TRUE)
  aa3 <- as.data.frame(a3, xy=TRUE)
  aa4 <- as.data.frame(a4, xy=TRUE)
  
  aa_final <- aa1[,3]+aa2[,3]+aa3[,3]+aa4[,3]
  empty_data[c(1:259200),i-130] <- aa_final
  
} 

#pick up coordinates
final_nfer <- as.data.frame(cbind(aa1[,c(1:2)],rowSums(empty_data,na.rm=T)))
names(final_nfer) <- c("lon","lat","nfer")

plot_map3(final_nfer)

#prepare lon and lat
library(ncdf4)
ncin <- nc_open("~/data/watch_wfdei/WFDEI-elevation.nc")
lon <- ncvar_get(ncin,"lon")
lat<-ncvar_get(ncin,"lat")

nfer_nc <- list(df_to_grid(final_nfer,varnam = "nfer", lonnam = "lon", latnam = "lat"))
names(nfer_nc) <- "nfer"
varams = "nfer"
test <- list(lon,lat,nfer_nc,varams)
names(test) <- c("lon","lat","vars","varams")
write_nc2(test,varnams = "nfer",long_name = "nfer",units = "g/m2",
          path = "~/data/nimpl_sofun_inputs/map/Final_ncfile/nfer.nc")
