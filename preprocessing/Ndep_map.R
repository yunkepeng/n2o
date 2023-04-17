#output N deposition for 2000-2009.
devtools::load_all("/Users/yunpeng/yunkepeng/latest_packages/rbeni/")
library(ncdf4)
devtools::load_all("/Users/yunpeng/yunkepeng/latest_packages/ingestr/")

###create a land-based global grid
elev_nc <- read_nc_onefile("~/data/watch_wfdei/WFDEI-elevation.nc")
elev <- as.data.frame(nc_to_df(elev_nc, varnam = "elevation"))
summary(elev)
#only select those sites with available elevation (land grid) 
grids <- subset(elev,is.na(elevation)==F)
dim(grids)
###3. calculate weighted-sum

grids$nhx <- NA
grids$noy <- NA

for (i in 1:nrow(grids)) {
  tryCatch({
    print(i)
    df_ndep <- ingest_bysite(
      sitename  = paste("a",i,sep=""),
      source    = "ndep",
      lon       = grids$lon[i],
      lat       = grids$lat[i],
      year_start= 2000,
      year_end  = 2009,
      timescale = "y",
      dir       = "~/data/ndep_lamarque/",
      verbose   = FALSE
    )
    grids$noy[i] <- mean(df_ndep$noy,na.rm=TRUE)
    grids$nhx[i] <- mean(df_ndep$nhx,na.rm=TRUE)
  }, error=function(e){})} 

grids$ndep <- grids$nhx+grids$noy
plot_map3(grids[,c("lon","lat","ndep")],
          varnam = "ndep",latmin = -65, latmax = 85)

ndep_final <- merge(elev[,c("lon","lat")],grids[,c("lon","lat","ndep")],by=c("lon","lat"),all.x=TRUE)
dim(ndep_final)

library(ncdf4)
ncin <- nc_open("~/data/watch_wfdei/WFDEI-elevation.nc")
lon <- ncvar_get(ncin,"lon")
lat<-ncvar_get(ncin,"lat")

ndep_final_nc <- list(df_to_grid(ndep_final,varnam = "ndep", lonnam = "lon", latnam = "lat"))
names(ndep_final_nc) <- "ndep"
varams = "ndep"
test <- list(lon,lat,ndep_final_nc,varams)
names(test) <- c("lon","lat","vars","varams")
write_nc2(test,varnams = "ndep",long_name = "n_deposition_2000_2009",units = "gN/m2/yr",
          path = "~/data/n2o_Yunke/final_map/ndep.nc")
