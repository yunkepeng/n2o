rm(list=ls())
library(ncdf4)
library(spgwr)
library(raster)

#input fapar
ncfname <- paste("~/data/fAPAR/fAPAR3g_v2/", "fAPAR3g_v2_1982_2016_FILLED", ".nc", sep="")
dname <- "FAPAR_FILLED"
ncin <- nc_open(ncfname)

lon <- ncvar_get(ncin,"LON")
nlon <- dim(lon) 

lat<-ncvar_get(ncin,"LAT")
nlat <- dim(lat)

FAPAR <- ncvar_get(ncin,"FAPAR_FILLED")
nc_close(ncin)

pre.vec.long <- as.vector(FAPAR)

final_fAPAR <- matrix(pre.vec.long, nrow = nlon * nlat, ncol = 420) # 259200 * 420 (420 is 35 years: 1982-2016)

lonlat <- expand.grid(lon, lat)
names(lonlat) <- c("lon","lat")

elev <- as.data.frame(nc_to_df(read_nc_onefile("~/data/watch_wfdei/WFDEI-elevation.nc"), varnam = "elevation"))
lonlat$z <- elev$elevation

fAPAR_input <- as.data.frame(cbind(final_fAPAR,lonlat))
names(fAPAR_input)
fAPAR_input <- subset(fAPAR_input,z>=0)

#input site info
gwr_sites <- read.csv("~/data/n2o_Yunke/forcing/siteinfo_measurementyear.csv")

gwr_sites <- gwr_sites[,c("lon","lat","elv","year_start","year_end")]
names(gwr_sites) <- c("lon","lat","z","year_start","year_end")
gwr_sites$Begin_year <- gwr_sites$year_start
gwr_sites$End_year <- gwr_sites$year_end

dim(gwr_sites)
summary(gwr_sites)

gwr_sites$year_end[gwr_sites$year_start<1982] <- 1991
gwr_sites$year_start[gwr_sites$year_start<1982] <- 1982
 
summary(gwr_sites)

total_month <- (2016-1982+1) * 12


d <- 1.5 # range of surrounded grids (in degree) when applying each gwr

#start gwr 
#create a function here
for (i in c(1:nrow(gwr_sites))){
    print(i)
    start_yr <- gwr_sites$year_start[i]
    end_yr <- gwr_sites$year_end[i]
    lon_site <- gwr_sites$lon[i]
    lat_site <- gwr_sites$lat[i]
    z_site <- gwr_sites$z[i]
    
    climate_distance <- subset(fAPAR_input,lon>(lon_site-d)&lon<(lon_site+d)&lat>(lat_site-d)&lat<(lat_site+d))
    climate_distance_yr <- climate_distance[,((start_yr-1981)*12-11):((end_yr-1981)*12)]
    number_of_month <- ncol(climate_distance_yr)
    
    climate_distance_yr_xyz <- cbind(climate_distance[,c("lon","lat","z")],climate_distance_yr)
    # re-create column name for multiple month, which is important when applying gwr within a for loop
    names(climate_distance_yr_xyz) <- c("lon","lat","z",paste("a", 1:number_of_month, sep=""))
    climate_distance_yr_xyz <- na.omit(climate_distance_yr_xyz)
    #filter the condition when without available climates data after subset (filter within a distance of 1.5)
    if (nrow(climate_distance_yr_xyz) <=1){
      gwr_sites[i,8:(7+number_of_month)] <- NA
    } else {
      gwr_sites[i,8:(7+number_of_month)] <- NA
      coordinates(climate_distance_yr_xyz) <- c("lon","lat")
      gridded(climate_distance_yr_xyz) <- TRUE
      
      input_sites <- gwr_sites[i,c("lon","lat","z")]
      coordinates(input_sites) <- c("lon","lat")
      for (a in c(1:number_of_month)){
        #a +5 means, add outputted monthly tmn (within measurement year) directly after lon+lat+z+Begin_year+End_year.
        gwr_sites[i,a+7] <- (gwr( get(paste("a",a, sep="")) ~ z, climate_distance_yr_xyz, bandwidth = 1.06, fit.points = input_sites,predictions=TRUE))$SDF$pred
      }
    }
}

#convert year_start and year_end so that it can be merged then
gwr_sites$year_start <- read.csv("~/data/n2o_Yunke/forcing/siteinfo_measurementyear.csv")$year_start
gwr_sites$year_end <- read.csv("~/data/n2o_Yunke/forcing/siteinfo_measurementyear.csv")$year_end
#remove two no-useful columns

gwr_sites2 <- dplyr::select(gwr_sites, -c('Begin_year', 'End_year'))

csvfile <- paste("/Users/yunpeng/data/n2o_Yunke/forcing/siteinfo_measurementyear_fapar3g.csv")
write_csv(gwr_sites2, path = csvfile)
