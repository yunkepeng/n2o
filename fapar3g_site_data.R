library(ncdf4)
library(spgwr)
library(readr)
library(raster)

devtools::load_all("/Users/yunpeng/yunkepeng/latest_packages/rbeni/") 

#input site info (fapar3g is from 1982-2016)
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

#input tmx, tmn, vap, pre and radi
load(file = "/Users/yunpeng/yunkepeng/nimpl_sofun_inputs_final/climates_30yrs_monthly/WFDEI_CRU_1980_2016.Rdata")

total_month <- (2016-1980+1) * 12

elev <- as.data.frame(nc_to_df(read_nc_onefile("~/data/watch_wfdei/WFDEI-elevation.nc"), varnam = "elevation"))

monthly_tmn$lon <- elev$lon
monthly_tmn$lat <- elev$lat
monthly_tmn$z <- elev$elevation
monthly_tmn <- monthly_tmn[,c(3: (total_month+5))] #lon, lat, z in last 3
monthly_tmn <- subset(monthly_tmn,z>=0)

monthly_tmx$lon <-  elev$lon
monthly_tmx$lat <- elev$lat
monthly_tmx$z <- elev$elevation
monthly_tmx <- monthly_tmx[,c(3:(total_month+5))] #lon, lat, z in last 3
monthly_tmx <- subset(monthly_tmx,z>=0)


#create a function here
gwr_methods <- function(sites,monthly_grid){
  for (i in c(1:nrow(sites))){
    print(i)
    start_yr <- sites$year_start[i]
    end_yr <- sites$year_end[i]
    lon_site <- sites$lon[i]
    lat_site <- sites$lat[i]
    z_site <- sites$z[i]
    
    climate_distance <- subset(monthly_grid,lon>(lon_site-d)&lon<(lon_site+d)&lat>(lat_site-d)&lat<(lat_site+d))
    climate_distance_yr <- climate_distance[,((start_yr-1979)*12-11):((end_yr-1979)*12)]
    number_of_month <- ncol(climate_distance_yr)
    
    climate_distance_yr_xyz <- cbind(climate_distance[,c("lon","lat","z")],climate_distance_yr)
    # re-create column name for multiple month, which is important when applying gwr within a for loop
    names(climate_distance_yr_xyz) <- c("lon","lat","z",paste("a", 1:number_of_month, sep=""))
    
    #filter the condition when without available climates data after subset (filter within a distance of 1.5)
    if (nrow(climate_distance) == 0){
      sites[i,8:(7+number_of_month)] <- NA
    } else {
      sites[i,8:(7+number_of_month)] <- NA
      coordinates(climate_distance_yr_xyz) <- c("lon","lat")
      gridded(climate_distance_yr_xyz) <- TRUE
      
      input_sites <- sites[i,c("lon","lat","z")]
      coordinates(input_sites) <- c("lon","lat")
      for (a in c(1:number_of_month)){
        sites[i,a+7] <- (gwr( get(paste("a",a, sep="")) ~ z, climate_distance_yr_xyz, bandwidth = 1.06, fit.points = input_sites,predictions=TRUE))$SDF$pred
      }
    }
  }
  output <- sites
  return(output)
}

d <- 1.5 # range of surrounded grids (in degree) when applying each gwr

tmx_output <- gwr_methods(gwr_sites,monthly_tmx)

tmn_output <- gwr_methods(gwr_sites,monthly_tmn)

no_total_months <- ncol(tmx_output) #find the most months length the site covers

tmx_site <- tmx_output[,8:no_total_months]
tmn_site <- tmn_output[,8:no_total_months]

#1. Tg
#solar declination from Jan to Dec
s1 <- (-23.1+ -17.3)/2
s2 <- (-17.3 + -8)/2
s3 <- (-8 + 4.1)/2
s4 <- (4.1 + 14.8)/2
s5 <- (14.8 + 21.9)/2
s6 <- (21.9 + 23.2)/2
s7 <- (23.2 + 18.3)/2
s8 <- (18.3 + 8.6)/2
s9 <-  (8.6 + -2.8)/2
s10 <- (-2.8 + -14.1)/2
s11 <- (-14.1 + -21.6)/2
s12 <- (-21.6 + -23.1)/2

s <- c(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12)
lat <- gwr_sites$lat
xx <- data.frame(matrix(nrow=nrow(gwr_sites), ncol=no_total_months-7))
output_Tg <- data.frame(matrix(nrow=nrow(gwr_sites), ncol=no_total_months-7))
#xx = acos(h), h = hour angle of the sun
for (a in 1:12){ 
  month_no <- seq(from = 1, to = no_total_months-7, by = 12)+a-1
  xx[1:nrow(gwr_sites),month_no]<- -tan(pi*lat/180)*tan(s[a]*pi/180)
}

#check each part of Tg formula
part1 <- (0.5+((1-xx^2)^(0.5))/(2*acos(xx)))[,1:12]
part2 <- (0.5-((1-xx^2)^(0.5))/(2*acos(xx)))[,1:12]
summary(part1)
summary(part2)

#the percentage of tmx was dominated overall
Tg_site <- tmx_site*(0.5+((1-xx^2)^(0.5))/(2*acos(xx)))+ tmn_site*(0.5-((1-xx^2)^(0.5))/(2*acos(xx)))
Tg_site[Tg_site =="NaN"] <- NA
Tg_site[Tg_site < 0] <- NA

#1014 are site numbers, 288 are total months
dim(Tg_site)

rm(monthly_tmn)
rm(monthly_tmx)

#start processing fapar
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

#start gwr to interpolate fapar
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

fAPAR_site <- gwr_sites[,c(8:295)]
dim(fAPAR_site)
dim(Tg_site)
fAPAR_growing <- fAPAR_sites+Tg_site-Tg_site # + and then - to remove those non-growing months (shown as NA in Tg_sites)

gwr_sites_final <- gwr_sites[,c(1:7)]

gwr_sites_final$min_fapar <- apply(fAPAR_site, 1, FUN = min, na.rm = TRUE)
gwr_sites_final$max_fapar <- apply(fAPAR_site, 1, FUN = max, na.rm = TRUE)
gwr_sites_final$mean_fapar <- apply(fAPAR_site, 1, FUN = mean, na.rm = TRUE)

gwr_sites_final$min_fapar_growing <- apply(fAPAR_growing, 1, FUN = min, na.rm = TRUE)
gwr_sites_final$max_fapar_growing <- apply(fAPAR_growing, 1, FUN = max, na.rm = TRUE)
gwr_sites_final$mean_fapar_growing <- apply(fAPAR_growing, 1, FUN = mean, na.rm = TRUE)

gwr_sites_final[sapply(gwr_sites_final, is.nan)] <- NA
gwr_sites_final[sapply(gwr_sites_final, is.infinite)] <- NA

gwr_sites_final$year_start <- read.csv("~/data/n2o_Yunke/forcing/siteinfo_measurementyear.csv")$year_start
gwr_sites_final$year_end <- read.csv("~/data/n2o_Yunke/forcing/siteinfo_measurementyear.csv")$year_end
#remove two no-useful columns
gwr_sites_final2 <- dplyr::select(gwr_sites_final, -c('Begin_year', 'End_year'))

summary(gwr_sites_final2)

csvfile <- paste("~/data/n2o_Yunke/forcing/siteinfo_measurementyear_fapar3g.csv")
write_csv(gwr_sites_final2, path = csvfile)
