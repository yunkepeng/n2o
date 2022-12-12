library(ncdf4)
library(dplyr)
library(raster)
library(readr)
devtools::load_all("/Users/yunpeng/yunkepeng/latest_packages/rbeni/") 

siteinfo <- read.csv("~/data/n2o_Yunke/forcing/siteinfo_measurementyear.csv")
sitemean <- unique(siteinfo[,c("lon","lat")])
sp_sites <- SpatialPoints(sitemean) # only select lon and lat

fapar_df_new <- list.files("/Volumes/My Passport/data/modis_ndvi_MOD13C2_006/netcdf/",full.names = T)
year_info <- substr(sub('.*NDVI.', '', fapar_df_new),1,nchar(sub('.*NDVI.', '', fapar_df_new))-3) 
year_fapar <- as.data.frame(cbind(fapar_df_new,year_info))
fapar_data <- arrange(year_fapar,year_info)
nrow(fapar_data) #from 20000201 to 20200901

#we only select complete years, from 20010101 to 20191201
fapar_final <- fapar_data[c(12:239),]
dim(fapar_final)


empty_fapar <- data.frame(matrix(NA)) 

for (i in 1:(nrow(fapar_final))) {
  print(i)
  raster_fapar <- raster(fapar_final$fapar_df_new[i])
  fapar_monthly <- (raster::extract(raster_fapar, sp_sites, sp = TRUE) %>% as_tibble() %>%
                      right_join(sitemean, by = c("lon", "lat")))[,1]
  rm(raster_fapar)
  empty_fapar[1:nrow(sitemean),i]<- fapar_monthly
} 

#this is monthly fapar (from 2001 to 2019) #lon + lat 228 months
fapar_sites <- as.data.frame(cbind(sitemean,empty_fapar))
dim(fapar_sites)

# convert years <2001 to 2002-2011 average 
siteinfo <- read.csv("~/data/n2o_Yunke/forcing/siteinfo_measurementyear.csv")
summary(siteinfo$year_start)
siteinfo$begin_yr <- siteinfo$year_start
siteinfo$end_yr <- siteinfo$year_end
siteinfo$begin_yr[ siteinfo$year_start<2001] <- 2001
siteinfo$end_yr[ siteinfo$year_start<2001] <- 2010
summary(siteinfo)

#now, starting select years of siteinfo
for (i in 1:(nrow(siteinfo))) {
  print(i)
  fapar_selected <- subset(fapar_sites,lon==siteinfo$lon[i]&lat==siteinfo$lat[i])
  fapar_selected <- fapar_selected[,c(3:230)] #remove lon and lat, and only keeps fapar data
  fapar_selected <- t(fapar_selected)
  start_years <- siteinfo$begin_yr[i]
  end_years <- siteinfo$end_yr[i]
  
  fapar_data <- (fapar_selected[c(((start_years-2000)*12-11):((end_years-2000)*12))])
  siteinfo[i,(1+8):(length(fapar_data)+8)] <- fapar_data
} 

dim(siteinfo) #this should fapar data at measurement year (from column 9)

#now, start with Tg
gwr_sites <- siteinfo[,c("lon","lat","elv","year_start","year_end","begin_yr","end_yr")]
names(gwr_sites) <- c("lon","lat","z","Begin_year","End_year","year_start","year_end")
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

#1014 are site numbers, 144 are total months
dim(Tg_site)

fAPAR_site <- siteinfo[,c(9:ncol(siteinfo))]
dim(fAPAR_site)

fAPAR_growing <- fAPAR_site+Tg_site-Tg_site # + and then - to remove those non-growing months (shown as NA in Tg_sites)

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

csvfile <- paste("~/data/n2o_Yunke/forcing/siteinfo_measurementyear_fapar_modis.csv")
write_csv(gwr_sites_final2, path = csvfile)
