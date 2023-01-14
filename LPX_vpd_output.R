#prepare sites
rm(list=ls())
devtools::load_all("/Users/yunpeng/yunkepeng/latest_packages/rbeni/") 

#input site info (read an existing database - just including lon, lat, z, pft)
gwr_sites <- read.csv("~/data/n2o_Yunke/forcing/LPX_annual_maxfapar.csv")

gwr_sites <- unique(gwr_sites[,c("lon","lat","z")])
gwr_sites$year_start <- 1980
gwr_sites$year_end <- 2016
gwr_sites$Begin_year <- gwr_sites$year_start
gwr_sites$End_year <- gwr_sites$year_end

dim(gwr_sites)
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

monthly_vap$lon <-  elev$lon
monthly_vap$lat <- elev$lat
monthly_vap$z <- elev$elevation
monthly_vap <- monthly_vap[,c(3:(total_month+5))] #lon, lat, z in last 3
monthly_vap <- subset(monthly_vap,z>=0)

d <- 1.5 # range of surrounded grids (in degree) when applying each gwr

#start gwr 
library(spgwr)
library(raster)

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
        #a +5 means, add outputted monthly tmn (within measurement year) directly after lon+lat+z+Begin_year+End_year.
        sites[i,a+7] <- (gwr( get(paste("a",a, sep="")) ~ z, climate_distance_yr_xyz, bandwidth = 1.06, fit.points = input_sites,predictions=TRUE))$SDF$pred
      }
    }
  }
  output <- sites
  return(output)
}


tmx_output <- gwr_methods(gwr_sites,monthly_tmx)

tmn_output <- gwr_methods(gwr_sites,monthly_tmn)

vap_output <- gwr_methods(gwr_sites,monthly_vap)


# now, calculate Tg, vpd and PPFD 
no_total_months <- ncol(tmx_output) 

tmx_site <- tmx_output[,8:no_total_months]
tmn_site <- tmn_output[,8:no_total_months]
vap_site <- vap_output[,8:no_total_months]

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

vpd_site <- 0.611*exp(17.27*(Tg_site)/((Tg_site)+237.3))-vap_site*0.1 #vap in hPa
vpd_site[vpd_site =="NaN"] <- NA
vpd_site[vpd_site < 0] <- NA

annual_vpd <- data.frame(matrix(NA))
dim(vpd_site)
for (i in c(1:37)) {
  annual_vpd[1:nrow(vpd_site),i] <- rowMeans(vpd_site[1:nrow(vpd_site),c((i*12-11):(i*12))],na.rm=T)
  print(i)
} 

gwr_sites2 <- as.data.frame(cbind(gwr_sites[,c("lon","lat","z")],annual_vpd))

final_sites <- unique(read.csv("~/data/n2o_Yunke/forcing/LPX_annual_maxfapar.csv")[,c("lon","lat","z","pft")])
dim(final_sites)
final_vpd <- merge(final_sites,gwr_sites2,by=c("lon","lat","z"),all.x=TRUE)
dim(final_vpd)
names(final_vpd) <- c("lon","lat","z","pft",paste0("year",c(1980:2016)))

csvfile <- paste("~/data/n2o_Yunke/forcing/LPX_annual_vpd.csv")
write_csv(final_vpd, path = csvfile)
