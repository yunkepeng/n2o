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
  
  max_fapar <- max(fapar_selected[c(((start_years-2000)*12-11):((end_years-2000)*12))],na.rm=T)
  mean_fapar <- mean(fapar_selected[c(((start_years-2000)*12-11):((end_years-2000)*12))],na.rm=T)
  
  siteinfo[i,c("max_fapar")] <- max_fapar
  siteinfo[i,c("mean_fapar")] <- mean_fapar
} 

siteinfo$max_fapar[siteinfo$max_fapar=="-Inf"] <- NA
siteinfo$max_fapar[siteinfo$max_fapar<0] <- NA
siteinfo$mean_fapar[siteinfo$mean_fapar<0] <- NA

summary(siteinfo)

csvfile <- paste("~/data/n2o_Yunke/forcing/siteinfo_measurementyear_fapar_modis.csv")
write_csv(siteinfo[,c("lon","lat","elv","year_start","year_end","sitename","max_fapar","mean_fapar")], path = csvfile)
