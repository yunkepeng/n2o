library(ncdf4)
library(dplyr)
library(raster)
library(readr)
library(spgwr)
devtools::load_all("/Users/yunpeng/yunkepeng/latest_packages/rbeni/") 

siteinfo <- read.csv("~/data/n2o_Yunke/forcing/siteinfo_measurementyear.csv")
sitemean <- unique(siteinfo[,c("lon","lat")])

fapar_df_new <- list.files("/Users/yunpeng/data/fapar3g_zhu/data/",full.names = T)
year_info <- substr(sub('.*AVHRRBUVI04.', '', fapar_df_new),1,nchar(sub('.*AVHRRBUVI04.', '', fapar_df_new))-8) 
year_fapar <- as.data.frame(cbind(fapar_df_new,year_info))
fapar_final <- arrange(year_fapar,year_info)
nrow(fapar_final) #840 (bi-weekly data, from 19820101 to 20161014: 35 years *12 months *2 )

#test first one 
raster(fapar_final$fapar_df_new[1]) # resolution 0.08333333 (1/12)

empty_fapar_nfocal0 <- data.frame(matrix(NA)) 
empty_fapar_nfocal1 <- data.frame(matrix(NA)) 
empty_fapar_nfocal2 <- data.frame(matrix(NA)) 

for (i in 1:(nrow(fapar_final))) {
  print(i)
  raster_fapar <- raster(fapar_final$fapar_df_new[i])
  
  #get data at each n_focal
  #n_focal = 0
  s9 <- as.data.frame(cbind(sitemean$lon ,sitemean$lat ));names(s9) <- c("lon","lat");sp9 <- SpatialPoints(s9)
  #n_focal = 1
  s1 <- as.data.frame(cbind(sitemean$lon+ (1/12),sitemean$lat));names(s1) <- c("lon","lat");sp1 <- SpatialPoints(s1)
  s2 <- as.data.frame(cbind(sitemean$lon- (1/12),sitemean$lat));names(s2) <- c("lon","lat");sp2 <- SpatialPoints(s2)
  s3 <- as.data.frame(cbind(sitemean$lon,sitemean$lat+ (1/12)));names(s3) <- c("lon","lat");sp3 <- SpatialPoints(s3)
  s4 <- as.data.frame(cbind(sitemean$lon,sitemean$lat- (1/12)));names(s4) <- c("lon","lat");sp4 <- SpatialPoints(s4)
  s5 <- as.data.frame(cbind(sitemean$lon+ (1/12),sitemean$lat+ (1/12)));names(s5) <- c("lon","lat");sp5 <- SpatialPoints(s5)
  s6 <- as.data.frame(cbind(sitemean$lon+ (1/12),sitemean$lat- (1/12)));names(s6) <- c("lon","lat");sp6 <- SpatialPoints(s6)
  s7 <- as.data.frame(cbind(sitemean$lon- (1/12),sitemean$lat+ (1/12)));names(s7) <- c("lon","lat");sp7 <- SpatialPoints(s7)
  s8 <- as.data.frame(cbind(sitemean$lon- (1/12),sitemean$lat- (1/12)));names(s8) <- c("lon","lat");sp8 <- SpatialPoints(s8)
  #n_focal =2
  s10 <- as.data.frame(cbind(sitemean$lon- (2/12),sitemean$lat+ (2/12)));names(s10) <- c("lon","lat");sp10 <- SpatialPoints(s10)
  s11 <- as.data.frame(cbind(sitemean$lon- (1/12),sitemean$lat+ (2/12)));names(s11) <- c("lon","lat");sp11 <- SpatialPoints(s11)
  s12 <- as.data.frame(cbind(sitemean$lon        ,sitemean$lat+ (2/12)));names(s12) <- c("lon","lat");sp12 <- SpatialPoints(s12)
  s13 <- as.data.frame(cbind(sitemean$lon+ (1/12),sitemean$lat+ (2/12)));names(s13) <- c("lon","lat");sp13 <- SpatialPoints(s13)
  s14 <- as.data.frame(cbind(sitemean$lon+ (2/12),sitemean$lat+ (2/12)));names(s14) <- c("lon","lat");sp14 <- SpatialPoints(s14)
  s15 <- as.data.frame(cbind(sitemean$lon- (2/12),sitemean$lat+ (1/12)));names(s15) <- c("lon","lat");sp15 <- SpatialPoints(s15)
  s16 <- as.data.frame(cbind(sitemean$lon+ (2/12),sitemean$lat+ (1/12)));names(s16) <- c("lon","lat");sp16 <- SpatialPoints(s16)
  s17 <- as.data.frame(cbind(sitemean$lon- (2/12),sitemean$lat ));names(s17) <- c("lon","lat");sp17 <- SpatialPoints(s17)
  s18 <- as.data.frame(cbind(sitemean$lon+ (2/12),sitemean$lat ));names(s18) <- c("lon","lat");sp18 <- SpatialPoints(s18)
  s19 <- as.data.frame(cbind(sitemean$lon- (2/12),sitemean$lat- (1/12)));names(s19) <- c("lon","lat");sp19 <- SpatialPoints(s19)
  s20 <- as.data.frame(cbind(sitemean$lon+ (2/12),sitemean$lat- (1/12)));names(s20) <- c("lon","lat");sp20 <- SpatialPoints(s20)
  s21 <- as.data.frame(cbind(sitemean$lon- (2/12),sitemean$lat- (2/12)));names(s21) <- c("lon","lat");sp21 <- SpatialPoints(s21)
  s22 <- as.data.frame(cbind(sitemean$lon- (1/12),sitemean$lat- (2/12)));names(s22) <- c("lon","lat");sp22 <- SpatialPoints(s22)
  s23 <- as.data.frame(cbind(sitemean$lon        ,sitemean$lat- (2/12)));names(s23) <- c("lon","lat");sp23 <- SpatialPoints(s23)
  s24 <- as.data.frame(cbind(sitemean$lon+ (1/12),sitemean$lat- (2/12)));names(s24) <- c("lon","lat");sp24 <- SpatialPoints(s24)
  s25 <- as.data.frame(cbind(sitemean$lon+ (2/12),sitemean$lat- (2/12)));names(s25) <- c("lon","lat");sp25 <- SpatialPoints(s25)
  
  d1 <- (raster::extract(raster_fapar, sp1, sp = TRUE) %>% as_tibble() %>%right_join(s1, by = c("lon", "lat")))[,1]
  d2 <- (raster::extract(raster_fapar, sp2, sp = TRUE) %>% as_tibble() %>%right_join(s2, by = c("lon", "lat")))[,1]
  d3 <- (raster::extract(raster_fapar, sp3, sp = TRUE) %>% as_tibble() %>%right_join(s3, by = c("lon", "lat")))[,1]
  d4 <- (raster::extract(raster_fapar, sp4, sp = TRUE) %>% as_tibble() %>%right_join(s4, by = c("lon", "lat")))[,1]
  d5 <- (raster::extract(raster_fapar, sp5, sp = TRUE) %>% as_tibble() %>%right_join(s5, by = c("lon", "lat")))[,1]
  d6 <- (raster::extract(raster_fapar, sp6, sp = TRUE) %>% as_tibble() %>%right_join(s6, by = c("lon", "lat")))[,1]
  d7 <- (raster::extract(raster_fapar, sp7, sp = TRUE) %>% as_tibble() %>%right_join(s7, by = c("lon", "lat")))[,1]
  d8 <- (raster::extract(raster_fapar, sp8, sp = TRUE) %>% as_tibble() %>%right_join(s8, by = c("lon", "lat")))[,1]
  d9 <- (raster::extract(raster_fapar, sp9, sp = TRUE) %>% as_tibble() %>%right_join(s9, by = c("lon", "lat")))[,1]
  d10 <- (raster::extract(raster_fapar, sp10, sp = TRUE) %>% as_tibble() %>%right_join(s10, by = c("lon", "lat")))[,1]
  d11 <- (raster::extract(raster_fapar, sp11, sp = TRUE) %>% as_tibble() %>%right_join(s11, by = c("lon", "lat")))[,1]
  d12 <- (raster::extract(raster_fapar, sp12, sp = TRUE) %>% as_tibble() %>%right_join(s12, by = c("lon", "lat")))[,1]
  d13 <- (raster::extract(raster_fapar, sp13, sp = TRUE) %>% as_tibble() %>%right_join(s13, by = c("lon", "lat")))[,1]
  d14 <- (raster::extract(raster_fapar, sp14, sp = TRUE) %>% as_tibble() %>%right_join(s14, by = c("lon", "lat")))[,1]
  d15 <- (raster::extract(raster_fapar, sp15, sp = TRUE) %>% as_tibble() %>%right_join(s15, by = c("lon", "lat")))[,1]
  d16 <- (raster::extract(raster_fapar, sp16, sp = TRUE) %>% as_tibble() %>%right_join(s16, by = c("lon", "lat")))[,1]
  d17 <- (raster::extract(raster_fapar, sp17, sp = TRUE) %>% as_tibble() %>%right_join(s17, by = c("lon", "lat")))[,1]
  d18 <- (raster::extract(raster_fapar, sp18, sp = TRUE) %>% as_tibble() %>%right_join(s18, by = c("lon", "lat")))[,1]
  d19 <- (raster::extract(raster_fapar, sp19, sp = TRUE) %>% as_tibble() %>%right_join(s19, by = c("lon", "lat")))[,1]
  d20 <- (raster::extract(raster_fapar, sp20, sp = TRUE) %>% as_tibble() %>%right_join(s20, by = c("lon", "lat")))[,1]
  d21 <- (raster::extract(raster_fapar, sp21, sp = TRUE) %>% as_tibble() %>%right_join(s21, by = c("lon", "lat")))[,1]
  d22 <- (raster::extract(raster_fapar, sp22, sp = TRUE) %>% as_tibble() %>%right_join(s22, by = c("lon", "lat")))[,1]
  d23 <- (raster::extract(raster_fapar, sp23, sp = TRUE) %>% as_tibble() %>%right_join(s23, by = c("lon", "lat")))[,1]
  d24 <- (raster::extract(raster_fapar, sp24, sp = TRUE) %>% as_tibble() %>%right_join(s24, by = c("lon", "lat")))[,1]
  d25 <- (raster::extract(raster_fapar, sp25, sp = TRUE) %>% as_tibble() %>%right_join(s25, by = c("lon", "lat")))[,1]
  
  rm(raster_fapar)
  empty_fapar_nfocal0[1:nrow(sitemean),i]<- d9
  empty_fapar_nfocal1[1:nrow(sitemean),i]<- rowMeans(as.data.frame(cbind(d1,d2,d3,d4,d5,d6,d7,d8,d9)),na.rm=T)
  empty_fapar_nfocal2[1:nrow(sitemean),i]<- rowMeans(as.data.frame(cbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,
                                                                         d13,d14,d15,d16,d17,d18,d19,d20,d21,d22,d23,d24,d25)),na.rm=T)
  
} 

#write.csv --> sitemean, nfocal0, 1, 2
csvfile <- paste("~/data/n2o_Yunke/forcing/fapar_nfocal0.csv")
write_csv(empty_fapar_nfocal0, path = csvfile)
csvfile <- paste("~/data/n2o_Yunke/forcing/fapar_nfocal1.csv")
write_csv(empty_fapar_nfocal1, path = csvfile)
csvfile <- paste("~/data/n2o_Yunke/forcing/fapar_nfocal2.csv")
write_csv(empty_fapar_nfocal2, path = csvfile)
csvfile <- paste("~/data/n2o_Yunke/forcing/fapar_sitemean.csv")
write_csv(sitemean, path = csvfile)

#convert bi-weekly to monthly data (average each 2 columns)
empty_fapar2_nfocal0 <- data.frame(matrix(NA)) 
empty_fapar2_nfocal1 <- data.frame(matrix(NA)) 
empty_fapar2_nfocal2 <- data.frame(matrix(NA)) 

for (i in 1:(ncol(empty_fapar_nfocal0)/2)) {
  empty_fapar2_nfocal0[1:nrow(sitemean),i]<- rowMeans(empty_fapar_nfocal0[,c((2*i-1):(2*i))])
  empty_fapar2_nfocal1[1:nrow(sitemean),i]<- rowMeans(empty_fapar_nfocal1[,c((2*i-1):(2*i))])
  empty_fapar2_nfocal2[1:nrow(sitemean),i]<- rowMeans(empty_fapar_nfocal2[,c((2*i-1):(2*i))])
} 

#this is monthly fapar (from 1982 to 2016) #lon + lat 420 months
fapar_sites_nfocal0 <- as.data.frame(cbind(sitemean,empty_fapar2_nfocal0))
fapar_sites_nfocal1 <- as.data.frame(cbind(sitemean,empty_fapar2_nfocal1))
fapar_sites_nfocal2 <- as.data.frame(cbind(sitemean,empty_fapar2_nfocal2))

# convert years <1982 to 1982-1991 average 
siteinfo <- read.csv("~/data/n2o_Yunke/forcing/siteinfo_measurementyear.csv")
summary(siteinfo$year_start)
siteinfo$begin_yr <- siteinfo$year_start
siteinfo$end_yr <- siteinfo$year_end
siteinfo$begin_yr[ siteinfo$year_start<1982] <- 1982
siteinfo$end_yr[ siteinfo$year_start<1982] <- 1991
summary(siteinfo)

#create three n_focal conditions
siteinfo_nfocal0 <- siteinfo
siteinfo_nfocal1 <- siteinfo
siteinfo_nfocal2 <- siteinfo

#now, starting select years of siteinfo
for (i in 1:(nrow(siteinfo_nfocal0))) {
  print(i)
  fapar_selected <- subset(fapar_sites_nfocal0,lon==siteinfo_nfocal0$lon[i]&lat==siteinfo_nfocal0$lat[i])
  fapar_selected <- fapar_selected[,c(3:422)] #remove lon and lat, and only keeps fapar data
  fapar_selected <- t(fapar_selected)
  start_years <- siteinfo_nfocal0$begin_yr[i]
  end_years <- siteinfo_nfocal0$end_yr[i]
  
  fapar_data <- (fapar_selected[c(((start_years-1981)*12-11):((end_years-1981)*12))])
  siteinfo_nfocal0[i,(1+8):(length(fapar_data)+8)] <- fapar_data
} 

for (i in 1:(nrow(siteinfo_nfocal1))) {
  print(i)
  fapar_selected <- subset(fapar_sites_nfocal1,lon==siteinfo_nfocal1$lon[i]&lat==siteinfo_nfocal1$lat[i])
  fapar_selected <- fapar_selected[,c(3:422)] #remove lon and lat, and only keeps fapar data
  fapar_selected <- t(fapar_selected)
  start_years <- siteinfo_nfocal1$begin_yr[i]
  end_years <- siteinfo_nfocal1$end_yr[i]
  
  fapar_data <- (fapar_selected[c(((start_years-1981)*12-11):((end_years-1981)*12))])
  siteinfo_nfocal1[i,(1+8):(length(fapar_data)+8)] <- fapar_data
} 

for (i in 1:(nrow(siteinfo_nfocal2))) {
  print(i)
  fapar_selected <- subset(fapar_sites_nfocal2,lon==siteinfo_nfocal2$lon[i]&lat==siteinfo_nfocal2$lat[i])
  fapar_selected <- fapar_selected[,c(3:422)] #remove lon and lat, and only keeps fapar data
  fapar_selected <- t(fapar_selected)
  start_years <- siteinfo_nfocal2$begin_yr[i]
  end_years <- siteinfo_nfocal2$end_yr[i]
  
  fapar_data <- (fapar_selected[c(((start_years-1981)*12-11):((end_years-1981)*12))])
  siteinfo_nfocal2[i,(1+8):(length(fapar_data)+8)] <- fapar_data
} 

dim(siteinfo_nfocal0) #this should fapar data at measurement year (from column 9 to 296)

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

fAPAR_site_nfocal0 <- siteinfo_nfocal0[,c(9:ncol(siteinfo_nfocal0))]
fAPAR_site_nfocal1 <- siteinfo_nfocal1[,c(9:ncol(siteinfo_nfocal1))]
fAPAR_site_nfocal2 <- siteinfo_nfocal2[,c(9:ncol(siteinfo_nfocal2))]

fAPAR_growing_nfocal0 <- fAPAR_site_nfocal0+Tg_site-Tg_site # + and then - to remove those non-growing months (shown as NA in Tg_sites)
fAPAR_growing_nfocal1 <- fAPAR_site_nfocal1+Tg_site-Tg_site # + and then - to remove those non-growing months (shown as NA in Tg_sites)
fAPAR_growing_nfocal2 <- fAPAR_site_nfocal2+Tg_site-Tg_site # + and then - to remove those non-growing months (shown as NA in Tg_sites)

gwr_sites_final <- gwr_sites[,c(1:7)]

###
gwr_sites_final$min_fapar_nfocal0 <- apply(fAPAR_site_nfocal0, 1, FUN = min, na.rm = TRUE)
gwr_sites_final$min_fapar_nfocal1 <- apply(fAPAR_site_nfocal1, 1, FUN = min, na.rm = TRUE)
gwr_sites_final$min_fapar_nfocal2 <- apply(fAPAR_site_nfocal2, 1, FUN = min, na.rm = TRUE)

gwr_sites_final$max_fapar_nfocal0 <- apply(fAPAR_site_nfocal0, 1, FUN = max, na.rm = TRUE)
gwr_sites_final$max_fapar_nfocal1 <- apply(fAPAR_site_nfocal1, 1, FUN = max, na.rm = TRUE)
gwr_sites_final$max_fapar_nfocal2 <- apply(fAPAR_site_nfocal2, 1, FUN = max, na.rm = TRUE)

gwr_sites_final$mean_fapar_nfocal0 <- apply(fAPAR_site_nfocal0, 1, FUN = mean, na.rm = TRUE)
gwr_sites_final$mean_fapar_nfocal1 <- apply(fAPAR_site_nfocal1, 1, FUN = mean, na.rm = TRUE)
gwr_sites_final$mean_fapar_nfocal2 <- apply(fAPAR_site_nfocal2, 1, FUN = mean, na.rm = TRUE)

gwr_sites_final$min_fapar_growing_nfocal0 <- apply(fAPAR_growing_nfocal0, 1, FUN = min, na.rm = TRUE)
gwr_sites_final$min_fapar_growing_nfocal1 <- apply(fAPAR_growing_nfocal1, 1, FUN = min, na.rm = TRUE)
gwr_sites_final$min_fapar_growing_nfocal2 <- apply(fAPAR_growing_nfocal2, 1, FUN = min, na.rm = TRUE)

gwr_sites_final$max_fapar_growing_nfocal0 <- apply(fAPAR_growing_nfocal0, 1, FUN = max, na.rm = TRUE)
gwr_sites_final$max_fapar_growing_nfocal1 <- apply(fAPAR_growing_nfocal1, 1, FUN = max, na.rm = TRUE)
gwr_sites_final$max_fapar_growing_nfocal2 <- apply(fAPAR_growing_nfocal2, 1, FUN = max, na.rm = TRUE)

gwr_sites_final$mean_fapar_growing_nfocal0 <- apply(fAPAR_growing_nfocal0, 1, FUN = mean, na.rm = TRUE)
gwr_sites_final$mean_fapar_growing_nfocal1 <- apply(fAPAR_growing_nfocal1, 1, FUN = mean, na.rm = TRUE)
gwr_sites_final$mean_fapar_growing_nfocal2 <- apply(fAPAR_growing_nfocal2, 1, FUN = mean, na.rm = TRUE)

gwr_sites_final$year_start <- read.csv("~/data/n2o_Yunke/forcing/siteinfo_measurementyear.csv")$year_start
gwr_sites_final$year_end <- read.csv("~/data/n2o_Yunke/forcing/siteinfo_measurementyear.csv")$year_end
#remove two no-useful columns
gwr_sites_final2 <- dplyr::select(gwr_sites_final, -c('Begin_year', 'End_year'))

summary(gwr_sites_final2)

#also, finally including fapar data of all years, including n_focal0, 1, 2 separately
for (i in 1:(nrow(siteinfo))) {
  print(i)
  fapar_selected <- subset(fapar_sites_nfocal0,lon==siteinfo$lon[i]&lat==siteinfo$lat[i])
  fapar_selected <- fapar_selected[,c(3:422)] #remove lon and lat, and only keeps fapar data
  fapar_selected <- t(fapar_selected)
  siteinfo[i,(1+8):(420+8)] <- fapar_selected
} 
fAPAR_site2 <- siteinfo[,c(9:428)]
gwr_sites_final2$min_fapar_35yrs_nfocal0 <- apply(fAPAR_site2, 1, FUN = min, na.rm = TRUE)
gwr_sites_final2$max_fapar_35yrs_nfocal0 <- apply(fAPAR_site2, 1, FUN = max, na.rm = TRUE)
gwr_sites_final2$mean_fapar_35yrs_nfocal0 <- apply(fAPAR_site2, 1, FUN = mean, na.rm = TRUE)

for (i in 1:(nrow(siteinfo))) {
  print(i)
  fapar_selected <- subset(fapar_sites_nfocal1,lon==siteinfo$lon[i]&lat==siteinfo$lat[i])
  fapar_selected <- fapar_selected[,c(3:422)] #remove lon and lat, and only keeps fapar data
  fapar_selected <- t(fapar_selected)
  siteinfo[i,(1+8):(420+8)] <- fapar_selected
} 
fAPAR_site2 <- siteinfo[,c(9:428)]
gwr_sites_final2$min_fapar_35yrs_nfocal1 <- apply(fAPAR_site2, 1, FUN = min, na.rm = TRUE)
gwr_sites_final2$max_fapar_35yrs_nfocal1 <- apply(fAPAR_site2, 1, FUN = max, na.rm = TRUE)
gwr_sites_final2$mean_fapar_35yrs_nfocal1 <- apply(fAPAR_site2, 1, FUN = mean, na.rm = TRUE)

for (i in 1:(nrow(siteinfo))) {
  print(i)
  fapar_selected <- subset(fapar_sites_nfocal2,lon==siteinfo$lon[i]&lat==siteinfo$lat[i])
  fapar_selected <- fapar_selected[,c(3:422)] #remove lon and lat, and only keeps fapar data
  fapar_selected <- t(fapar_selected)
  siteinfo[i,(1+8):(420+8)] <- fapar_selected
} 
fAPAR_site2 <- siteinfo[,c(9:428)]
gwr_sites_final2$min_fapar_35yrs_nfocal2 <- apply(fAPAR_site2, 1, FUN = min, na.rm = TRUE)
gwr_sites_final2$max_fapar_35yrs_nfocal2 <- apply(fAPAR_site2, 1, FUN = max, na.rm = TRUE)
gwr_sites_final2$mean_fapar_35yrs_nfocal2 <- apply(fAPAR_site2, 1, FUN = mean, na.rm = TRUE)

gwr_sites_final2[sapply(gwr_sites_final2, is.nan)] <- NA
gwr_sites_final2[sapply(gwr_sites_final2, is.infinite)] <- NA

csvfile <- paste("~/data/n2o_Yunke/forcing/siteinfo_measurementyear_fapar3g_zhu.csv")
write_csv(gwr_sites_final2, path = csvfile)

