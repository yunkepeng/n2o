library(ncdf4)
library(dplyr)
library(raster)
library(readr)
library(spgwr)
library(maps)
library(rworldmap)
devtools::load_all("/Users/yunpeng/yunkepeng/latest_packages/rbeni/") 
devtools::load_all("/Users/yunpeng/yunkepeng/latest_packages/ingestr/")

#input data 
df1 <- read_csv("~/data/n2o_wang_oikos/n2o_tables1.csv")
df1_coord <- unique(df1[,c("longitude_degree","latitude_degree")])
df1_coord$treatment <- "co2"

df2 <- read_csv("~/data/n2o_wang_oikos/n2o_tables2.csv")
df2_coord <- unique(df2[,c("longitude_degree","latitude_degree")])
df2_coord$treatment <- "warming"

df_all <- na.omit(as.data.frame(rbind(df1_coord,df2_coord)))
names(df_all) <- c("lon","lat","treatment")
#project data
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-180, 180), ylim = c(-75, 75), asp = 1)
points(df_all$lon,df_all$lat, col="red", pch=16,cex=1)

#get elevations
df_all
df_all$sitename <- paste("sitename",c(1:nrow(df_all)),sep="")
df_etopo <- ingest(
  df_all,
  source = "etopo1",
  dir = "~/data/etopo/"
)
df_all$z <- as.numeric(as.data.frame(df_etopo$data))
summary(df_all$z)
#measurement year recorded in paper: 1990-2019
sitemean <- df_all[,c("lon","lat")]

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
  
  d1 <- (raster::extract(raster_fapar, sp1, sp = TRUE) %>% as_tibble())[,1]
  d2 <- (raster::extract(raster_fapar, sp2, sp = TRUE) %>% as_tibble())[,1]
  d3 <- (raster::extract(raster_fapar, sp3, sp = TRUE) %>% as_tibble())[,1]
  d4 <- (raster::extract(raster_fapar, sp4, sp = TRUE) %>% as_tibble())[,1]
  d5 <- (raster::extract(raster_fapar, sp5, sp = TRUE) %>% as_tibble())[,1]
  d6 <- (raster::extract(raster_fapar, sp6, sp = TRUE) %>% as_tibble())[,1]
  d7 <- (raster::extract(raster_fapar, sp7, sp = TRUE) %>% as_tibble())[,1]
  d8 <- (raster::extract(raster_fapar, sp8, sp = TRUE) %>% as_tibble())[,1]
  d9 <- (raster::extract(raster_fapar, sp9, sp = TRUE) %>% as_tibble())[,1]
  d10 <- (raster::extract(raster_fapar, sp10, sp = TRUE) %>% as_tibble())[,1]
  d11 <- (raster::extract(raster_fapar, sp11, sp = TRUE) %>% as_tibble())[,1]
  d12 <- (raster::extract(raster_fapar, sp12, sp = TRUE) %>% as_tibble())[,1]
  d13 <- (raster::extract(raster_fapar, sp13, sp = TRUE) %>% as_tibble())[,1]
  d14 <- (raster::extract(raster_fapar, sp14, sp = TRUE) %>% as_tibble())[,1]
  d15 <- (raster::extract(raster_fapar, sp15, sp = TRUE) %>% as_tibble())[,1]
  d16 <- (raster::extract(raster_fapar, sp16, sp = TRUE) %>% as_tibble())[,1]
  d17 <- (raster::extract(raster_fapar, sp17, sp = TRUE) %>% as_tibble())[,1]
  d18 <- (raster::extract(raster_fapar, sp18, sp = TRUE) %>% as_tibble())[,1]
  d19 <- (raster::extract(raster_fapar, sp19, sp = TRUE) %>% as_tibble())[,1]
  d20 <- (raster::extract(raster_fapar, sp20, sp = TRUE) %>% as_tibble())[,1]
  d21 <- (raster::extract(raster_fapar, sp21, sp = TRUE) %>% as_tibble())[,1]
  d22 <- (raster::extract(raster_fapar, sp22, sp = TRUE) %>% as_tibble())[,1]
  d23 <- (raster::extract(raster_fapar, sp23, sp = TRUE) %>% as_tibble())[,1]
  d24 <- (raster::extract(raster_fapar, sp24, sp = TRUE) %>% as_tibble())[,1]
  d25 <- (raster::extract(raster_fapar, sp25, sp = TRUE) %>% as_tibble())[,1]
  
  rm(raster_fapar)
  empty_fapar_nfocal0[1:nrow(sitemean),i]<- d9
  empty_fapar_nfocal1[1:nrow(sitemean),i]<- rowMeans(as.data.frame(cbind(d1,d2,d3,d4,d5,d6,d7,d8,d9)),na.rm=T)
  empty_fapar_nfocal2[1:nrow(sitemean),i]<- rowMeans(as.data.frame(cbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,
                                                                         d13,d14,d15,d16,d17,d18,d19,d20,d21,d22,d23,d24,d25)),na.rm=T)
  
} 

#write.csv --> sitemean, nfocal0, 1, 2
csvfile <- paste("~/data/n2o_Yunke/forcing/co2_fapar_nfocal0.csv")
write_csv(empty_fapar_nfocal0, path = csvfile)
csvfile <- paste("~/data/n2o_Yunke/forcing/co2_fapar_nfocal1.csv")
write_csv(empty_fapar_nfocal1, path = csvfile)
csvfile <- paste("~/data/n2o_Yunke/forcing/co2_fapar_nfocal2.csv")
write_csv(empty_fapar_nfocal2, path = csvfile)
csvfile <- paste("~/data/n2o_Yunke/forcing/co2_fapar_sitemean.csv")
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

# convert monthly fapar (from 1982-2016: 420 months)  to 1990-2016 average (consistent with paper)
fapar_site_nfocal0 <- as.data.frame(cbind(empty_fapar2_nfocal0[,c(97:420)]))
fapar_site_nfocal1 <- as.data.frame(cbind(empty_fapar2_nfocal1[,c(97:420)]))
fapar_site_nfocal2 <- as.data.frame(cbind(empty_fapar2_nfocal2[,c(97:420)]))

gwr_sites_final <- sitemean

###
gwr_sites_final$min_fapar_nfocal0 <- apply(fapar_site_nfocal0, 1, FUN = min, na.rm = TRUE)
gwr_sites_final$min_fapar_nfocal1 <- apply(fapar_site_nfocal1, 1, FUN = min, na.rm = TRUE)
gwr_sites_final$min_fapar_nfocal2 <- apply(fapar_site_nfocal2, 1, FUN = min, na.rm = TRUE)

gwr_sites_final$max_fapar_nfocal0 <- apply(fapar_site_nfocal0, 1, FUN = max, na.rm = TRUE)
gwr_sites_final$max_fapar_nfocal1 <- apply(fapar_site_nfocal1, 1, FUN = max, na.rm = TRUE)
gwr_sites_final$max_fapar_nfocal2 <- apply(fapar_site_nfocal2, 1, FUN = max, na.rm = TRUE)

gwr_sites_final$mean_fapar_nfocal0 <- apply(fapar_site_nfocal0, 1, FUN = mean, na.rm = TRUE)
gwr_sites_final$mean_fapar_nfocal1 <- apply(fapar_site_nfocal1, 1, FUN = mean, na.rm = TRUE)
gwr_sites_final$mean_fapar_nfocal2 <- apply(fapar_site_nfocal2, 1, FUN = mean, na.rm = TRUE)

gwr_sites_final[sapply(gwr_sites_final, is.nan)] <- NA
gwr_sites_final[sapply(gwr_sites_final, is.infinite)] <- NA

csvfile <- paste("~/data/n2o_Yunke/forcing/co2_siteinfo_measurementyear_fapar3g_zhu.csv")
write_csv(gwr_sites_final, path = csvfile)

