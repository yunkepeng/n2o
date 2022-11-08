devtools::load_all("/Users/yunpeng/yunkepeng/latest_packages/ingestr/")
siteinfo_final <- read.csv("~/data/n2o_Yunke/forcing/siteinfo_measurementyear.csv")
summary(siteinfo_final)
for (i in 1:(nrow(siteinfo_final))){
  df_watch <- ingest(
    siteinfo  = siteinfo_final[i,],
    source    = "watch_wfdei",
    getvars   = c("temp", "prec", "ppfd", "vpd", "patm"), 
    dir       = "/Volumes/My Passport/data/watch_wfdei/",
    settings  = list(correct_bias = "worldclim", dir_bias = "/Volumes/My Passport/data/worldclim/"))
  df_cru <- ingest(
    siteinfo  = siteinfo_final[i,],
    source    = "cru",
    getvars   = "ccov",
    dir       = "/Volumes/My Passport/data/cru/ts_4.01/")
  df_co2 <- ingest(
    siteinfo  = siteinfo_final[i,],
   source  = "co2_mlo",
   verbose = FALSE)
  df_co2_final <- as.data.frame(df_co2$data)
  df_co2_final2 <- df_co2_final[!(format(df_co2_final$date,"%m") == "02" & format(df_co2_final$date, "%d") == "29"), , drop = FALSE] # make columns consistent
  co2 <- df_co2_final2$co2
  ddf_meteo <- as_tibble(cbind(as.data.frame(df_watch$data),as.data.frame(df_cru$data)[,c("ccov_int","ccov")],co2))
  ddf_meteo$fapar <- 1
  ddf_meteo$tmax <- ddf_meteo$temp
  ddf_meteo$tmin <- ddf_meteo$temp
  ddf_meteo$sitename <- siteinfo_final[i,c("sitename")]
  ddf_meteo$lon <- siteinfo_final[i,c("lon")]
  ddf_meteo$lat <-  siteinfo_final[i,c("lat")]
  ddf_meteo$elv <-  siteinfo_final[i,c("elv")]
  ddf_meteo$year_start <-  siteinfo_final[i,c("year_start")]
  ddf_meteo$year_end <-  siteinfo_final[i,c("year_end")]
  csvfile <- paste("~/data/n2o_Yunke/ingest_data/",siteinfo_final$sitename[i],".csv",sep = "")
  write.csv(ddf_meteo, csvfile, row.names = TRUE)
}
