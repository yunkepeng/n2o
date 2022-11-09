rm(list=ls())
library(dplyr)
library(ggplot2)
library(patchwork)
library(readr)
library(lubridate)
library(plotrix)
library(hwsdr)
devtools::load_all("~/yunkepeng/computational_rsofun/rsofun_latest/rsofun/")

pars <- list(
  kphio                 = 0.04607080,
  soilm_par_a           = 2.75687824,
  soilm_par_b           = 1.68140444,
  tau_acclim_tempstress = 7.35259044,
  par_shape_tempstress  = 0.09863961)

ingest_df <- list.files("~/data/n2o_Yunke/ingest_data",full.names = T)
datalist = vector("list", length = length(ingest_df))

for (i in 1:(length(ingest_df))){
  
  df1 <- read.csv(ingest_df[i])
  
  siteinfo_final <- tibble(sitename=df1$sitename[1],lon=df1$lon[1],
                           lat=df1$lat[1],elv=df1$elv[1],
                           year_start=df1$year_start[1],year_end=df1$year_end[1]) 
  
  forcing <- rsofun::p_model_drivers
  
  forcing$forcing[[1]] <- (as_tibble(df1[,c("date","temp","prec","vpd","ppfd",
                                            "patm","ccov_int","ccov","snow",
                                            "rain","fapar","co2","tmin","tmax")]))
  
  forcing$sitename <- df1$sitename[1]
  forcing$site_info[[1]]$lon <- df1$lon[1]
  forcing$site_info[[1]]$lat <- df1$lat[1]
  forcing$site_info[[1]]$elv <-  df1$elv[1]
  forcing$site_info[[1]]$date_start <- as.Date(df1$date[1])
  forcing$site_info[[1]]$date_end <- as.Date(df1$date[length(df1$date)])
  forcing$params_siml[[1]]$firstyeartrend <- df1$year_start[1]
  forcing$site_info[[1]]$year_end <- df1$year_end[i]
  forcing$params_siml[[1]]$nyeartrend <- df1$year_end[1]-df1$year_start[1]+1
  tmp <- forcing
  tmp$params_siml[[1]]$spinupyears <- 1500
  tmp$params_siml[[1]]$recycle <- 1
  
  modlist2 <- rsofun::runread_pmodel_f(tmp,par = pars)
  relative_soil_moisture <- mean(modlist2$data[[1]]$wscal,na.rm=T)
  alpha <- mean(modlist2$data[[1]]$transp/modlist2$data[[1]]$pet,na.rm=T)
  
  siteinfo_final$soil_moisture <- relative_soil_moisture
  siteinfo_final$alpha <- alpha
  
  datalist[[i]] <- siteinfo_final
  print(i)
  print(siteinfo_final$soil_moisture)
}

output_df = do.call(rbind, datalist)
summary(output_df)
csvfile <- paste("~/data/n2o_Yunke/forcing/siteinfo_measurementyear_moisture_alpha.csv")
write_csv(output_df, path = csvfile)

