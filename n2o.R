library(dplyr)
library(ggplot2)
library(patchwork)
library(readr)
devtools::load_all("/Users/yunpeng/yunkepeng/latest_packages/rbeni/") # using beni's latest package.
devtools::load_all("/Users/yunpeng/yunkepeng/latest_packages/ingestr/") # using beni's latest package.

pars <- list(
  kphio                 = 0.04607080,
  soilm_par_a           = 2.75687824,
  soilm_par_b           = 1.68140444,
  tau_acclim_tempstress = 7.35259044,
  par_shape_tempstress  = 0.09863961,
  f_nretain             = 0.500000,
  fpc_tree_max          = 0.950000,
  growtheff             = 0.600000,
  r_root                = 0.913000,
  r_sapw                = 0.044000,
  exurate               = 0.003000,
  k_decay_leaf_base     = 1.00000,
  k_decay_leaf_width    = 2.00000,
  k_decay_root          = 1.00000,
  k_decay_labl          = 0.00000,
  k_decay_sapw          = 1.00000,
  r_cton_root           = 37.0000,
  r_cton_wood           = 100.000,
  ncw_min               = 0.056,
  r_n_cw_v              = 0.4,
  r_ctostructn_leaf     = 35.0000,
  kbeer                 = 0.500000,
  gddbase               = 5.0,
  ramp                  = 0.0,
  phentype              = 2.0,
  perc_k1               = 5.0,        
  thdiff_wp             = 0.2,          
  thdiff_whc15          = 0.8,
  thdiff_fc             = 0.4,          
  forg                  = 0.01,
  wbwp                  = 0.029,  
  por                   = 0.421,    
  fsand                 = 0.82,      
  fclay                 = 0.06,      
  fsilt                 = 0.12,  
  kA                    = 107,     
  kalb_sw               = 0.17,    
  kalb_vis              = 0.03,    
  kb                    = 0.20,    
  kc                    = 0.25,    
  kCw                   = 1.05,    
  kd                    = 0.50,    
  ke                    = 0.0167,  
  keps                  = 23.44,   
  kWm                   = 220.0,   
  kw                    = 0.26,    
  komega                = 283.0,
  maxmeltrate           = 3.0,
  klitt_af10            = 1.2,
  klitt_as10            = 0.35,
  klitt_bg10            = 0.35,
  kexu10                = 50.0,
  ksoil_fs10            = 0.021,
  ksoil_sl10            = 7.0e-04,
  ntoc_crit1            = 0.45,
  ntoc_crit2            = 0.76,
  cton_microb           = 10.0,
  cton_soil             = 9.77,
  fastfrac              = 0.985,
  eff_nup               = 0.600000,
  minimumcostfix        = 1.000000,
  fixoptimum            = 25.15000,
  a_param_fix           = -3.62000,
  b_param_fix           = 0.270000,
  maxnitr               = 0.1,
  non                   = 0.01,
  n2on                  = 0.0005,
  kn                    = 83.0,
  kdoc                  = 17.0,
  docmax                = 1.0,
  dnitr2n2o             = 0.01,
  beta                  = 146.000000,
  rd_to_vcmax           = 0.01400000,
  tau_acclim            = 10,
  tmppar                = 9999)

#download forcing file
#siteinfo_final <- as.data.frame(tibble(sitename="CH-Cha",lon =8.41044444,
#                            lat = 47.21022222,elv =  393,year_start=2012,year_end=2015))
#  df_watch <- ingest(
#    siteinfo  = siteinfo_final[1,],
#    source    = "watch_wfdei",
#    getvars   = c("temp", "prec", "ppfd", "vpd", "patm"), 
#    dir       = "/Volumes/My Passport/data/watch_wfdei/",
#    settings  = list(correct_bias = "worldclim", dir_bias = "/Volumes/My Passport/data/worldclim/"))
#  df_cru <- ingest(
#    siteinfo  = siteinfo_final[1,],
#    source    = "cru",
#    getvars   = "ccov",
#    dir       = "/Volumes/My Passport/data/cru/ts_4.01/")
#  df_co2 <- ingest(
#    siteinfo  = siteinfo_final[1,],
#    source  = "co2_mlo",
#    verbose = FALSE)
#  df_co2_final <- as.data.frame(df_co2$data)
#  df_co2_final2 <- df_co2_final[!(format(df_co2_final$date,"%m") == "02" & format(df_co2_final$date, "%d") == "29"), , drop = FALSE] # make columns consistent
#  co2 <- df_co2_final2$co2
#  ddf_meteo <- as_tibble(cbind(as.data.frame(df_watch$data),as.data.frame(df_cru$data)[,c("ccov_int","ccov")],co2))
#  ddf_meteo$fapar <- 1
#  ddf_meteo$tmax <- ddf_meteo$temp
#  ddf_meteo$tmin <- ddf_meteo$temp
#  ddf_meteo$sitename <- siteinfo_final[1,c("sitename")]
#  ddf_meteo$lon <- siteinfo_final[1,c("lon")]
#  ddf_meteo$lat <-  siteinfo_final[1,c("lat")]
#  ddf_meteo$elv <-  siteinfo_final[1,c("elv")]
#  ddf_meteo$year_start <-  siteinfo_final[1,c("year_start")]
#  ddf_meteo$year_end <-  siteinfo_final[1,c("year_end")]
#  csvfile <- paste("~/data/n2o_kathrin/forcing_wfdei/",siteinfo_final$sitename[1],".csv",sep = "")
#  write.csv(ddf_meteo, csvfile, row.names = TRUE)

#read n2o data
df <- read.csv("~/data/n2o_kathrin/n2o_data/2012-2015_full_output_QC_FINAL_FLAG.csv")

#subset data, n2o = 0 (best data)is accepted. n2o = 1 (normal data), n2o = 2 (bad data) or -9999 (bad data) was removed. 
df_good <- subset(df,qc_n2o_flux == 0)
dim(df);dim(df_good)

df_good$days <- as.Date(df_good$TIMESTAMP)
df_good$n2o_flux <- as.numeric(df_good$n2o_flux)
df_good2 <- df_good[,c("days","n2o_flux")]
df_daily <- aggregate(df_good2,by=list(df_good2$days), FUN=mean, na.rm=TRUE)
dim(df_daily)
plot(n2o_flux~days,df_daily)

#read forcing file from wfdei

siteinfo_site <- as.data.frame(tibble(sitename="CH-Cha",lon =8.41044444,
                            lat = 47.21022222,elv =  393,year_start=2012,year_end=2015))

sitename <- siteinfo_site$sitename

df1 <- read.csv("/Users/yunpeng/data/n2o_kathrin/forcing_wfdei/CH-Cha.csv")

df1$date <- as.Date(df1$date);df1$doy <- 1:length(df1$date)
forcing <- rsofun::p_model_drivers
library(dplyr)
forcing$forcing[[1]] <- as_tibble(df1[,c("date","temp","prec","vpd","ppfd","patm","ccov_int","ccov","snow","rain","fapar","co2","doy","tmin","tmax")])
forcing$sitename <- df1$sitename[1]
forcing$site_info[[1]]$lon <- df1$lon[1]
forcing$site_info[[1]]$lat <- df1$lat[1]
forcing$site_info[[1]]$elv <-  df1$elv[1]
forcing$site_info[[1]]$date_start <- df1$date[1]
forcing$site_info[[1]]$date_end <- df1$date[length(df1$date)]
forcing$params_siml[[1]]$firstyeartrend <- 2012
forcing$params_siml[[1]]$nyeartrend <- 4
tmp <- forcing %>% mutate(forcing = purrr::map(forcing, ~mutate(., fharv = 0.0, dno3 = 0.1,dnh4 = 0.1)))
tmp$params_siml[[1]]$spinupyears <- 1500
tmp$params_siml[[1]]$recycle <- 1
modlist1 <- rsofun::runread_pmodel_f(tmp,par = pars)

n2o_dataset_wfdei <- as.data.frame(modlist1$data[[1]])
n2o_wfdei <- n2o_dataset_wfdei$en2o

#forcing basing on metero
forcing <- read.csv("/Users/yunpeng/data/n2o_kathrin/forcing_flux/CH-Cha_meteo_flux_management_2005-2020_vID20210820.csv")
forcing$days <- as.Date(forcing$TIMESTAMP)
forcing_daily <- aggregate(forcing,by=list(forcing$days), FUN=mean, na.rm=TRUE)
forcing_daily$years <- format(forcing_daily$Group.1, format = "%Y")
forcing_daily_years <- subset(forcing_daily,years>=2012 & years <=2015)

forcing_daily2 <- subset(forcing_daily_years,Group.1!="2012-02-29")
dim(forcing_daily2)

#now, convert this data to forcing file 

#forcing$P_rain # Precipitation sum (mm)
#forcing$Tair_f # Air temperature gapfilled (first half of 2005 with MeteoSwiss data from Cham station) at 2 m measurement height 
#forcing$PA #Air pressure measured at the station (393 m a.s.l.) (hPa)
#forcing$PPFD_IN #Photosynthetic photon flux density (umol/m2/s)
#forcing$VPD_f # Vapor pressure deficit gapfilled (Tair_f and RH used for calculation) (hPa)

df_daily_forcing <- merge(forcing_daily2,df_daily[c("Group.1","n2o_flux")],by=c("Group.1"),all.x=TRUE)
summary(df_daily_forcing)

df_daily_forcing$date <- as.Date(df_daily_forcing$Group.1)
df_daily_forcing$doy <- 1:length(df_daily_forcing$date)

df_daily_forcing$rain <- df_daily_forcing$P_rain/1800 #this unit is mm/half-hour - needs to convert to mm/s
df_daily_forcing$snow <- df1$snow #this was using forcing_wfdei data

df_daily_forcing$prec <- df_daily_forcing$rain+df_daily_forcing$snow

df_daily_forcing$temp <- df_daily_forcing$Tair_f
df_daily_forcing$tmin <- df_daily_forcing$Tair_f
df_daily_forcing$tmax <- df_daily_forcing$Tair_f

df_daily_forcing$vpd <- df_daily_forcing$VPD_f*100 #convert from hPa to Pa

df_daily_forcing$ppfd <- df_daily_forcing$PPFD_IN/1000000 # convert from umol/m2/s to mol/m2/s

df_daily_forcing$patm <- df_daily_forcing$PA*100  #convert from hPa to Pa
df_daily_forcing$fapar <- 1
df_daily_forcing$ccov <- df1$ccov
df_daily_forcing$ccov_int <- df1$ccov_int
df_daily_forcing$co2 <- df1$co2
#assume cld = 65
#assume co2 = 400


final_forcing <- (as_tibble(df_daily_forcing[,c("date","temp","prec","vpd","ppfd",
                      "patm","ccov_int","ccov","snow",
                      "rain","fapar","co2","tmin","tmax")]))

final_forcing$ppfd[is.na(final_forcing$ppfd)==TRUE] <- 0
final_forcing$patm[is.na(final_forcing$patm)==TRUE] <- mean(final_forcing$patm,na.rm=TRUE)

summary(final_forcing)


#start cnmodel



#this plots not work
sitename <- "CH-Cha"
forcing <- rsofun::p_model_drivers

forcing$forcing[[1]] <- final_forcing
forcing$sitename <- "CH-Cha"
forcing$site_info[[1]]$lon <- 8.41044444 
forcing$site_info[[1]]$lat <- 47.21022222
forcing$site_info[[1]]$elv <-  393
forcing$site_info[[1]]$year_end <- 2015

forcing$site_info[[1]]$date_start <- (final_forcing$date[1])
forcing$site_info[[1]]$date_end <-  (final_forcing$date[length(final_forcing$date)])
forcing$params_siml[[1]]$firstyeartrend <- 2012
forcing$params_siml[[1]]$nyeartrend <- 4

tmp <- forcing %>% mutate(forcing = purrr::map(forcing, ~mutate(., fharv = 0.0, dno3 = 0.1,dnh4 = 0.1)))
tmp$params_siml[[1]]$spinupyears <- 1500
tmp$params_siml[[1]]$recycle <- 1

modlist1 <- rsofun::runread_pmodel_f(tmp,par = pars)#ambient
modlist1 <- as.data.frame(modlist1$data[[1]])
summary(modlist1)
modlist1$en2o

df_daily_forcing$pred_n2o <- modlist1$en2o
df_daily_forcing$pred_n2o_wfdei <- n2o_wfdei

ggplot(data=df_daily_forcing, aes(x=date, y=pred_n2o))+
  geom_point( aes(x=date, y=n2o_flux),color="black")+
  geom_line( aes(x=date, y=n2o_flux),color="black")+
  geom_line( aes(x=date, y=pred_n2o),color="red")+
  geom_line( aes(x=date, y=pred_n2o_wfdei),color="orange")+ theme_classic()

ggsave(paste("~/data/n2o.jpg",sep=""),width = 10, height = 5)

df_daily_forcing$years <- as.numeric(format(df_daily_forcing$Group.1, format = "%Y"))

ggplot(data=subset(df_daily_forcing,years==2014), aes(x=date, y=pred_n2o))+
  geom_point( aes(x=date, y=n2o_flux),color="black")+
  geom_line( aes(x=date, y=n2o_flux),color="black")+
  geom_line( aes(x=date, y=pred_n2o),color="red")+
  geom_line( aes(x=date, y=pred_n2o_wfdei),color="orange")+ theme_classic()

##