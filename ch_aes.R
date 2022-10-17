library(dplyr)
library(ggplot2)
library(patchwork)
library(readr)
library(lubridate)
library(plotrix)
library(hwsdr)

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
# see site info in /Users/yunpeng/data/ch_aes/Site description_soil_Yunke.docx

siteinfo_final <- as.data.frame(tibble(sitename=c("CH-oe2","CH-aes"),
                                       lon =c(7.73,7.66),
                                       lat = c(47.28,47.17),elv =  c(452,465),
                                       year_start=c(2012,2012),year_end=c(2016,2016)))
siteinfo_final
#for (i in 1:(nrow(siteinfo_final))){
#  df_watch <- ingest(
#    siteinfo  = siteinfo_final[i,],
#    source    = "watch_wfdei",
#    getvars   = c("temp", "prec", "ppfd", "vpd", "patm"), 
#    dir       = "/Volumes/My Passport/data/watch_wfdei/",
#    settings  = list(correct_bias = "worldclim", dir_bias = "/Volumes/My Passport/data/worldclim/"))
#  df_cru <- ingest(
#    siteinfo  = siteinfo_final[i,],
#    source    = "cru",
#    getvars   = "ccov",
#    dir       = "/Volumes/My Passport/data/cru/ts_4.01/")
#  df_co2 <- ingest(
#    siteinfo  = siteinfo_final[i,],
#   source  = "co2_mlo",
#    verbose = FALSE)
#  df_co2_final <- as.data.frame(df_co2$data)
#  df_co2_final2 <- df_co2_final[!(format(df_co2_final$date,"%m") == "02" & format(df_co2_final$date, "%d") == "29"), , drop = FALSE] # make columns consistent
#  co2 <- df_co2_final2$co2
#  ddf_meteo <- as_tibble(cbind(as.data.frame(df_watch$data),as.data.frame(df_cru$data)[,c("ccov_int","ccov")],co2))
#  ddf_meteo$fapar <- 1
#  ddf_meteo$tmax <- ddf_meteo$temp
#  ddf_meteo$tmin <- ddf_meteo$temp
#  ddf_meteo$sitename <- siteinfo_final[i,c("sitename")]#  ddf_meteo$lon <- siteinfo_final[1,c("lon")]
#  ddf_meteo$lat <-  siteinfo_final[i,c("lat")]
#  ddf_meteo$elv <-  siteinfo_final[i,c("elv")]
#  ddf_meteo$year_start <-  siteinfo_final[i,c("year_start")]
#  ddf_meteo$year_end <-  siteinfo_final[i,c("year_end")]
#  csvfile <- paste("~/data/ch_oe2/forcing_wfdei/",siteinfo_final$sitename[i],".csv",sep = "")
#  write.csv(ddf_meteo, csvfile, row.names = TRUE)
#}

#deal with ch_aes
#read 2012-2016 and combine into daily average 
df1 <- read.csv("~/data/ch_oe2/forcing_wfdei/CH-aes.csv")

df1$date <- as.Date(df1$date)
forcing <- rsofun::p_model_drivers

df2 <- df1 %>% mutate(ymonth = month(date),
                      yday = day(date)) %>% 
  group_by(ymonth, yday) %>% 
  summarise(temp = mean(temp, na.rm = TRUE),prec = mean(prec, na.rm = TRUE),vpd = mean(vpd, na.rm = TRUE),
            ppfd = mean(ppfd, na.rm = TRUE),patm = mean(patm, na.rm = TRUE),ccov_int = mean(ccov_int, na.rm = TRUE),ccov = mean(ccov, na.rm = TRUE),
            snow = mean(snow, na.rm = TRUE),rain = mean(rain, na.rm = TRUE),fapar = mean(fapar, na.rm = TRUE),
            co2 = mean(co2, na.rm = TRUE),tmin = mean(tmin, na.rm = TRUE),tmax = mean(tmax, na.rm = TRUE))

#create a repeated years, 2 years
df3 <- rbind(df2,df2)
date_length <- seq(as.Date("2019-01-01"), as.Date("2020-12-31"), "days")
date_length<- date_length[!(format(date_length,"%m") == "02" & format(date_length, "%d") == "29"),drop = FALSE]
length(date_length)
df3$date <- date_length

df3$doy <- 1:length(df3$date)

forcing_data <- as_tibble(df3[,c("date","temp","prec","vpd","ppfd","patm","ccov_int","ccov","snow","rain","fapar","co2","doy","tmin","tmax")])

#forcing basing on metero
a2019_2010 <- read.csv("~/data/ch_aes/CH-AES_meteo_fluxes_2019-2020_Yunke.csv")
a2019_2010$date <- as.Date(a2019_2010$TIMESTAMP)
n2o_daily <- aggregate(a2019_2010,by=list(a2019_2010$date), FUN=mean, na.rm=TRUE)
dim(n2o_daily)
summary(n2o_daily)

#now, convert this data to forcing file 

#PREC Precipitation sum (mm)
#TA air temperature 
#Rg: shortwave incoming radiation
#Solar radiation (Wm–2) was converted to PPFD by multiplication by the energy-to-flux conversion factor 2.04 (μmol J–1)1 (Davies et al. 2017)
#VPD: Vapor pressure deficit gapfilled (Tair_f and RH used for calculation) (Pa)
n2o_daily$rain_meteo <- n2o_daily$PREC/1800 #this unit is mm/half-hour - needs to convert to mm/s

n2o_daily$temp_meteo <- n2o_daily$TA

n2o_daily$vpd_meteo <- n2o_daily$VPD

n2o_daily$ppfd_meteo <- n2o_daily$Rg*2.04/1000000 # convert from umol/m2/s to mol/m2/s

df_daily_forcing <- merge(forcing_data,n2o_daily[,c("date","rain_meteo","temp_meteo","vpd_meteo","ppfd_meteo","N2O_gf")],by=c("date"),all.x=TRUE)
summary(df_daily_forcing)
#compare meteo and wfdei data
plot(df_daily_forcing$temp~df_daily_forcing$temp_meteo)
plot(df_daily_forcing$vpd~df_daily_forcing$vpd_meteo)
ppfd_wfdei <- df_daily_forcing$ppfd*1000000
ppfd_meteo <- df_daily_forcing$ppfd_meteo*1000000
plot(ppfd_wfdei~ppfd_meteo)
rain_wfdei <- df_daily_forcing$rain*1000000
rain_meteo <- df_daily_forcing$rain_meteo*1000000
plot(rain_wfdei~rain_meteo)

df_daily_forcing$prec <- df_daily_forcing$rain+df_daily_forcing$snow
df_daily_forcing$tmin <- df_daily_forcing$temp
df_daily_forcing$tmax <- df_daily_forcing$temp

#ndep
df_ndep <- ingest_bysite(
  sitename  = "ch-aes",source    = "ndep",
  lon  = 7.66,lat= 47.17,year_start= 2000,year_end  = 2009,
  timescale = "y",dir= "~/data/ndep_lamarque/",verbose   = FALSE)
noy_chaes <- mean(df_ndep$noy,na.rm=TRUE)/365 #gN/m2/yr to gN/m2/day
nhx_chaes <-mean(df_ndep$nhx,na.rm=TRUE)/365 #gN/m2/yr to gN/m2/day


forcing$forcing[[1]] <- (as_tibble(df_daily_forcing[,c("date","temp","prec","vpd","ppfd",
                                                       "patm","ccov_int","ccov","snow",
                                                       "rain","fapar","co2","tmin","tmax")]))

forcing$sitename <- df1$sitename[1]
forcing$site_info[[1]]$lon <- df1$lon[1]
forcing$site_info[[1]]$lat <- df1$lat[1]
forcing$site_info[[1]]$elv <-  df1$elv[1]
forcing$site_info[[1]]$date_start <- as.Date("2019-01-01")
forcing$site_info[[1]]$date_end <- as.Date("2020-12-31")
forcing$params_siml[[1]]$firstyeartrend <- 2019
forcing$site_info[[1]]$year_end <- 2020
forcing$params_siml[[1]]$nyeartrend <- 2
tmp <- forcing %>% mutate(forcing = purrr::map(forcing, ~mutate(., fharv = 0.0, dno3 = noy_chaes,
                                                                dnh4 = nhx_chaes,
                                                                cseed=0,nseed=0)))
tmp$params_siml[[1]]$spinupyears <- 1500
tmp$params_siml[[1]]$recycle <- 1

#/Users/yunpeng/data/ch_aes/CH-AES_management_2019-2020.docx
#manure fertilization twice so was not included 

#22.05.2020 urea application and decomposition: (NH2)2CO → CNO− + NH4+
tmp$forcing[[1]]$dno3[tmp$forcing[[1]]$date=="2020-05-22"] <- 9.2
tmp$forcing[[1]]$dnh4[tmp$forcing[[1]]$date=="2020-05-22"] <- 9.2

modlist2 <- rsofun::runread_pmodel_f(tmp,par = pars)
modlist2$data

#repeat again for using meteo
df_daily_forcing$temp[is.na(df_daily_forcing$temp_meteo)==F] <- df_daily_forcing$temp_meteo[is.na(df_daily_forcing$temp_meteo)==F]
df_daily_forcing$vpd[is.na(df_daily_forcing$vpd_meteo)==F] <- df_daily_forcing$vpd_meteo[is.na(df_daily_forcing$vpd_meteo)==F]
df_daily_forcing$ppfd[is.na(df_daily_forcing$ppfd_meteo)==F] <- df_daily_forcing$ppfd_meteo[is.na(df_daily_forcing$ppfd_meteo)==F]
df_daily_forcing$rain[is.na(df_daily_forcing$rain_meteo)==F] <- df_daily_forcing$rain_meteo[is.na(df_daily_forcing$rain_meteo)==F]

tmp$forcing[[1]]$temp <- df_daily_forcing$temp
tmp$forcing[[1]]$vpd <- df_daily_forcing$vpd
tmp$forcing[[1]]$ppfd <- df_daily_forcing$ppfd
tmp$forcing[[1]]$rain <- df_daily_forcing$rain

modlist2_meteo <- rsofun::runread_pmodel_f(tmp,par = pars)

en2o <- as.data.frame(modlist2$data[[1]])$en2o
en2o_meteo <- as.data.frame(modlist2_meteo$data[[1]])$en2o

df_daily_forcing$obs_n2o <- df_daily_forcing$N2O_gf/1000 #nmol/m2/s to umol/m2/s
df_daily_forcing$pred_n2o <- en2o*1000000/14/86400   #convert from gN/m2/d to umol/m2/s
df_daily_forcing$pred_n2o_meteo <- en2o_meteo*1000000/14/86400  #umol/m2/s

df_daily_forcing$years <- format(df_daily_forcing$date, format = "%Y")

obs_aes_2019 <- as.numeric(subset(df_daily_forcing,
                              years==2019 & is.na(obs_n2o)==F)$obs_n2o)
pred_aes_2019 <- as.numeric(subset(df_daily_forcing,
                               years==2019 & is.na(obs_n2o)==F)$pred_n2o)
mean(obs_aes_2019)*1000
std.error(obs_aes_2019)*1000
mean(pred_aes_2019)*1000
std.error(pred_aes_2019)*1000

ggplot(data=subset(df_daily_forcing,years==2019 & is.na(obs_n2o)==F), 
       aes(x=date, y=obs_n2o))+
  geom_point( aes(x=date, y=obs_n2o*1000),color="black")+
  geom_line( aes(x=date, y=obs_n2o*1000),color="black")+
  geom_line( aes(x=date, y=pred_n2o*1000),color="red")+
  geom_line( aes(x=date, y=pred_n2o_meteo*1000),color="blue")+
  theme_classic()+
  ylab("N2O (nmol/m2/s)")+xlab("2019")+theme(axis.text=element_text(size=12))
ggsave(paste("~/data/n2o_2019_aes.jpg",sep=""),width = 10, height = 5)

obs_aes_2020 <- as.numeric(subset(df_daily_forcing,
                              years==2020 & is.na(obs_n2o)==F)$obs_n2o)
pred_aes_2020 <- as.numeric(subset(df_daily_forcing,
                               years==2020 & is.na(obs_n2o)==F)$pred_n2o)
mean(obs_aes_2020)*1000
std.error(obs_aes_2020)*1000
mean(pred_aes_2020)*1000
std.error(pred_aes_2020)*1000

ggplot(data=subset(df_daily_forcing,years==2020& is.na(obs_n2o)==F),
       aes(x=date, y=obs_n2o))+
  geom_point( aes(x=date, y=obs_n2o*1000),color="black")+
  geom_line( aes(x=date, y=obs_n2o*1000),color="black")+
  geom_line( aes(x=date, y=pred_n2o*1000),color="red")+
  geom_line( aes(x=date, y=pred_n2o_meteo*1000),color="blue")+
  theme_classic()+
  ylab("N2O (nmol/m2/s)")+xlab("2019")+theme(axis.text=element_text(size=12))
ggsave(paste("~/data/n2o_2020_aes.jpg",sep=""),width = 10, height = 5)
