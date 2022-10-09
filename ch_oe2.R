library(dplyr)
library(ggplot2)
library(patchwork)
library(readr)
library(lubridate)
library(plotrix)
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

#The study site was located in Aeschi in the canton of Solothurn in Switzerland (47° 10′ 41.5” N, 7° 39′ 54.4″ E, 465 m a.s.l.). 
#https://doi.org/10.1016/j.scitotenv.2022.157541
#only has year until 2016 or 2018 - so just using 2012-2016 and aggregating them to one year

#siteinfo_final <- as.data.frame(tibble(sitename="CH-oe2",lon =47.17819444,
#                            lat = 7.66511111,elv =  465,year_start=2012,year_end=2016))
#  df_watch <- ingest(
#    siteinfo  = siteinfo_final[1,],
#    source    = "watch_wfdei",
#    getvars   = c("temp", "prec", "ppfd", "vpd", "patm"), 
#    dir       = "/Volumes/My Passport/data/watch_wfdei/",
#    settings  = list(correct_bias = "worldclim", dir_bias = "/Volumes/My Passport/data/worldclim/"))
# df_cru <- ingest(
#    siteinfo  = siteinfo_final[1,],
#    source    = "cru",
#    getvars   = "ccov",
#    dir       = "/Volumes/My Passport/data/cru/ts_4.01/")
#  df_co2 <- ingest(
#    siteinfo  = siteinfo_final[1,],
#   source  = "co2_mlo",
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
#  csvfile <- paste("~/data/ch_oe2/forcing_wfdei/",siteinfo_final$sitename[1],".csv",sep = "")
#  write.csv(ddf_meteo, csvfile, row.names = TRUE)

#read forcing file from wfdei

#version 1- using wfdei
#read 2012-2016 and combine into daily average 
siteinfo_site <- as.data.frame(tibble(sitename="CH-oe2",lon =47.17819444,
                            lat = 7.66511111,elv =  465,year_start=2018,year_end=2019))

sitename <- siteinfo_site$sitename

df1 <- read.csv("~/data/ch_oe2/forcing_wfdei/CH-oe2.csv")

df1$date <- as.Date(df1$date)
forcing <- rsofun::p_model_drivers
library(dplyr)
dim(df1)

df2 <- df1 %>% mutate(ymonth = month(date),
                      yday = day(date)) %>% 
  group_by(ymonth, yday) %>% 
  summarise(temp = mean(temp, na.rm = TRUE),prec = mean(prec, na.rm = TRUE),vpd = mean(vpd, na.rm = TRUE),
            ppfd = mean(ppfd, na.rm = TRUE),patm = mean(patm, na.rm = TRUE),ccov_int = mean(ccov_int, na.rm = TRUE),ccov = mean(ccov, na.rm = TRUE),
            snow = mean(snow, na.rm = TRUE),rain = mean(rain, na.rm = TRUE),fapar = mean(fapar, na.rm = TRUE),
            co2 = mean(co2, na.rm = TRUE),tmin = mean(tmin, na.rm = TRUE),tmax = mean(tmax, na.rm = TRUE))

#create a repeated years, 2 years
df3 <- rbind(df2,df2)
df3$date <- seq(as.Date("2018-01-01"), as.Date("2019-12-31"), "days")

df3$doy <- 1:length(df3$date)

forcing_data <- as_tibble(df3[,c("date","temp","prec","vpd","ppfd","patm","ccov_int","ccov","snow","rain","fapar","co2","doy","tmin","tmax")])

forcing$forcing[[1]] <- forcing_data

forcing$sitename <- df1$sitename[1]
forcing$site_info[[1]]$lon <- df1$lon[1]
forcing$site_info[[1]]$lat <- df1$lat[1]
forcing$site_info[[1]]$elv <-  df1$elv[1]
forcing$site_info[[1]]$date_start <- df1$date[1]
forcing$site_info[[1]]$date_end <- df1$date[length(df1$date)]
forcing$params_siml[[1]]$firstyeartrend <- 2018
forcing$params_siml[[1]]$nyeartrend <- 2
tmp <- forcing %>% mutate(forcing = purrr::map(forcing, ~mutate(., fharv = 0.0, dno3 = 0.1,dnh4 = 0.1)))

#modify fharv, dno3 and dnh4
tmp$forcing[[1]]$fharv[tmp$forcing[[1]]$date>="2018-07-12"&tmp$forcing[[1]]$date<"2018-10-11"] <- 1.0
tmp$forcing[[1]]$fharv[tmp$forcing[[1]]$date>="2019-07-19"&tmp$forcing[[1]]$date<"2019-10-04"] <- 1.0

tmp$forcing[[1]]$dno3[tmp$forcing[[1]]$date=="2019-02-28"] <- 5
tmp$forcing[[1]]$dnh4[tmp$forcing[[1]]$date=="2019-02-28"] <- 5

tmp$forcing[[1]]$dno3[tmp$forcing[[1]]$date=="2019-04-02"] <- 4
tmp$forcing[[1]]$dnh4[tmp$forcing[[1]]$date=="2019-04-02"] <- 4

tmp$params_siml[[1]]$spinupyears <- 1500
tmp$params_siml[[1]]$recycle <- 1
modlist1 <- rsofun::runread_pmodel_f(tmp,par = pars)
modlist1$data


####verion 2 - using meteo
#forcing basing on metero
a2018 <- read.csv("~/data/ch_oe2/CH-OE2_meteo_fluxes_2018_Yunke.csv")
a2019 <- read.csv("~/data/ch_oe2/CH-OE2_meteo_fluxes_2019_Yunke.csv")
a_final <- rbind(a2018,a2019)
a_final$date <- as.Date(a_final$TIMESTAMP)
n2o_daily <- aggregate(a_final,by=list(a_final$date), FUN=mean, na.rm=TRUE)


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
#plot difference between wfdei and meteo

ggplot(data=df_daily_forcing, aes(x=temp_meteo, y=temp))+
  geom_point( color="black")+theme_classic()

ggplot(data=df_daily_forcing, aes(x=vpd_meteo, y=vpd))+
  geom_point( color="black")+theme_classic()

ggplot(data=df_daily_forcing, aes(x=ppfd_meteo, y=ppfd))+
  geom_point( color="black")+theme_classic()

#some days magnitude's rain looks not good?
ggplot(data=df_daily_forcing, aes(x=rain_meteo, y=rain))+
  geom_point( color="black")+theme_classic()

#update meteo into forcing
df_daily_forcing$temp[is.na(df_daily_forcing$temp_meteo)==F] <- df_daily_forcing$temp_meteo[is.na(df_daily_forcing$temp_meteo)==F]
df_daily_forcing$vpd[is.na(df_daily_forcing$vpd_meteo)==F] <- df_daily_forcing$vpd_meteo[is.na(df_daily_forcing$vpd_meteo)==F]
df_daily_forcing$ppfd[is.na(df_daily_forcing$ppfd_meteo)==F] <- df_daily_forcing$ppfd_meteo[is.na(df_daily_forcing$ppfd_meteo)==F]
df_daily_forcing$rain[is.na(df_daily_forcing$rain_meteo)==F] <- df_daily_forcing$rain_meteo[is.na(df_daily_forcing$rain_meteo)==F]
df_daily_forcing$prec <- df_daily_forcing$rain+df_daily_forcing$snow
df_daily_forcing$tmin <- df_daily_forcing$temp
df_daily_forcing$tmax <- df_daily_forcing$temp

final_forcing <- (as_tibble(df_daily_forcing[,c("date","temp","prec","vpd","ppfd",
                                                "patm","ccov_int","ccov","snow",
                                                "rain","fapar","co2","tmin","tmax")]))
final_forcing2 <- final_forcing
final_forcing2$obs_n2o <- df_daily_forcing$N2O_gf/1000 #nmol/m2/s to umol/m2/s

#NRE_site newly including Ndep
library(hwsdr)
#devtools::load_all("/Users/yunpeng/yunkepeng/compuetational_ingestr/ingestr/")

final_forcing2$nhx <- NA
final_forcing2$noy <- NA
#maximum year is 2009
df_ndep <- ingest_bysite(
  sitename  = "ch-oe2",source    = "ndep",
  lon  = 47.17819,lat= 7.665111,year_start= 2000,year_end  = 2009,
  timescale = "y",dir= "~/data/ndep_lamarque/",verbose   = FALSE)
final_forcing2$noy <- mean(df_ndep$noy,na.rm=TRUE)
final_forcing2$nhx <- mean(df_ndep$nhx,na.rm=TRUE)

csvfile <- paste("~/data/ch_oe2/demo/CHOE2_demo.csv",sep = "")
write.csv(final_forcing2, csvfile, row.names = FALSE)

forcing$forcing[[1]] <- final_forcing

forcing$sitename <- df1$sitename[1]
forcing$site_info[[1]]$lon <- df1$lon[1]
forcing$site_info[[1]]$lat <- df1$lat[1]
forcing$site_info[[1]]$elv <-  df1$elv[1]
forcing$site_info[[1]]$date_start <- as.Date("2018-01-01")
forcing$site_info[[1]]$date_end <- as.Date("2019-12-31")
forcing$site_info[[1]]$year_end <- 2019
forcing$params_siml[[1]]$firstyeartrend <- 2018
forcing$params_siml[[1]]$nyeartrend <- 2
tmp <- forcing %>% mutate(forcing = purrr::map(forcing, ~mutate(., fharv = 0.0, dno3 = 0.09611016,dnh4 = 0.1154754)))
tmp$params_siml[[1]]$spinupyears <- 1500
tmp$params_siml[[1]]$recycle <- 1

tmp$forcing[[1]]$dno3[tmp$forcing[[1]]$date=="2019-02-28"] <- 5
tmp$forcing[[1]]$dnh4[tmp$forcing[[1]]$date=="2019-02-28"] <- 5

tmp$forcing[[1]]$dno3[tmp$forcing[[1]]$date=="2019-04-02"] <- 4
tmp$forcing[[1]]$dnh4[tmp$forcing[[1]]$date=="2019-04-02"] <- 4

modlist2 <- rsofun::runread_pmodel_f(tmp,par = pars)
modlist2$data

en2o_wfdei <- as.data.frame(modlist1$data[[1]])$en2o
en2o_meteo <- as.data.frame(modlist2$data[[1]])$en2o

df_daily_forcing$en2o_wfdei <- en2o_wfdei*1000000/14/86400 #convert from gN/m2/d to umol/m2/s
df_daily_forcing$en2o_meteo <- en2o_meteo*1000000/14/86400
df_daily_forcing$obs_n2o <- df_daily_forcing$N2O_gf/1000 #nmol/m2/s to umol/m2/s
df_daily_forcing$pred_n2o <- df_daily_forcing$en2o_meteo  #umol/m2/s

df_daily_forcing$years <- format(df_daily_forcing$date, format = "%Y")

obs_2018 <- as.numeric(subset(df_daily_forcing,
                              years==2018 & is.na(obs_n2o)==F)$obs_n2o)
pred_2018 <- as.numeric(subset(df_daily_forcing,
                               years==2018 & is.na(obs_n2o)==F)$pred_n2o)
mean(obs_2018)*1000
std.error(obs_2018)*1000
mean(pred_2018)*1000
std.error(pred_2018)*1000

ggplot(data=subset(df_daily_forcing,years==2018 & is.na(obs_n2o)==F), 
       aes(x=date, y=obs_n2o))+
  geom_point( aes(x=date, y=obs_n2o*1000),color="black")+
  geom_line( aes(x=date, y=obs_n2o*1000),color="black")+
  geom_line( aes(x=date, y=pred_n2o*1000),color="red")+ theme_classic()+
  ylab("N2O (nmol/m2/s)")+xlab("2018")+theme(axis.text=element_text(size=12))
ggsave(paste("~/data/n2o_2018.jpg",sep=""),width = 10, height = 5)

obs_2019 <- as.numeric(subset(df_daily_forcing,
                              years==2019 & is.na(obs_n2o)==F)$obs_n2o)
pred_2019 <- as.numeric(subset(df_daily_forcing,
                               years==2019 & is.na(obs_n2o)==F)$pred_n2o)
mean(obs_2019)*1000
std.error(obs_2019)*1000
mean(pred_2019)*1000
std.error(pred_2019)*1000

ggplot(data=subset(df_daily_forcing,years==2019& is.na(obs_n2o)==F),
       aes(x=date, y=obs_n2o))+
  geom_point( aes(x=date, y=obs_n2o*1000),color="black")+
  geom_line( aes(x=date, y=obs_n2o*1000),color="black")+
  geom_line( aes(x=date, y=pred_n2o*1000),color="red")+ theme_classic()+
  ylab("N2O (nmol/m2/s)")+xlab("2019")+theme(axis.text=element_text(size=12))
ggsave(paste("~/data/n2o_2019.jpg",sep=""),width = 10, height = 5)

data_output <- as.data.frame(modlist1$data[[1]])
csvfile <- paste("~/data/ch-oe2.csv",sep = "")
write.csv(data_output, csvfile, row.names = TRUE)

#check modlist2
output_df <- as.data.frame(modlist2$data)

output_df$n_total<- (output_df$nleaf+output_df$nroot+output_df$nsoil+output_df$ninorg+
  output_df$nlabl+output_df$seedn+output_df$nlitt)
output_df$delta_n_total <- c(NA,diff(output_df$n_total)) #the first day is expressed as NA

ggplot(data=output_df)+
  geom_line( aes(x=date, y=delta_n_total),color="orange")+
  geom_line( aes(x=date, y=nloss),color="red")+
  geom_line( aes(x=date, y=nfix),color="purple")

#check values
summary(output_df$delta_n_total+output_df$nloss)

#some notes
# N[total] = nleaf + nroot + nsoil + ninorg +  nlabl + seedn + nlitt

#N[loss] = nloss?

#check: 
# delta-N[total] - nfix = nloss?

#???N[initial] - N[fixed] = Nfix
#???nfix=0?