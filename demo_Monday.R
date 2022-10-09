library(dplyr)
library(ggplot2)
library(patchwork)
library(readr)
library(lubridate)
#r-markdown not work - can only be applied in R
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

df_daily_forcing <- read.csv("~/data/ch_oe2/demo/CHOE2_demo.csv")
final_forcing <- (as_tibble(df_daily_forcing[,c("date","temp","prec","vpd","ppfd",
                                                "patm","ccov_int","ccov","snow",
                                                "rain","fapar","co2","tmin","tmax")]))

forcing <- rsofun::p_model_drivers
forcing$forcing[[1]] <- as_tibble(final_forcing)

forcing$sitename <- "CH-oe2"
forcing$site_info[[1]]$lon <- 47.17819
forcing$site_info[[1]]$lat <- 7.665111
forcing$site_info[[1]]$elv <-  465
forcing$site_info[[1]]$date_start <- as.Date("2018-01-01")
forcing$site_info[[1]]$date_end <- as.Date("2019-12-31")
forcing$site_info[[1]]$year_end <- 2019
forcing$params_siml[[1]]$firstyeartrend <- 2018
forcing$params_siml[[1]]$nyeartrend <- 2
tmp <- forcing %>% mutate(forcing = purrr::map(forcing, ~mutate(., fharv = 0.0,
                                                                dno3 = df_daily_forcing$noy[1],dnh4 = df_daily_forcing$nhx[1])))
tmp$params_siml[[1]]$spinupyears <- 1500
tmp$params_siml[[1]]$recycle <- 1

modlist1 <- rsofun::runread_pmodel_f(tmp,par = pars)
modlist1$data

#Question 1: check N-balance
#check N balance
# N[total] = nleaf + nroot + nsoil + ninorg +  nlabl + seedn + nlitt
# Then: delta-N[total] - nfix = nloss?
#now it is not completely equal
output_df <- as.data.frame(modlist1$data)

output_df$n_total<- (output_df$nleaf+output_df$nroot+output_df$nsoil+output_df$ninorg+
                       output_df$nlabl+output_df$seedn+output_df$nlitt)
output_df$delta_n_total <- c(NA,diff(output_df$n_total)) #the first day is expressed as NA

ggplot(data=output_df)+
  geom_line( aes(x=date, y=delta_n_total),color="orange")+
  geom_line( aes(x=date, y=nloss),color="red")+
  geom_line( aes(x=date, y=nfix),color="purple")

#Question 2; fharv was not applied in model
tmp$forcing[[1]]$fharv[tmp$forcing[[1]]$date=="2018-07-12"] <- 1.0
modlist2 <- rsofun::runread_pmodel_f(tmp,par = pars)
summary(modlist2$data[[1]]$en2o-modlist1$data[[1]]$en2o)

#apply N fertilization
tmp$forcing[[1]]$dno3[tmp$forcing[[1]]$date=="2019-02-28"] <- 5
tmp$forcing[[1]]$dnh4[tmp$forcing[[1]]$date=="2019-02-28"] <- 5

tmp$forcing[[1]]$dno3[tmp$forcing[[1]]$date=="2019-04-02"] <- 4
tmp$forcing[[1]]$dnh4[tmp$forcing[[1]]$date=="2019-04-02"] <- 4

modlist3 <- rsofun::runread_pmodel_f(tmp,par = pars)
summary(modlist3$data[[1]]$en2o-modlist2$data[[1]]$en2o)

df_daily_forcing$en2o_meteo <- modlist3$data[[1]]$en2o
df_daily_forcing$years <- year(df_daily_forcing$date)
df_daily_forcing$date <- as.Date(df_daily_forcing$date)
ggplot(data=subset(df_daily_forcing,years==2018 & is.na(obs_n2o)==F),
       aes(x=date, y=obs_n2o))+
  geom_point( aes(x=date, y=obs_n2o),color="black")+
  geom_line( aes(x=date, y=obs_n2o),color="black")+
  geom_line( aes(x=date, y=en2o_meteo),color="red")+ theme_classic()+
  ylab("N2O (umol/m2/s)")+xlab("2018")

ggplot(data=subset(df_daily_forcing,years==2019 & is.na(obs_n2o)==F),
       aes(x=date, y=obs_n2o))+
  geom_point( aes(x=date, y=obs_n2o),color="black")+
  geom_line( aes(x=date, y=obs_n2o),color="black")+
  geom_line( aes(x=date, y=en2o_meteo),color="red")+ theme_classic()+
  ylab("N2O (umol/m2/s)")+xlab("2019")
