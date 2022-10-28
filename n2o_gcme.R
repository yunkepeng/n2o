rm(list = ls())
library(readr)
library(dplyr)
library(metafor)  
library(ggplot2)
library(stringr)
library(tidyverse) 
library(ncmeta)
library(viridis)
library(ggthemes)
library(LSD)
library(yardstick)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(gplots)
library(tidyselect)
library(extrafont)
devtools::load_all("/Users/yunpeng/yunkepeng/rbeni/")
library(raster)
library(maps)
library(rworldmap)
library(cowplot)
library(ncdf4)
library(scales)
library(ggpubr)
library(MAd)

kevin_othervars <- read.csv("~/data/gcme/kevin_20220222/MESI_2022.csv")
kevin_othervars <- rename(kevin_othervars, c(ambient = x_c, elevated=x_t, ambient_Sd=sd_c, elevated_Sd=sd_t,ambient_Se=se_c,elevated_Se=se_t,n_plots=rep_c,
                                             z=elevation, co2_a=c_c, co2_e=c_t, nfertQ_a = n_c, nfertQ_e = n_t, pfertQ_a = p_c, pfertQ_e = p_t,kfertQ_a = k_c, kfertQ_e = k_t,
                                             warmQ_e1 = w_t1, warmQ_e2 = w_t2, warmQ_e3 = w_t3, Unit=x_units))
kevin_othervars$ambient <-as.numeric(kevin_othervars$ambient)
kevin_othervars$elevated <-as.numeric(kevin_othervars$elevated)
kevin_othervars$ambient_Sd  <-as.numeric(kevin_othervars$ambient_Sd)
kevin_othervars$elevated_Sd  <-as.numeric(kevin_othervars$elevated_Sd)
kevin_othervars$ambient_Se <- as.numeric(kevin_othervars$ambient_Se)
kevin_othervars$elevated_Se <- as.numeric(kevin_othervars$elevated_Se)
kevin_othervars$n_plots  <-as.numeric(kevin_othervars$n_plots)
kevin_othervars$z <- as.numeric(kevin_othervars$z)
kevin_othervars$exp_nam <- kevin_othervars$site


response_list <- unique(kevin_othervars$response)
res_df <- as.data.frame(cbind(response_list,c(1:length(response_list))))
soillist <- c("soil_denitrification","soil_gross_n_immobilization","soil_gross_n_mineralization","soil_gross_nitrification",
              "soil_n_immobilization","soil_n_leaching","soil_n_mineralization","soil_n2o_flux",
              "soil_net_ammonification","soil_net_n_immobilization","soil_net_n_mineralization","soil_net_nitrification","soil_nh4_immobilization",
              "soil_nitrification","soil_no3_immobilization","soil_no3_leaching")

kevin_soil <- kevin_othervars %>% filter(response %in% soillist)
unique(kevin_soil$treatment)
kevin_soil_n2o <- subset(kevin_soil,response=="soil_n2o_flux")   
kevin_soil_n2o %>% group_by(treatment) %>% summarise(number=n())
#treatment is dominant

kevin_soil_c <- subset(kevin_soil,treatment=="c")   
dim(kevin_soil_c)

kevin_soil_c %>% group_by(response) %>% summarise(number=n())

#primarily focus on soil_n2o_flux
kevin_soil_c_n2o <- subset(kevin_soil_c,response=="soil_n2o_flux")   
#temporaily only keeping positive now (we will handle unit problem later)
dim(kevin_soil_c_n2o)
kevin_soil_c_n2o <- subset(kevin_soil_c_n2o,ambient>0&elevated>0)
dim(kevin_soil_c_n2o)
kevin_soil_c_n2o$logr <- log(kevin_soil_c_n2o$elevated/kevin_soil_c_n2o$ambient)/log(kevin_soil_c_n2o$co2_e/kevin_soil_c_n2o$co2_a)

kevin_soil_c_n2o %>%
  ggplot( aes(x=exp, y=logr)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA)+
  geom_jitter(size=2,width = 0.3) +
  geom_hline( linetype = 'dotted',yintercept=0.0, size=0.5)+ 
  theme_classic()+coord_flip()+theme(axis.text=element_text(size=12))+
  labs(x="", y="N2O sensitivity coefficient to eCO2") 
