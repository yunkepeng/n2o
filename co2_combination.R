library(readr)
library(ggplot2)
library(dplyr)
library(lme4)
library(MuMIn)
library(lmerTest)
library(Deriv)
#source("/Users/yunpeng/yunkepeng/CNuptake_MS/R/stepwise.R")
source("/Users/yunpeng/yunkepeng/CNuptake_MS/R/stepwise_lm.R")
library(raster)
devtools::load_all("/Users/yunpeng/yunkepeng/latest_packages/rbeni/") 
library(cowplot)
library(plotrix)
stepwise <- function(df_input,target_var){
  #-----------------------------------------------------------------------
  # Input:  whole dataframe and target variable
  #assume that duration_random is the only random factor
  #-----------------------------------------------------------------------
  target <- target_var
  df <- df_input
  
  preds <- df %>% dplyr::select(-c(target,duration_random)) %>% 
    names()
  
  r_list <- c()
  #For loop functions, include all predictor's r2 at the end
  for (var in preds){
    forml <- paste( 'lmer(', target, '~', var, '+(1|duration_random), data = df)')
    fit_lin <- eval(parse(text = forml)) 
    rsq <- r.squaredGLMM(fit_lin)[1]
    r_list <- c(r_list,rsq)
  }
  
  #convert to a dataframe, including all r2
  All_rsquare <- data.frame (
    preds = factor(preds,levels=preds), 
    rsq = r_list)
  
  #select max r2 in all predictors
  max(r_list)
  
  new_All_rsquare <- All_rsquare %>% 
    # desc orders from largest to smallest
    arrange(desc(rsq))
  
  #2. stepwise regression selection
  
  ## list
  list_aic <- list()
  list_bic <- list()
  list_R <- list()
  list_variable <- list()
  
  # predictors retained in the model firstly
  preds_retained <- as.character(new_All_rsquare[1,1])
  preds_candidate <- preds[-which(preds == preds_retained)] 
  
  
  for (a in 1:(length(preds)-1)){
    rsq_candidates <- c()
    linmod_candidates <- list()
    for (i in 1:length(preds_candidate)){
      pred_add <- c(preds_retained, preds_candidate[i])
      forml  <- paste( 'lmer(', target, '~', paste(pred_add, collapse = '+'), '+(1|duration_random), data = df)')
      # create a function and make its format available to output in for loop
      fit_lin <- eval(parse(text = forml))
      linmod_candidates[[ i ]] <- fit_lin
      # obtain multiple r2 at each selection, and find the best one at the end
      rsq <- r.squaredGLMM(fit_lin)[1]
      rsq_candidates[i] <- rsq
    }
    pred_max <- preds_candidate[ which.max(rsq_candidates) ]
    # include best factors in retained factor
    preds_retained <- c(preds_retained, pred_max)
    list_variable[[a]] <- pred_max 
    # include AIC, BIC, adjusted R2, R2, cross-validated R2 and RMSE at each k 
    list_aic[[  a ]] <- AIC(eval(parse(text = paste( 'lmer(', target, '~', paste(preds_retained, collapse = '+'),  '+(1|duration_random), data = df)'))))
    
    list_bic[[ a ]] <- BIC(eval(parse(text = paste( 'lmer(', target, '~', paste(preds_retained, collapse = '+'),  '+(1|duration_random), data = df)'))))
    
    list_R[[ a ]] <- r.squaredGLMM(eval(parse(text = paste( 'lmer(', target, '~', paste(preds_retained, collapse = '+'),  '+(1|duration_random), data = df)'))))[1]
    preds_candidate <- preds_candidate[-which(preds_candidate == pred_max)]
  }
  
  
  R_null <- r.squaredGLMM(eval(parse(text = paste( 'lmer(', target, '~', paste(preds_retained[1], collapse = '+'),  '+(1|duration_random), data = df)'))))[1]
  AIC_null <- AIC(eval(parse(text = paste( 'lmer(', target, '~', paste(preds_retained[1], collapse = '+'),  '+(1|duration_random), data = df)'))))
  BIC_null <- BIC(eval(parse(text = paste( 'lmer(', target, '~', paste(preds_retained[1], collapse = '+'),  '+(1|duration_random), data = df)'))))
  variable_null <- preds_retained[1]
  
  R_all <- round(as.numeric(c(R_null,list_R)),2)
  AIC_all <- round(as.numeric(c(AIC_null,list_aic)),2)
  BIC_all <- round(as.numeric(c(BIC_null,list_bic)),2)
  variable_all <- (as.character(c(variable_null,list_variable)))
  
  df1 <- as.data.frame(cbind(variable_all,R_all,AIC_all,BIC_all))
  
  #Adjusted-R
  p1 <- ggplot() + 
    geom_point(data = df1, aes(x = factor(variable_all,level = variable_all), y = R_all)) 
  #AIC
  p2 <- ggplot() + 
    geom_point(data = df1, aes(x = factor(variable_all,level = variable_all), y = AIC_all)) 
  #BIC
  p3 <- ggplot() + 
    geom_point(data = df1, aes(x = factor(variable_all,level = variable_all), y = BIC_all))
  
  output_list <- list(p1,p2,p3)
  
  return(output_list)
  #-----------------------------------------------------------------------
  # Output: four figures 
  #-----------------------------------------------------------------------
}

library(visreg)
visreg_ggplot <- function(obj,var_name,color1,color2){
  final1 <- ggplot() + geom_line(data = obj, aes_string(var_name, "visregFit", group="plt", color="plt"),size=2) +
    theme_classic()+theme(text = element_text(size=20),legend.position="none")+ 
    geom_ribbon(data = obj,aes_string(var_name, ymin="visregLwr", ymax="visregUpr",fill="plt"),alpha=0.5)+
    scale_colour_manual(values=c(Measurement=color1,LPX=color2))+
    scale_fill_manual(values=c(Measurement=color1,LPX=color2))
  
  return(final1)
}
#include gcme database
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

kevin_othervars_n2o_c <- subset(kevin_othervars,response=="soil_n2o_flux" & treatment=="c")
unique(kevin_othervars_n2o_c[,c("lon","lat","citation")])
#All already repeated
kevin_othervars_n2o_w <- subset(kevin_othervars,response=="soil_n2o_flux" & treatment=="w")
unique(kevin_othervars_n2o_w[,c("lon","lat","citation")])
#All already repeated

#co2-only effect
df1 <- read_csv("~/data/n2o_wang_oikos/n2o_tables1.csv")

names(df1) <- c("ref","location","n_amb","n_elv","n_site","n2o_amb",
                "n2o_elv","co2_amb","co2_elv","dco2","duration","pft","logr",
                "weight","group","method","species","Nfer","other","latitude","longitude","lat","lon","comments","days")

df1$Nfer <- as.numeric(df1$Nfer)*10 #convert g/m2 to kg/ha
df1$co2_amb <- as.numeric(df1$co2_amb)
df1$co2_elv <- as.numeric(df1$co2_elv)
df1$paper_logr <- df1$logr 

df1$logr <- log(df1$n2o_elv/df1$n2o_amb)
df1$log_dco2 <- log(df1$dco2)
df1$log_dn2o <- log(df1$n2o_elv-df1$n2o_amb)

df1$log_co2 <- log(df1$co2_elv/df1$co2_amb)
df1$sen_n2o <- df1$logr/df1$log_co2
df1$nferinfo<- "no"
df1$nferinfo[df1$Nfer>0]<- "fertilized"
df1$other_exp[df1$other==""]<- "no_treatment"
df1$other_exp[df1$other!=""]<- "with_treatment"

df1$logr[df1$logr=="Inf"] <- NA
df1$logr[df1$logr=="-Inf"] <- NA
df1$log_dn2o[df1$log_dn2o=="-Inf"] <- NA
df1$log_dn2o[df1$log_dn2o=="-Inf"] <- NA

#check central value of ambient
summary(df1$co2_amb) #mean or median is 380ppm

#now, start coordinates conversion
#add fapar3g from 1/12 resolution (monthly max and mean)
fapar3g_df_zhu <- read.csv("~/data/n2o_Yunke/forcing/co2_siteinfo_measurementyear_fapar3g_zhu.csv")
dim(fapar3g_df_zhu)
summary(fapar3g_df_zhu)

fapar3g_df_zhu <- fapar3g_df_zhu %>% mutate(min_fapar = coalesce(min_fapar_nfocal0,min_fapar_nfocal1,min_fapar_nfocal2)) %>%
  mutate(mean_fapar = coalesce(mean_fapar_nfocal0,mean_fapar_nfocal1,mean_fapar_nfocal2)) %>%
  mutate(max_fapar = coalesce(max_fapar_nfocal0,max_fapar_nfocal1,max_fapar_nfocal2))
fapar3g_df_zhu2 <- fapar3g_df_zhu[,c("lon","lat","min_fapar","mean_fapar","max_fapar")]

#read climate and soil predictors
climates_soil <- read.csv("~/data/n2o_Yunke/forcing/co2_siteinfo_predictors.csv")

#merge with both
df1_a <- merge(df1,fapar3g_df_zhu2,by=c("lon","lat"),all.x=TRUE)

df1_all <- merge(df1_a,climates_soil[,c("lon","lat","z","PPFD_total_fapar","PPFD_total","Tg","PPFD","vpd","alpha","ndep","ORGC")],
                 by=c("lon","lat"),all.x=TRUE)

#create site-name
sitename <- unique(df1_all[,c("lon","lat")])
sitename$site_a <- paste0("site",c(1:nrow(sitename)))
sitename <- na.omit(sitename)
  
df1_all <- merge(df1_all,sitename,
                 by=c("lon","lat"),all.x=TRUE)

df1_all$PPFD_a <- log(df1_all$PPFD)
df1_all$PPFD_total_a <- log(df1_all$PPFD_total)
df1_all$vpd_a <- log(df1_all$vpd)
df1_all$ndep_a <- log(df1_all$ndep)
df1_all$orgc_a <- log(df1_all$ORGC)

df1_all[sapply(df1_all, is.infinite)] <- NA
df1_all$Nfer_a <- sqrt(df1_all$Nfer)
df1_all$Nfer_a[is.na(df1_all$Nfer_a)==T] <- 0

#only keep measurement higher than 1
df1_all$days_a <- log(df1_all$days)
df1_all$duration_random <- "years"
df1_all$duration_random[df1_all$days<365] <- "days"

hist(df1_all$co2_amb)
hist(df1_all$co2_elv)


#test
df1_all_test <- na.omit(df1_all[,c("duration_random","log_co2","Nfer_a",
                                   "min_fapar","mean_fapar","max_fapar","PPFD_total_a",
                                   "Tg","vpd_a","ndep_a","orgc_a","logr")])
stepwise(df1_all_test,"logr")[[1]]

summary(lmer(logr~log_co2+Nfer_a+PPFD_total_a+(1|duration_random),df1_all))

#duration_random nearly explained no variance

#if as an explantary factor - the second factor after days_a can even not be sucessfully added!
df1_all_test <- na.omit(df1_all[,c("days_a","log_co2","Nfer_a",
                                   "min_fapar","mean_fapar","max_fapar","PPFD_total_a",
                                   "Tg","vpd_a","ndep_a","orgc_a","logr")])
stepwise_lm(df1_all_test,"logr")[[1]]

summary(lm(logr~days_a,df1_all))
summary(lm(logr~days_a+log_co2,df1_all))

#Start fitting model
df1_all_test <- na.omit(df1_all[,c("log_co2","Nfer_a","min_fapar","mean_fapar","max_fapar","PPFD_total_a",
                                   "Tg","vpd_a","orgc_a","logr")]) #removed ndep_a
stepwise_lm(df1_all_test,"logr")[[1]]
mod1 <- (lm(logr~log_co2+Nfer_a+PPFD_total_a,df1_all_test))
summary(mod1)

mod1a <- visreg(mod1,"log_co2",type="contrast")
mod1b <-visreg(mod1,"Nfer_a",type="contrast")
mod1c <- visreg(mod1,"PPFD_total_a",type="contrast")

summary(df1_all$logr/df1_all$log_co2)
summary(df1_all$logr)

#using LPX
mod1 <- (lm(logr~log_co2+Nfer_a+PPFD_total_a,df1_all_test))
summary(mod1)
LPX_co2_sitemean <- na.omit(df1_all[,c("lon","lat","z","pft","logr","log_co2","Nfer_a","PPFD_total_a")])
LPX_co2_sitemean <- unique(LPX_co2_sitemean[,c("lon","lat","z","pft")])
dim(LPX_co2_sitemean)
LPX_co2_sitemean$pft[LPX_co2_sitemean$pft=="Grassland"] <- "grassland"
LPX_co2_sitemean$pft[LPX_co2_sitemean$pft=="Forest"] <- "forest"
LPX_co2_sitemean$pft[LPX_co2_sitemean$pft=="Cropland"] <- "cropland"

lpx_n2o <- read.csv("~/data/n2o_Yunke/forcing/eCO2_LPX_annual_n2o.csv")
#use the year 2006's map, since this is step experiment's initial year (2006)
lpx_nfer <- read.csv("~/data/n2o_Yunke/forcing/eCO2_LPX_annual_nfer.csv")[,c("lon","lat","z","pft","year2006")]
names(lpx_nfer) <-c("lon","lat","z","pft","nfer")

lpx_PPFD <- read.csv("~/data/n2o_Yunke/forcing/eCO2_LPX_annual_PPFD.csv")[,c("lon","lat","z","pft","year2006")]
names(lpx_PPFD) <-c("lon","lat","z","pft","PPFD")
LPX_all <-Reduce(function(x,y) merge(x = x, y = y, c("lon","lat","z","pft"),all.x=TRUE),
                 list(LPX_co2_sitemean,lpx_n2o,lpx_nfer,lpx_PPFD))

a1 <- c(log(LPX_all$dT0_C416/LPX_all$dT0_C380),
        log(LPX_all$dT0_C582/LPX_all$dT0_C380),
        log(LPX_all$dT0_C813/LPX_all$dT0_C380)) #n2o
a2 <- c(rep(log(416/380),nrow(LPX_all)),rep(log(582/380),nrow(LPX_all)),rep(log(813/380),nrow(LPX_all))) #co2
a3 <- c(LPX_all$nfer,LPX_all$nfer,LPX_all$nfer) #nfer
a4 <- c(LPX_all$PPFD,LPX_all$PPFD,LPX_all$PPFD) #ppfd

final_lpx_data <- as.data.frame(cbind(a1,a2,a3,a4))
names(final_lpx_data) <- c("logr","log_co2","Nfer_a","PPFD_total_a")
final_lpx_data$Nfer_a <- sqrt(final_lpx_data$Nfer_a)
final_lpx_data$PPFD_total_a <- log(final_lpx_data$PPFD_total_a)

final_lpx_data[sapply(final_lpx_data, is.nan)] <- NA
final_lpx_data[sapply(final_lpx_data, is.infinite)] <- NA

mod2 <- (lm(logr~log_co2+Nfer_a+PPFD_total_a,final_lpx_data))
summary(mod2)

mod1 <- (lm(logr~log_co2+Nfer_a+PPFD_total_a,df1_all_test))
summary(mod1)

mod1_co2 <- visreg(mod1,"log_co2",type="contrast")
mod1_nfer <-visreg(mod1,"Nfer_a",type="contrast")
mod1_ppfd <- visreg(mod1,"PPFD_total_a",type="contrast")

mod2_co2 <- visreg(mod2,"log_co2",type="contrast")
mod2_nfer <-visreg(mod2,"Nfer_a",type="contrast")
mod2_ppfd <- visreg(mod2,"PPFD_total_a",type="contrast")

fits_co2 <- dplyr::bind_rows(mutate(mod1_co2$fit, plt = "Measurement"),mutate(mod2_co2$fit, plt = "LPX"))
fits_nfer <- dplyr::bind_rows(mutate(mod1_nfer$fit, plt = "Measurement"),mutate(mod2_nfer$fit, plt = "LPX"))
fits_ppfd <- dplyr::bind_rows(mutate(mod1_ppfd$fit, plt = "Measurement"),mutate(mod2_ppfd$fit, plt = "LPX"))

visreg_ggplot <- function(obj,var_name,color1,color2,xlab_name,ylab_name){
  final1 <- ggplot() + geom_line(data = obj, aes_string(var_name, "visregFit", group="plt", color="plt"),size=2) +
    theme_classic()+theme(text = element_text(size=20),legend.position="none")+
    geom_ribbon(data = obj,aes_string(var_name, ymin="visregLwr", ymax="visregUpr",fill="plt"),alpha=0.5)+
    scale_colour_manual(values=c(Measurement=color1,LPX=color2))+
    scale_fill_manual(values=c(Measurement=color1,LPX=color2))+xlab(xlab_name) + ylab(ylab_name)
  
  return(final1)
}


g1 <- visreg_ggplot(fits_co2,"log_co2","black","red","ln CO2e/CO2a","ln N2Oe/lnN2Oa")
g1

g2 <- visreg_ggplot(fits_nfer,"Nfer_a","black","red","sqrt N fertilisation (kg/ha)"," ")
g2

g3 <- visreg_ggplot(fits_ppfd,"PPFD_total_a","black","red","ln total gPPFD (mol/m2)"," ")
g3

#co2 finanlised

#select a forest that has lower forest cover 
forest_cover <- read.csv("/Users/yunpeng/data/n2o_Yunke/forcing/forestcover_site.csv")
subset(forest_cover,forest_cover<0.8)

#warming only effect
df2 <- read_csv("~/data/n2o_wang_oikos/n2o_tables2.csv")
summary(df2)
names(df2) <- c("ref","location","n_amb","n_elv","n_site","n2o_amb",
                "n2o_elv","dT","duration","pft","logr",
                "weight","group","species","Nfer","other","latitude","longitude","lat","lon","comments","days")

df2$Nfer <- as.numeric(df2$Nfer)*10 #convert g/m2 to kg/ha

df2$logr <- log(df2$n2o_elv/df2$n2o_amb)

df2$nferinfo<- "no"
df2$nferinfo[df2$Nfer>0]<- "fertilized"
df2$other_exp[df2$other==""]<- "no_treatment"
df2$other_exp[df2$other!=""]<- "with_treatment"

df2$sen_n2o <- df2$logr/df2$dT
df2$nferinfo<- "no"
df2$nferinfo[df2$Nfer>0]<- "fertilized"

#QQQ: the one with very low logr looks confusing, (ref = Zhao et al., 2017), n2o decreased from 61440 to 1680??? While another sample of this ref looks ok. 
#not found in paper!!! - In Fig.6 it seems that at AS species was decreased by -15%.
#remove them by contacting first author
df2 <- subset(df2,logr!=min(df2$logr,na.rm=T))

hist(df2$dT)

#merge with both
df2_a <- merge(df2,fapar3g_df_zhu2,by=c("lon","lat"),all.x=TRUE)

df2_all <- merge(df2_a,climates_soil[,c("lon","lat","z","PPFD_total_fapar","PPFD_total","Tg","PPFD","vpd","alpha","ndep","ORGC")],
                 by=c("lon","lat"),all.x=TRUE)

#create site-name
sitename <- unique(df2_all[,c("lon","lat")])
sitename$site_a <- paste0("site",c(1:nrow(sitename)))
sitename <- na.omit(sitename)

df2_all <- merge(df2_all,sitename,
                 by=c("lon","lat"),all.x=TRUE)

df2_all$PPFD_a <- log(df2_all$PPFD)
df2_all$PPFD_total_a <- log(df2_all$PPFD_total)
df2_all$vpd_a <- log(df2_all$vpd)
df2_all$ndep_a <- log(df2_all$ndep)
df2_all$orgc_a <- log(df2_all$ORGC)

df2_all[sapply(df2_all, is.infinite)] <- NA
df2_all$Nfer_a <- sqrt(df2_all$Nfer)
df2_all$Nfer_a[is.na(df2_all$Nfer_a)==T] <- 0

df2_all$days_a <- log(df2_all$days)
df2_all$duration_random <- "years"
df2_all$duration_random[df2_all$days<365] <- "days"

#test -random factor - nearly no variance
df2_all_test <- na.omit(df2_all[,c("duration_random","dT","Nfer_a","min_fapar","mean_fapar","max_fapar","PPFD_total_a",
                                   "vpd_a","ndep_a","orgc_a","logr")])

stepwise(df2_all_test,"logr")[[1]]
summary(lmer(logr~orgc_a+dT+(1|duration_random),df2_all_test))

#test - fixed factor
df2_all_test <- na.omit(df2_all[,c("days_a","dT","Nfer_a","min_fapar","mean_fapar","max_fapar","PPFD_total_a",
                                   "vpd_a","ndep_a","orgc_a","logr")])

stepwise_lm(df2_all_test,"logr")[[1]]
stepwise_lm(df2_all_test,"logr")[[2]]
#AIC(lm(logr~orgc_a+dT+days_a,df2_all))
#AIC(lm(logr~orgc_a+dT,df2_all))
#even worse

#Start fitting model
df2_all_test <- na.omit(df2_all[,c("dT","Nfer_a","min_fapar","mean_fapar","max_fapar","PPFD_total_a",
                                   "vpd_a","orgc_a","logr")])#remove ndep_a
stepwise_lm(df2_all_test,"logr")[[1]]
mod3 <- (lm(logr~orgc_a+dT,df2_all))
summary(mod3)
mod3a <- visreg(mod3,"orgc_a",type="contrast")
mod3b <-visreg(mod3,"dT",type="contrast")

#applied in lpx model
LPX_warming_sitemean <- na.omit(df2_all[,c("lon","lat","dT","orgc_a","logr")])
LPX_warming_sitemean <- unique(df2_all[,c("lon","lat","z","pft")])

forest_cover <- read.csv("/Users/yunpeng/data/n2o_Yunke/forcing/forestcover_site.csv")
subset(forest_cover,forest_cover<0.8)

#remove lon==91.758
LPX_warming_sitemean <- subset(LPX_warming_sitemean,lon!=91.758)

lpx_n2o <- read.csv("~/data/n2o_Yunke/forcing/eCO2_LPX_annual_n2o.csv")

LPX_warming_sitemean$pft[LPX_warming_sitemean$pft=="Grassland"] <- "grassland"
LPX_warming_sitemean$pft[LPX_warming_sitemean$pft=="Forest"] <- "forest"
LPX_warming_sitemean$pft[LPX_warming_sitemean$pft=="Cropland"] <- "cropland"

lpx_soc <- (unique(df2_all[,c("lon","lat","orgc_a")]))
LPX_sitemean_soc <- merge(LPX_warming_sitemean,lpx_soc,by=c("lon","lat"),all.x=TRUE)

LPX_all <-Reduce(function(x,y) merge(x = x, y = y, c("lon","lat","z","pft"),all.x=TRUE),
                 list(LPX_sitemean_soc,lpx_n2o))


a1 <- c(log(LPX_all$dT0.39_C380/LPX_all$dT0_C380),
        log(LPX_all$dT3.95_C380/LPX_all$dT0_C380),
        log(LPX_all$dT7.5_C380/LPX_all$dT0_C380)) #n2o
a2 <- c(rep(0.39,nrow(LPX_all)),rep(3.95,nrow(LPX_all)),rep(7.5,nrow(LPX_all))) #co2
a3 <- c(LPX_all$orgc_a,LPX_all$orgc_a,LPX_all$orgc_a) #soc

final_lpx_data <- as.data.frame(cbind(a1,a2,a3))
names(final_lpx_data) <- c("logr","dT","orgc_a")

final_lpx_data[sapply(final_lpx_data, is.nan)] <- NA
final_lpx_data[sapply(final_lpx_data, is.infinite)] <- NA

mod4 <- (lm(logr~dT,final_lpx_data))
#mod4_orgc <- visreg(mod4,"orgc_a",type="contrast")
mod4_dT <-visreg(mod4,"dT",type="contrast")
summary(mod4)


mod3 <- (lm(logr~orgc_a+dT,df2_all))
summary(mod3)
mod3_orgc <- visreg(mod3,"orgc_a",type="contrast")
mod3_dT <-visreg(mod3,"dT",type="contrast")


fits_orgc <- dplyr::bind_rows(mutate(mod3_orgc$fit, plt = "Measurement"))
fits_dT <- dplyr::bind_rows(mutate(mod3_dT$fit, plt = "Measurement"),mutate(mod4_dT$fit, plt = "LPX"))

g4 <- visreg_ggplot(fits_orgc,"orgc_a","black","red","ln SOC (g/kg)","ln N2Oe/lnN2Oa")
g4

g5 <- visreg_ggplot(fits_dT,"dT","black","red","dT", " ")
g5


white <- theme(plot.background=element_rect(fill="white", color="white"))
plot_grid
plot_grid(g1,g2,g3,
          g4,g5,white,
          nrow=2,label_x = 0.8, label_y = 0.8)+white
ggsave(paste("/Users/yunpeng/Desktop/b.jpg",sep=""), width = 15, height = 10)



#upscalling 
soc_nc <- read_nc_onefile("~/data/nimpl_sofun_inputs/map/Final_ncfile/ORGC.nc")
orgc_df <- as.data.frame(nc_to_df(soc_nc, varnam = "ORGC"))
summary(orgc_df)

#upscalling
summary(mod3)

#calculate area conversion
source("~/yunkepeng/CNuptake_MS/R/calc_area.R")
area_m2 <- calc_area(orgc_df$lat,0.5,0.5)
#fland - to show each grid's land cover percentage
nc <- read_nc_onefile("~/data/fland/global.fland.nc") #Input nc
output_fland <- nc_to_df(nc, varnam = "fland")
fland <- output_fland$fland
summary(fland)
#area_m2 * fland = land area at each grid
conversion <- area_m2 * fland
aa <- sum(conversion,na.rm=T)
fraction <- conversion/aa
sum(fraction,na.rm=T)

#here fraction is fraction of each grid's land cover
#value using 2016's value: https://www.eea.europa.eu/data-and-maps/daviz/atmospheric-concentration-of-carbon-dioxide-5#tab-chart_5_filters=%7B%22rowFilters%22%3A%7B%7D%3B%22columnFilters%22%3A%7B%22pre_config_polutant%22%3A%5B%22CH4%20(ppb)%22%5D%7D%7D
#n2o_a = 329.29 (0.12)
#without intercept term, values are
#below is when dT = 0.39 and 7.5
final1 <- sum(329.29*fraction*exp((summary(mod3)$coefficients[1,1])+ (summary(mod3)$coefficients[2,1])*log(orgc_df$ORGC)+(summary(mod3)$coefficients[3,1])*0.39),na.rm=T)
final3 <- sum(329.29*fraction*exp((summary(mod3)$coefficients[1,1])+ (summary(mod3)$coefficients[2,1])*log(orgc_df$ORGC)+(summary(mod3)$coefficients[3,1])*7.5),na.rm=T)

#calculate N2Oe uncertainty
#after error propogation: ln (n2o-e/n2o-a) = model
#uncertainty n2o_e =sqrt(delta-model^2 + (delta-n2o_a/ n2o_a)^2)
uncertainty_model <- sqrt(((summary(mod3)$coefficients[1,1])^2 *
                             summary(mod3)$coefficients[1,2]^2+
                             (summary(mod3)$coefficients[2,1])^2 *
                             summary(mod3)$coefficients[2,2]^2)+
                            ( (summary(mod3)$coefficients[3,1])^2 *
                                summary(mod3)$coefficients[3,2]^2))
uncertainty_fN <- sqrt(uncertainty_model^2+ (0.12/329.29)^2)
#here uncertainty_fN presents percentage of uncertainty -> will multuply with n2o_e to get actual uncertainty

fN<-function(N,N0,C_mean,M_mean,N_mean){(-8.0*10^(-6)*C_mean+4.2*10^(-6)*N_mean-4.9*10^(-6)*M_mean+0.117)*(sqrt(N)-sqrt(N0))}

err_fN<-function(N,N0,C_mean,M_mean,N_mean,err_N,err_N0){
  DN<-Deriv(fN,"N");DM0<-Deriv(fN,"M0");DN0<-Deriv(fN,"N0")
  sqrt( DN(N,N0,C_mean,M_mean,N_mean)^2*err_N^2 + 
          DN0(N,N0,C_mean,M_mean,N_mean)^2*err_N0^2 )
}

#value and S.E.
rN_value<- fN(final3,final1,402.88,1842.4,(final3+final1)/2)/(7.50-0.39)
rN_value
rN_SE_value<- err_fN(final3,final1,402.88,1842.4,(final3+final1)/2,final3*uncertainty_fN,final1*uncertainty_fN)/(7.50-0.39)
rN_SE_value
#value of gains - See Liu et al. SI (corrected version)
lamda <- 0.875
se_lamda <- 0.38/1.96

gains <- lamda*rN_value
gains

gains_uncertainty <- sqrt(rN_value^2 * se_lamda^2 + lamda^2 * rN_SE_value^2)
gains_uncertainty

#using LPX
lpx <- read.csv("~/data/n2o_Yunke/forcing/eCO2_warming_LPX_total_n2o.csv")
#assume n2o_a, again, is 329.29 (0.12)
std.error(lpx$dT_0)
std.error(lpx$dT_0.39)
std.error(lpx$dT_7.5)

final1_lpx <- 329.29*mean(lpx$dT_0.39)/mean(lpx$dT_0)
final1_lpx_uncertainty <- sqrt((0.12/329.29)^2+(std.error(lpx$dT_0)/mean(lpx$dT_0))^2+(std.error(lpx$dT_0.39)/mean(lpx$dT_0.39))^2)
final3_lpx <-329.29*mean(lpx$dT_7.5)/mean(lpx$dT_0)
final3_lpx_uncertainty <- sqrt((0.12/329.29)^2+(std.error(lpx$dT_0)/mean(lpx$dT_0))^2+(std.error(lpx$dT_7.5)/mean(lpx$dT_7.5))^2)

rN_value_lpx<- fN(final3_lpx,final1_lpx,402.88,1842.4,(final3_lpx+final1_lpx)/2)/(7.50-0.39)
rN_value_lpx

rN_SE_value_lpx <- err_fN(final3_lpx,final1_lpx,402.88,1842.4,(final3_lpx+final1_lpx)/2,final3_lpx_uncertainty,final1_lpx_uncertainty)/(7.50-0.39)
rN_SE_value_lpx

gains_lpx <- lamda*rN_value_lpx
gains_lpx

gains_uncertainty_lpx <- sqrt(rN_value_lpx^2 * se_lamda^2 + lamda^2 * rN_SE_value_lpx^2)
gains_uncertainty_lpx

#co2 feedback
#value using 2016 value: https://www.eea.europa.eu/data-and-maps/daviz/atmospheric-concentration-of-carbon-dioxide-5#tab-chart_5_filters=%7B%22rowFilters%22%3A%7B%7D%3B%22columnFilters%22%3A%7B%22pre_config_polutant%22%3A%5B%22CO2%20(ppm)%22%5D%7D%7D
nfer_nc <- read_nc_onefile("~/data/nimpl_sofun_inputs/map/Final_ncfile/nfer.nc")#unit g/m2
nfer_df <- as.data.frame(nc_to_df(nfer_nc, varnam = "nfer"))
summary(nfer_df)
PPFD_total_nc <- read_nc_onefile("~/data/nimpl_sofun_inputs/map/Final_ncfile/PPFD_total.nc")
PPFD_total_df <- as.data.frame(nc_to_df(PPFD_total_nc, varnam = "PPFD_total"))
summary(PPFD_total_df)

#without intercept term, values are
final1a <- sum(329.29*fraction*exp(0+ summary(mod1)$coefficients[2,1]*log(416/380)+
                                    summary(mod1)$coefficients[3,1]*sqrt(nfer_df$nfer*10)+
                                    summary(mod1)$coefficients[4,1]*log(PPFD_total_df$PPFD_total)),na.rm=T)

final3a <- sum(329.29*fraction*exp(0+ summary(mod1)$coefficients[2,1]*log(813/380)+
                                    summary(mod1)$coefficients[3,1]*sqrt(nfer_df$nfer*10)+
                                    summary(mod1)$coefficients[4,1]*log(PPFD_total_df$PPFD_total)),na.rm=T)
final1a
final3a
#final1a*uncertainty_fN2
#final3a*uncertainty_fN2

uncertainty_model2 <- sqrt(summary(mod1)$coefficients[2,1]^2 *
                             summary(mod1)$coefficients[2,2]^2 +
                            summary(mod1)$coefficients[3,1]^2 *
                                summary(mod1)$coefficients[3,2]^2+
                              summary(mod1)$coefficients[4,1]^2 *
                                  summary(mod1)$coefficients[4,2]^2)
uncertainty_fN2 <- sqrt(uncertainty_model2^2+ (0.12/329.29)^2)
uncertainty_fN2

#feedback value
rN_value_eCO2<- fN(final3a,final1a,(813+416)/2,1842.4,(final3a+final1a)/2)/(813-416)

rN_value_eCO2_se <- err_fN(final3a,final1a,(813+416)/2,1842.4,(final3a+final1a)/2,final3a*uncertainty_fN2,final1a*uncertainty_fN2)/(813-416)

#using LPX
lpx <- read.csv("~/data/n2o_Yunke/forcing/eCO2_warming_LPX_total_n2o.csv")
#assume n2o_a, again, is 329.29 (0.12)
std.error(lpx$dT_0)
std.error(lpx$eCO2_416)
std.error(lpx$eCO2_813)

final1_lpx_eCO2 <- 329.29*mean(lpx$eCO2_416)/mean(lpx$dT_0)
final1_lpx_eCO2_uncertainty <- sqrt((0.12/329.29)^2+(std.error(lpx$dT_0)/mean(lpx$dT_0))^2+(std.error(lpx$eCO2_416)/mean(lpx$eCO2_416))^2)
final3_lpx_eCO2 <-329.29*mean(lpx$eCO2_813)/mean(lpx$dT_0)
final3_lpx_eCO2_uncertainty <- sqrt((0.12/329.29)^2+(std.error(lpx$dT_0)/mean(lpx$dT_0))^2+(std.error(lpx$eCO2_813)/mean(lpx$eCO2_813))^2)

rN_value_eCO2_lpx<- fN(final3_lpx_eCO2,final1_lpx_eCO2,(813+416)/2,1842.4,(final3_lpx_eCO2+final1_lpx_eCO2)/2)/(813-416)
rN_value_eCO2_lpx
rN_value_eCO2_lpx_se <- err_fN(final3_lpx_eCO2,final1_lpx_eCO2,(813+416)/2,1842.4,(final3_lpx_eCO2+final1_lpx_eCO2)/2,final3_lpx_eCO2_uncertainty,final1_lpx_eCO2_uncertainty)/(813-416)

#lamda of eCO2
#quote from IPCC AR6:
#The assessed ERF for a doubling of carbon dioxide compared to 1750 levels (3.93 ± 0.47 W m–2) is larger than in AR5
lamda <- (560-280)/3.93
lamda_se <- (560-280)*0.47/(3.93^2)

gains_co2 <- lamda*rN_value_eCO2
gains_co2

gains_uncertainty <- sqrt(rN_value_eCO2^2 * lamda_se^2 + lamda^2 * rN_value_eCO2_se^2)
gains_uncertainty

gains_co2_lpx <- lamda*rN_value_eCO2_lpx
gains_co2_lpx

gains_uncertainty_lpx <- sqrt(rN_value_eCO2_lpx^2 * lamda_se^2 + lamda^2 * rN_value_eCO2_lpx_se^2)
gains_uncertainty_lpx
