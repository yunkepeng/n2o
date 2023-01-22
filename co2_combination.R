library(readr)
library(ggplot2)
library(dplyr)
library(lme4)
library(MuMIn)
library(lmerTest)
#source("/Users/yunpeng/yunkepeng/CNuptake_MS/R/stepwise.R")
source("/Users/yunpeng/yunkepeng/CNuptake_MS/R/stepwise_lm.R")

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

df1$Nfer <- as.numeric(df1$Nfer)
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

hist(df1_all$dco2)


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

df1_all_test <- na.omit(df1_all[,c("log_co2","Nfer_a","min_fapar","mean_fapar","max_fapar","PPFD_total_a",
                                   "Tg","vpd_a","ndep_a","orgc_a","logr")])
stepwise_lm(df1_all_test,"logr")[[1]]
stepwise_lm(df1_all_test,"logr")[[2]]

mod1 <- (lm(logr~log_co2+Nfer_a+PPFD_total_a,df1_all))
summary(mod1)
length(unique(df1_all_test$Tg))
mod1a <- visreg(mod1,"log_co2",type="contrast")
mod1b <-visreg(mod1,"Nfer_a",type="contrast")
mod1c <- visreg(mod1,"PPFD_total_a",type="contrast")

summary(df1_all$logr/df1_all$log_co2)
summary(df1_all$logr)

#using LPX
mod1 <- (lm(logr~log_co2+Nfer_a+PPFD_total_a,df1_all_test))

LPX_co2_sitemean <- na.omit(df1_all[,c("lon","lat","z","pft","logr","log_co2","Nfer_a","PPFD_total_a")])
LPX_co2_sitemean <- unique(LPX_co2_sitemean[,c("lon","lat","z","pft")])
dim(LPX_co2_sitemean)
LPX_co2_sitemean$pft[LPX_co2_sitemean$pft=="Grassland"] <- "grassland"
LPX_co2_sitemean$pft[LPX_co2_sitemean$pft=="Forest"] <- "forest"
LPX_co2_sitemean$pft[LPX_co2_sitemean$pft=="Cropland"] <- "cropland"

lpx_n2o <- read.csv("~/data/n2o_Yunke/forcing/eCO2_LPX_annual_n2o.csv")

lpx_nfer <- read.csv("~/data/n2o_Yunke/forcing/eCO2_LPX_annual_nfer.csv")

lpx_PPFD <- read.csv("~/data/n2o_Yunke/forcing/eCO2_LPX_annual_PPFD.csv")

LPX_all <-Reduce(function(x,y) merge(x = x, y = y, c("lon","lat","z","pft"),all.x=TRUE),
                 list(LPX_co2_sitemean,lpx_n2o,lpx_nfer,lpx_PPFD))

LPX_sitemean_n2o <- LPX_all[,c(5:175)]
LPX_sitemean_nfer <- LPX_all[,c(176:346)]
LPX_sitemean_PPFD <- LPX_all[,c(346:516)]

#read co2
#repeat co2 data by covering all sites
co2 <- read.csv("/Users/yunpeng/data/LPX/data/global_co2_ann_1700_2020.csv")
co2_effect <- subset(co2,year>=1850 & year <=2020)[,c("co2")]
co2_data <- rep(co2_effect,each=30)
co2_df <- matrix(co2_data,nrow = 30) 

#calculate difference year-by-year (36 years in total)
logr_n2o <- data.frame(matrix(NA)) 
ppfd<- data.frame(matrix(NA)) 
nfer<- data.frame(matrix(NA)) 
co2_final <- data.frame(matrix(NA)) 

for (i in c(1:170)) {
  LPX_n2o_only <- LPX_sitemean_n2o
  logr_n2o[c(1:nrow(LPX_n2o_only)),i] <- log(LPX_n2o_only[,i+1]/LPX_n2o_only[,i])
  
  LPX_PPFD_only <- LPX_sitemean_PPFD
  ppfd[c(1:nrow(LPX_PPFD_only)),i] <- log((LPX_PPFD_only[,i+1]+LPX_PPFD_only[,i])/2)
  
  nfer_only <- LPX_sitemean_nfer
  nfer[c(1:nrow(nfer_only)),i] <- sqrt(nfer_only[,i+1]/nfer_only[,i])
  
  co2_final[c(1:nrow(LPX_PPFD_only)),i] <- log(co2_df[,i+1]/co2_df[,i])

} 

final_n2o <- data.frame(x=unlist(logr_n2o))
final_ppfd <- data.frame(x=unlist(ppfd))
final_nfer <- data.frame(x=unlist(nfer))
final_co2 <- data.frame(x=unlist(co2_final))

final_lpx_data <- na.omit(as.data.frame(cbind(final_n2o,final_ppfd,final_nfer,final_co2)))
names(final_lpx_data) <- c("logr","PPFD_total_a","Nfer_a","log_co2")
final_lpx_data[sapply(final_lpx_data, is.nan)] <- NA
final_lpx_data[sapply(final_lpx_data, is.infinite)] <- NA

mod1 <- (lm(logr~log_co2+Nfer_a+PPFD_total_a,df1_all_test))

mod2 <- (lm(logr~log_co2+Nfer_a+PPFD_total_a,final_lpx_data))
summary(mod2)

mod1_co2 <- visreg(mod1,"log_co2",type="contrast")
mod1_nfer <-visreg(mod1,"Nfer_a",type="contrast")
mod1_ppfd <- visreg(mod1,"PPFD_total_a",type="contrast")

mod2_co2 <- visreg(mod2,"log_co2",type="contrast")
mod2_nfer <-visreg(mod2,"Nfer_a",type="contrast")
mod2_ppfd <- visreg(mod2,"PPFD_total_a",type="contrast")

fits_co2 <- dplyr::bind_rows(mutate(mod1_co2$fit, plt = "Measurement"),mutate(mod2_co2$fit, plt = "LPX"))
fits_nfer <- dplyr::bind_rows(mutate(mod1_nfer$fit, plt = "Measurement"),mutate(mod2_nfer$fit, plt = "LPX"))
fits_ppfd <- dplyr::bind_rows(mutate(mod1_ppfd$fit, plt = "Measurement"),mutate(mod2_ppfd$fit, plt = "LPX"))

visreg_ggplot <- function(obj,var_name,color1,color2){
  final1 <- ggplot() + geom_line(data = obj, aes_string(var_name, "visregFit", group="plt", color="plt"),size=2) +
    theme_classic()+theme(text = element_text(size=20),legend.position="none")+ 
    geom_ribbon(data = obj,aes_string(var_name, ymin="visregLwr", ymax="visregUpr",fill="plt"),alpha=0.5)+
    scale_colour_manual(values=c(Measurement=color1,LPX=color2))+
    scale_fill_manual(values=c(Measurement=color1,LPX=color2))
  
  return(final1)
}


g1 <- visreg_ggplot(fits_co2,"log_co2","black","red")
g1

g2 <- visreg_ggplot(fits_nfer,"Nfer_a","black","red")
g2

g3 <- visreg_ggplot(fits_ppfd,"PPFD_total_a","black","red")
g3


#select a forest that has lower forest cover 
forest_cover <- read.csv("/Users/yunpeng/data/n2o_Yunke/forcing/forestcover_site.csv")
subset(forest_cover,forest_cover<0.8)

#warming only effect
df2 <- read_csv("~/data/n2o_wang_oikos/n2o_tables2.csv")
summary(df2)
names(df2) <- c("ref","location","n_amb","n_elv","n_site","n2o_amb",
                "n2o_elv","dT","duration","pft","logr",
                "weight","group","species","Nfer","other","latitude","longitude","lat","lon","comments","days")

df2$Nfer <- as.numeric(df2$Nfer)

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
#shall we remove them? 
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
AIC(lm(logr~orgc_a+dT+days_a,df2_all))
AIC(lm(logr~orgc_a+dT,df2_all))
#even worse

#finally, still removing days
df2_all_test <- na.omit(df2_all[,c("dT","Nfer_a","min_fapar","mean_fapar","max_fapar","PPFD_total_a",
                                   "vpd_a","ndep_a","orgc_a","logr")])

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

lpx_T <- read.csv("~/data/n2o_Yunke/forcing/eCO2_LPX_annual_T.csv")

LPX_warming_sitemean$pft[LPX_warming_sitemean$pft=="Grassland"] <- "grassland"
LPX_warming_sitemean$pft[LPX_warming_sitemean$pft=="Forest"] <- "forest"
LPX_warming_sitemean$pft[LPX_warming_sitemean$pft=="Cropland"] <- "cropland"

LPX_all <-Reduce(function(x,y) merge(x = x, y = y, c("lon","lat","z","pft"),all.x=TRUE),
                 list(LPX_warming_sitemean,lpx_n2o,lpx_T))

LPX_sitemean_n2o <- LPX_all[,c(5:175)]
LPX_sitemean_T <- LPX_all[,c(176:346)]

lpx_soc <- (unique(df2_all[,c("lon","lat","orgc_a")]))
LPX_sitemean_soc <- merge(LPX_warming_sitemean,lpx_soc,by=c("lon","lat"),all.x=TRUE)
LPX_sitemean_soc[,c(6:176)] <- LPX_sitemean_soc[,5] #expand to multiple years, though with the same value
LPX_sitemean_soc <- LPX_sitemean_soc[,c(5:176)]

#calculate difference year-by-year (36 years in total)
logr_n2o <- data.frame(matrix(NA)) 
warming_final <- data.frame(matrix(NA)) 
soc_final <- data.frame(matrix(NA)) 

for (i in c(1:170)) {
  LPX_n2o_only <- LPX_sitemean_n2o
  logr_n2o[c(1:nrow(LPX_n2o_only)),i] <- log(LPX_n2o_only[,i+1]/LPX_n2o_only[,i])
  
  LPX_T_only <- LPX_sitemean_T
  warming_final[c(1:nrow(LPX_T_only)),i] <-  LPX_T_only[,i+1]-LPX_T_only[,i]
  
  LPX_soc_only <- LPX_sitemean_soc
  soc_final[c(1:nrow(LPX_soc_only)),i] <-  log((LPX_soc_only[,i+1]+LPX_soc_only[,i])/2)
  
} 

final_n2o <- data.frame(x=unlist(logr_n2o))
final_warming <- data.frame(x=unlist(warming_final))
final_soc <- data.frame(x=unlist(soc_final))

final_lpx_data <- na.omit(as.data.frame(cbind(final_n2o,final_warming,final_soc)))
names(final_lpx_data) <- c("logr","dT","orgc_a")
final_lpx_data[sapply(final_lpx_data, is.nan)] <- NA
final_lpx_data[sapply(final_lpx_data, is.infinite)] <- NA


mod3 <- (lm(logr~orgc_a+dT,df2_all))
summary(mod3)
mod3_orgc <- visreg(mod3,"orgc_a",type="contrast")
mod3_dT <-visreg(mod3,"dT",type="contrast")

mod4 <- (lm(logr~orgc_a+dT,final_lpx_data))
mod4_orgc <- visreg(mod4,"orgc_a",type="contrast")
mod4_dT <-visreg(mod4,"dT",type="contrast")

fits_orgc <- dplyr::bind_rows(mutate(mod3_orgc$fit, plt = "Measurement"),mutate(mod4_orgc$fit, plt = "LPX"))
fits_dT <- dplyr::bind_rows(mutate(mod3_dT$fit, plt = "Measurement"),mutate(mod4_dT$fit, plt = "LPX"))

g4 <- visreg_ggplot(fits_orgc,"orgc_a","black","red")
g4

g5 <- visreg_ggplot(fits_dT,"dT","black","red")
g5

####not used
df2_plotmean <- aggregate(df2,by=list(df2$ref,df2$nferinfo,df2$pft), FUN=mean, na.rm=TRUE)
dim(df1_plotmean)

ggplot(df2_plotmean)+geom_point(aes(x=dT,y=logr,color=Group.2,shape=Group.3),size=3)+geom_smooth(aes(x=dT,y=logr),method="lm")+ylab("log (N2O-e / N2O-a)")
summary(lm(logr~dT,df2_plotmean)) # R2 = 0.21

ggplot(df2_plotmean)+geom_point(aes(x=dT,y=logr,color=Group.2,shape=Group.3),size=3)+
  geom_smooth(aes(x=dT,y=logr),method="lm",formula=y~0+x)+ylab("log (N2O-e / N2O-a)")
summary(lm(logr~-1+dT,df2_plotmean)) # R2 = 0.21
df2_plotmean$dT_2 <- df2_plotmean$dT^2
model2 <- (lm(logr~ -1+dT+dT_2,df2_plotmean)) # R2 = 0.24
summary(model2)
dTValues <- seq(0, 5, 0.01)

model_predict2 <- predict(model2,list(dT=dTValues, 
                                     dT_2=dTValues^2))

df2_line <- as.data.frame(cbind(dTValues,model_predict2))
ggplot(df2_plotmean)+geom_point(aes(x=dT,y=logr,color=Group.2,shape=Group.3),size=3)+
  geom_line(data=df2_line,aes(x=dTValues,y=model_predict2))+ylab("log (N2O-e / N2O-a)")


df2 %>%
  ggplot() +
  geom_boxplot( aes(x=pft, y=sen_n2o),alpha = 0.6,outlier.shape = NA)+
  geom_boxplot(data=df2,aes(y=sen_n2o),alpha = 0.6,outlier.shape = NA)+
  geom_jitter(aes(x=pft, y=sen_n2o,color=nferinfo),size=2,width = 0.3) +
  geom_hline( linetype = 'dotted',yintercept=0.0, size=0.5)+ 
  theme_classic()+coord_flip()+theme(axis.text=element_text(size=12))+
  labs(x="", y=~paste(N2O," response to ",warming)) 

df2 %>%
  ggplot( aes(x=pft, y=logr)) +
  geom_boxplot(alpha = 0.6,outlier.shape = NA)+
  geom_jitter(aes(size=1/weight)) + #weight = na * ni / (na+ni))
  geom_hline( linetype = 'dotted',yintercept=0.0, size=0.5)+ 
  theme_classic()+coord_flip()+theme(axis.text=element_text(size=12))+
  labs(x="", y=~paste(N2O," response to ",eCO[2])) 

df2$group[df2$group!=">2"] <- "<=2"
unique(df2$group)
df2 %>%
  ggplot() +
  geom_boxplot( aes(x=group, y=sen_n2o),alpha = 0.6,outlier.shape = NA)+
  geom_boxplot(data=df1,aes(y=sen_n2o),alpha = 0.6,outlier.shape = NA)+
  geom_jitter(aes(x=group, y=sen_n2o,shape=pft,color=nferinfo),size=2,width = 0.3) +
  geom_hline( linetype = 'dotted',yintercept=0.0, size=0.5)+ 
  theme_classic()+coord_flip()+theme(axis.text=element_text(size=12))+
  labs(x="", y=~paste(N2O," response to ",eCO[2])) 

#co2 and warming combination effects
df3 <- read_csv("~/data/n2o_wang_oikos/n2o_tables3.csv")
summary(df3)
names(df3) <- c("ref","location","n_amb","n_elv","n_site","n2o_amb",
                "n2o_elv","dT","co2_amb","co2_elv","dCO2","duration","pft","logr",
                "weight","method","species","Nfer","other")

df3$Nfer <- as.numeric(df3$Nfer)

df3$logr <- log(df3$n2o_elv/df3$n2o_amb)

df3$nferinfo<- "no"
df3$nferinfo[df3$Nfer>0]<- "fertilized"

summary(lm(logr~dT+dCO2,data=df3))

df3 %>%
  ggplot() +
  geom_boxplot( aes(x=pft, y=logr),alpha = 0.6,outlier.shape = NA)+
  geom_boxplot(data=df3,aes(y=logr),alpha = 0.6,outlier.shape = NA)+
  geom_jitter(aes(x=pft, y=logr,color=nferinfo),size=2,width = 0.3) +
  geom_hline( linetype = 'dotted',yintercept=0.0, size=0.5)+ 
  theme_classic()+coord_flip()+theme(axis.text=element_text(size=12))+
  labs(x="", y=~paste(N2O," response to ",warming_and_co2)) 
