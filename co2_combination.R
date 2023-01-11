library(readr)
library(ggplot2)
library(dplyr)
library(lme4)
library(MuMIn)
library(lmerTest)
source("/Users/yunpeng/yunkepeng/CNuptake_MS/R/stepwise.R")
source("/Users/yunpeng/yunkepeng/CNuptake_MS/R/stepwise_lm.R")
library(visreg)
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
summary(df1)
names(df1) <- c("ref","location","n_amb","n_elv","n_site","n2o_amb",
                "n2o_elv","co2_amb","co2_elv","dco2","duration","pft","logr",
                "weight","group","method","species","Nfer","other","latitude","longitude","lat","lon","comments")

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

df1_all_test <- na.omit(df1_all[,c("log_co2","Nfer_a","min_fapar","mean_fapar","max_fapar","PPFD_total_a",
                                   "Tg","vpd_a","ndep_a","orgc_a","logr")])
stepwise_lm(df1_all_test,"logr")[[1]]
stepwise_lm(df1_all_test,"logr")[[2]]

mod1 <- (lm(logr~log_co2+Nfer_a+PPFD_total_a,df1_all_test))
summary(mod1)
length(unique(df1_all_test$Tg))
mod1a <- visreg(mod1,"log_co2",type="contrast")
mod1b <-visreg(mod1,"Nfer_a",type="contrast")
mod1c <- visreg(mod1,"PPFD_total_a",type="contrast")

summary(df1_all$logr/df1_all$log_co2)
summary(df1_all$logr)

#warming only effect
df2 <- read_csv("~/data/n2o_wang_oikos/n2o_tables2.csv")
summary(df2)
names(df2) <- c("ref","location","n_amb","n_elv","n_site","n2o_amb",
                "n2o_elv","dT","duration","pft","logr",
                "weight","group","species","Nfer","other","latitude","longitude","lat","lon","comments")

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

#merge with both
df2_a <- merge(df2,fapar3g_df_zhu2,by=c("lon","lat"),all.x=TRUE)

df2_all <- merge(df2_a,climate_soil2[,c("lon","lat","z","PPFD_total_fapar","PPFD_total","Tg","PPFD","vpd","alpha","ndep","ORGC")],
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

df2_all_test <- na.omit(df2_all[,c("dT","Nfer_a","min_fapar","mean_fapar","max_fapar","PPFD_a",
                                   "vpd_a","ndep_a","orgc_a","logr")])

length(unique(df2_all_test$vpd_a))

mod1a <- visreg(mod1,"log_co2",type="contrast")
mod1b <-visreg(mod1,"Nfer_a",type="contrast")

stepwise_lm(df2_all_test,"logr")[[1]]
stepwise_lm(df2_all_test,"logr")[[2]]

mod2 <- (lm(logr~orgc_a+dT,df2_all_test))
summary(mod2)
mod2a <- visreg(mod2,"orgc_a",type="contrast")
mod2b <-visreg(mod2,"dT",type="contrast")

df2_all_test <- na.omit(df2_all[,c("dT","Nfer_a","min_fapar","mean_fapar","max_fapar","PPFD_total_a",
                                   "vpd_a","ndep_a","orgc_a","logr")])
stepwise_lm(df2_all_test,"logr")[[1]]
stepwise_lm(df2_all_test,"logr")[[2]]

summary(lm(logr~orgc_a+dT,df2_all_test))


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
