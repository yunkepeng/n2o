library(readr)
library(ggplot2)
library(dplyr)
library(lme4)
library(MuMIn)
library(lmerTest)
library(Deriv)
library(raster)
devtools::load_all("/Users/yunpeng/yunkepeng/latest_packages/rbeni/") 

#co2-only effect
df1 <- read_csv("~/data/n2o_wang_oikos/n2o_tables1.csv")

names(df1) <- c("ref","location","n_amb","n_elv","n_site","n2o_amb",
                "n2o_elv","co2_amb","co2_elv","dco2","duration","pft","logr",
                "weight","group","method","species","Nfer","other","latitude","longitude","lat","lon","comments","days")

df1$Nfer <- as.numeric(df1$Nfer)*10 #convert g/m2 to kg/ha
df1$co2_amb <- as.numeric(df1$co2_amb)
df1$co2_elv <- as.numeric(df1$co2_elv)
df1$logr <- log(df1$n2o_elv/df1$n2o_amb)
df1$log_co2 <- log(df1$co2_elv/df1$co2_amb)
df1$logr[df1$logr=="Inf"] <- NA
df1$logr[df1$logr=="-Inf"] <- NA

#add fapar3g from 1/12 resolution (monthly max and mean)
#code see: co2_fapar.R
fapar3g_df_zhu <- read.csv("~/data/n2o_Yunke/final_forcing/co2_siteinfo_measurementyear_fapar3g_zhu.csv")
dim(fapar3g_df_zhu)
summary(fapar3g_df_zhu)

fapar3g_df_zhu <- fapar3g_df_zhu %>% mutate(min_fapar = coalesce(min_fapar_nfocal0,min_fapar_nfocal1,min_fapar_nfocal2)) %>%
  mutate(mean_fapar = coalesce(mean_fapar_nfocal0,mean_fapar_nfocal1,mean_fapar_nfocal2)) %>%
  mutate(max_fapar = coalesce(max_fapar_nfocal0,max_fapar_nfocal1,max_fapar_nfocal2))
fapar3g_df_zhu2 <- fapar3g_df_zhu[,c("lon","lat","min_fapar","mean_fapar","max_fapar")]

#read  predictors
#For code see: co2_predictors.R
climates_soil <- read.csv("~/data/n2o_Yunke/final_forcing/co2_siteinfo_predictors.csv")[,c("lon","lat","z","vpd","Tg","PPFD_total","PPFD","ndep","ORGC")]

#merge with both
df1_a <- merge(df1,fapar3g_df_zhu2,by=c("lon","lat"),all.x=TRUE)

df1_all <- merge(df1_a,climates_soil,
                 by=c("lon","lat"),all.x=TRUE)

df1_all$Nfer[is.na(df1_all$Nfer)==T] <- 0

#warming only effect
df2 <- read_csv("~/data/n2o_wang_oikos/n2o_tables2.csv")
summary(df2)
names(df2) <- c("ref","location","n_amb","n_elv","n_site","n2o_amb",
                "n2o_elv","dT","duration","pft","logr",
                "weight","group","species","Nfer","other","latitude","longitude","lat","lon","comments","days")

df2$Nfer <- as.numeric(df2$Nfer)*10 #convert g/m2 to kg/ha

df2$logr <- log(df2$n2o_elv/df2$n2o_amb)

#QQQ: the one with very low logr looks confusing, (ref = Zhao et al., 2017), n2o decreased from 61440 to 1680??? While another sample of this ref looks ok. 
#not found in paper!!! - In Fig.6 it seems that at AS species was decreased by -15%.
#remove them by contacting first author
min(df2$logr,na.rm=T)
df2 <- subset(df2,logr!=min(df2$logr,na.rm=T))

#merge with both
df2_a <- merge(df2,fapar3g_df_zhu2,by=c("lon","lat"),all.x=TRUE)

df2_all <- merge(df2_a,climates_soil,
                 by=c("lon","lat"),all.x=TRUE)

df2_all$Nfer[is.na(df2_all$Nfer)==T] <- 0

#output it 
df1_all_output <- df1_all[,c("lon","lat","ref","location","n2o_amb","n2o_elv",
                            "co2_amb","co2_elv","pft","logr","method",
                            "species","Nfer","other","days","log_co2","min_fapar",
                            "mean_fapar","max_fapar","z","vpd","Tg","PPFD_total",
                            "PPFD","ndep","ORGC")]
names(df1_all_output) <-  c("lon","lat","original_ref","location","n2o_amb","n2o_elv",
                             "co2_amb","co2_elv","pft","logr","method",
                             "species","Nfer_kgha","other_treatment","days_of_duration","log_co2","min_fapar",
                             "mean_fapar","max_fapar","z","vpd","Tg","PPFD_total",
                             "PPFD","ndep","ORGC")

csvfile <- paste("~/data/n2o_Yunke/final_obs_dataset/obs_eCO2_dataset.csv")
write_csv(df1_all_output, path = csvfile)

df2_all_output <- df2_all[,c("lon","lat","ref","location","n2o_amb","n2o_elv",
                             "dT","pft","logr",
                             "species","Nfer","other","days","min_fapar",
                             "mean_fapar","max_fapar","z","vpd","Tg","PPFD_total",
                             "PPFD","ndep","ORGC")]

names(df2_all_output) <-  c("lon","lat","original_ref","location","n2o_amb","n2o_elv",
                            "dT","pft","logr",
                            "species","Nfer_kgha","other_treatment","days_of_duration","min_fapar",
                            "mean_fapar","max_fapar","z","vpd","Tg","PPFD_total",
                            "PPFD","ndep","ORGC")
csvfile <- paste("~/data/n2o_Yunke/final_obs_dataset/obs_warming_dataset.csv")
write_csv(df2_all_output, path = csvfile)

#convert some values
df1_all$Nfer_a<- sqrt(df1_all$Nfer)
df1_all$PPFD_total_a <- log(df1_all$PPFD_total)
df1_all$vpd_a <- log(df1_all$vpd)
df1_all$orgc_a <- log(df1_all$ORGC)

df1_all_test <- na.omit(df1_all[,c("log_co2","Nfer_a","min_fapar","mean_fapar","max_fapar","PPFD_total_a",
                                   "Tg","vpd_a","orgc_a","logr")])

summary(lm(logr~log_co2+Nfer_a+PPFD_total_a,df1_all_test))
r.squaredGLMM(lm(logr~log_co2+Nfer_a+PPFD_total_a,df1_all))

df2_all$Nfer_a<- sqrt(df2_all$Nfer)
df2_all$PPFD_total_a <- log(df2_all$PPFD_total)
df2_all$vpd_a <- log(df2_all$vpd)
df2_all$orgc_a <- log(df2_all$ORGC)

df2_all_test <- na.omit(df2_all[,c("dT","Nfer_a","min_fapar","mean_fapar","max_fapar","PPFD_total_a",
                                   "vpd_a","orgc_a","logr")])#remove ndep_a
summary(lm(logr~orgc_a+dT,df2_all_test))