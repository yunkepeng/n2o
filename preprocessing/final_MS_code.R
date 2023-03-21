library(readr)  
library(ggplot2)
source("/Users/yunpeng/yunkepeng/n2o_MS/R/analyse_modobs2.R")
source("/Users/yunpeng/yunkepeng/n2o_MS/R/stepwise.R")
source("/Users/yunpeng/yunkepeng/n2o_MS/R/calc_area.R")
source("/Users/yunpeng/yunkepeng/n2o_MS/R/visreg_ggplot.R")
source("/Users/yunpeng/yunkepeng/n2o_MS/R/read_nc_onefile.R")
source("/Users/yunpeng/yunkepeng/n2o_MS/R/nc_to_df.R")
library(visreg)
library(lmerTest)
library(lme4)
library("PerformanceAnalytics")
library(MuMIn)
library(relaimpo)
library(Deriv)
library(rworldmap)
library(maps)
#1. read observation csv and output validation

all_n2o_df <- read.csv("~/data/n2o_Yunke/final_obs_dataset/obs_field_dataset.csv")
#for those forest cover < 80%, remove it (around 8% of data)
length(all_n2o_df$pred_n2o[all_n2o_df$pft=="forest"&all_n2o_df$forest_cover<0.8])/length(all_n2o_df$pred_n2o[all_n2o_df$pft=="forest"])
all_n2o_df$pred_n2o[all_n2o_df$pft=="forest"&all_n2o_df$forest_cover<0.8] <- NA

all_n2o_df$log_pred_n2o <- log(all_n2o_df$pred_n2o)
all_n2o_df$log_obs_n2o <- log(all_n2o_df$obs_n2o)

nrow(subset(all_n2o_df,pft=="forest"))
length(unique(subset(all_n2o_df,pft=="forest")$sitename))
nrow(subset(all_n2o_df,pft=="grassland"))
length(unique(subset(all_n2o_df,pft=="grassland")$sitename))
nrow(subset(all_n2o_df,pft=="cropland"))
length(unique(subset(all_n2o_df,pft=="cropland")$sitename))

a1 <- analyse_modobs2(subset(all_n2o_df,pft=="forest"),"log_pred_n2o","log_obs_n2o", type = "points",relative=TRUE)$gg+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15),
        plot.subtitle=element_text(size=15))+xlab("Predicted ln Forest N2O (ug/m2/h)")+ylab("Measured ln Forest N2O (ug/m2/h)")

a2 <- analyse_modobs2(subset(all_n2o_df,pft=="grassland"),"log_pred_n2o","log_obs_n2o", type = "points",relative=TRUE)$gg+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15),
        plot.subtitle=element_text(size=15))+xlab("Predicted ln Grassland N2O (ug/m2/h)")+ylab("Measured ln Grassland N2O (ug/m2/h)")

a3 <- analyse_modobs2(subset(all_n2o_df,pft=="cropland"),"log_pred_n2o","log_obs_n2o", type = "points",relative=TRUE)$gg+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15),
        plot.subtitle=element_text(size=15))+xlab("Predicted ln Cropland N2O (ug/m2/h)")+ylab("Measured ln Cropland N2O (ug/m2/h)")

#2. fit statistical model 
#(log-transfromed data)
all_n2o_df$n2o_a <- log(all_n2o_df$obs_n2o)
all_n2o_df$Tg_a <- all_n2o_df$Tg
all_n2o_df$PPFD_a <- log(all_n2o_df$PPFD)
all_n2o_df$PPFD_total_a <- log(all_n2o_df$PPFD_total)
all_n2o_df$vpd_a <- log(all_n2o_df$vpd)
all_n2o_df$ndep_a <- log(all_n2o_df$ndep)
all_n2o_df$sqrt_Nfer_kgha <- sqrt(all_n2o_df$Nfer_kgha)
all_n2o_df$orgc_a <- log(all_n2o_df$ORGC)
all_n2o_df$site_a <- (all_n2o_df$sitename)

#check soil moisture metrics (only forest is significant)
summary(lmer(n2o_a~obs_moisture+(1|site_a),data=subset(all_n2o_df,pft=="forest")))
summary(lmer(n2o_a~obs_moisture+(1|site_a),data=subset(all_n2o_df,pft=="grassland")))
summary(lmer(n2o_a~obs_moisture+(1|site_a),data=subset(all_n2o_df,pft=="cropland")))

#forest dataset - the best model should be fitted by Tg and moisture
forest_data <- subset(all_n2o_df,pft=="forest")[,c("n2o_a","site_a","obs_moisture","orgc_a","Tg_a","PPFD_total_a","ndep_a","min_fapar","max_fapar")]
stepwise(forest_data,"n2o_a")[[1]]
stepwise(forest_data,"n2o_a")[[2]]
dim(forest_data)
mod1 <- (lmer(n2o_a~Tg_a+obs_moisture+(1|site_a),data=forest_data))
summary(mod1)
r.squaredGLMM(mod1)
#test: add one more variables - n.s.
summary(lmer(n2o_a~Tg_a+obs_moisture+ndep_a+(1|site_a),data=forest_data))

#merged data with LPX - using the same database to the model - and make sure it includes available pred_n2o (that not account for forest cover <80%)
forest_moisture_data_forLPX <- na.omit(subset(all_n2o_df,pft=="forest")[,c("n2o_a","pred_n2o","Tg_a","obs_moisture","site_a","lon","lat","z","pft")])

forest_data_sitemean <- unique(forest_moisture_data_forLPX[,c("lon","lat","z","pft")])

lpx_forest_n2o <- subset(read.csv("~/data/n2o_Yunke/final_forcing/LPX_annual_n2o.csv"),pft=="forest")
lpx_forest_moisture <- subset(read.csv("~/data/n2o_Yunke/final_forcing/LPX_annual_moisture.csv"),pft=="forest")
lpx_forest_temperature <- subset(read.csv("~/data/n2o_Yunke/final_forcing/LPX_annual_T.csv"),pft=="forest")

LPX_forest_sitemean_n2o <- merge(forest_data_sitemean,lpx_forest_n2o,by=c("lon","lat","z","pft"),all.x=TRUE)
LPX_forest_sitemean_moisture <- merge(forest_data_sitemean,lpx_forest_moisture,by=c("lon","lat","z","pft"),all.x=TRUE)
LPX_forest_sitemean_temperature <- merge(forest_data_sitemean,lpx_forest_temperature,by=c("lon","lat","z","pft"),all.x=TRUE)

n2o_final <- log(data.frame(x=unlist(LPX_forest_sitemean_n2o[,c(5:41)])))
moisture_final <- data.frame(x=unlist(LPX_forest_sitemean_moisture[,c(5:41)]))
Tg_final <- data.frame(x=unlist(LPX_forest_sitemean_temperature[,c(5:41)]))

#extract year-by-year data and used for modelling
final_forest_lpx <- as.data.frame(cbind(n2o_final,moisture_final,Tg_final))
names(final_forest_lpx) <- c("n2o_a","obs_moisture","Tg_a")
mod2 <- (lm(n2o_a~Tg_a+obs_moisture,final_forest_lpx))
summary(mod2)

mod1_moisture <- visreg(mod1,"obs_moisture",type="contrast");mod1_Tg <-visreg(mod1,"Tg_a",type="contrast")
mod2_moisture <- visreg(mod2,"obs_moisture",type="contrast");mod2_Tg <- visreg(mod2,"Tg_a",type="contrast")

fits_moisture <- dplyr::bind_rows(mutate(mod1_moisture$fit, plt = "Measurement"),mutate(mod2_moisture$fit, plt = "LPX"))
fits_tg <- dplyr::bind_rows(mutate(mod1_Tg$fit, plt = "Measurement"),mutate(mod2_Tg$fit, plt = "LPX"))

###converted to lm
visreg_ggplot <- function(obj,var_name,color1,color2,xlab_name,ylab_name){
  final1 <- ggplot() + geom_line(data = obj, aes_string(var_name, "visregFit", group="plt", color="plt"),size=2) +
    theme_classic()+theme(text = element_text(size=20),legend.position="none")+
    geom_ribbon(data = obj,aes_string(var_name, ymin="visregLwr", ymax="visregUpr",fill="plt"),alpha=0.5)+
    scale_colour_manual(values=c(Measurement=color1,LPX=color2))+
    scale_fill_manual(values=c(Measurement=color1,LPX=color2))+xlab(xlab_name) + ylab(ylab_name)
  
  return(final1)
}

g1 <- visreg_ggplot(fits_tg,"Tg_a","black","red",~paste(T[g]," (°C)"),~paste("Forest ln ", N[2], O, " (", mu,"g m"^-2,"h"^-1,")"))
g1
g2 <- visreg_ggplot(fits_moisture,"obs_moisture","black","red","Soil moisture"," ")
g2

#grassland model - follow stepwise regression
grassland_data <- (na.omit(subset(all_n2o_df,pft=="grassland")[,c("lon","lat","z","pft","site_a","n2o_a","orgc_a",
                                                            "Tg_a","PPFD_total_a","ndep_a","sqrt_Nfer_kgha","vpd_a",
                                                            "min_fapar","max_fapar")]))
grassland_data_model <- grassland_data[,!(names(grassland_data) %in% c("lon","lat","z","pft"))]

stepwise(grassland_data_model,"n2o_a")[[1]]
stepwise(grassland_data_model,"n2o_a")[[2]]
#the best model is N fertilisation and min_fapar
mod3 <- (lmer(n2o_a~sqrt_Nfer_kgha+min_fapar+(1|site_a),data=grassland_data))
summary(mod3)
r.squaredGLMM(mod3)
#test: add one more varialbes - n.s.
summary(lmer(n2o_a~sqrt_Nfer_kgha+min_fapar+max_fapar+(1|site_a),data=grassland_data))

grassland_data_sitemean <- unique(grassland_data[,c("lon","lat","z","pft")])

lpx_grassland_n2o <- subset(read.csv("~/data/n2o_Yunke/final_forcing/LPX_annual_n2o.csv"),pft=="grassland")
lpx_grassland_nfer <- subset(read.csv("~/data/n2o_Yunke/final_forcing/LPX_annual_nfer.csv"),pft=="grassland")
lpx_grassland_minfapar <- subset(read.csv("~/data/n2o_Yunke/final_forcing/LPX_annual_actual_minfapar.csv"),pft=="grassland")

LPX_grassland_sitemean_n2o <- merge(grassland_data_sitemean,lpx_grassland_n2o,by=c("lon","lat","z","pft"),all.x=TRUE)
LPX_grassland_sitemean_nfer <- merge(grassland_data_sitemean,lpx_grassland_nfer,by=c("lon","lat","z","pft"),all.x=TRUE)
LPX_grassland_sitemean_minfapar <- merge(grassland_data_sitemean,lpx_grassland_minfapar,by=c("lon","lat","z","pft"),all.x=TRUE)

n2o_final <- log(data.frame(x=unlist(LPX_grassland_sitemean_n2o[,c(5:41)])))
nfer_final <- sqrt(data.frame(x=unlist(LPX_grassland_sitemean_nfer[,c(5:41)])))
minfapar_final <- data.frame(x=unlist(LPX_grassland_sitemean_minfapar[,c(5:41)]))

final_grassland_lpx <- as.data.frame(cbind(n2o_final,nfer_final,minfapar_final))
names(final_grassland_lpx) <- c("n2o_a","sqrt_Nfer_kgha","min_fapar")

mod4 <- (lm(n2o_a~sqrt_Nfer_kgha+min_fapar,data=final_grassland_lpx))
summary(mod4)
r.squaredGLMM(mod4)

mod3_nfer <- visreg(mod3,"sqrt_Nfer_kgha",type="contrast");mod3_minfapar <- visreg(mod3,"min_fapar",type="contrast")
mod4_nfer <- visreg(mod4,"sqrt_Nfer_kgha",type="contrast");mod4_minfapar <- visreg(mod4,"min_fapar",type="contrast")

fits_nfer <- dplyr::bind_rows(mutate(mod3_nfer$fit, plt = "Measurement"),mutate(mod4_nfer$fit, plt = "LPX"))
fits_minfapar <- dplyr::bind_rows(mutate(mod3_minfapar$fit, plt = "Measurement"),mutate(mod4_minfapar$fit, plt = "LPX"))

g3 <- visreg_ggplot(fits_nfer,"sqrt_Nfer_kgha","black","red",~paste("sqrt N fertilisation (kg"," ha"^-1,")"),~paste("Forest ln ", N[2], O, " (", mu,"g m"^-2,"h"^-1,")"))
g3

g4 <- visreg_ggplot(fits_minfapar,"min_fapar","black","red","min fAPAR"," ")
g4

#3. cropland model
cropland_data <- (na.omit(subset(all_n2o_df,pft=="cropland")[,c("lon","lat","z","pft","site_a","n2o_a","orgc_a","vpd_a",
                                                                  "Tg_a","PPFD_total_a","ndep_a","sqrt_Nfer_kgha",
                                                                  "min_fapar","max_fapar")]))
cropland_data_model <- cropland_data[,!(names(cropland_data) %in% c("lon","lat","z","pft"))]

stepwise(cropland_data_model,"n2o_a")[[1]]
stepwise(cropland_data_model,"n2o_a")[[2]]
stepwise(cropland_data_model,"n2o_a")[[3]]
#the best model is when only including sqrt_Nfer_kgha+orgc_a+PPFD_total_a+max_fapar
summary((lmer(n2o_a~sqrt_Nfer_kgha+orgc_a+PPFD_total_a+max_fapar+(1|site_a),data=cropland_data)))

#if including all of them all - ndep is most non-significant -> so remove them
summary((lmer(n2o_a~sqrt_Nfer_kgha+orgc_a+PPFD_total_a+max_fapar+vpd_a+ndep_a+min_fapar+Tg_a+(1|site_a),data=cropland_data)))
summary((lmer(n2o_a~sqrt_Nfer_kgha+orgc_a+PPFD_total_a+max_fapar+vpd_a+min_fapar+Tg_a+(1|site_a),data=cropland_data)))

#this is our model
mod5 <- ((lmer(n2o_a~orgc_a+sqrt_Nfer_kgha+vpd_a+Tg_a+PPFD_total_a+max_fapar+min_fapar+(1|site_a),data=cropland_data)))
summary(mod5)
LPX_cropland_sitemean <- unique(cropland_data[,c("lon","lat","z","pft")])

lpx_cropland_n2o <- subset(read.csv("~/data/n2o_Yunke/final_forcing/LPX_annual_n2o.csv"),pft=="cropland")
lpx_cropland_nfer <- subset(read.csv("~/data/n2o_Yunke/final_forcing/LPX_annual_nfer.csv"),pft=="cropland")
lpx_cropland_temperature <- subset(read.csv("~/data/n2o_Yunke/final_forcing/LPX_annual_T.csv"),pft=="cropland")
lpx_cropland_PPFD <- subset(read.csv("~/data/n2o_Yunke/final_forcing/LPX_annual_PPFD.csv"),pft=="cropland")
lpx_cropland_maxfapar <- subset(read.csv("~/data/n2o_Yunke/final_forcing/LPX_annual_actual_maxfapar.csv"),pft=="cropland")
lpx_cropland_minfapar <- subset(read.csv("~/data/n2o_Yunke/final_forcing/LPX_annual_actual_minfapar.csv"),pft=="cropland")

LPX_cropland_sitemean_n2o <- merge(LPX_cropland_sitemean,lpx_cropland_n2o,by=c("lon","lat","z","pft"),all.x=TRUE)
LPX_cropland_sitemean_nfer <- merge(LPX_cropland_sitemean,lpx_cropland_nfer,by=c("lon","lat","z","pft"),all.x=TRUE)
LPX_cropland_sitemean_temperature <- merge(LPX_cropland_sitemean,lpx_cropland_temperature,by=c("lon","lat","z","pft"),all.x=TRUE)
LPX_cropland_sitemean_PPFD <- merge(LPX_cropland_sitemean,lpx_cropland_PPFD,by=c("lon","lat","z","pft"),all.x=TRUE)
LPX_cropland_sitemean_maxfapar <- merge(LPX_cropland_sitemean,lpx_cropland_maxfapar,by=c("lon","lat","z","pft"),all.x=TRUE)
LPX_cropland_sitemean_minfapar <- merge(LPX_cropland_sitemean,lpx_cropland_minfapar,by=c("lon","lat","z","pft"),all.x=TRUE)

cropland_n2o <- log(data.frame(x=unlist(LPX_cropland_sitemean_n2o[,c(5:41)])))
nfer_n2o <- sqrt(data.frame(x=unlist(LPX_cropland_sitemean_nfer[,c(5:41)])))
temperature_n2o <- data.frame(x=unlist(LPX_cropland_sitemean_temperature[,c(5:41)]))
PPFD_n2o <- log(data.frame(x=unlist(LPX_cropland_sitemean_PPFD[,c(5:41)])))
maxfapar_n2o <- data.frame(x=unlist(LPX_cropland_sitemean_maxfapar[,c(5:41)]))
minfapar_n2o <- data.frame(x=unlist(LPX_cropland_sitemean_minfapar[,c(5:41)]))

final_cropland_lpx <- as.data.frame(cbind(cropland_n2o,nfer_n2o,
                                          temperature_n2o,PPFD_n2o,
                                          maxfapar_n2o,minfapar_n2o))
names(final_cropland_lpx) <- c("n2o_a","sqrt_Nfer_kgha","Tg_a",
                               "PPFD_total_a","max_fapar","min_fapar")

mod6 <- ((lm(n2o_a~sqrt_Nfer_kgha+Tg_a+PPFD_total_a+max_fapar+min_fapar,data=final_cropland_lpx)))
summary(mod6)

mod5_nfer <- visreg(mod5,"sqrt_Nfer_kgha",type="contrast")
mod5_soc <- visreg(mod5,"orgc_a",type="contrast")
mod5_ppfd_total <- visreg(mod5,"PPFD_total_a",type="contrast")
mod5_vpd <- visreg(mod5,"vpd_a",type="contrast")
mod5_Tg <- visreg(mod5,"Tg_a",type="contrast")
mod5_max_fapar <- visreg(mod5,"max_fapar",type="contrast")
mod5_min_fapar <- visreg(mod5,"min_fapar",type="contrast")

mod6_nfer <- visreg(mod6,"sqrt_Nfer_kgha",type="contrast")
mod6_ppfd_total <- visreg(mod6,"PPFD_total_a",type="contrast")
mod6_Tg <- visreg(mod6,"Tg_a",type="contrast")
mod6_max_fapar <- visreg(mod6,"max_fapar",type="contrast")
mod6_min_fapar <- visreg(mod6,"min_fapar",type="contrast")

fits_nfer <- dplyr::bind_rows(mutate(mod5_nfer$fit, plt = "Measurement"),mutate(mod6_nfer$fit, plt = "LPX"))
fits_soc <- dplyr::bind_rows(mutate(mod5_soc$fit, plt = "Measurement"))
fits_ppfd_total <- dplyr::bind_rows(mutate(mod5_ppfd_total$fit, plt = "Measurement"),mutate(mod6_ppfd_total$fit, plt = "LPX"))
fits_vpd <- dplyr::bind_rows(mutate(mod5_vpd$fit, plt = "Measurement"))
fits_Tg <- dplyr::bind_rows(mutate(mod5_Tg$fit, plt = "Measurement"),mutate(mod6_Tg$fit, plt = "LPX"))
fits_max_fapar <- dplyr::bind_rows(mutate(mod5_max_fapar$fit, plt = "Measurement"),mutate(mod6_max_fapar$fit, plt = "LPX"))
fits_min_fapar <- dplyr::bind_rows(mutate(mod5_min_fapar$fit, plt = "Measurement"),mutate(mod6_min_fapar$fit, plt = "LPX"))

g5 <- visreg_ggplot(fits_nfer,"sqrt_Nfer_kgha","black","red","sqrt N fertilisation (kg/ha)","ln N2O (ug/m2/h)")
g6 <- visreg_ggplot(fits_soc,"orgc_a","black","red","ln SOC (g/kg)"," ")
g7 <- visreg_ggplot(fits_ppfd_total,"PPFD_total_a","black","red","ln total gPPFD (mol/m2)"," ")
g8 <- visreg_ggplot(fits_Tg,"Tg_a","black","red","Tg (°C)"," ")
g9 <- visreg_ggplot(fits_vpd,"vpd_a","black","red","ln vpd (kPa)"," ")
g10 <- visreg_ggplot(fits_max_fapar,"max_fapar","black","red","max fAPAR"," ")
g11 <- visreg_ggplot(fits_min_fapar,"min_fapar","black","red","min fAPAR"," ")

g5
g6
g7
g8
g9
g10
g11

#now, include co2 model
df1_all <- read.csv("~/data/n2o_Yunke/final_obs_dataset/obs_eCO2_dataset.csv")
nrow(df1_all)
nrow(unique(df1_all[,c("lon","lat")]))

summary(df1_all)
df1_all$Nfer_a<- sqrt(df1_all$Nfer)
df1_all$PPFD_total_a <- log(df1_all$PPFD_total)
df1_all$vpd_a <- log(df1_all$vpd)
df1_all$orgc_a <- log(df1_all$ORGC)
df1_all$ndep_a <- log(df1_all$ndep)

#Start fitting model
df1_all_test <- na.omit(df1_all[,c("log_co2","Nfer_a","min_fapar","max_fapar","PPFD_total_a",
                                   "ndep_a","Tg","vpd_a","orgc_a","logr")]) 

#start stepwise regression
#log(co2-e/co2-a) should be the first factor selected, then select factors step-by-step
AIC(lm(logr~log_co2,df1_all_test))
AIC(lm(logr~log_co2+Nfer_a,df1_all_test)) # this one is the best
AIC(lm(logr~log_co2+min_fapar,df1_all_test)) 
AIC(lm(logr~log_co2+max_fapar,df1_all_test))
AIC(lm(logr~log_co2+PPFD_total_a,df1_all_test))
AIC(lm(logr~log_co2+Tg,df1_all_test))
AIC(lm(logr~log_co2+vpd_a,df1_all_test))
AIC(lm(logr~log_co2+orgc_a,df1_all_test))
AIC(lm(logr~log_co2+ndep_a,df1_all_test))

#then in further
AIC(lm(logr~log_co2+Nfer_a+max_fapar,df1_all_test))
AIC(lm(logr~log_co2+Nfer_a+PPFD_total_a,df1_all_test)) #this one is the best
AIC(lm(logr~log_co2+Nfer_a+Tg,df1_all_test))
AIC(lm(logr~log_co2+Nfer_a+vpd_a,df1_all_test))
AIC(lm(logr~log_co2+Nfer_a+orgc_a,df1_all_test))
AIC(lm(logr~log_co2+Nfer_a+ndep_a,df1_all_test))

#then - model is even worse:
AIC(lm(logr~log_co2+Nfer_a+PPFD_total_a+max_fapar,df1_all_test))
AIC(lm(logr~log_co2+Nfer_a+PPFD_total_a+Tg,df1_all_test))
AIC(lm(logr~log_co2+Nfer_a+PPFD_total_a+vpd_a,df1_all_test))
AIC(lm(logr~log_co2+Nfer_a+PPFD_total_a+orgc_a,df1_all_test))
AIC(lm(logr~log_co2+Nfer_a+PPFD_total_a+ndep_a,df1_all_test))

#so the best model is: 
mod1 <- (lm(logr~log_co2+Nfer_a+PPFD_total_a,df1_all_test))
summary(mod1)

#start LPX comparasion
LPX_co2_sitemean <- na.omit(df1_all[,c("lon","lat","z","pft","logr","log_co2","Nfer_a","PPFD_total_a")])
LPX_co2_sitemean <- unique(LPX_co2_sitemean[,c("lon","lat","z","pft")])
dim(LPX_co2_sitemean)
LPX_co2_sitemean$pft[LPX_co2_sitemean$pft=="Grassland"] <- "grassland"
LPX_co2_sitemean$pft[LPX_co2_sitemean$pft=="Forest"] <- "forest"
LPX_co2_sitemean$pft[LPX_co2_sitemean$pft=="Cropland"] <- "cropland"

lpx_n2o <- read.csv("~/data/n2o_Yunke/final_forcing/eCO2_LPX_annual_n2o.csv")
#use the year 2006's map, since this is step experiment's initial year (2006)
lpx_nfer <- read.csv("~/data/n2o_Yunke/final_forcing/eCO2_LPX_annual_nfer.csv")[,c("lon","lat","z","pft","year2006")]
names(lpx_nfer) <-c("lon","lat","z","pft","nfer")
lpx_PPFD <- read.csv("~/data/n2o_Yunke/final_forcing/eCO2_LPX_annual_PPFD.csv")[,c("lon","lat","z","pft","year2006")]
names(lpx_PPFD) <-c("lon","lat","z","pft","PPFD")
LPX_all <-Reduce(function(x,y) merge(x = x, y = y, c("lon","lat","z","pft"),all.x=TRUE),
                 list(LPX_co2_sitemean,lpx_n2o,lpx_nfer,lpx_PPFD))

#combine it into a dataframe
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

mod1_co2 <- visreg(mod1,"log_co2",type="contrast")
mod1_nfer <-visreg(mod1,"Nfer_a",type="contrast")
mod1_ppfd <- visreg(mod1,"PPFD_total_a",type="contrast")

mod2_co2 <- visreg(mod2,"log_co2",type="contrast")
mod2_nfer <-visreg(mod2,"Nfer_a",type="contrast")
mod2_ppfd <- visreg(mod2,"PPFD_total_a",type="contrast")

fits_co2 <- dplyr::bind_rows(mutate(mod1_co2$fit, plt = "Measurement"),mutate(mod2_co2$fit, plt = "LPX"))
fits_nfer <- dplyr::bind_rows(mutate(mod1_nfer$fit, plt = "Measurement"),mutate(mod2_nfer$fit, plt = "LPX"))
fits_ppfd <- dplyr::bind_rows(mutate(mod1_ppfd$fit, plt = "Measurement"),mutate(mod2_ppfd$fit, plt = "LPX"))

g1a <- visreg_ggplot(fits_co2,"log_co2","black","red",~paste("ln (",CO2[ele.]," / ", CO2[amb.],")"),~paste("ln (",N2O[ele.]," / ", N2O[amb.], ")"))
g1a

g2a <- visreg_ggplot(fits_nfer,"Nfer_a","black","red","sqrt N fertilisation (kg/ha)"," ")
g2a

g3a <- visreg_ggplot(fits_ppfd,"PPFD_total_a","black","red","ln total gPPFD (mol/m2)"," ")
g3a
                        
#warming experiments
df2_all <- read.csv("~/data/n2o_Yunke/final_obs_dataset/obs_warming_dataset.csv")
nrow(df2_all)
nrow(unique(df2_all[,c("lon","lat")]))

df2_all$Nfer_a<- sqrt(df2_all$Nfer)
df2_all$PPFD_total_a <- log(df2_all$PPFD_total)
df2_all$vpd_a <- log(df2_all$vpd)
df2_all$orgc_a <- log(df2_all$ORGC)
df2_all$ndep_a <- log(df2_all$ndep)

#Start fitting model
df2_all_test <- na.omit(df2_all[,c("dT","Nfer_a","min_fapar","max_fapar","PPFD_total_a",
                                   "ndep_a","vpd_a","orgc_a","logr")]) 

#start stepwise regression
#dT should be the first factor selected, then select factors step-by-step

AIC(lm(logr~dT,df2_all_test))
AIC(lm(logr~dT+Nfer_a,df2_all_test)) 
AIC(lm(logr~dT+min_fapar,df2_all_test)) 
AIC(lm(logr~dT+max_fapar,df2_all_test))
AIC(lm(logr~dT+PPFD_total_a,df2_all_test))
AIC(lm(logr~dT+vpd_a,df2_all_test))
AIC(lm(logr~dT+orgc_a,df2_all_test))# this one is the best
AIC(lm(logr~dT+ndep_a,df2_all_test))  

#then, further
AIC(lm(logr~dT+orgc_a+Nfer_a,df2_all_test)) 
AIC(lm(logr~dT+orgc_a+min_fapar,df2_all_test)) 
AIC(lm(logr~dT+orgc_a+max_fapar,df2_all_test))
AIC(lm(logr~dT+orgc_a+PPFD_total_a,df2_all_test))
AIC(lm(logr~dT+orgc_a+vpd_a,df2_all_test))
AIC(lm(logr~dT+orgc_a+ndep_a,df2_all_test))  

#so the best should just be:
mod3 <- (lm(logr~orgc_a+dT,df2_all))
summary(mod3)

#applied in lpx model
LPX_warming_sitemean <- na.omit(df2_all[,c("lon","lat","dT","orgc_a","logr")])
LPX_warming_sitemean <- unique(df2_all[,c("lon","lat","z","pft")])

#check forestcover of all experimental sites - one site shows forest cover lower than 0.8
co2_forestcover_site<- read.csv("~/data/n2o_Yunke/final_forcing/co2_forestcover_site.csv")
subset(co2_forestcover_site,forest_cover<0.8)
#remove lon==91.758 (since it has vegetation cover less than 80%)
LPX_warming_sitemean <- subset(LPX_warming_sitemean,lon!=91.758)

lpx_n2o <- read.csv("~/data/n2o_Yunke/final_forcing/eCO2_LPX_annual_n2o.csv")

LPX_warming_sitemean$pft[LPX_warming_sitemean$pft=="Grassland"] <- "grassland"
LPX_warming_sitemean$pft[LPX_warming_sitemean$pft=="Forest"] <- "forest"
LPX_warming_sitemean$pft[LPX_warming_sitemean$pft=="Cropland"] <- "cropland"

LPX_all <-Reduce(function(x,y) merge(x = x, y = y, c("lon","lat","z","pft"),all.x=TRUE),
                 list(LPX_warming_sitemean,lpx_n2o))

a1 <- c(log(LPX_all$dT0.39_C380/LPX_all$dT0_C380),
        log(LPX_all$dT3.95_C380/LPX_all$dT0_C380),
        log(LPX_all$dT7.5_C380/LPX_all$dT0_C380)) #n2o
a2 <- c(rep(0.39,nrow(LPX_all)),rep(3.95,nrow(LPX_all)),rep(7.5,nrow(LPX_all))) #co2

final_lpx_data <- as.data.frame(cbind(a1,a2))
names(final_lpx_data) <- c("logr","dT")

final_lpx_data[sapply(final_lpx_data, is.nan)] <- NA
final_lpx_data[sapply(final_lpx_data, is.infinite)] <- NA

mod4 <- (lm(logr~dT,final_lpx_data))
mod4_dT <-visreg(mod4,"dT",type="contrast")

summary(mod4)

mod3 <- (lm(logr~orgc_a+dT,df2_all))
summary(mod3)
mod3_orgc <- visreg(mod3,"orgc_a",type="contrast")
mod3_dT <-visreg(mod3,"dT",type="contrast")


fits_orgc <- dplyr::bind_rows(mutate(mod3_orgc$fit, plt = "Measurement"))
fits_dT <- dplyr::bind_rows(mutate(mod3_dT$fit, plt = "Measurement"),mutate(mod4_dT$fit, plt = "LPX"))

g4a <- visreg_ggplot(fits_orgc,"orgc_a","black","red","ln SOC (g/kg)","ln N2Oe/lnN2Oa")
g4a

g5a <- visreg_ggplot(fits_dT,"dT","black","red","dT", " ")
g5a


#calculate feedback factor

#use SOC map for upscalling
soc_nc <- read_nc_onefile("~/data/n2o_Yunke/final_map/ORGC.nc")
orgc_df <- as.data.frame(nc_to_df(soc_nc, varnam = "ORGC"))
summary(orgc_df)

#input functions/map for area and land cover percentage at each grid
#so we can know actual land area at each grid
#then we calculate (grid's land area)/(total land area) at each grid
#this will help us to upscal to get feedback and gains values
area_m2 <- calc_area(orgc_df$lat,0.5,0.5)

#fland - to show each grid's land cover percentage
nc <- read_nc_onefile("~/data/fland/global.fland.nc")
output_fland <- nc_to_df(nc, varnam = "fland")
fland <- output_fland$fland
#area_m2 * fland = land area at each grid
conversion <- area_m2 * fland
aa <- sum(conversion,na.rm=T)
#here fraction is (grid's land area)/(total land area) at each grid
fraction <- conversion/aa

#we will assume n2o at ambient condition (n2o_a) as 329.29ppb (uncertainty 0.12)
#value using 2016's value: https://www.eea.europa.eu/data-and-maps/daviz/atmospheric-concentration-of-carbon-dioxide-5#tab-chart_5_filters=%7B%22rowFilters%22%3A%7B%7D%3B%22columnFilters%22%3A%7B%22pre_config_polutant%22%3A%5B%22CH4%20(ppb)%22%5D%7D%7D

#then we set dT = 0.39 and 7.5, then after regressions and upscal, to get n2o_e
final1 <- sum(329.29*fraction*exp((summary(mod3)$coefficients[1,1])+ (summary(mod3)$coefficients[2,1])*log(orgc_df$ORGC)+(summary(mod3)$coefficients[3,1])*0.39),na.rm=T)
final3 <- sum(329.29*fraction*exp((summary(mod3)$coefficients[1,1])+ (summary(mod3)$coefficients[2,1])*log(orgc_df$ORGC)+(summary(mod3)$coefficients[3,1])*7.5),na.rm=T)
final1
final3

#calculate N2Oe uncertainty
#after error propogation: ln (n2o-e/n2o-a) = model
#uncertainty n2o_e =n2o_e * sqrt(uncertainty-model^2 + (delta-n2o_a/ n2o_a)^2)
uncertainty_model <- sqrt(((summary(mod3)$coefficients[1,1])^2 *
                             summary(mod3)$coefficients[1,2]^2+
                             (summary(mod3)$coefficients[2,1])^2 *
                             summary(mod3)$coefficients[2,2]^2)+
                            ( (summary(mod3)$coefficients[3,1])^2 *
                                summary(mod3)$coefficients[3,2]^2))
uncertainty_fN <- sqrt(uncertainty_model^2+ (0.12/329.29)^2)
#here uncertainty_fN presents percentage of uncertainty -> will multuply with n2o_e to get actual uncertainty

#estimate emission sensitivity (ES) of our model
#using 8.07 from mean(lpx$dT_0) from LPX simulations (average Tg/yr of ambient n2o conditions)
E1 <- sum(8.07*fraction*exp((summary(mod3)$coefficients[1,1])+ (summary(mod3)$coefficients[2,1])*log(orgc_df$ORGC)+(summary(mod3)$coefficients[3,1])*0.39),na.rm=T)
E2 <- sum(8.07*fraction*exp((summary(mod3)$coefficients[1,1])+ (summary(mod3)$coefficients[2,1])*log(orgc_df$ORGC)+(summary(mod3)$coefficients[3,1])*7.5),na.rm=T)
ES <- (E2-E1)/(7.5-0.39)
ES 

#this function is for feedback value
fN<-function(N,N0,C_mean,M_mean,N_mean){(-8.0*10^(-6)*C_mean+4.2*10^(-6)*N_mean-4.9*10^(-6)*M_mean+0.117)*(sqrt(N)-sqrt(N0))}

#this function is for uncertainty of feedback value
err_fN<-function(N,N0,C_mean,M_mean,N_mean,err_N,err_N0){
  DN<-Deriv(fN,"N");DM0<-Deriv(fN,"M0");DN0<-Deriv(fN,"N0")
  sqrt( DN(N,N0,C_mean,M_mean,N_mean)^2*err_N^2 + 
          DN0(N,N0,C_mean,M_mean,N_mean)^2*err_N0^2 )
}

#value and S.E.
#for other values here (co2, ch4, using 2016's value - see above link)
rN_value<- fN(final3,final1,402.88,1842.4,(final3+final1)/2)/(7.50-0.39)
rN_value # this is feedback value of paper
rN_SE_value<- err_fN(final3,final1,402.88,1842.4,(final3+final1)/2,final3*uncertainty_fN,final1*uncertainty_fN)/(7.50-0.39)
rN_SE_value  # this is S.E. of feedback value of paper

#value of gains - See Liu et al. SI (corrected version)
lamda <- 0.875
se_lamda <- 0.38/1.96

gains <- lamda*rN_value
gains# this is gain of paper

gains_uncertainty <- sqrt(rN_value^2 * se_lamda^2 + lamda^2 * rN_SE_value^2)
gains_uncertainty # this is S.E. of gain of paper

#using LPX
#here we input global estimation of n2o under step experiment at LPX, when dT changes from 0 to 0.39, until 7.5
#it runs 100 years - therefore show 100 values at each experiment (unit: Tg/yr)
lpx <- read.csv("~/data/n2o_Yunke/final_forcing/eCO2_warming_LPX_total_n2o.csv")

#assume ambient n2o is still 329ppb, then we get response ratio when temperature change - so we can know concentrations
final1_lpx <- 329.29*mean(lpx$dT_0.39)/mean(lpx$dT_0)
final3_lpx <-329.29*mean(lpx$dT_7.5)/mean(lpx$dT_0)

rN_value_lpx<- fN(final3_lpx,final1_lpx,402.88,1842.4,(final3_lpx+final1_lpx)/2)/(7.50-0.39)
rN_value_lpx # this is feedback value of LPX

gains_lpx <- lamda*rN_value_lpx
gains_lpx # this is gain value of LPX

gains_uncertainty_lpx <- sqrt(rN_value_lpx^2 * se_lamda^2 + lamda^2 * 0^2)
gains_uncertainty_lpx # this is uncertainty of gain value of LPX

#estimate emission sensitivity (ES) of LPX
ES <- (mean(lpx$dT_7.5)-mean(lpx$dT_0))/(7.5-0)
ES #1.05 this value is ES from LPX

#co2 feedback
#including global map from N fertilisation and PPFD
nfer_nc <- read_nc_onefile("~/data/n2o_Yunke/final_map/nfer.nc")#unit g/m2
nfer_df <- as.data.frame(nc_to_df(nfer_nc, varnam = "nfer"))
summary(nfer_df)
PPFD_total_nc <- read_nc_onefile("~/data/n2o_Yunke/final_map/PPFD_total.nc")
PPFD_total_df <- as.data.frame(nc_to_df(PPFD_total_nc, varnam = "PPFD_total"))
summary(PPFD_total_df)

#the method is same as warming model - but here we remove intercept term because the intercept is non-significant
#we assume n2o at ambient condition is 380, then co2-e at 416 and 813ppm separately to get feedback value
final1a <- sum(329.29*fraction*exp(0+ summary(mod1)$coefficients[2,1]*log(416/380)+
                                     summary(mod1)$coefficients[3,1]*sqrt(nfer_df$nfer*10)+
                                     summary(mod1)$coefficients[4,1]*log(PPFD_total_df$PPFD_total)),na.rm=T)

final3a <- sum(329.29*fraction*exp(0+ summary(mod1)$coefficients[2,1]*log(813/380)+
                                     summary(mod1)$coefficients[3,1]*sqrt(nfer_df$nfer*10)+
                                     summary(mod1)$coefficients[4,1]*log(PPFD_total_df$PPFD_total)),na.rm=T)
final1a
final3a

#calculate N2Oe uncertainty
#after error propogation: ln (n2o-e/n2o-a) = model
#uncertainty n2o_e =n2o_e * sqrt(uncertainty-model^2 + (delta-n2o_a/ n2o_a)^2)
uncertainty_model2 <- sqrt(summary(mod1)$coefficients[2,1]^2 *
                             summary(mod1)$coefficients[2,2]^2 +
                             summary(mod1)$coefficients[3,1]^2 *
                             summary(mod1)$coefficients[3,2]^2+
                             summary(mod1)$coefficients[4,1]^2 *
                             summary(mod1)$coefficients[4,2]^2)
uncertainty_fN2 <- sqrt(uncertainty_model2^2+ (0.12/329.29)^2)
#here uncertainty_fN2 presents percentage of uncertainty -> will multuply with n2o_e to get actual uncertainty

#feedback value
rN_value_eCO2<- fN(final3a,final1a,(813+416)/2,1842.4,(final3a+final1a)/2)/(813-416)
rN_value_eCO2 #feedback value of eCO2 model
rN_value_eCO2_se <- err_fN(final3a,final1a,(813+416)/2,1842.4,(final3a+final1a)/2,final3a*uncertainty_fN2,final1a*uncertainty_fN2)/(813-416)
rN_value_eCO2_se #S.E. feedback value of eCO2 model

#using LPX
#here we input global estimation of n2o under step experiment at LPX, when eCO2 changes from 380 to 416, until 813
#it runs 100 years - therefore show 100 values at each experiment (unit: Tg/yr)
lpx <- read.csv("~/data/n2o_Yunke/final_forcing/eCO2_warming_LPX_total_n2o.csv")
#assume n2o_a, again, is 329.29 (0.12)
final1_lpx_eCO2 <- 329.29*mean(lpx$eCO2_416)/mean(lpx$dT_0)
final3_lpx_eCO2 <-329.29*mean(lpx$eCO2_813)/mean(lpx$dT_0)

rN_value_eCO2_lpx<- fN(final3_lpx_eCO2,final1_lpx_eCO2,(813+416)/2,1842.4,(final3_lpx_eCO2+final1_lpx_eCO2)/2)/(813-416)
rN_value_eCO2_lpx #feedback value of eCO2 in LPX

#lamda of eCO2
#quote from IPCC AR6:
#The assessed ERF for a doubling of carbon dioxide compared to 1750 levels (3.93 ± 0.47 W m–2) is larger than in AR5
lamda <- (560-280)/3.93
lamda_se <- (560-280)*0.47/(3.93^2)

gains_co2 <- lamda*rN_value_eCO2
gains_co2 #gains of eCO2 of our model

gains_uncertainty <- sqrt(rN_value_eCO2^2 * lamda_se^2 + lamda^2 * rN_value_eCO2_se^2)
gains_uncertainty #uncertainty gains of eCO2 of our model

gains_co2_lpx <- lamda*rN_value_eCO2_lpx
gains_co2_lpx #gains of eCO2 of LPX

gains_uncertainty_lpx <- sqrt(rN_value_eCO2_lpx^2 * lamda_se^2 + lamda^2 * 0^2)
gains_uncertainty_lpx #uncertainty of gains of eCO2 of LPX


#show global distribution map
#newmap <- getMap(resolution = "low")
#plot(newmap, xlim = c(-180, 180), ylim = c(-75, 75), asp = 1)
#points(subset(all_n2o_df,pft=="forest")$lon,subset(all_n2o_df,pft=="forest")$lat, col="green", pch=16,cex=1)
#points(subset(all_n2o_df,pft=="grassland")$lon,subset(all_n2o_df,pft=="grassland")$lat, col="yellow", pch=16,cex=1)
#points(subset(all_n2o_df,pft=="cropland")$lon,subset(all_n2o_df,pft=="cropland")$lat, col="brown", pch=16,cex=1)

#newmap <- getMap(resolution = "low")
#plot(newmap, xlim = c(-180, 180), ylim = c(-75, 75), asp = 1)
#points(df1_all$lon,df1_all$lat, col="red", pch=16,cex=1)
#points(df2_all$lon,df2_all$lat, col="blue", pch=16,cex=1)

#variable importance from lmer model
#https://gist.github.com/BERENZ/e9b581a4b7160357934e
calc.relip.mm <- function(model,type='lmg') {
  if (!isLMM(model) & !isGLMM(model)) {
    stop('Currently supports only lmer/glmer objects', call. = FALSE)
  }
  require(lme4)
  X <- getME(model,'X')
  X <- X[,-1]
  Y <- getME(model,'y')
  s_resid <- sigma(model)
  s_effect <- getME(model,'theta')*s_resid
  s2 <- sum(s_resid^2,s_effect^2)
  V <- Diagonal(x = s2,n=nrow(X))
  YX <- cbind(Y,X)
  cov_XY <- solve( t(YX) %*% solve(V) %*% as.matrix(YX))
  colnames(cov_XY) <- rownames(cov_XY) <- colnames(YX)
  importances <- calc.relimp(as.matrix(cov_XY),rela=T,type=type)
  return(importances)
}
