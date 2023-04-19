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


#warming experiments
df2_all <- read.csv("~/data/n2o_Yunke/final_obs_dataset/obs_warming_dataset.csv")
unique(df2_all$other_treatment)
df2_all$other_treatment2 <- df2_all$other_treatment

df2_all$other_treatment2[df2_all$other_treatment=="elevated precipitation"|
                           df2_all$other_treatment=="drained"|
                           df2_all$other_treatment=="high irrigation"] <- "irrigation"

df2_all$other_treatment2[df2_all$other_treatment=="elevated precipitation and N addition"] <- "irrigation+N"

df2_all$other_treatment2[df2_all$other_treatment=="N addition"|
                         df2_all$other_treatment=="no grazing N addition"] <- "N addition"

df2_all$other_treatment2[df2_all$other_treatment=="CO 2  enrichment"|
                           df2_all$other_treatment=="CO 2 enrichment"|
                           df2_all$other_treatment=="CO 2\nenrichment, Richmond D"|
                           df2_all$other_treatment=="CO 2\nenrichment,\nArmidale C"|
                           df2_all$other_treatment=="CO 2\nenrichment,\nArmidale R"|
                           df2_all$other_treatment=="CO 2  enrichment and drought"] <- "co2"

df2_all$other_treatment2[df2_all$other_treatment=="biochar addition" ] <- "biochar addition"

df2_all$other_treatment2[df2_all$other_treatment=="drained"|
                           df2_all$other_treatment=="drying"] <- "drainage"

df2_all$other_treatment2[df2_all$other_treatment=="grazing"|
                           df2_all$other_treatment=="growing season, Grazing"|
                           df2_all$other_treatment=="no growing season, grazing"|
                           df2_all$other_treatment=="grazing and no fertilization"] <- "grazing"


df2_all$other_treatment2[df2_all$other_treatment=="without plant"|
                           df2_all$other_treatment=="drought"|
                           df2_all$other_treatment=="undrained"|
                           df2_all$other_treatment=="befor grazing"|
                           df2_all$other_treatment=="No grazing"|
                           df2_all$other_treatment=="growing season, No grazing"|
                           df2_all$other_treatment=="no grazing and no fertilization"|
                           df2_all$other_treatment=="no growing season,No grazing"|
                           df2_all$other_treatment=="No tillage"|
                           df2_all$other_treatment=="rainfall redution"|
                           df2_all$other_treatment=="Richmond D"|
                           df2_all$other_treatment=="Armidale R"|
                           is.na(df2_all$other_treatment)==T] <- "No_treatment"

df2_all$other_treatment2[df2_all$other_treatment=="Convential\ntillage"] <- "tillage"
df2_all$other_treatment2[df2_all$other_treatment=="grazing and N addition"] <- "grazing+N"

unique(df2_all$other_treatment2)

#number of sites and observations
nrow(df2_all)
nrow(unique(df2_all[,c("lon","lat")]))

summary(lm(logr~dT,df2_all))

df2_all$Nfer_a<- sqrt(df2_all$Nfer)
df2_all$PPFD_total_a <- log(df2_all$PPFD_total)
df2_all$vpd_a <- log(df2_all$vpd)
df2_all$orgc_a <- log(df2_all$ORGC)
df2_all$ndep_a <- log(df2_all$ndep)
df2_all$RR <- log(df2_all$n2o_elv/df2_all$n2o_amb)/(df2_all$dT)
summary(df2_all$RR)

ggplot() +
  geom_boxplot(aes(x = other_treatment2, y = RR), data = df2_all)

df2_all_no_treatment2 <- subset(df2_all,other_treatment2=="No_treatment")

#create site-name
aaa <- unique(df2_all[,c("lon","lat")])
aaa$site <- paste("a",c(1:nrow(aaa)),sep="")

df2_all_test <- merge(df2_all,aaa,by=c("lon","lat"),all.x=TRUE)

#start stepwise regression
#dT should be the first factor selected, then select factors step-by-step

summary(lm(RR~Nfer_a,df2_all_test)) 
summary(lm(RR~min_fapar,df2_all_test)) 
summary(lm(RR~max_fapar,df2_all_test))
summary(lm(RR~PPFD_total_a,df2_all_test))
summary(lm(RR~vpd_a,df2_all_test))
summary(lm(RR~orgc_a,df2_all_test))# this one is the best

mod1 <- (lm(RR~orgc_a,df2_all_test))# this one is the best
summary(mod1)
visreg(mod1)
exp(sum(fraction* (0.117*log(orgc_df$ORGC)),na.rm=T))

exp(sum(fraction* (-0.2875957 +
                     0.1161154*log(orgc_df$ORGC)),na.rm=T))

  

#using LPX
#here we input global estimation of n2o under step experiment at LPX, when dT changes from 0 to 0.39, until 7.5
#it runs 100 years - therefore show 100 values at each experiment (unit: Tg/yr)
lpx <- read.csv("~/data/n2o_Yunke/final_forcing/eCO2_warming_LPX_total_n2o.csv")
mean(exp(log((lpx$dT_0.39)/(lpx$dT_0))/0.39))
mean(exp(log((lpx$dT_3.95)/(lpx$dT_0))/3.95))
mean(exp(log((lpx$dT_7.5)/(lpx$dT_0))/7.5))

#1.097937
aa1 <- c((lpx$dT_0),(lpx$dT_0.39),(lpx$dT_3.95),(lpx$dT_7.5))
aa2 <-  c(rep(0,100),rep(0.39,100),rep(3.95,100),rep(7.5,100))
ggplot()+geom_point(aes(x=aa2,y=aa1))+xlab("dT")+ylab("n2o TgN/yr")+geom_smooth(aes(x=aa2,y=aa1),method="lm") + geom_abline(slope = 1)

fN<-function(N,N0,C_mean,M_mean,N_mean){(-8.0*10^(-6)*C_mean+4.2*10^(-6)*N_mean-4.9*10^(-6)*M_mean+0.117)*(sqrt(N)-sqrt(N0))}

fN(443,403,402.88,1842.4,(443+403)/2)
fN(437,403,402.88,1842.4,(437+403)/2)
fN(671,403,402.88,1842.4,(671+403)/2)

#co2 effect
df1_all <- read.csv("~/data/n2o_Yunke/final_obs_dataset/obs_eCO2_dataset.csv")
unique(df1_all$other_treatment)

df1_all$other_treatment2 <- df1_all$other_treatment
df1_all$other_treatment2[df1_all$other_treatment=="N addition"|
                           df1_all$other_treatment=="burned, N addition,"|df1_all$other_treatment=="fertile soil"|
                           df1_all$other_treatment=="high N"|df1_all$other_treatment=="low N" ] <- "N addition"

df1_all$other_treatment2[df1_all$other_treatment=="burned, N addition, increased precipitattion"|
                           df1_all$other_treatment=="N addition,\nincreased precipitation"] <- "irrigation+N"

df1_all$other_treatment2[df1_all$other_treatment=="increased precipitation"|
                           df1_all$other_treatment== "burned, increased precipitation"] <- "irrigation"

df1_all$other_treatment2[df1_all$other_treatment=="increased precipitation"|
                           df1_all$other_treatment== "burned, increased precipitation"|
                           df1_all$other_treatment=="+H 2 O"] <- "irrigation"
df1_all$other_treatment2[df1_all$other_treatment=="increased precipitation, N addition, warming"] <- "irrigation+N+warming"
df1_all$other_treatment2[df1_all$other_treatment=="increased precipitation,\nwarming"] <- "irrigation+warming"
df1_all$other_treatment2[df1_all$other_treatment=="warming"|
                           df1_all$other_treatment=="warming and without plant"|
                         df1_all$other_treatment=="warming and drought"|
                         df1_all$other_treatment== "driftway soil, warming"|
                         df1_all$other_treatment== "camerons\nsoil, warming"|
                           df1_all$other_treatment=="rosemary\nsoil, warming" ] <- "warming"
df1_all$other_treatment2[df1_all$other_treatment=="N addition, warming"] <- "warming+N"
df1_all$other_treatment2[df1_all$other_treatment=="conventional till"|
                           df1_all$other_treatment=="conservation till"] <- "tillage"
#others are no_treatment
df1_all$other_treatment2[df1_all$other_treatment== "burned" |
                           df1_all$other_treatment=="without plant"|
                           df1_all$other_treatment=="O 3"|
                           df1_all$other_treatment=="unfertile soil"|
                           df1_all$other_treatment=="pH=5.9"|
                           df1_all$other_treatment=="pH=7.0"|
                           df1_all$other_treatment=="pH=4.3"|
                           df1_all$other_treatment=="drought"|
                           df1_all$other_treatment=="peat soil, same moisture"|
                           df1_all$other_treatment=="peat soil, same watering"|
                           df1_all$other_treatment=="sandy soil, same watering"|
                           df1_all$other_treatment=="sandy soil, same moisture"|
                           df1_all$other_treatment=="driftway soil"|
                           df1_all$other_treatment=="rosemary soil"|
                           df1_all$other_treatment=="acidic loam"|
                           df1_all$other_treatment=="calcareous sand"|
                           is.na(df1_all$other_treatment)==T] <- "no_treatment"


#number of observation and sites
nrow(df1_all)
nrow(unique(df1_all[,c("lon","lat")]))

summary(df1_all)
df1_all$Nfer_a<- sqrt(df1_all$Nfer)
df1_all$PPFD_total_a <- log(df1_all$PPFD_total)
df1_all$vpd_a <- log(df1_all$vpd)
df1_all$orgc_a <- log(df1_all$ORGC)
df1_all$ndep_a <- log(df1_all$ndep)
df1_all$RR <- df1_all$logr/df1_all$log_co2

ggplot() +
  geom_boxplot(aes(x = other_treatment2, y = RR), data = df1_all)

#Start fitting model
df1_all_test <- na.omit(df1_all[,c("log_co2","Nfer_a","min_fapar","max_fapar","PPFD_total_a",
                                   "ndep_a","Tg","vpd_a","orgc_a","RR","other_treatment2")]) 



#start stepwise regression
#log(co2-e/co2-a) should be the first factor selected, then select factors step-by-step
summary(lm(RR~Nfer_a,df1_all_test)) # this one is the best
summary(lm(RR~min_fapar,df1_all_test)) 
summary(lm(RR~max_fapar,df1_all_test))
summary(lm(RR~PPFD_total_a,df1_all_test))
summary(lm(RR~Tg,df1_all_test))
summary(lm(RR~vpd_a,df1_all_test))
summary(lm(RR~orgc_a,df1_all_test))
summary(lm(RR~ndep_a,df1_all_test))

#upscalling
#use SOC map for upscalling
ndep_nc <- read_nc_onefile("~/data/n2o_Yunke/final_map/ndep.nc")
ndep_df <- as.data.frame(nc_to_df(ndep_nc, varnam = "ndep"))
summary(ndep_df)

#input functions/map for area and land cover percentage at each grid
#so we can know actual land area at each grid
#then we calculate (grid's land area)/(total land area) at each grid
#this will help us to upscal to get feedback and gains values
area_m2 <- calc_area(ndep_df$lat,0.5,0.5)

#fland - to show each grid's land cover percentage
nc <- read_nc_onefile("~/data/fland/global.fland.nc")
output_fland <- nc_to_df(nc, varnam = "fland")
fland <- output_fland$fland
#area_m2 * fland = land area at each grid
conversion <- area_m2 * fland
aa <- sum(conversion,na.rm=T)
#here fraction is (grid's land area)/(total land area) at each grid
fraction <- conversion/aa

mod2 <- lm(logr~ndep_a,df1_all_test)
visreg(mod2)
summary(mod2)

(sum(fraction* (0.7840 +0.6529*log(ndep_df$ndep)),na.rm=T))
exp(log(2)*0.065)

