library(readr)
library(ggplot2)
library(dplyr)
library(lme4)
library(MuMIn)
library(lmerTest)
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
df1 <- read.csv("~/data/n2o_wang_oikos/n2o_tables1.csv")
summary(df1)
names(df1) <- c("ref","location","n_amb","n_elv","n_site","n2o_amb",
                "n2o_elv","co2_amb","co2_elv","dco2","duration","pft","logr",
                "weight","group","method","species","Nfer","other")
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


ggplot(df1)+geom_point(aes(x=dco2,y=logr,color=nferinfo))+geom_smooth(aes(x=dco2,y=logr),method="lm")
ggplot(df1)+geom_point(aes(x=dco2,y=logr,color=pft))+geom_smooth(aes(x=dco2,y=logr),method="lm",color="black")
ggplot(df1)+geom_point(aes(x=dco2,y=logr,color=other_exp))+geom_smooth(aes(x=dco2,y=logr),method="lm",color="black")

#ggplot(df1)+geom_point(aes(x=dco2,y=logr,color=pft))+geom_smooth(aes(x=dco2,y=logr,color=pft),method="lm")
#looks ok
summary(lm(logr~dco2,df1))

#combine into site-mean (or basing on citations, that is, site-mean and measurement year)?
unique(df1$ref)
df1_plotmean <- aggregate(df1,by=list(df1$ref,df1$nferinfo,df1$pft), FUN=mean, na.rm=TRUE)
dim(df1_plotmean)

ggplot(df1_plotmean)+geom_point(aes(x=dco2,y=logr,color=Group.2,shape=Group.3),size=3)+geom_smooth(aes(x=dco2,y=logr),method="lm")+ylab("log (N2O-e / N2O-a)")
summary(lm(logr~ dco2,df1_plotmean)) # R2 = 0.24

df1_plotmean$dco2_2 <- df1_plotmean$dco2^2
ggplot(df1_plotmean)+geom_point(aes(x=dco2,y=logr,color=Group.2,shape=Group.3),size=3)+geom_smooth(aes(x=dco2,y=logr),method="lm",formula=y~0+x)+ylab("log (N2O-e / N2O-a)")
model1 <- (lm(logr~ -1+dco2+dco2_2,df1_plotmean)) # R2 = 0.24
co2Values <- seq(0, 400, 0.1)


model_predict <- predict(model1,list(dco2=co2Values, 
                                        dco2_2=co2Values^2))
plot(df1_plotmean$dco2, df1_plotmean$logr,pch=16)
lines(co2Values, model_predict, col='blue')
df1_line <- as.data.frame(cbind(co2Values,model_predict))
ggplot(df1_plotmean)+geom_point(aes(x=dco2,y=logr,color=Group.2,shape=Group.3),size=3)+
  geom_line(data=df1_line,aes(x=co2Values,y=model_predict))+ylab("log (N2O-e / N2O-a)")


#subset(df1,nferinfo=="no")
df1 %>%
  ggplot() +
  geom_boxplot( aes(x=pft, y=sen_n2o),alpha = 0.6,outlier.shape = NA)+
  geom_boxplot(data=df1,aes(y=sen_n2o),alpha = 0.6,outlier.shape = NA)+
  geom_jitter(aes(x=pft, y=sen_n2o,color=nferinfo),size=2,width = 0.3) +
  geom_hline( linetype = 'dotted',yintercept=0.0, size=0.5)+ 
  theme_classic()+coord_flip()+theme(axis.text=element_text(size=12))+
  labs(x="", y=~paste(N2O," response to ",eCO[2])) 

df1 %>%
  ggplot( aes(x=pft, y=logr)) +
  geom_boxplot(alpha = 0.6,outlier.shape = NA)+
  geom_jitter(aes(size=1/weight)) + #weight = na * ni / (na+ni))
  geom_hline( linetype = 'dotted',yintercept=0.0, size=0.5)+ 
  theme_classic()+coord_flip()+theme(axis.text=element_text(size=12))+
  labs(x="", y=~paste(N2O," response to ",eCO[2])) 

dim(subset(df1,pft=="Grassland"))

#defined by groups
subset(df1,group=="A")$dco2
subset(df1,group=="B")$dco2
subset(df1,group=="C")$dco2
df1$group[df1$group=="A"] <- "0-150ppm"
df1$group[df1$group=="B"] <- "150-300ppm"
df1$group[df1$group=="C"] <- ">300ppm"

df1 %>%
  ggplot() +
  geom_boxplot( aes(x=group, y=sen_n2o),alpha = 0.6,outlier.shape = NA)+
  geom_boxplot(data=df1,aes(y=sen_n2o),alpha = 0.6,outlier.shape = NA)+
  geom_jitter(aes(x=group, y=sen_n2o,shape=pft,color=nferinfo),size=2,width = 0.3) +
  geom_hline( linetype = 'dotted',yintercept=0.0, size=0.5)+ 
  theme_classic()+coord_flip()+theme(axis.text=element_text(size=12))+
  labs(x="", y=~paste(N2O," response to ",eCO[2])) 

#warming only effect
df2 <- read.csv("~/data/n2o_wang_oikos/n2o_tables2.csv")
summary(df2)
names(df2) <- c("ref","location","n_amb","n_elv","n_site","n2o_amb",
                "n2o_elv","dT","duration","pft","logr",
                "weight","group","species","Nfer","other")

df2$Nfer <- as.numeric(df2$Nfer)

df2$logr <- log(df2$n2o_elv/df2$n2o_amb)

df2$nferinfo<- "no"
df2$nferinfo[df2$Nfer>0]<- "fertilized"
df2$other_exp[df2$other==""]<- "no_treatment"
df2$other_exp[df2$other!=""]<- "with_treatment"

df2$sen_n2o <- df2$logr/df2$dT
df2$nferinfo<- "no"
df2$nferinfo[df2$Nfer>0]<- "fertilized"

ggplot(df2,aes(x=dT,y=logr))+geom_point(aes(shape=pft,color=nferinfo))+
  geom_smooth(method="lm")
summary(lm(logr~dT,df2))

#QQQ: the one with very low logr looks confusing, (ref = Zhao et al., 2017), n2o decreased from 61440 to 1680??? While another sample of this ref looks ok. 
#not found in paper!!! - In Fig.6 it seems that at AS species was decreased by -15%.
#shall we remove them? 
df2 <- subset(df2,logr!=min(df2$logr,na.rm=T))

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
df3 <- read.csv("~/data/n2o_wang_oikos/n2o_tables3.csv")
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
