library(readr)
library(ggplot2)
library(dplyr)
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

df1$log_co2 <- log(df1$co2_elv/df1$co2_amb)

ggplot(df1,aes(x=log_co2,y=logr,shape=pft,color=Nfer))+geom_point()

df1$sen_n2o <- df1$logr/df1$log_co2

df1$nferinfo<- "no"
df1$nferinfo[df1$Nfer>0]<- "fertilized"
df1$other_exp[df1$other==""]<- "no_treatment"
df1$other_exp[df1$other!=""]<- "with_treatment"

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

ggplot(df2,aes(x=dT,y=logr))+geom_point(aes(shape=pft,color=Nfer))+
  geom_smooth(method="lm")
summary(lm(logr~dT,df2))

df2$sen_n2o <- df2$logr/df2$dT

df2$nferinfo<- "no"
df2$nferinfo[df2$Nfer>0]<- "fertilized"

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

df3 %>%
  ggplot() +
  geom_boxplot( aes(x=pft, y=logr),alpha = 0.6,outlier.shape = NA)+
  geom_boxplot(data=df3,aes(y=logr),alpha = 0.6,outlier.shape = NA)+
  geom_jitter(aes(x=pft, y=logr,color=nferinfo),size=2,width = 0.3) +
  geom_hline( linetype = 'dotted',yintercept=0.0, size=0.5)+ 
  theme_classic()+coord_flip()+theme(axis.text=element_text(size=12))+
  labs(x="", y=~paste(N2O," response to ",warming_and_co2)) 
