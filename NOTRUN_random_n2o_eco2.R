aa <- na.omit(df1_all[,c("logr","log_co2","Nfer_a","PPFD_total_a")])
summary(aa$log_co2)

mod3 <- (lm(logr~orgc_a+dT,df2_all))
aa <- na.omit(df2_all[,c("logr","orgc_a","dT")])
summary(aa$dT)


df1_all
names(df1_all)

df1_all$pft[df1_all$pft=="Forest"]<- "forest"
df1_all$pft[df1_all$pft=="Grassland"]<- "grassland"
df1_all$pft[df1_all$pft=="Cropland"]<- "cropland"

a1 <- read.csv("/Users/yunpeng/data/n2o_Yunke/forcing/eCO2_LPX_annual_nfer.csv")[,c("lon","lat","z","pft","year1988")]
a2 <- read.csv("/Users/yunpeng/data/n2o_Yunke/forcing/eCO2_LPX_annual_PPFD.csv")[,c("lon","lat","z","pft","year1988")]
a3 <- read.csv("/Users/yunpeng/data/n2o_Yunke/forcing/eCO2_LPX_annual_n2o.csv")[,c("lon","lat","z","pft","year1988")]
a4 <- read.csv("/Users/yunpeng/data/n2o_Yunke/forcing/eCO2_LPX_annual_n2o.csv")[,c("lon","lat","z","pft","year2020")]

df1_a <-Reduce(function(x,y) merge(x = x, y = y, c("lon","lat","z","pft"),all.x=TRUE),
                 list(df1_all,a1,a2,a3,a4))
names(df1_a)
dff <- df1_a[,c("year1988.x","year1988.y","year1988","year2020")]
names(dff) <- c("nfer","PPFD","n2o_a","n2o_e")

dff$nfer_a <-sqrt(dff$nfer)
dff$PPFD_a <-log(dff$PPFD)
dff$logr_n2o <-log(dff$n2o_e/dff$n2o_a)
dff$logr_co2 <-log(412/350)

dff$logr_n2o_v2 <-log(dff$n2o_e/dff$n2o_a)+runif(106, min=-0.3, max=3)
dff$logr_co2_v2 <-log(477/350)

dff$logr_n2o_v3 <-log(dff$n2o_e/dff$n2o_a)+runif(106, min=-0.6, max=6)
dff$logr_co2_v3 <-log(595/350)

dff$logr_n2o_v4 <-log(dff$n2o_e/dff$n2o_a)+runif(106, min=-0.9, max=9)
dff$logr_co2_v4 <-log(750/350)

logr <- c(dff[,c("logr_n2o")],dff[,c("logr_n2o_v2")],dff[,c("logr_n2o_v3")],dff[,c("logr_n2o_v4")])
log_co2 <- c(dff[,c("logr_co2")],dff[,c("logr_co2_v2")],dff[,c("logr_co2_v3")],dff[,c("logr_co2_v4")])
PPFD_total_a <- c(dff[,c("PPFD_a")],dff[,c("PPFD_a")],dff[,c("PPFD_a")],dff[,c("PPFD_a")])
Nfer_a <- c(dff[,c("nfer_a")],dff[,c("nfer_a")],dff[,c("nfer_a")],dff[,c("nfer_a")])
final <- as.data.frame(cbind(logr,log_co2,PPFD_total_a,Nfer_a))

mod1 <- (lm(logr~log_co2+Nfer_a+PPFD_total_a,df1_all_test))

mod2 <- (lm(logr~log_co2+Nfer_a+PPFD_total_a,final))
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


g1 <- visreg_ggplot(fits_co2,"log_co2","black","red")
g1

g2 <- visreg_ggplot(fits_nfer,"Nfer_a","black","red")
g2

g3 <- visreg_ggplot(fits_ppfd,"PPFD_total_a","black","red")
g3
