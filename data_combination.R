#library(tidyverse)
library(dplyr)
library(maps)
library(rworldmap)
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
devtools::load_all("/Users/yunpeng/yunkepeng/latest_packages/rbeni/")
devtools::load_all("/Users/yunpeng/yunkepeng/latest_packages/ingestr/")
#library(rbeni)
library(raster)
library(maps)
library(rworldmap)
library(cowplot)
library(ncdf4)
library(scales)
library(spgwr)
source("/Users/yunpeng/yunkepeng/CNuptake_MS/R/stepwise.R")
library(lmerTest)
library(lme4)
library("PerformanceAnalytics")
library(MuMIn)
library(car)
library(visreg)
library(readr)
library(matrixStats)
library(car)
### Liao et al. 2020 GCB: https://onlinelibrary.wiley.com/doi/full/10.1111/gcb.16365
#1a: field-based n2o
liao_field <- read.csv("~/data/n2o_liao/org/Global_change_biology_GCB-22-1572_primary_field_data.csv")
liao_field2<- liao_field[,c("Ref","Study.area","Year","Latitude","Longitude","N.addition..kg.ha.1.",
                            "Altitude..m.","N2O.fluxes..μg.N.m.2.h.1.","Total.N..g.kg.1.","NO3....mg.kg.1.",
                            "NH4...mg.kg.1.","pH","SOC..g.kg.1.","Ecosystem.types","Soil.moisture....",
                            "AOA..copy.numbers.g.1.dry.soil.","AOB..copy.numbers.g.1.dry.soil.","nirS...copy.numbers.g.1.dry.soil.",
                            "nirK...copy.numbers.g.1.dry.soil.","nosZ...copy.numbers.g.1.dry.soil.")]
names(liao_field2) <- c("ref","site","year","lat","lon","Nfer_kgha",
                        "z","n2o_ugm2h","totaln_gkg","no3_mgkg","nh4_mgkg","pH","soc_gkg","pft","obs_moisture",
                        "AOA","AOB","nirS","nirK","nosZ")
#set all value = -9999 to NA
liao_field2[liao_field2 == -9999] <- NA

summary(liao_field2)
liao_field2$obs_moisture<- liao_field2$obs_moisture/100
liao_field2$method <- "field"
#correct start_yr
unique(liao_field2$year)
liao_field2$year[liao_field2$year==" 1996-1998"] <- "1996-1998"
liao_field2$year[liao_field2$year=="200-2002"] <- "2000-2002" #after checked reference
liao_field2$start_yr <- substr(liao_field2$year, 1, 4)
unique(liao_field2$start_yr) #looks ok
#select last 4 character
liao_field2$end_yr <- substr(liao_field2$year, nchar(liao_field2$year)-3, nchar(liao_field2$year))
unique(liao_field2$end_yr) #looks ok
liao_field2$start_yr <-as.numeric(liao_field2$start_yr)
liao_field2$end_yr <-as.numeric(liao_field2$end_yr)
liao_field2$end_yr[liao_field2$end_yr==1014] <- 2014
summary(liao_field2)
summary(liao_field2$end_yr -liao_field2$start_yr)

#1b: pot-based n2o
liao_pot <- read.csv("~/data/n2o_liao/org/Global_change_biology_GCB-22-1572_primary_pot_data.csv")
#by checking original paper (e.g. Ref_ID = 42 or 51), mg N kg-1 is actually N addition. NO2 fluxes (μg N m-2 h-1) is actually N2O fluxes
liao_pot2<- liao_pot[,c("Ref","Study.area","Year","Latitude","Longitude","mg.N.kg.1",
                        "Altitude..m.","NO2.fluxes..μg.N.m.2.h.1.","Total.N..g.kg.1.","NO3....mg.kg.1.",
                        "NH4...mg.kg.1.","pH","SOC..g.kg.1.","Ecosystem.types","Soil.moisture....",
                        "AOA..copy.numbers.g.1.dry.soil.","AOB..copy.numbers.g.1.dry.soil.","nirS...copy.numbers.g.1.dry.soil.",
                        "nirK...copy.numbers.g.1.dry.soil.","nosZ...copy.numbers.g.1.dry.soil.")]
names(liao_pot2) <- c("ref","site","year","lat","lon","Nfer_mgkg",
                      "z","n2o_ugm2h","totaln_gkg","no3_mgkg","nh4_mgkg","pH","soc_gkg","pft","obs_moisture",
                      "AOA","AOB","nirS","nirK","nosZ")
liao_pot2[liao_pot2 == -9999] <- NA

liao_pot2$method <- "pot"
liao_pot2$obs_moisture <- liao_pot2$obs_moisture/100

#correct start_yr
unique(liao_pot2$year)
liao_pot2$start_yr <- substr(liao_pot2$year, 1, 4)
unique(liao_pot2$start_yr) #looks ok
#select last 4 character
liao_pot2$end_yr <- substr(liao_pot2$year, nchar(liao_pot2$year)-3, nchar(liao_pot2$year))
unique(liao_pot2$end_yr) #looks ok
liao_pot2$start_yr <-as.numeric(liao_pot2$start_yr)
liao_pot2$end_yr <-as.numeric(liao_pot2$end_yr)
summary(liao_pot2)
summary(liao_pot2$end_yr -liao_pot2$start_yr)

#combine them
liao_all <- dplyr::bind_rows(liao_field2,liao_pot2)

summary(liao_all$n2o_ugm2h)
liao_all$file <- "Liao et al. gcb"

#2a. Cui et al. Nature Food (without fallow)
cui <- read.csv("~/data/n2o_cui_naturefood/43016_2021_384_MOESM3_ESM.csv")
summary(cui)
#QQQ: how could all sites measured pH, does it from soilGrids? Not found any info in paper.
#QQQ: back ground flux??
#QQQ: after n2o conversion (see below) - the values are not consistent with Liao et al. (basing on the same paper) - should be resolved later.
#QQQ: check replicates to Cui et al. then
#Nrate is kg_ha - no need to change

cui$n2o <- cui$E/cui$Duration #E was at kg N2O-N ha-1, duration is in days
#the unit is now kg/ha/day
cui$n2o <- cui$n2o*1000000000/10000/24 #convert from kg/ha/day to ug/m2/h
summary(cui$n2o)
cui2 <- cui[,c("Reference","Latitude","Longitude","Start_year","End_year","Fertilizers",
               "Crop.type","Nrate","n2o","Tillage","Irrigation","Site")]

names(cui2) <- c("ref","lat","lon","start_yr","end_yr","fertilizers",
                 "crop","Nfer_kgha","n2o_ugm2h","tillage","irrigation","site")
cui2$method <- "field"
cui2$file <- "cui et al. nature food"
cui2$pft <- "cropland"

#2b Cui et al. Nature Food (with fallow)
cui_fallow <- read.csv("~/data/n2o_cui_naturefood/43016_2021_384_MOESM3_ESM_fallow.csv")

cui_fallow$n2o <- cui_fallow$E/cui_fallow$Duration_new #E was at kg N2O-N ha-1, duration is in days
#checked one paper (e.g. Scheer et al. 2016 Soil Research) as an example - this conversion should be correct
#the unit is now kg/ha/day
cui_fallow$n2o <- cui_fallow$n2o*1000000000/10000/24 #convert from kg/ha/day to ug/m2/h
summary(cui_fallow$n2o)
cui2_fallow <- cui_fallow[,c("Reference","Latitude","Longitude","Start_year","End_year","Fertilizers",
                             "Crop.type","Nrate","n2o","Tillage","Irrigation","Site")]
cui2_fallow$Site <- paste(cui2_fallow$Site,"_fallow",sep="")

names(cui2_fallow) <- c("ref","lat","lon","start_yr","end_yr","fertilizers",
                        "crop","Nfer_kgha","n2o_ugm2h","tillage","irrigation","site")

cui2_fallow$method <- "field_fallow"
cui2_fallow$file <- "cui et al. nature food"
#convert some years
unique(cui2_fallow$start_yr)
unique(cui2_fallow$end_yr)
cui2_fallow$start_yr[cui2_fallow$start_yr=="2011/2012"] <- 2011
cui2_fallow$start_yr[cui2_fallow$start_yr=="2011/2013"] <- 2011
cui2_fallow$start_yr[cui2_fallow$start_yr=="2011/2014"] <- 2011
cui2_fallow$start_yr[cui2_fallow$start_yr=="2011/2015"] <- 2011
cui2_fallow$end_yr[cui2_fallow$end_yr=="2012/2013"] <- 2013
cui2_fallow$start_yr <- as.numeric(cui2_fallow$start_yr)
cui2_fallow$end_yr <- as.numeric(cui2_fallow$end_yr)
summary(cui2_fallow)
cui2_fallow$pft <- "fallow"

#site distribution
#newmap <- getMap(resolution = "low")
#plot(newmap, xlim = c(-180, 180), ylim = c(-75, 75), asp = 1)
#points(site_record$lon,site_record$lat, col="red", pch=16,cex=1)

#Xu-Ri not yet finished - since it is highly replicated to many sites

#3.hortnagl et al. gcb - 9 grassland sites
hortnagl <- read.csv("~/data/n2o_hortnagl_gcb/n2o_hortnagl_gcb.csv")
names(hortnagl)
hortnagl$n2o <- hortnagl$cumulative_n2o_kgha/hortnagl$duration_days
#the unit is now kg/ha/day
hortnagl$n2o_ugm2h <- hortnagl$n2o*1000000000/10000/24
hortnagl$Nfer_kgha <- hortnagl$organic_n_input_kgha+hortnagl$mineral_n_input_kgha

hortnagl2 <- hortnagl[,c("site","start_yr","end_yr","lon","lat",
                         "z","ref","n2o_ugm2h","Nfer_kgha")]
hortnagl2$file <- "hortnagl et al. 2018 gcb"
hortnagl2$pft <- "grassland"
hortnagl2$method <- "field"

#add Xu-Ri
xuri <- read.csv("~/data/n2o_xuri/xuri_newphy.csv")
names(xuri) <- c("no","lon","lat","pft","year","n2o_kghayr","location","ref")
xuri$n2o_ugm2h <- xuri$n2o_kghayr*1000000000/10000/(365*24)
#correct pft
xuri$pft[xuri$pft=="Temperate grassland"|xuri$pft=="Tropical grassland"|
           xuri$pft=="Alpine grassland"|xuri$pft=="Subarctic grassland"]<-"grassland"

xuri$pft[xuri$pft=="Tropical rain forest"|xuri$pft=="Tropical moist forest"|
           xuri$pft=="Tropical forest"|xuri$pft=="Tropical Forest"|
           xuri$pft=="Subtropical moist forest"|xuri$pft=="Montane rain forest"|
           xuri$pft=="Temperate forest"|xuri$pft=="Boreal forest"|
           xuri$pft=="Temperate pine forest"] <- "forest"
xuri2 <- xuri[,c("lon","lat","pft","year","n2o_ugm2h","ref")]
xuri2$file <- "Xu-Ri et al. (2012) New Phytol"
xuri2$method <- "field"

#correct years
unique(xuri2$year)
xuri2$start_yr <- substr(xuri2$year, 1, 4)
unique(xuri2$start_yr) #looks ok
#select last 4 character
xuri2$end_yr <- substr(xuri2$year, nchar(xuri2$year)-3, nchar(xuri2$year))
unique(xuri2$end_yr) #looks ok
xuri2$start_yr <-as.numeric(xuri2$start_yr)
xuri2$end_yr <-as.numeric(xuri2$end_yr)
summary(xuri2)
summary(xuri2$end_yr -xuri2$start_yr)

all_n2o <- dplyr::bind_rows(liao_all,cui2,cui2_fallow,hortnagl2,xuri2)

all_n2o$pft[all_n2o$pft==" wetland"|all_n2o$pft=="wetland"] <- "wetland"
all_n2o$pft[all_n2o$pft==" forest"|all_n2o$pft=="forest"] <- "forest"
all_n2o$pft[all_n2o$pft==" grassland"|all_n2o$pft=="grassland"] <- "grassland"
all_n2o$pft[all_n2o$pft=="Temperate shortgrass steppe"|all_n2o$pft=="Temperate steppe"|all_n2o$pft=="Sagebrush steppe"] <- "steppe"
all_n2o$pft[all_n2o$pft=="Tundra"|all_n2o$pft=="Alpine tundra"] <- "tundra"
all_n2o$pft[all_n2o$pft=="desert"|all_n2o$pft=="Desert"] <- "desert"

unique(all_n2o$pft)

site_record <- unique(all_n2o[,c("lon","lat","z")])
dim(site_record)

#interpolate missing elevation - let's interpolate them by etopo
site_record_missing <- site_record
site_record_missing2 <- (unique(site_record_missing[,c("lon","lat")]))
site_record_missing2$sitename <- paste("sitename",c(1:nrow(site_record_missing2)),sep="")
df_etopo <- ingest(
  site_record_missing2,
  source = "etopo1",
  dir = "~/data/etopo/"
)
site_record_missing2$z2 <- as.numeric(as.data.frame(df_etopo$data))
summary(site_record_missing2$z2)
site_record_missing2$z2[site_record_missing2$z2<0] <- 0

#now, interpolate those NA elevation by etopo
site_record2 <- merge(site_record,site_record_missing2,by=c("lon","lat"),all.x=TRUE)
plot(site_record2$z~site_record2$z2) #check: or data that has z - it looks ok.
site_record2$z[is.na(site_record2$z)==TRUE] <- site_record2$z2[is.na(site_record2$z)==TRUE]
summary(site_record2)
dim(site_record2)
#prepare for site forcing from gwr
#aggregate basing on lon and lat
site_record2 <- as.data.frame(site_record2 %>% group_by(lon,lat)  %>% summarise(z = mean(z)))
dim(site_record2)
dim(unique(site_record2[,c("lon","lat")]))
summary(site_record2)
csvfile <- paste("~/data/n2o_Yunke/forcing/siteinfo.csv")
write_csv(site_record2[,c("lon","lat","z")], path = csvfile)

#first merge to get all elevation values
site_record2$sitename <- paste("siteno",c(1:nrow(site_record2)),sep="")
all_n2o <- all_n2o[,!(names(all_n2o) %in% c("z"))]
all_n2o_z <- merge(all_n2o,site_record2,by=c("lon","lat"),all.x=TRUE)

#create lon, lat, z, start_yr, end_yr
site_ingest <- unique(all_n2o_z[,c("lon","lat","z","start_yr","end_yr")])
site_ingest$start_yr[is.na(site_ingest$start_yr)==T] <- 1991
site_ingest$end_yr[is.na(site_ingest$end_yr)==T] <- 2010
summary(site_ingest)
#cru and wfdei together: data available from 1979-2016
subset(site_ingest,start_yr<1980)
#convert 1978 to 1979-1988
site_ingest$end_yr[site_ingest$start_yr<1980]<-1989
site_ingest$start_yr[site_ingest$start_yr<1980]<-1980

#convert >2016 to 1997-2016
subset(site_ingest,start_yr>2016|end_yr>2016)
site_ingest$info[site_ingest$start_yr>2016|site_ingest$end_yr>2016] <- "higher_year"
site_ingest$start_yr[site_ingest$info=="higher_year"]<-2007
site_ingest$end_yr[site_ingest$info=="higher_year"]<-2016

summary(site_ingest$end_yr- site_ingest$start_yr)
summary(site_ingest)
site_ingest_df<- site_ingest[,c("lon","lat","z","start_yr","end_yr")]
names(site_ingest_df) <- c("lon","lat","elv","year_start","year_end")
site_ingest_df$sitename <- paste("ingest",c(1:nrow(site_ingest_df)),sep="")
#csvfile <- paste("~/data/n2o_Yunke/forcing/siteinfo_measurementyear.csv")
#write_csv(site_ingest_df, path = csvfile)

#convert n2o original data's year, too.
all_n2o_z$start_yr[is.na(all_n2o_z$start_yr)==T] <- 1991
all_n2o_z$end_yr[is.na(all_n2o_z$end_yr)==T] <- 2010
all_n2o_z$end_yr[all_n2o_z$start_yr<1980]<-1989
all_n2o_z$start_yr[all_n2o_z$start_yr<1980]<-1980
all_n2o_z$info[all_n2o_z$start_yr>2016|all_n2o_z$end_yr>2016] <- "higher_year"
all_n2o_z$start_yr[all_n2o_z$info=="higher_year"]<-2007
all_n2o_z$end_yr[all_n2o_z$info=="higher_year"]<-2016
summary(all_n2o_z$start_yr)
summary(all_n2o_z$end_yr)

#add gwr forced climates
gwr_climate <- read.csv("~/data/n2o_Yunke/forcing/siteinfo_measurementyear_gwrclimate.csv")
names(gwr_climate) <- c("lon","lat","z","start_yr","end_yr","alpha_sites","PPFD_sites","Tg_sites","vpd_sites","PPFD_total_sites")
all_n2o_z2 <- merge(all_n2o_z,gwr_climate,by=c("lon","lat","z","start_yr","end_yr"),all.x=TRUE)
names(all_n2o_z2)
summary(all_n2o_z2)

#now, read all predictors data
allpredictors <- read.csv("~/data/n2o_Yunke/forcing/siteinfo_predictors.csv")
allpredictors <- allpredictors[,!(names(allpredictors) %in% c("sitename"))]
all_n2o_df <- merge(all_n2o_z2,allpredictors,by=c("lon","lat","z"),all.x=TRUE)

#read soil moisture and alpha
moisture <- read.csv("~/data/n2o_Yunke/forcing/siteinfo_measurementyear_moisture_alpha.csv")
moisture<- moisture[,c("lon","lat","elv","year_start","year_end","soil_moisture","alpha")]
names(moisture) <- c("lon","lat","z","start_yr","end_yr","moisture_splash","alpha_splash")
all_n2o_df <- merge(all_n2o_df,moisture,by=c("lon","lat","z","start_yr","end_yr"),all.x=TRUE)
summary(all_n2o_df)
#QQQ: check NA values in Tg_sites and moisture_splash -> they might record wrong coordinates in literatures!

#add fapar3g from 1/12 resolution (monthly max and mean)
fapar3g_df_zhu <- read.csv("~/data/n2o_Yunke/forcing/siteinfo_measurementyear_fapar3g_zhu.csv")

fapar3g_df_zhu <- fapar3g_df_zhu %>% rename("start_yr" = "year_start",
                                            "end_yr" = "year_end")

fapar3g_df_zhu <- fapar3g_df_zhu %>% mutate(min_fapar = coalesce(min_fapar_nfocal0,min_fapar_nfocal1,min_fapar_nfocal2)) %>%
  mutate(mean_fapar = coalesce(mean_fapar_nfocal0,mean_fapar_nfocal1,mean_fapar_nfocal2)) %>%
  mutate(max_fapar = coalesce(max_fapar_nfocal0,max_fapar_nfocal1,max_fapar_nfocal2)) %>%
  mutate(min_gfapar = coalesce(min_fapar_growing_nfocal0,min_fapar_growing_nfocal1,min_fapar_growing_nfocal2)) %>%
  mutate(mean_gfapar = coalesce(mean_fapar_growing_nfocal0,mean_fapar_growing_nfocal1,mean_fapar_growing_nfocal2))%>%
  mutate(max_gfapar = coalesce(max_fapar_growing_nfocal0,max_fapar_growing_nfocal1,max_fapar_growing_nfocal2))%>%
  mutate(min_fapar_35yrs = coalesce(min_fapar_35yrs_nfocal0,min_fapar_35yrs_nfocal1,min_fapar_35yrs_nfocal2))%>%
  mutate(mean_fapar_35yrs = coalesce(mean_fapar_35yrs_nfocal0,mean_fapar_35yrs_nfocal1,mean_fapar_35yrs_nfocal2))%>%
  mutate(max_fapar_35yrs = coalesce(max_fapar_35yrs_nfocal0,max_fapar_35yrs_nfocal1,max_fapar_35yrs_nfocal2))

fapar3g_df_zhu2 <- fapar3g_df_zhu[,c("lon","lat","z","start_yr","end_yr","min_fapar","mean_fapar","max_fapar",
                                     "min_gfapar","mean_gfapar","max_gfapar","min_fapar_35yrs","mean_fapar_35yrs","max_fapar_35yrs")]

summary(fapar3g_df_zhu2)
fapar3g_df_zhu2[is.na(fapar3g_df_zhu2)] = 0
summary(fapar3g_df_zhu2)

#all from 0 to 1
all_n2o_df <- merge(all_n2o_df,fapar3g_df_zhu2,
                    by=c("lon","lat","z","start_yr","end_yr"),all.x=TRUE)
summary(all_n2o_df)
#values are all consistent 
all_n2o_df$max_mean_fapar <- all_n2o_df$max_fapar-all_n2o_df$mean_fapar
all_n2o_df$max_min_fapar <- all_n2o_df$max_fapar-all_n2o_df$min_fapar
all_n2o_df$max_mean_gfapar <- all_n2o_df$max_gfapar-all_n2o_df$mean_gfapar
all_n2o_df$max_min_gfapar <- all_n2o_df$max_gfapar-all_n2o_df$min_gfapar
all_n2o_df$max_mean_fapar_35yrs <- all_n2o_df$max_fapar_35yrs-all_n2o_df$mean_fapar_35yrs
all_n2o_df$max_min_fapar_35yrs <- all_n2o_df$max_fapar_35yrs-all_n2o_df$min_fapar_35yrs

all_n2o_df$n2o_ugm2h[all_n2o_df$n2o_ugm2h<=0] <- NA
all_n2o_df$n2o_a <- log(all_n2o_df$n2o_ugm2h)
all_n2o_df$Tg_a <- all_n2o_df$Tg
all_n2o_df$PPFD_a <- log(all_n2o_df$PPFD)
all_n2o_df$PPFD_total_a <- log(all_n2o_df$PPFD_total)

all_n2o_df$vpd_a <- log(all_n2o_df$vpd)
all_n2o_df$Tg_sites_a <- all_n2o_df$Tg_sites
all_n2o_df$PPFD_sites_a <- log(all_n2o_df$PPFD_sites)
all_n2o_df$vpd_sites_a <- log(all_n2o_df$vpd_sites)
all_n2o_df$PPFD_total_sites_a <- log(all_n2o_df$PPFD_total_sites)
all_n2o_df$fAPAR_a <- all_n2o_df$fAPAR
all_n2o_df$CNrt_a <- log(all_n2o_df$CNrt)
all_n2o_df$ndep_a <- log(all_n2o_df$ndep)
all_n2o_df$nfer_a <- (all_n2o_df$Nfer_kgha)
all_n2o_df$gpp_a <- log(all_n2o_df$mapped_gpp)
all_n2o_df$orgc_a <- log(all_n2o_df$ORGC)
all_n2o_df$pH_a <- (all_n2o_df$PHAQ)
all_n2o_df$alpha_a <- (all_n2o_df$alpha)
all_n2o_df$site_a <- (all_n2o_df$sitename)
all_n2o_df$totaln_a <- log(all_n2o_df$TOTN)

all_n2o_df$obs_orgc_a <- log(all_n2o_df$soc_gkg)
all_n2o_df$obs_pH_a <- log(all_n2o_df$pH)
all_n2o_df$obs_totalN_a <- log(all_n2o_df$totaln_gkg)
all_n2o_df$mineral_N <- (all_n2o_df$nh4_mgkg+all_n2o_df$no3_mgkg)
all_n2o_df$obs_mineral_N_a <- log(all_n2o_df$mineral_N)

all_n2o_df$sqrt_Nfer_kgha<- sqrt(all_n2o_df$Nfer_kgha)

#plot nfer tilisation
ggplot(subset(all_n2o_df,pft=="cropland"|pft=="forest"|pft=="grassland"|pft=="fallow"),
       aes(x=Nfer_kgha,y=n2o_a,color=pft))+geom_point()
summary(lm(Nfer_kgha~n2o_a,data=subset(all_n2o_df,pft=="cropland"|pft=="forest"|pft=="grassland"|pft=="fallow")))
forest <- subset(all_n2o_df,pft=="forest")
dim(forest)
forest %>% group_by(file)  %>% summarise(number = n())
forest %>% group_by(method)  %>% summarise(number = n())

#check duplicates
unique_coord <- unique(forest[,c("lon","lat","file","ref")])
forest$rep<-NA
forest$rep[forest$lon==-84.00] <- "rep"
forest$rep[forest$lon==-72.00 & forest$lat==42.50] <- "rep"
forest$rep[forest$lon==-66.00] <- "rep"
forest$rep[forest$lon==-63.00] <- "rep"
forest$rep[forest$lon==-62.50 & forest$lat==-10.50] <- "rep"
forest$rep[forest$lon==-55.00] <- "rep"
forest$rep[forest$lon==8.00&forest$lat==47.00] <- "rep"
forest$rep[forest$lon==11.00] <- "rep"
forest$rep[forest$lon==145.50] <- "rep"

forest2 <- subset(forest,is.na(rep)==T)
dim(forest2)

#check
unique(subset(forest2,is.na(Nfer_kgha)==T)$file)
forest2$sqrt_Nfer_kgha[is.na(forest2$sqrt_Nfer_kgha)==T & forest2$file=="Xu-Ri et al. (2012) New Phytol"] <- 0

forest2_field <- subset(forest2,method=="field")

test1 <- (na.omit(forest2_field[,c("site_a","n2o_a","orgc_a","CNrt_a","ndep_a",
                                   "obs_moisture","Tg_a",
                                   "PPFD_total_a","PPFD_a",
                                   "min_fapar","max_fapar","mean_fapar","max_min_fapar","max_mean_fapar")]))
dim(test1)
stepwise(test1,"n2o_a")[[1]]

mod1 <- (lmer(n2o_a~Tg_a+obs_moisture+(1|site_a),data=forest2_field))
summary(mod1)
r.squaredGLMM(mod1)
n1b <- visreg(mod1,"obs_moisture",type="contrast")
n1c <- visreg(mod1,"Tg_a",type="contrast")

#Nfer is less good
summary(lmer(n2o_a~sqrt_Nfer_kgha+(1|site_a),data=forest2_field))
r.squaredGLMM(lmer(n2o_a~sqrt_Nfer_kgha+(1|site_a),data=forest2_field))


#grassland - check rep
unique(all_n2o_df$pft)
grassland <- subset(all_n2o_df,pft=="grassland"|pft=="Tropical pastures"|pft=="Savanna")
unique_coord <- unique(grassland[,c("lon","lat","file","ref")])
grassland$rep<-NA
grassland$rep[grassland$lon==172.50 & grassland$lat==-43.50] <- "rep"
grassland$rep[grassland$lon==116.00 & grassland$lat==43.50] <- "rep"
grassland$rep[grassland$lon==116.50 & grassland$lat==43.50] <- "rep"
grassland$rep[grassland$lon==8.54 & grassland$lat==47.12] <- "rep"
grassland$rep[grassland$lon==10.00 & grassland$lat==47.50] <- "rep"
grassland$rep[grassland$lon==8.50 & grassland$lat==50.50] <- "rep"

grassland2 <- subset(grassland,is.na(rep)==T)
dim(grassland2)

#check
grassland2_field <- subset(grassland2,method=="field")
dim(grassland2_field)
summary(grassland2_field$Nfer_kgha)
unique(subset(grassland2_field,is.na(Nfer_kgha)==T)$file)
grassland2_field$sqrt_Nfer_kgha[is.na(grassland2_field$sqrt_Nfer_kgha)==T & grassland2_field$file=="Xu-Ri et al. (2012) New Phytol"] <- 0

test2 <- (na.omit(grassland2_field[,c("site_a","n2o_a","sqrt_Nfer_kgha","orgc_a","CNrt_a","ndep_a",
                                      "Tg_a",
                                      "PPFD_total_a","PPFD_a",
                                      "min_fapar","max_fapar","mean_fapar","max_min_fapar","max_mean_fapar")]))
stepwise(test2,"n2o_a")[[1]]
stepwise(test2,"n2o_a")[[2]]

mod2<- (lmer(n2o_a~sqrt_Nfer_kgha+min_fapar+(1|site_a),data=test2))
summary(mod2)
r.squaredGLMM(mod2)
n2a <- visreg(mod2,"sqrt_Nfer_kgha",type="contrast")
n2a <- visreg(mod2,"min_fapar",type="contrast")

#finally cropland
cropland <- subset(all_n2o_df,pft=="cropland"|pft=="plantation"|pft=="fallow"|pft=="bare")
#unique_coord_n2o <- as.data.frame(cropland %>% group_by(lon,lat,file)  %>% summarise(n2o = mean(n2o_ugm2h,na.rm=T)))
#merged_coord <- merge(subset(unique_coord_n2o,file=="cui et al. nature food"),
#                      subset(unique_coord_n2o,file=="Liao et al. gcb"),
#                      by=c("lon","lat"),all.x=TRUE,all.y=TRUE)
#QQQ: why cui and liao are so differ?
#at least, this plots from cui should be removed
#repeated_column <- subset(merged_coord,is.na(file.x)==F & is.na(file.y)==F)[,c("lon","lat","file.x")]
#names(repeated_column) <- c("lon","lat","file")
#repeated_column$rep <- "rep"
#cropland2 <- merge(cropland,repeated_column,
#                      by=c("lon","lat","file"),all.x=TRUE)
#cropland2$rep
#cropland2 <- subset(cropland2,is.na(rep)==TRUE)
#unique(cropland2$file)
#cropland2_liao <- subset(cropland2,file=="Liao et al. gcb" & method=="field")
#dim(cropland2_liao)
# firstly, using Liao's data only: cropland2_liao
#test3 <- (na.omit(cropland2_liao[,c("site_a","n2o_a","orgc_a","sqrt_Nfer_kgha",
#                                      "ndep_a",
#                                      "Tg_a","vpd_a","CNrt_a","gpp_a")]))
cropland2_liao <- subset(cropland,file=="Liao et al. gcb")
dim(cropland2_liao)
#obs. moisture is completely non-significant, so removed here to produce more sites
#remove soil cn and ppfd_a here
test3 <- (na.omit(cropland2_liao[,c("site_a","n2o_a","sqrt_Nfer_kgha","orgc_a","ndep_a",
                                    "vpd_a","Tg_a",
                                    "PPFD_total_a",
                                    "min_fapar","max_fapar","mean_fapar")]))

dim(test3)
stepwise(test3,"n2o_a")[[1]]
stepwise(test3,"n2o_a")[[2]]

#directly using existing models
mod3 <- ((lmer(n2o_a~orgc_a+sqrt_Nfer_kgha+vpd_a+Tg_a+PPFD_total_a+max_fapar+min_fapar+(1|site_a),data=cropland2_liao)))
r.squaredGLMM((lmer(n2o_a~orgc_a+sqrt_Nfer_kgha+vpd_a+Tg_a+PPFD_total_a+max_fapar+min_fapar+(1|site_a),data=cropland2_liao)))
summary(mod3)

visreg(mod3,"sqrt_Nfer_kgha",type="contrast")
visreg(mod3,"orgc_a",type="contrast")
visreg(mod3,"PPFD_total_a",type="contrast")
visreg(mod3,"vpd_a",type="contrast")
visreg(mod3,"Tg_a",type="contrast")
visreg(mod3,"max_fapar",type="contrast")
visreg(mod3,"min_fapar",type="contrast")
r.squaredGLMM(mod3)
summary(mod3)

