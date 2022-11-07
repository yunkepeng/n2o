library(tidyverse)
library(dplyr)
library(maps)
library(rworldmap)
library(tidyverse) 
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
### Liao et al. 2020 GCB: https://onlinelibrary.wiley.com/doi/full/10.1111/gcb.16365
#1a: field-based n2o
liao_field <- read.csv("~/data/n2o_liao/org/Global_change_biology_GCB-22-1572_primary_field_data.csv")
liao_field2<- liao_field[,c("Ref","Study.area","Year","Latitude","Longitude","N.addition..kg.ha.1.",
                           "Altitude..m.","N2O.fluxes..μg.N.m.2.h.1.","Total.N..g.kg.1.","NO3....mg.kg.1.",
                           "NH4...mg.kg.1.","pH","SOC..g.kg.1.","Ecosystem.types")]
names(liao_field2) <- c("ref","site","year","lat","lon","Nfer_kgha",
                        "z","n2o_ugm2h","totaln_gkg","no3_mgkg","nh4_mgkg","pH","soc_gkg","pft")
#set all value = -9999 to NA
liao_field2[liao_field2 == -9999] <- NA

summary(liao_field2)
liao_field2$method <- "pot"

#1b: pot-based n2o
liao_pot <- read.csv("~/data/n2o_liao/org/Global_change_biology_GCB-22-1572_primary_pot_data.csv")
#by checking original paper (e.g. Ref_ID = 42 or 51), mg N kg-1 is actually N addition. NO2 fluxes (μg N m-2 h-1) is actually N2O fluxes
liao_pot2<- liao_pot[,c("Ref","Study.area","Year","Latitude","Longitude","mg.N.kg.1",
                            "Altitude..m.","NO2.fluxes..μg.N.m.2.h.1.","Total.N..g.kg.1.","NO3....mg.kg.1.",
                            "NH4...mg.kg.1.","pH","SOC..g.kg.1.","Ecosystem.types")]
names(liao_pot2) <- c("ref","site","year","lat","lon","Nfer_mgkg",
                        "z","n2o_ugm2h","totaln_gkg","no3_mgkg","nh4_mgkg","pH","soc_gkg","pft")
liao_pot2[liao_pot2 == -9999] <- NA

liao_pot2$method <- "pot"

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

#csvfile <- paste("~/data/site_record.csv")
#write_csv(site_record, path = csvfile)
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

all_n2o <- dplyr::bind_rows(liao_all,cui2,cui2_fallow,hortnagl2,xuri2) 

all_n2o$pft[all_n2o$pft==" wetland"|all_n2o$pft=="wetland"] <- "wetland"
all_n2o$pft[all_n2o$pft==" forest"|all_n2o$pft=="forest"] <- "forest"
all_n2o$pft[all_n2o$pft==" grassland"|all_n2o$pft=="grassland"] <- "grassland"
all_n2o$pft[all_n2o$pft=="Temperate shortgrass steppe"|all_n2o$pft=="Temperate steppe"|all_n2o$pft=="Sagebrush steppe"] <- "steppe"
all_n2o$pft[all_n2o$pft=="Tundra"|all_n2o$pft=="Alpine tundra"] <- "tundra"
all_n2o$pft[all_n2o$pft=="desert"|all_n2o$pft=="Desert"] <- "desert"

unique(all_n2o$pft)

site_record <- unique(all_n2o[,c("lon","lat","z","pft")])
dim(site_record)
site_record %>% group_by(pft)  %>% summarise(number = n())

#map
#newmap <- getMap(resolution = "low")
#plot(newmap, xlim = c(-180, 180), ylim = c(-75, 75), asp = 1)
#points(subset(site_record,pft=="forest")$lon,subset(site_record,pft=="forest")$lat, col="red", pch=16,cex=0.5)
#points(subset(site_record,pft=="grassland")$lon,subset(site_record,pft=="grassland")$lat, col="green", pch=16,cex=0.5)
#points(subset(site_record,pft=="cropland")$lon,subset(site_record,pft=="cropland")$lat, col="purple", pch=16,cex=0.5)
#points(subset(site_record,pft=="plantation")$lon,subset(site_record,pft=="plantation")$lat, col="purple", pch=16,cex=0.5)
#points(subset(site_record,pft=="fallow")$lon,subset(site_record,pft=="fallow")$lat, col="purple", pch=16,cex=0.5)

#interpolate missing elevation - let's interpolate them by etopo
site_record_missing <- subset(site_record,is.na(z)==TRUE)
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
site_record2$z[is.na(site_record2$z)==TRUE] <- site_record2$z2[is.na(site_record2$z)==TRUE]
summary(site_record2)
#prepare for site forcing from gwr
csvfile <- paste("~/data/n2o_Yunke/forcing/siteinfo.csv")
write_csv(site_record2[,c("lon","lat","z","pft")], path = csvfile)

#now, read all predictors data
allpredictors <- read.csv("~/data/n2o_Yunke/forcing/siteinfo_predictors.csv")
#first merge to get all elevation values
allpredictors <- aggregate(allpredictors,by=list(allpredictors$lon,allpredictors$lat), FUN=mean, na.rm=TRUE) 

allpredictors <- allpredictors[,!(names(allpredictors) %in% c("Group.1","Group.2","sitename"))]
allpredictors$sitename <- paste("siteno",c(1:nrow(allpredictors)),sep="")

#remove n2o's z as will be newly combined
all_n2o <- all_n2o[,!(names(all_n2o) %in% c("z"))]

all_n2o_df <- merge(all_n2o,allpredictors,by=c("lon","lat"),all.x=TRUE)

#start analysis

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

#first, look at data-driven model with nfer
forest2$n2o_ugm2h[forest2$n2o_ugm2h<=0] <- NA
forest2$n2o_a <- log(forest2$n2o_ugm2h)
forest2$Tg_a <- forest2$Tg
forest2$PPFD_a <- log(forest2$PPFD)
forest2$vpd_a <- log(forest2$vpd)
forest2$fAPAR_a <- forest2$fAPAR
forest2$CNrt_a <- log(forest2$CNrt)
forest2$ndep_a <- log(forest2$ndep)
forest2$nfer_a <- (forest2$Nfer_kgha)
forest2$gpp_a <- log(forest2$mapped_gpp)
forest2$orgc_a <- log(forest2$ORGC)
forest2$pH_a <- (forest2$PHAQ)
forest2$site_a <- (forest2$sitename)

dim(forest2)
summary(forest2)
forest3 <- na.omit(forest2[,c("n2o_a","Tg_a","PPFD_a","vpd_a","fAPAR_a",
                   "CNrt_a","ndep_a","nfer_a","gpp_a","orgc_a","pH_a","site_a")])
dim(forest3)

stepwise(forest3,"n2o_a")[[1]]
stepwise(forest3,"n2o_a")[[2]]

summary(lmer(n2o_a~orgc_a+PPFD_a+Tg_a+fAPAR_a+nfer_a+(1|site_a),data=forest3))


forest4 <- na.omit(forest2[,c("n2o_a","Tg_a","PPFD_a","vpd_a","fAPAR_a",
                              "CNrt_a","ndep_a","gpp_a","orgc_a","pH_a","site_a")])
dim(forest4)
stepwise(forest4,"n2o_a")[[1]]
stepwise(forest4,"n2o_a")[[2]]
summary(lmer(n2o_a~orgc_a+PPFD_a+Tg_a+(1|site_a),data=forest4))
