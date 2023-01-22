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
devtools::load_all("/Users/yunpeng/yunkepeng/latest_packages/rbeni/") 

### Liao et al. 2020 GCB: https://onlinelibrary.wiley.com/doi/full/10.1111/gcb.16365
#1a: field-based n2o
liao_field <- read.csv("~/data/n2o_liao/org/Global_change_biology_GCB-22-1572_primary_field_data.csv")
liao_field2<- liao_field[,c("Ref","Study.area","Year","Latitude","Longitude","N.addition..kg.ha.1.",
                            "Altitude..m.","N2O.fluxes..μg.N.m.2.h.1.","Total.N..g.kg.1.","NO3....mg.kg.1.",
                            "NH4...mg.kg.1.","pH","SOC..g.kg.1.","Ecosystem.types","Soil.moisture....",
                            "AOA..copy.numbers.g.1.dry.soil.","AOB..copy.numbers.g.1.dry.soil.","nirS...copy.numbers.g.1.dry.soil.",
                            "nirK...copy.numbers.g.1.dry.soil.","nosZ...copy.numbers.g.1.dry.soil.",
                            "Bulk.density..g.cm.3.")]
names(liao_field2) <- c("ref","site","year","lat","lon","Nfer_kgha",
                        "z","n2o_ugm2h","totaln_gkg","no3_mgkg","nh4_mgkg","pH","soc_gkg","pft","obs_moisture",
                        "AOA","AOB","nirS","nirK","nosZ","bulk_density")
#set all value = -9999 to NA
liao_field2[liao_field2 == -9999] <- NA

summary(liao_field2)
#three papers showing wrong data - the value is g/g in original paper and should be converted to % by *100, but Liao et al. failed to do that.
#here we *100 and then multiply with soil bulk density
liao_field2$bulk_density[liao_field2$ref=="Biological invasion alters regional nitrogen-oxide emissions from tropical rainforests"] <- 1
  
liao_field2$obs_moisture[liao_field2$ref=="Biological invasion alters regional nitrogen-oxide emissions from tropical rainforests"]<- 
  liao_field2$obs_moisture[liao_field2$ref=="Biological invasion alters regional nitrogen-oxide emissions from tropical rainforests"]*100*liao_field2$bulk_density[liao_field2$ref=="Biological invasion alters regional nitrogen-oxide emissions from tropical rainforests"]

liao_field2$obs_moisture[liao_field2$ref=="Nitrous oxide flux dynamics of grassland undergoing afforestation"]<-
  liao_field2$obs_moisture[liao_field2$ref=="Nitrous oxide flux dynamics of grassland undergoing afforestation"]*100*liao_field2$bulk_density[liao_field2$ref=="Nitrous oxide flux dynamics of grassland undergoing afforestation"]

liao_field2$obs_moisture[liao_field2$ref=="Relationship between N2O and NO emission potentials and soil properties in Japanese forest soils"] <- 
  liao_field2$obs_moisture[liao_field2$ref=="Relationship between N2O and NO emission potentials and soil properties in Japanese forest soils"]*100*liao_field2$bulk_density[liao_field2$ref=="Relationship between N2O and NO emission potentials and soil properties in Japanese forest soils"]

#for more details see /Users/yunpeng/data/n2o_liao/org/moisture_check.xlsx
#I think now it should all be volumetric SWC (a few are converted from mass to volume based SWC - if they lacked bulk density they just converted by assuming bulk density as 1)

liao_field2$obs_moisture<- liao_field2$obs_moisture/100
summary(liao_field2$obs_moisture)

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
summary(lm(site_record2$z~site_record2$z2)) #check: or data that has z - it looks ok.

site_record2$z[is.na(site_record2$z)==TRUE] <- site_record2$z2[is.na(site_record2$z)==TRUE]
summary(site_record2)
dim(site_record2)
#prepare for site forcing from gwr
#aggregate basing on lon and lat
site_record2 <- as.data.frame(site_record2 %>% group_by(lon,lat)  %>% summarise(z = mean(z)))
dim(site_record2)
dim(unique(site_record2[,c("lon","lat")]))
summary(site_record2)
#csvfile <- paste("~/data/n2o_Yunke/forcing/siteinfo.csv")
#write_csv(site_record2[,c("lon","lat","z")], path = csvfile)

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
summary(site_ingest) # easrliest year is 1978, latest year is 2020
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
#dim(test1)
stepwise(test1,"n2o_a")[[1]]
stepwise(test1,"n2o_a")[[2]]
#temperature and moisture were selected first - then significant
#then no matter how select other factors - these are wrong
mod1 <- (lmer(n2o_a~Tg_a+obs_moisture+(1|site_a),data=forest2_field))
summary(mod1)
r.squaredGLMM(mod1)

#applied in LPX model
output_df_forest <- na.omit(forest2_field[,c("lon","lat","z","start_yr","end_yr",
                                             "n2o_a","Tg_a","obs_moisture")])
LPX_forest_sitemean <- unique(output_df_forest[,c("lon","lat")])
dim(LPX_forest_sitemean)

lpx_forest_n2o <- subset(read.csv("~/data/n2o_Yunke/forcing/LPX_annual_n2o.csv"),pft=="forest")
lpx_forest_n2o <- (dplyr::select(lpx_forest_n2o, -c(z,pft)) )

lpx_forest_moisture <- subset(read.csv("~/data/n2o_Yunke/forcing/LPX_annual_moisture.csv"),pft=="forest")
lpx_forest_moisture <- (dplyr::select(lpx_forest_moisture, -c(z,pft)) )

lpx_forest_temperature <- subset(read.csv("~/data/n2o_Yunke/forcing/LPX_annual_T.csv"),pft=="forest")
lpx_forest_temperature <- (dplyr::select(lpx_forest_temperature, -c(z,pft)) )

LPX_forest_sitemean_n2o <- merge(LPX_forest_sitemean,lpx_forest_n2o,by=c("lon","lat"),all.x=TRUE)
LPX_forest_sitemean_moisture <- merge(LPX_forest_sitemean,lpx_forest_moisture,by=c("lon","lat"),all.x=TRUE)
LPX_forest_sitemean_temperature <- merge(LPX_forest_sitemean,lpx_forest_temperature,by=c("lon","lat"),all.x=TRUE)

#check if coordinates are consistent
summary(LPX_forest_sitemean_n2o$lat-LPX_forest_sitemean_moisture$lat)
summary(LPX_forest_sitemean_temperature$lat-LPX_forest_sitemean_moisture$lat)

n2o_final <- log(data.frame(x=unlist(LPX_forest_sitemean_n2o[,c(3:39)])))
moisture_final <- data.frame(x=unlist(LPX_forest_sitemean_moisture[,c(3:39)]))
Tg_final <- data.frame(x=unlist(LPX_forest_sitemean_temperature[,c(3:39)]))

final_forest_lpx <- as.data.frame(cbind(n2o_final,moisture_final,Tg_final))
names(final_forest_lpx) <- c("n2o_a","obs_moisture","Tg_a")
mod2 <- (lm(n2o_a~Tg_a+obs_moisture,final_forest_lpx))
summary(mod2)

mod1 <- (lmer(n2o_a~Tg_a+obs_moisture+(1|site_a),data=forest2_field))
summary(mod1)
r.squaredGLMM(mod1)

summary(mod1)
summary(mod2)

summary(forest2_field$obs_moisture)
summary(final_forest_lpx$obs_moisture)

mod1_moisture <- visreg(mod1,"obs_moisture",type="contrast");mod1_Tg <-visreg(mod1,"Tg_a",type="contrast")
mod2_moisture <- visreg(mod2,"obs_moisture",type="contrast");mod2_Tg <- visreg(mod2,"Tg_a",type="contrast")

fits_moisture <- dplyr::bind_rows(mutate(mod1_moisture$fit, plt = "Measurement"),mutate(mod2_moisture$fit, plt = "LPX"))
fits_tg <- dplyr::bind_rows(mutate(mod1_Tg$fit, plt = "Measurement"),mutate(mod2_Tg$fit, plt = "LPX"))

###converted to lm 
visreg_ggplot <- function(obj,var_name,color1,color2){
  final1 <- ggplot() + geom_line(data = obj, aes_string(var_name, "visregFit", group="plt", color="plt"),size=2) +
    theme_classic()+theme(text = element_text(size=20),legend.position="none")+ 
    geom_ribbon(data = obj,aes_string(var_name, ymin="visregLwr", ymax="visregUpr",fill="plt"),alpha=0.5)+
    scale_colour_manual(values=c(Measurement=color1,LPX=color2))+
    scale_fill_manual(values=c(Measurement=color1,LPX=color2))

  return(final1)
}

g1 <- visreg_ggplot(fits_tg,"Tg_a","black","red")
g1
g2 <- visreg_ggplot(fits_moisture,"obs_moisture","black","red")
g2

#check comparasion
dim(lpx_forest_n2o)
predict_forest_n2o <- as.data.frame(cbind(lpx_forest_n2o[c(1:2)],rowMeans(lpx_forest_n2o[,c(3:39)],na.rm=T)))
names(predict_forest_n2o) <-c("lon","lat","lpx_n2o")
forest2_field
forest_compare <- merge(forest2_field,predict_forest_n2o,by=c("lon","lat"),all.x=TRUE)
forest_compare <- na.omit(forest_compare[,c("lon","lat","n2o_a","lpx_n2o")])
names(forest_compare) <- c("lon","lat","obs_n2o","pred_n2o")
forest_compare_sitemean <- as.data.frame(forest_compare %>% group_by(lon,lat)  %>%
                                  summarise(obs_n2o = mean(obs_n2o),
                                            pred_n2o = mean(pred_n2o)))
analyse_modobs2(forest_compare_sitemean,"pred_n2o","obs_n2o", type = "points",relative=TRUE)$gg 
analyse_modobs2(forest_compare,"pred_n2o","obs_n2o", type = "points",relative=TRUE)$gg 


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

test2 <- (na.omit(grassland2_field[,c("site_a","n2o_a","sqrt_Nfer_kgha","orgc_a","ndep_a",
                                      "Tg_a",
                                      "PPFD_total_a","PPFD_a",
                                      "min_fapar","max_fapar","mean_fapar")]))
stepwise(test2,"n2o_a")[[1]]
stepwise(test2,"n2o_a")[[2]]

mod3 <- (lmer(n2o_a~sqrt_Nfer_kgha+min_fapar+(1|site_a),data=grassland2_field))
summary(mod3)
r.squaredGLMM(mod3)

#applied in LPX model
output_df_grassland <- na.omit(grassland2_field[,c("lon","lat","z","start_yr","end_yr",
                                                   "n2o_a","sqrt_Nfer_kgha","min_fapar")])

LPX_grassland_sitemean <- unique(output_df_grassland[,c("lon","lat")])
dim(LPX_grassland_sitemean)

lpx_grassland_n2o <- subset(read.csv("~/data/n2o_Yunke/forcing/LPX_annual_n2o.csv"),pft=="grassland")
lpx_grassland_n2o <- (dplyr::select(lpx_grassland_n2o, -c(z,pft)) )

lpx_grassland_nfer <- subset(read.csv("~/data/n2o_Yunke/forcing/LPX_annual_nfer.csv"),pft=="grassland")
lpx_grassland_nfer <- (dplyr::select(lpx_grassland_nfer, -c(z,pft)) )

lpx_grassland_minfapar <- subset(read.csv("~/data/n2o_Yunke/forcing/LPX_annual_minfapar.csv"),pft=="grassland")
lpx_grassland_minfapar <- (dplyr::select(lpx_grassland_minfapar, -c(z,pft)) )

LPX_grassland_sitemean_n2o <- merge(LPX_grassland_sitemean,lpx_grassland_n2o,by=c("lon","lat"),all.x=TRUE)
LPX_grassland_sitemean_nfer <- merge(LPX_grassland_sitemean,lpx_grassland_nfer,by=c("lon","lat"),all.x=TRUE)
LPX_grassland_sitemean_minfapar <- merge(LPX_grassland_sitemean,lpx_grassland_minfapar,by=c("lon","lat"),all.x=TRUE)

#check if coordinates are consistent
summary(LPX_grassland_sitemean_n2o$lat-LPX_grassland_sitemean_nfer$lat)
summary(LPX_grassland_sitemean_nfer$lat-LPX_grassland_sitemean_minfapar$lat)

grass_n2o <- log(data.frame(x=unlist(LPX_grassland_sitemean_n2o[,c(3:39)])))
nfer_n2o <- sqrt(data.frame(x=unlist(LPX_grassland_sitemean_nfer[,c(3:39)])))
minfapar_n2o <- data.frame(x=unlist(LPX_grassland_sitemean_minfapar[,c(3:39)]))

final_grassland_lpx <- as.data.frame(cbind(grass_n2o,nfer_n2o,minfapar_n2o))
names(final_grassland_lpx) <- c("n2o_a","sqrt_Nfer_kgha","min_fapar")

mod4 <- (lm(n2o_a~sqrt_Nfer_kgha+min_fapar,data=final_grassland_lpx))
summary(mod4)
r.squaredGLMM(mod4)

mod3_nfer <- visreg(mod3,"sqrt_Nfer_kgha",type="contrast");mod3_minfapar <- visreg(mod3,"min_fapar",type="contrast")
mod4_nfer <- visreg(mod4,"sqrt_Nfer_kgha",type="contrast");mod4_minfapar <- visreg(mod4,"min_fapar",type="contrast")

fits_nfer <- dplyr::bind_rows(mutate(mod3_nfer$fit, plt = "Measurement"),mutate(mod4_nfer$fit, plt = "LPX"))
fits_minfapar <- dplyr::bind_rows(mutate(mod3_minfapar$fit, plt = "Measurement"),mutate(mod4_minfapar$fit, plt = "LPX"))

###converted to lm 

g3 <- visreg_ggplot(fits_nfer,"sqrt_Nfer_kgha","black","red")
g3

g4 <- visreg_ggplot(fits_minfapar,"min_fapar","black","red")
g4

summary(mod3)
summary(mod4)

#check comparasion
dim(lpx_grassland_n2o)
predict_grassland_n2o <- as.data.frame(cbind(lpx_grassland_n2o[c(1:2)],rowMeans(lpx_grassland_n2o[,c(3:39)],na.rm=T)))
names(predict_grassland_n2o) <-c("lon","lat","lpx_n2o")
grassland2_field
grassland_compare <- merge(grassland2_field,predict_grassland_n2o,by=c("lon","lat"),all.x=TRUE)
grassland_compare <- na.omit(grassland_compare[,c("lon","lat","n2o_a","lpx_n2o")])
names(grassland_compare) <- c("lon","lat","obs_n2o","pred_n2o")
grassland_compare_sitemean <- as.data.frame(grassland_compare %>% group_by(lon,lat)  %>%
                                  summarise(obs_n2o = mean(obs_n2o),
                                            pred_n2o = mean(pred_n2o)))

analyse_modobs2(grassland_compare_sitemean,"pred_n2o","obs_n2o", type = "points",relative=TRUE)$gg 
analyse_modobs2(grassland_compare,"pred_n2o","obs_n2o", type = "points",relative=TRUE)$gg 

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

#directly using existing models/ comparasion 
mod5 <- ((lmer(n2o_a~orgc_a+sqrt_Nfer_kgha+vpd_a+Tg_a+PPFD_total_a+max_fapar+min_fapar+(1|site_a),data=cropland2_liao)))
summary(mod5)
AIC(mod5)
r.squaredGLMM(mod5)

#applied in LPX model
output_df_cropland <- na.omit(cropland2_liao[,c("lon","lat","z","start_yr","end_yr",
                                                "sqrt_Nfer_kgha","orgc_a","n2o_a","vpd_a","Tg_a","PPFD_total_a","max_fapar","min_fapar")])

LPX_cropland_sitemean <- unique(output_df_cropland[,c("lon","lat")])
dim(LPX_cropland_sitemean)

lpx_cropland_n2o <- subset(read.csv("~/data/n2o_Yunke/forcing/LPX_annual_n2o.csv"),pft=="cropland")
lpx_cropland_n2o <- (dplyr::select(lpx_cropland_n2o, -c(z,pft)) )

lpx_cropland_nfer <- subset(read.csv("~/data/n2o_Yunke/forcing/LPX_annual_nfer.csv"),pft=="cropland")
lpx_cropland_nfer <- (dplyr::select(lpx_cropland_nfer, -c(z,pft)) )

lpx_cropland_vpd <- subset(read.csv("~/data/n2o_Yunke/forcing/LPX_annual_vpd.csv"),pft=="cropland")
lpx_cropland_vpd <- (dplyr::select(lpx_cropland_vpd, -c(z,pft)) )

lpx_cropland_temperature <- subset(read.csv("~/data/n2o_Yunke/forcing/LPX_annual_T.csv"),pft=="cropland")
lpx_cropland_temperature <- (dplyr::select(lpx_cropland_temperature, -c(z,pft)) )

lpx_cropland_PPFD <- subset(read.csv("~/data/n2o_Yunke/forcing/LPX_annual_PPFD.csv"),pft=="cropland")
lpx_cropland_PPFD <- (dplyr::select(lpx_cropland_PPFD, -c(z,pft)) )

lpx_cropland_maxfapar <- subset(read.csv("~/data/n2o_Yunke/forcing/LPX_annual_maxfapar.csv"),pft=="cropland")
lpx_cropland_maxfapar <- (dplyr::select(lpx_cropland_maxfapar, -c(z,pft)) )

lpx_cropland_minfapar <- subset(read.csv("~/data/n2o_Yunke/forcing/LPX_annual_minfapar.csv"),pft=="cropland")
lpx_cropland_minfapar <- (dplyr::select(lpx_cropland_minfapar, -c(z,pft)) )

LPX_cropland_sitemean_n2o <- merge(LPX_cropland_sitemean,lpx_cropland_n2o,by=c("lon","lat"),all.x=TRUE)
LPX_cropland_sitemean_nfer <- merge(LPX_cropland_sitemean,lpx_cropland_nfer,by=c("lon","lat"),all.x=TRUE)
LPX_cropland_sitemean_vpd <- merge(LPX_cropland_sitemean,lpx_cropland_vpd,by=c("lon","lat"),all.x=TRUE)
LPX_cropland_sitemean_temperature <- merge(LPX_cropland_sitemean,lpx_cropland_temperature,by=c("lon","lat"),all.x=TRUE)
LPX_cropland_sitemean_PPFD <- merge(LPX_cropland_sitemean,lpx_cropland_PPFD,by=c("lon","lat"),all.x=TRUE)
LPX_cropland_sitemean_maxfapar <- merge(LPX_cropland_sitemean,lpx_cropland_maxfapar,by=c("lon","lat"),all.x=TRUE)
LPX_cropland_sitemean_minfapar <- merge(LPX_cropland_sitemean,lpx_cropland_minfapar,by=c("lon","lat"),all.x=TRUE)

lpx_soc <- (unique(output_df_cropland[,c("lon","lat","orgc_a")]))
LPX_cropland_sitemean_soc <- merge(LPX_cropland_sitemean,lpx_soc,by=c("lon","lat"),all.x=TRUE)
LPX_cropland_sitemean_soc[,c(4:39)] <- LPX_cropland_sitemean_soc[,3] #expand to multiple years, though with the same value

#check if coordinates are consistent
summary(LPX_cropland_sitemean_n2o$lat-LPX_cropland_sitemean_nfer$lat)
summary(LPX_cropland_sitemean_nfer$lat-LPX_cropland_sitemean_vpd$lat)
summary(LPX_cropland_sitemean_vpd$lat-LPX_cropland_sitemean_temperature$lat)
summary(LPX_cropland_sitemean_temperature$lat-LPX_cropland_sitemean_PPFD$lat)
summary(LPX_cropland_sitemean_PPFD$lat-LPX_cropland_sitemean_maxfapar$lat)
summary(LPX_cropland_sitemean_maxfapar$lat-LPX_cropland_sitemean_minfapar$lat)
summary(LPX_cropland_sitemean_minfapar$lat-LPX_cropland_sitemean_soc$lat)

cropland_n2o <- log(data.frame(x=unlist(LPX_cropland_sitemean_n2o[,c(3:39)])))
nfer_n2o <- sqrt(data.frame(x=unlist(LPX_cropland_sitemean_nfer[,c(3:39)])))
vpd_n2o <- log(data.frame(x=unlist(LPX_cropland_sitemean_vpd[,c(3:39)])))
temperature_n2o <- data.frame(x=unlist(LPX_cropland_sitemean_temperature[,c(3:39)]))
PPFD_n2o <- log(data.frame(x=unlist(LPX_cropland_sitemean_PPFD[,c(3:39)])))
maxfapar_n2o <- data.frame(x=unlist(LPX_cropland_sitemean_maxfapar[,c(3:39)]))
minfapar_n2o <- data.frame(x=unlist(LPX_cropland_sitemean_minfapar[,c(3:39)]))
soc_n2o <- data.frame(x=unlist(LPX_cropland_sitemean_soc[,c(3:39)]))

final_cropland_lpx <- as.data.frame(cbind(cropland_n2o,nfer_n2o,vpd_n2o,
                                           temperature_n2o,PPFD_n2o,
                                           maxfapar_n2o,minfapar_n2o,soc_n2o))
names(final_cropland_lpx) <- c("n2o_a","sqrt_Nfer_kgha","vpd_a","Tg_a",
                                "PPFD_total_a","max_fapar","min_fapar","orgc_a")

mod6 <- ((lm(n2o_a~orgc_a+sqrt_Nfer_kgha+vpd_a+Tg_a+PPFD_total_a+max_fapar+min_fapar,data=final_cropland_lpx)))
summary(mod6)

mod5_nfer <- visreg(mod5,"sqrt_Nfer_kgha",type="contrast")
mod5_soc <- visreg(mod5,"orgc_a",type="contrast")
mod5_ppfd_total <- visreg(mod5,"PPFD_total_a",type="contrast")
mod5_vpd <- visreg(mod5,"vpd_a",type="contrast")
mod5_Tg <- visreg(mod5,"Tg_a",type="contrast")
mod5_max_fapar <- visreg(mod5,"max_fapar",type="contrast")
mod5_min_fapar <- visreg(mod5,"min_fapar",type="contrast")

mod6_nfer <- visreg(mod6,"sqrt_Nfer_kgha",type="contrast")
mod6_soc <- visreg(mod6,"orgc_a",type="contrast")
mod6_ppfd_total <- visreg(mod6,"PPFD_total_a",type="contrast")
mod6_vpd <- visreg(mod6,"vpd_a",type="contrast")
mod6_Tg <- visreg(mod6,"Tg_a",type="contrast")
mod6_max_fapar <- visreg(mod6,"max_fapar",type="contrast")
mod6_min_fapar <- visreg(mod6,"min_fapar",type="contrast")

fits_nfer <- dplyr::bind_rows(mutate(mod5_nfer$fit, plt = "Measurement"),mutate(mod6_nfer$fit, plt = "LPX"))
fits_soc <- dplyr::bind_rows(mutate(mod5_soc$fit, plt = "Measurement"),mutate(mod6_soc$fit, plt = "LPX"))
fits_ppfd_total <- dplyr::bind_rows(mutate(mod5_ppfd_total$fit, plt = "Measurement"),mutate(mod6_ppfd_total$fit, plt = "LPX"))
fits_vpd <- dplyr::bind_rows(mutate(mod5_vpd$fit, plt = "Measurement"),mutate(mod6_vpd$fit, plt = "LPX"))
fits_Tg <- dplyr::bind_rows(mutate(mod5_Tg$fit, plt = "Measurement"),mutate(mod6_Tg$fit, plt = "LPX"))
fits_max_fapar <- dplyr::bind_rows(mutate(mod5_max_fapar$fit, plt = "Measurement"),mutate(mod6_max_fapar$fit, plt = "LPX"))
fits_min_fapar <- dplyr::bind_rows(mutate(mod5_min_fapar$fit, plt = "Measurement"),mutate(mod6_min_fapar$fit, plt = "LPX"))

g5 <- visreg_ggplot(fits_nfer,"sqrt_Nfer_kgha","black","red")
g6 <- visreg_ggplot(fits_soc,"orgc_a","black","red")
g7 <- visreg_ggplot(fits_ppfd_total,"PPFD_total_a","black","red")
g8 <- visreg_ggplot(fits_Tg,"Tg_a","black","red")
g9 <- visreg_ggplot(fits_vpd,"vpd_a","black","red")
g10 <- visreg_ggplot(fits_max_fapar,"max_fapar","black","red")
g11 <- visreg_ggplot(fits_min_fapar,"min_fapar","black","red")

g5
g6
g7
g8
g9
g10
g11

summary(mod5)
summary(mod6)

#check comparasion
dim(lpx_cropland_n2o)
predict_cropland_n2o <- as.data.frame(cbind(lpx_cropland_n2o[c(1:2)],rowMeans(lpx_cropland_n2o[,c(3:39)],na.rm=T)))
names(predict_cropland_n2o) <-c("lon","lat","lpx_n2o")
cropland2_liao
cropland_compare <- merge(cropland2_liao,predict_cropland_n2o,by=c("lon","lat"),all.x=TRUE)
cropland_compare <- na.omit(cropland_compare[,c("lon","lat","n2o_a","lpx_n2o")])
names(cropland_compare) <- c("lon","lat","obs_n2o","pred_n2o")
cropland_compare_sitemean <- as.data.frame(cropland_compare %>% group_by(lon,lat)  %>%
                                              summarise(obs_n2o = mean(obs_n2o),
                                                        pred_n2o = mean(pred_n2o)))

analyse_modobs2(cropland_compare_sitemean,"pred_n2o","obs_n2o", type = "points",relative=TRUE)$gg 
analyse_modobs2(cropland_compare,"pred_n2o","obs_n2o", type = "points",relative=TRUE)$gg 


#AAA: output cropland data-frame for LPX model use
#and check cropland, grassland and forest data
#check if N2O lowest in forest, and highest in grassland - true

output_df_forest <- na.omit(forest2_field[,c("lon","lat","z","start_yr","end_yr",
                                                "n2o_a","Tg_a","obs_moisture")])
summary(output_df_forest$n2o_a)
output_df_forest <- unique(output_df_forest[,c("lon","lat","z","start_yr","end_yr")])

output_df_grassland <- na.omit(grassland2_field[,c("lon","lat","z","start_yr","end_yr",
                                             "n2o_a","sqrt_Nfer_kgha","min_fapar")])
summary(output_df_grassland$n2o_a)
output_df_grassland <- unique(output_df_grassland[,c("lon","lat","z","start_yr","end_yr")])

output_df_cropland <- na.omit(cropland2_liao[,c("lon","lat","z","start_yr","end_yr",
                                                "sqrt_Nfer_kgha","orgc_a","n2o_a","vpd_a","Tg_a","PPFD_total_a","max_fapar","min_fapar")])
summary(output_df_cropland$n2o_a)
output_df_cropland <- unique(output_df_cropland[,c("lon","lat","z","start_yr","end_yr")])

output_df_forest$pft <- "forest"
output_df_grassland$pft <- "grassland"
output_df_cropland$pft <- "cropland"

output_lpx <- as.data.frame(rbind(output_df_forest,output_df_grassland,output_df_cropland))

#csvfile <- paste("~/data/n2o_Yunke/forcing/lpx_sites_field.csv")
#write_csv(output_lpx, path = csvfile)

