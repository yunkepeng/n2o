library(readr)
devtools::load_all("/Users/yunpeng/yunkepeng/latest_packages/rbeni/")
devtools::load_all("/Users/yunpeng/yunkepeng/latest_packages/ingestr/")
library(lmerTest)
library(lme4)
library("PerformanceAnalytics")
library(MuMIn)

#1. input database

### Liao et al. 2020 GCB: https://onlinelibrary.wiley.com/doi/full/10.1111/gcb.16365
#1a: field-based n2o
liao_field <- read.csv("~/data/n2o_liao/org/Global_change_biology_GCB-22-1572_primary_field_data.csv")
liao_field2<- liao_field[,c("Ref","Study.area","Year","Latitude","Longitude","N.addition..kg.ha.1.",
                            "Altitude..m.","N2O.fluxes..μg.N.m.2.h.1.","Total.N..g.kg.1.","NO3....mg.kg.1.",
                            "NH4...mg.kg.1.","pH","SOC..g.kg.1.","Ecosystem.types","Soil.moisture....",
                            "AOA..copy.numbers.g.1.dry.soil.","AOB..copy.numbers.g.1.dry.soil.","nirS...copy.numbers.g.1.dry.soil.",
                            "nirK...copy.numbers.g.1.dry.soil.","nosZ...copy.numbers.g.1.dry.soil.",
                            "Bulk.density..g.cm.3.","WFPS....")]
names(liao_field2) <- c("ref","site","year","lat","lon","Nfer_kgha",
                        "z","n2o_ugm2h","totaln_gkg","no3_mgkg","nh4_mgkg","pH","soc_gkg","pft","obs_moisture",
                        "AOA","AOB","nirS","nirK","nosZ","bulk_density","wfps")

#set all value = -9999 to NA
liao_field2[liao_field2 == -9999] <- NA

#correct/update values of soil moisture and bulk density
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

#1b: pot-based n2o from Liao et al. (not used in paper)
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

cui2 <- cui[,c("Reference","Latitude","Longitude","Start_year","End_year","Fertilizers",
               "Crop.type","Nrate","Tillage","Irrigation","Site","EF")]

names(cui2) <- c("ref","lat","lon","start_yr","end_yr","fertilizers",
                 "crop","Nfer_kgha","tillage","irrigation","site","EF")

cui2$method <- "field"
cui2$file <- "cui et al. nature food"
cui2$pft <- "cropland"

#2b Cui et al. Nature Food (with fallow)
cui_fallow <- read.csv("~/data/n2o_cui_naturefood/43016_2021_384_MOESM3_ESM_fallow.csv")

cui2_fallow <- cui_fallow[,c("Reference","Latitude","Longitude","Start_year","End_year","Fertilizers",
                             "Crop.type","Nrate","Tillage","Irrigation","Site","EF")]
cui2_fallow$Site <- paste(cui2_fallow$Site,"_fallow",sep="")

names(cui2_fallow) <- c("ref","lat","lon","start_yr","end_yr","fertilizers",
                        "crop","Nfer_kgha","tillage","irrigation","site","EF")

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

all_n2o <- dplyr::bind_rows(liao_all,cui2,cui2_fallow,hortnagl2,xuri2)

all_n2o$pft[all_n2o$pft==" wetland"|all_n2o$pft=="wetland"] <- "wetland"
all_n2o$pft[all_n2o$pft==" forest"|all_n2o$pft=="forest"] <- "forest"
all_n2o$pft[all_n2o$pft==" grassland"|all_n2o$pft=="grassland"] <- "grassland"
all_n2o$pft[all_n2o$pft=="Temperate shortgrass steppe"|all_n2o$pft=="Temperate steppe"|all_n2o$pft=="Sagebrush steppe"] <- "steppe"
all_n2o$pft[all_n2o$pft=="Tundra"|all_n2o$pft=="Alpine tundra"] <- "tundra"
all_n2o$pft[all_n2o$pft=="desert"|all_n2o$pft=="Desert"] <- "desert"

unique(all_n2o$pft)

#interpolate missing elevation - let's interpolate them by etopo
site_record <- unique(all_n2o[,c("lon","lat","z")])
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
#aggregate basing on lon and lat
site_record2 <- as.data.frame(site_record2 %>% group_by(lon,lat)  %>% summarise(z = mean(z)))
dim(site_record2)
dim(unique(site_record2[,c("lon","lat")]))
summary(site_record2)
#merge to get all elevation values
site_record2$sitename <- paste("siteno",c(1:nrow(site_record2)),sep="")
all_n2o <- all_n2o[,!(names(all_n2o) %in% c("z"))]
all_n2o_z <- merge(all_n2o,site_record2,by=c("lon","lat"),all.x=TRUE)

#convert n2o original data's year
all_n2o_z$start_yr[is.na(all_n2o_z$start_yr)==T] <- 1991
all_n2o_z$end_yr[is.na(all_n2o_z$end_yr)==T] <- 2010
all_n2o_z$end_yr[all_n2o_z$start_yr<1980]<-1989
all_n2o_z$start_yr[all_n2o_z$start_yr<1980]<-1980
all_n2o_z$info[all_n2o_z$start_yr>2016|all_n2o_z$end_yr>2016] <- "higher_year"
all_n2o_z$start_yr[all_n2o_z$info=="higher_year"]<-2007
all_n2o_z$end_yr[all_n2o_z$info=="higher_year"]<-2016
summary(all_n2o_z$start_yr)
summary(all_n2o_z$end_yr)
unique(all_n2o_z$pft)

#now, select all needed data
names(all_n2o_z)
all_n2o_z2 <- all_n2o_z[,c("lon","lat","z","ref","site","start_yr","end_yr",
                          "Nfer_kgha","n2o_ugm2h","obs_moisture","bulk_density","wfps","pft","method",
                          "file","EF","sitename")]

#convert and select pft
all_n2o_z2$pft_original <- all_n2o_z2$pft
all_n2o_z2$pft[all_n2o_z2$pft=="forest"] <- "forest"
all_n2o_z2$pft[all_n2o_z2$pft=="grassland"] <- "grassland"
all_n2o_z2$pft[all_n2o_z2$pft=="Tropical pastures"] <- "grassland"
all_n2o_z2$pft[all_n2o_z2$pft=="Savanna"] <- "grassland"
all_n2o_z2$pft[all_n2o_z2$pft=="cropland"] <- "cropland"
all_n2o_z2$pft[all_n2o_z2$pft=="plantation"] <- "cropland"
all_n2o_z2$pft[all_n2o_z2$pft=="fallow"] <- "cropland"
all_n2o_z2$pft[all_n2o_z2$pft=="bare"] <- "cropland"
all_n2o_z3 <- subset(all_n2o_z2,pft=="cropland"|pft=="grassland"|pft=="forest")
#only field-data used
all_n2o_z4 <- subset(all_n2o_z3,method=="field")
#not including the data from Cui -> it is emission factor and not core of our anlaysis
unique(all_n2o_z4$file)
all_n2o_z5 <- subset(all_n2o_z4,file!="cui et al. nature food")
dim(all_n2o_z5)

#ouput EF database separately
cropland_EF <- subset(all_n2o_z4,is.na(EF)==FALSE)
csvfile <- paste("~/data/n2o_Yunke/final_forcing/EF_database.csv")
write_csv(cropland_EF, path = csvfile)

#output site information (used for environmental predictors ingest)
siteinfo <- (unique(all_n2o_z5[,c("lon","lat","z","start_yr","end_yr","pft")]))
csvfile <- paste("~/data/n2o_Yunke/final_forcing/n2o_siteinfo.csv")
write_csv(siteinfo, path = csvfile)

#now, read all predictors data
#For code see: field_predictors.R
allpredictors <- read.csv("~/data/n2o_Yunke/final_forcing/siteinfo_predictors.csv")[,c("lon","lat","z","vpd","Tg","PPFD_total","PPFD","ndep","ORGC")]
all_n2o_df <- merge(all_n2o_z5,allpredictors,by=c("lon","lat","z"),all.x=TRUE)

#add fapar3g from 1/12 resolution (monthly max and mean)
#for code see: field_fapar.R
fapar3g_df_zhu <- read.csv("~/data/n2o_Yunke/final_forcing/siteinfo_measurementyear_fapar3g_zhu.csv")

fapar3g_df_zhu <- fapar3g_df_zhu %>% mutate(min_fapar = coalesce(min_fapar_nfocal0,min_fapar_nfocal1,min_fapar_nfocal2)) %>%
  mutate(mean_fapar = coalesce(mean_fapar_nfocal0,mean_fapar_nfocal1,mean_fapar_nfocal2)) %>%
  mutate(max_fapar = coalesce(max_fapar_nfocal0,max_fapar_nfocal1,max_fapar_nfocal2))
  
fapar3g_df_zhu2 <- fapar3g_df_zhu[,c("lon","lat","z","pft","start_yr","end_yr","min_fapar","mean_fapar","max_fapar")]

fapar3g_df_zhu2[is.na(fapar3g_df_zhu2)] = 0

#all from 0 to 1
all_n2o_df <- merge(all_n2o_df,fapar3g_df_zhu2,
                    by=c("lon","lat","z","pft","start_yr","end_yr"),all.x=TRUE)
summary(all_n2o_df)

#do some further filtering

#convert n2o <= 0 as NA
all_n2o_df$n2o_ugm2h[all_n2o_df$n2o_ugm2h<=0] <- NA

#all nfertilisation here from Xu-Ri should be 0
all_n2o_df$Nfer_kgha[all_n2o_df$file=="Xu-Ri et al. (2012) New Phytol"] <-0

#some duplicates from Xu-Ri et al. need to be removed after manually check
all_n2o_df$rep <- NA

all_n2o_df$rep[all_n2o_df$lon==-84.00] <- "rep"
all_n2o_df$rep[all_n2o_df$lon==-72.00 & all_n2o_df$lat==42.50] <- "rep"
all_n2o_df$rep[all_n2o_df$lon==-66.00] <- "rep"
all_n2o_df$rep[all_n2o_df$lon==-63.00] <- "rep"
all_n2o_df$rep[all_n2o_df$lon==-62.50 & all_n2o_df$lat==-10.50&all_n2o_df$file=="Xu-Ri et al. (2012) New Phytol"] <- "rep"
all_n2o_df$rep[all_n2o_df$lon==-55.00] <- "rep"
all_n2o_df$rep[all_n2o_df$lon==8.00&all_n2o_df$lat==47.00] <- "rep"
all_n2o_df$rep[all_n2o_df$lon==11.00] <- "rep"
all_n2o_df$rep[all_n2o_df$lon==145.50] <- "rep"
#grassland - check rep
all_n2o_df$rep[all_n2o_df$lon==172.50 & all_n2o_df$lat==-43.50] <- "rep"
all_n2o_df$rep[all_n2o_df$lon==116.00 & all_n2o_df$lat==43.50] <- "rep"
all_n2o_df$rep[all_n2o_df$lon==116.50 & all_n2o_df$lat==43.50] <- "rep"
all_n2o_df$rep[all_n2o_df$lon==8.54 & all_n2o_df$lat==47.12] <- "rep"
all_n2o_df$rep[all_n2o_df$lon==10.00 & all_n2o_df$lat==47.50] <- "rep"
all_n2o_df$rep[all_n2o_df$lon==8.50 & all_n2o_df$lat==50.50] <- "rep"

all_n2o_df <- subset(all_n2o_df,is.na(rep)==T)

#combined with predicted n2o and forest n2o
#code: field_LPX.R
lpx_n2o <- read.csv("~/data/n2o_Yunke/final_forcing/LPX_annual_n2o.csv")
names(lpx_n2o)
lpx_n2o_sitemean <- as.data.frame(cbind(lpx_n2o[,c(1:4)],rowMeans(lpx_n2o[,c(5:ncol(lpx_n2o))])))
names(lpx_n2o_sitemean) <- c("lon","lat","z","pft","pred_n2o")
#all from 0 to 1
all_n2o_df <- merge(all_n2o_df,lpx_n2o_sitemean,
                    by=c("lon","lat","z","pft"),all.x=TRUE)

forest_cover <- read.csv("~/data/n2o_Yunke/final_forcing/forestcover_site.csv")
all_n2o_df <- merge(all_n2o_df,forest_cover,
                    by=c("lon","lat","z","pft"),all.x=TRUE)

#output data files
all_n2o_df <- all_n2o_df %>% 
  rename(obs_n2o = n2o_ugm2h,
         reference = file,
         original_ref = ref)

all_n2o_df <- all_n2o_df[,!names(all_n2o_df) %in% c("rep", "method", "EF")]
csvfile <- paste("~/data/n2o_Yunke/final_obs_dataset/obs_field_dataset.csv")
write_csv(all_n2o_df, path = csvfile)
