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
devtools::load_all("/Users/yunpeng/yunkepeng/latest_packages/ingestr/")
library(hwsdr)

#input data 
df1 <- read_csv("~/data/n2o_wang_oikos/n2o_tables1.csv")
df1_coord <- unique(df1[,c("longitude_degree","latitude_degree")])

df2 <- read_csv("~/data/n2o_wang_oikos/n2o_tables2.csv")
df2_coord <- unique(df2[,c("longitude_degree","latitude_degree")])

df_all <- na.omit(as.data.frame(rbind(df1_coord,df2_coord)))
names(df_all) <- c("lon","lat")
df_all <- unique(df_all)
#project data
#newmap <- getMap(resolution = "low")
#plot(newmap, xlim = c(-180, 180), ylim = c(-75, 75), asp = 1)
#points(df_all$lon,df_all$lat, col="red", pch=16,cex=1)

#get elevations
df_all
df_all$sitename <- paste("sitename",c(1:nrow(df_all)),sep="")
df_etopo <- ingest(
  df_all,
  source = "etopo1",
  dir = "~/data/etopo/"
)
df_all$z <- as.numeric(as.data.frame(df_etopo$data))
summary(df_all$z)

#ingest 
#input data
gwr_sites <- unique(df_all[,c("lon","lat","z")])

#now, extracting site values from Tg, alpha, c/n.....
elev_nc <- read_nc_onefile("~/data/watch_wfdei/WFDEI-elevation.nc")
elev <- as.data.frame(nc_to_df(elev_nc, varnam = "elevation"))

PPFD_total_fapar <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/nimpl_sofun_inputs/map/Final_ncfile/PPFD_total_fapar.nc"),
  varnam = "PPFD_total_fapar"))

PPFD_total <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/nimpl_sofun_inputs/map/Final_ncfile/PPFD_total.nc"),
  varnam = "PPFD_total"))

Tg <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/nimpl_sofun_inputs/map/Final_ncfile/Tg.nc"),
  varnam = "Tg"))

PPFD <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/nimpl_sofun_inputs/map/Final_ncfile/PPFD.nc"),
  varnam = "PPFD"))

vpd <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/nimpl_sofun_inputs/map/Final_ncfile/vpd.nc"),
  varnam = "vpd"))

alpha <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/nimpl_sofun_inputs/map/Final_ncfile/alpha.nc"),
  varnam = "alpha"))

fAPAR <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/nimpl_sofun_inputs/map/Final_ncfile/fAPAR.nc"),
  varnam = "fAPAR"))

age <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/nimpl_sofun_inputs/map/Final_ncfile/age.nc"),
  varnam = "age"))

CNrt <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/nimpl_sofun_inputs/map/Final_ncfile/CNrt.nc"),
  varnam = "CNrt"))

LMA <- as.data.frame(nc_to_df(read_nc_onefile(
  "~/data/nimpl_sofun_inputs/map/Final_ncfile/LMA.nc"),
  varnam = "LMA"))

#input vcmax and gpp map
inputnc <- function(name,start_year,end_year){
  output_allyears <- data.frame(matrix(NA))
  # first, include all years annual data into a daframe
  for (i in firstyr_data:endyr_data){
    if (name == "npp"){
      nc <- read_nc_onefile(alloutput_list[grepl("a.npp.nc", list.files(location,full.names = T))][i-firstyr_data+1]) #we only rely this to filter npp.nc file...
    } else {
      nc <- read_nc_onefile(alloutput_list[grepl(name, list.files(location,full.names = T))][i-firstyr_data+1]) #Input nc
    }
    output_year <- nc_to_df(nc, varnam = name)[,3] #Yearly output
    output_allyears[1:259200,i-firstyr_data+1] <- output_year #here first column represents first year of data file 's output
  }
  names(output_allyears) <- paste(name,firstyr_data:endyr_data,sep="")
  #this variable above (output_allyears), could be end of the function, which is variable at multiple years. But for our purporses, we need mean of select years
  #then, only calculate means of selected years
  output_selected_yrs <- rowMeans(output_allyears[,(start_year-firstyr_data+1):(end_year-firstyr_data+1)],na.rm = TRUE) # only calculated means based on selected start and end year (see function)
  coord <- nc_to_df(nc, varnam = name)[,1:2] # obtain lon and lat
  final_output <- cbind(coord,elev[,3],output_selected_yrs) # combine lon, lat,z with rowmeans variable
  names(final_output) <- c("lon","lat","z",name)
  return(final_output)
  #-----------------------------------------------------------------------
  # Output: output_final: the output data (259200 * 3) including lon, lat and value
  #-----------------------------------------------------------------------
}

firstyr_data <- 1982 # In data file, which is the first year
endyr_data <- 2011 # In data file, which is the last year
location <- "~/data/nimpl_sofun_inputs/map/Final_nc_file_vcmax25/"
#location also in: "~/data/output/latest_forest/"
alloutput_list <- list.files(location,full.names = T)
vcmax25_df <- inputnc("vcmax25",1982,2011)

location <- "~/data/nimpl_sofun_inputs/map/Final_nc_file_gpp/"
alloutput_list <- list.files(location,full.names = T)
gpp_df <- inputnc("gpp",1982,2011)

#check vcmax25 ready to combine
summary(vcmax25_df)
summary(gpp_df)
summary(vcmax25_df$lon -elev$lon)
summary(gpp_df$lat -gpp_df$lat)

#cbind all predictors, and its lon, lat, z
all_predictors <- cbind(elev,PPFD_total_fapar$PPFD_total_fapar,PPFD_total$PPFD_total,Tg$Tg,PPFD$PPFD,vpd$vpd,
                        alpha$alpha,fAPAR$fAPAR,age$age,
                        CNrt$CNrt,LMA$LMA,vcmax25_df$vcmax25,gpp_df$gpp)

names(all_predictors) <- c("lon","lat","z","PPFD_total_fapar","PPFD_total","Tg","PPFD","vpd",
                           "alpha","fAPAR","age","CNrt","LMA","vcmax25","gpp")

PPFD_total_fapar_df <- all_predictors[,c("lon","lat","z","PPFD_total_fapar")]
PPFD_total_df <- all_predictors[,c("lon","lat","z","PPFD_total")]
Tg_df <- all_predictors[,c("lon","lat","z","Tg")]
PPFD_df <- all_predictors[,c("lon","lat","z","PPFD")]
vpd_df <- all_predictors[,c("lon","lat","z","vpd")]
alpha_df <- all_predictors[,c("lon","lat","z","alpha")]
fAPAR_df <- all_predictors[,c("lon","lat","z","fAPAR")]
age_df <- all_predictors[,c("lon","lat","z","age")]
CNrt_df <- all_predictors[,c("lon","lat","z","CNrt")]
LMA_df <- all_predictors[,c("lon","lat","z","LMA")]
vcmax25_df <- all_predictors[,c("lon","lat","z","vcmax25")]
gpp_df <- all_predictors[,c("lon","lat","z","gpp")]

#now, apply gwr to extract site predictors' value
NPP_Forest <- gwr_sites
NPP_Forest$PPFD_total_fapar <- NA
NPP_Forest$PPFD_total <- NA
NPP_Forest$Tg <- NA
NPP_Forest$PPFD <- NA
NPP_Forest$vpd <- NA
NPP_Forest$alpha <- NA
NPP_Forest$fAPAR <- NA
NPP_Forest$age <- NA
NPP_Forest$CNrt <- NA
NPP_Forest$LMA <- NA
NPP_Forest$vcmax25 <- NA
NPP_Forest$mapped_gpp <- NA

a <- 1.5 # which degree (distance) of grid when interpolating gwr from global grids
#Extract Tg, PPFD, vpd, alpha,fAPAR,age,CNrt,LMA, max-vcmax25
for (i in 1:nrow(NPP_Forest)) {
  tryCatch({
    #PPFD_total_fapar
    PPFD_total_fapar_global <- na.omit(PPFD_total_fapar_df)
    NRE_part <- subset(PPFD_total_fapar_global,lon>(NPP_Forest[i,"lon"]-a)&lon<(NPP_Forest[i,"lon"]+a)&
                         lat>(NPP_Forest[i,"lat"]-a)&lat<(NPP_Forest[i,"lat"]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NPP_Forest[i,c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NPP_Forest[i,c("PPFD_total_fapar")] <- (gwr(PPFD_total_fapar ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #PPFD_total
    PPFD_total_global <- na.omit(PPFD_total_df)
    NRE_part <- subset(PPFD_total_global,lon>(NPP_Forest[i,"lon"]-a)&lon<(NPP_Forest[i,"lon"]+a)&
                         lat>(NPP_Forest[i,"lat"]-a)&lat<(NPP_Forest[i,"lat"]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NPP_Forest[i,c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NPP_Forest[i,c("PPFD_total")] <- (gwr(PPFD_total ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #Tg
    print(i)
    Tg_global <- na.omit(Tg_df)
    NRE_part <- subset(Tg_global,lon>(NPP_Forest[i,"lon"]-a)&lon<(NPP_Forest[i,"lon"]+a)&
                         lat>(NPP_Forest[i,"lat"]-a)&lat<(NPP_Forest[i,"lat"]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NPP_Forest[i,c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NPP_Forest[i,c("Tg")] <- (gwr(Tg ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #ppfd
    PPFD_global <- na.omit(PPFD_df)
    NRE_part <- subset(PPFD_global,lon>(NPP_Forest[i,"lon"]-a)&lon<(NPP_Forest[i,"lon"]+a)&
                         lat>(NPP_Forest[i,"lat"]-a)&lat<(NPP_Forest[i,"lat"]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NPP_Forest[i,c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NPP_Forest[i,c("PPFD")] <- (gwr(PPFD ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #vpd
    vpd_global <- na.omit(vpd_df)
    NRE_part <- subset(vpd_global,lon>(NPP_Forest[i,"lon"]-a)&lon<(NPP_Forest[i,"lon"]+a)&
                         lat>(NPP_Forest[i,"lat"]-a)&lat<(NPP_Forest[i,"lat"]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NPP_Forest[i,c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NPP_Forest[i,c("vpd")] <- (gwr(vpd ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #alpha
    alpha_global <- na.omit(alpha_df)
    NRE_part <- subset(alpha_global,lon>(NPP_Forest[i,"lon"]-a)&lon<(NPP_Forest[i,"lon"]+a)&
                         lat>(NPP_Forest[i,"lat"]-a)&lat<(NPP_Forest[i,"lat"]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NPP_Forest[i,c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NPP_Forest[i,c("alpha")]  <- (gwr(alpha ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #fAPAR
    fAPAR_global <- na.omit(fAPAR_df)
    NRE_part <- subset(fAPAR_global,lon>(NPP_Forest[i,"lon"]-a)&lon<(NPP_Forest[i,"lon"]+a)&
                         lat>(NPP_Forest[i,"lat"]-a)&lat<(NPP_Forest[i,"lat"]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NPP_Forest[i,c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NPP_Forest[i,c("fAPAR")]<- (gwr(fAPAR ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #age
    age_global <- na.omit(age_df)
    NRE_part <- subset(age_global,lon>(NPP_Forest[i,"lon"]-a)&lon<(NPP_Forest[i,"lon"]+a)&
                         lat>(NPP_Forest[i,"lat"]-a)&lat<(NPP_Forest[i,"lat"]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NPP_Forest[i,c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NPP_Forest[i,c("age")]  <- (gwr(age ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #CNrt
    CNrt_global <- na.omit(CNrt_df)
    NRE_part <- subset(CNrt_global,lon>(NPP_Forest[i,"lon"]-a)&lon<(NPP_Forest[i,"lon"]+a)&
                         lat>(NPP_Forest[i,"lat"]-a)&lat<(NPP_Forest[i,"lat"]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NPP_Forest[i,c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NPP_Forest[i,c("CNrt")]  <- (gwr(CNrt ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #LMA
    LMA_global <- na.omit(LMA_df)
    NRE_part <- subset(LMA_global,lon>(NPP_Forest[i,"lon"]-a)&lon<(NPP_Forest[i,"lon"]+a)&
                         lat>(NPP_Forest[i,"lat"]-a)&lat<(NPP_Forest[i,"lat"]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NPP_Forest[i,c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NPP_Forest[i,c("LMA")]  <- (gwr(LMA ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #vcmax25
    vcmax25_global <- na.omit(vcmax25_df)
    NRE_part <- subset(vcmax25_global,lon>(NPP_Forest[i,"lon"]-a)&lon<(NPP_Forest[i,"lon"]+a)&
                         lat>(NPP_Forest[i,"lat"]-a)&lat<(NPP_Forest[i,"lat"]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NPP_Forest[i,c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NPP_Forest[i,c("vcmax25")]  <- (gwr(vcmax25 ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
    #gpp
    gpp_global <- na.omit(gpp_df)
    NRE_part <- subset(gpp_global,lon>(NPP_Forest[i,"lon"]-a)&lon<(NPP_Forest[i,"lon"]+a)&
                         lat>(NPP_Forest[i,"lat"]-a)&lat<(NPP_Forest[i,"lat"]+a))
    coordinates(NRE_part) <- c("lon","lat")
    gridded(NRE_part) <- TRUE
    NRE_coord <- NPP_Forest[i,c("lon","lat","z")]
    coordinates(NRE_coord) <- c("lon","lat")
    NPP_Forest[i,c("mapped_gpp")]  <- (gwr(gpp ~ z, NRE_part, bandwidth = 1.06, fit.points =NRE_coord,predictions=TRUE))$SDF$pred
  }, error=function(e){})} 

summary(NPP_Forest)
NPP_Forest$vpd[NPP_Forest$vpd<0] <- NA
NPP_Forest$age[NPP_Forest$age<0] <- NA
NPP_Forest$vcmax25[NPP_Forest$vcmax25<0] <- NA
NPP_Forest$mapped_gpp[NPP_Forest$mapped_gpp<0] <- NA
NPP_Forest$fAPAR[NPP_Forest$fAPAR>1] <- NA
NPP_Forest$alpha[NPP_Forest$alpha>1] <- NA
NPP_Forest$PPFD_total[NPP_Forest$PPFD_total<0] <- NA
NPP_Forest$PPFD_total_fapar[NPP_Forest$PPFD_total_fapar<0] <- NA

summary(NPP_Forest)

#N deposition

NPP_Forest2 <- NPP_Forest
NPP_Forest2$nhx <- NA
NPP_Forest2$noy <- NA

for (i in 1:nrow(NPP_Forest2)) {
  tryCatch({
    print(i)
    df_ndep <- ingest_bysite(
      sitename  = paste("a",i,sep=""),
      source    = "ndep",
      lon       = NPP_Forest2$lon[i],
      lat       = NPP_Forest2$lat[i],
      year_start= 1980,
      year_end  = 2009,
      timescale = "y",
      dir       = "~/data/ndep_lamarque/",
      verbose   = FALSE
    )
    NPP_Forest2$noy[i] <- mean(df_ndep$noy,na.rm=TRUE)
    NPP_Forest2$nhx[i] <- mean(df_ndep$nhx,na.rm=TRUE)
  }, error=function(e){})} 

NPP_Forest2$ndep <- NPP_Forest2$noy + NPP_Forest2$nhx
NPP_Forest3 <- NPP_Forest2

#now, for organic C, total N and CNrt
settings_wise <- get_settings_wise(varnam = c("CNrt","ORGC","TOTN","PHAQ"), layer = 1:7)

NPP_Forest3$sitename <- paste("sitename",c(1:nrow(NPP_Forest3)),sep="")
NPP_Forest3$ID <- paste("sitename",c(1:nrow(NPP_Forest3)),sep="")

df_wise <- ingest(
  NPP_Forest3[,c("lon","lat","sitename")],
  source    = "wise",
  settings  = settings_wise,
  dir       = "~/data/soil/wise/"
) %>% 
  unnest(data)

df_wise$CNrt[df_wise$CNrt<=0] <- NA 
df_wise$ORGC[df_wise$ORGC<=0] <- NA 
df_wise$TOTN[df_wise$TOTN<=0] <- NA
df_wise$PHAQ[df_wise$PHAQ<=0] <- NA 

names(df_wise) <- c("sitename","CNrt_ingestr","ORGC","TOTN","PHAQ")

n2o_site <- merge(NPP_Forest3,df_wise,by=c("sitename"),all.x=TRUE)

csvfile <- paste("~/data/n2o_Yunke/forcing/co2_siteinfo_predictors.csv")
write_csv(n2o_site, path = csvfile)


