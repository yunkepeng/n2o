devtools::load_all("/Users/yunpeng/yunkepeng/latest_packages/rbeni/") 
devtools::load_all("/Users/yunpeng/yunkepeng/latest_packages/ingestr/")
library(tidyr)

#now, for organic C, total N and CNrt
settings_wise <- get_settings_wise(varnam = c("ORGC"), layer = 1:7)

elev_nc <- read_nc_onefile("~/data/watch_wfdei/WFDEI-elevation.nc")
elev <- as.data.frame(nc_to_df(elev_nc, varnam = "elevation"))
summary(elev)

elev$sitename <- paste("sitename",c(1:nrow(elev)),sep="")
summary(elev)

df_wise <- ingest(
  elev[,c("lon","lat","sitename")],
  source    = "wise",
  settings  = settings_wise,
  dir       = "~/data/soil/wise/"
) %>% 
  unnest(data)

summary(df_wise)

final <- merge(elev,df_wise,by=c("sitename"),all.x=TRUE)
summary(final) # unit is g/kg
final$ORGC[final$ORGC<=0] <- NA

dim(final)
plot_map3(final[,c("lon","lat","ORGC")])

final_orgc <- final[,c("lon","lat","ORGC")]
summary(final_orgc)

#prepare lon and lat
library(ncdf4)
ncin <- nc_open("~/data/watch_wfdei/WFDEI-elevation.nc")
lon <- ncvar_get(ncin,"lon")
lat<-ncvar_get(ncin,"lat")

orgc_nc <- list(df_to_grid(final_orgc,varnam = "ORGC", lonnam = "lon", latnam = "lat"))
names(orgc_nc) <- "ORGC"
varams = "ORGC"
test <- list(lon,lat,orgc_nc,varams)
names(test) <- c("lon","lat","vars","varams")
write_nc2(test,varnams = "ORGC",long_name = "ORGC",units = "g/kg",
          path = "~/data/nimpl_sofun_inputs/map/Final_ncfile/ORGC.nc")
