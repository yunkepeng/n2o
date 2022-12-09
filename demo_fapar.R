library(doSNOW)
devtools::load_all("/Users/yunpeng/yunkepeng/latest_packages/ingestr/")
library(dplyr)
library(readr)
siteinfo_final <- read.csv("/Users/yunpeng/data/n2o_fapar/fapar_siteinfo.csv")

settings_modis <- get_settings_modis(
  bundle            = "modis_fpar",
  data_path         = "~/data/n2o_fapar/data/",
  method_interpol   = "loess",
  keep              = TRUE,
  overwrite_raw     = FALSE,
  overwrite_interpol= TRUE,
  n_focal=0)

#now, fapar
NumberOfCluster <- 8
cl <- makeCluster(NumberOfCluster, type='SOCK')
registerDoSNOW(cl)
x0 <- foreach(i = c(1:nrow(siteinfo_final)),.combine = "rbind") %dopar% {
  devtools::load_all("/Users/yunpeng/yunkepeng/latest_packages/ingestr/")
  library(dplyr)
  df_modis_fpar <- ingestr::ingest_bysite(
    sitename  = siteinfo_final[i,c("sitename")],  # can be any name
    source    = "modis",
    year_start = siteinfo_final[i,c("start_yr")],
    year_end  = siteinfo_final[i,c("end_yr")],
    lon       = siteinfo_final[i,c("lon")],
    lat       = siteinfo_final[i,c("lat")],
    settings  = settings_modis,
    verbose   = FALSE
  )
}

stopCluster(cl)

#check 2. existing csv -> correct measurement year (before 2001 using 2001-2010 average)
devtools::load_all("/Users/yunpeng/yunkepeng/latest_packages/ingestr/")
library(dplyr)

siteinfo_final <- read.csv("/Users/yunpeng/data/n2o_fapar/fapar_siteinfo.csv")

siteinfo_final$end_yr[siteinfo_final$start_yr <=2002] <- 2012
siteinfo_final$start_yr[siteinfo_final$start_yr <=2002] <- 2003

settings_modis <- get_settings_modis(
  bundle            = "modis_fpar",
  data_path         = "~/data/n2o_fapar/data/",
  method_interpol   = "loess",
  keep              = TRUE,
  overwrite_raw     = FALSE,
  overwrite_interpol= TRUE,
  n_focal=0)
settings<- settings_modis

siteinfo_final$max_fapar <- NA
siteinfo_final$mean_fapar <- NA


for (i in 1:nrow(siteinfo_final)) {
  print(i)
    fapar_df_new <- list.files("~/data/n2o_fapar/data/MODIS_FPAR_MCD15A3H/raw/",full.names = T)
    sitename_raw <- substr(sub('.*MCD15A3H_', '', fapar_df_new[i]),1,nchar(sub('.*MCD15A3H_', '', fapar_df_new[i]))-14) 
    
    df <- read.csv(fapar_df_new[i])
    df <- df %>%
      
      # convert date
      dplyr::mutate(date = lubridate::ymd(calendar_date)) %>%
      
      # put QC info to a separate column
      dplyr::select(pixel, date, band, value) %>%
      tidyr::pivot_wider(values_from = value, names_from = band)
    
    # Determine scale factor from band info and scale values
    bands <- MODISTools::mt_bands(product = settings$prod) %>%
      as_tibble()
    scale_factor <- bands %>%
      dplyr::filter(band %in% settings$band_var) %>%
      pull(scale_factor) %>%
      as.numeric() %>%
      unique()
    
    if (length(scale_factor) != 1){
      stop("Multiple scaling factors found for ingested bands")
    } else {
      scaleme <- function(x, scale_factor){x * scale_factor}
      df <- df %>%
        mutate(across(settings$band_var, scaleme, scale_factor = scale_factor))
    }
    
    df <- df %>%rename(value = !!settings$band_var, qc = !!settings$band_qc)
    
    
    # Clean (gapfill and interpolate) full time series data to daily
    ddf <- gapfill_interpol(
      df,
      sitename=sitename_raw,
      date_start  = as.Date(ISOdate(siteinfo_final$start_yr[siteinfo_final$sitename==sitename_raw], 1, 1)),
      date_end = as.Date(ISOdate(siteinfo_final$end_yr[siteinfo_final$sitename==sitename_raw], 12, 31)),
      settings = settings)
    
    siteinfo_final$max_fapar[siteinfo_final$sitename==sitename_raw] <- max(ddf$fapar,na.rm = T)
    siteinfo_final$mean_fapar[siteinfo_final$sitename==sitename_raw] <- mean(ddf$fapar,na.rm = T)
} 

#fill original years
siteinfo_final$start_yr <- read.csv("/Users/yunpeng/data/n2o_fapar/fapar_siteinfo.csv")$start_yr
siteinfo_final$end_yr <- read.csv("/Users/yunpeng/data/n2o_fapar/fapar_siteinfo.csv")$end_yr

csvfile <- paste("~/data/n2o_Yunke/forcing/siteinfo_fapar_nfocal0.csv")
write_csv(siteinfo_final, path = csvfile)

#plot_maps
library(rworldmap)
outliers <- subset(siteinfo_final,is.na(mean_fapar)==T)
newmap <- getMap(resolution = "low")
sp::plot(newmap, xlim = c(-180, 180), ylim = c(-75, 75), asp = 1)
points(outliers$lon,outliers$lat, col="green", pch=16,cex=1)

csvfile <- paste("~/data/n2o_Yunke/forcing/outliers_fapar.csv")
write_csv(outliers, path = csvfile)

#some data is still not available - using old version of ingestr? (because it enables n_focal = 2)
devtools::load_all("/Users/yunpeng/yunkepeng/ingestr")

settings_modis <- get_settings_modis(
  bundle            = "modis_fpar",
  data_path         = "~/data/n2o_fapar/data_nfocal_2/",
  method_interpol   = "loess",
  keep              = TRUE,
  overwrite_raw     = FALSE,
  overwrite_interpol= TRUE)
library(dplyr)

outliers <- read.csv("~/data/n2o_Yunke/forcing/outliers_fapar.csv")

outliers$year_start <- outliers$start_yr
outliers$year_end <- outliers$end_yr
outliers$year_end[outliers$start_yr <=2002] <- 2012
outliers$year_start[outliers$start_yr <=2002] <- 2003

for (i in 1:nrow(outliers)){
  tryCatch({
    print(i)
    df_modis_fpar <- ingestr::ingest_bysite(
      sitename  = outliers[i,c("sitename")],
      source    = "modis",
      year_start = outliers[i,c("year_start")],
      year_end  = outliers[i,c("year_end")],
      lon       = outliers[i,c("lon")],
      lat       = outliers[i,c("lat")],
      settings  = settings_modis,
      verbose   = FALSE)
  }, error=function(e){})} 


#get other some sites
siteinfo_final <- read.csv("/Users/yunpeng/data/n2o_fapar/fapar_siteinfo.csv")

for (i in 1:(nrow(siteinfo_final)-1)) {
  print(i)
  fapar_df_new <- list.files("~/data/n2o_fapar/data_nfocal_2/",full.names = T)
  sitename_raw <- substr(sub('.*daily_', '', fapar_df_new[i]),1,nchar(sub('.*daily_', '', fapar_df_new[i]))-4) 
  
  df <- read.csv(fapar_df_new[i])
  
  siteinfo_final$max_fapar[siteinfo_final$sitename==sitename_raw] <- max(df$modisvar_filled,na.rm = T)
  siteinfo_final$mean_fapar[siteinfo_final$sitename==sitename_raw] <- mean(df$modisvar_filled,na.rm = T)
} 
siteinfo_final2 <- na.omit(siteinfo_final)
csvfile <- paste("~/data/n2o_Yunke/forcing/siteinfo_fapar_nfocal2.csv")
write_csv(siteinfo_final2, path = csvfile)
