library(doSNOW)
devtools::load_all("/Users/yunpeng/yunkepeng/latest_packages/ingestr/")

settings_modis <- get_settings_modis(
  bundle            = "modis_fpar",
  data_path         = "~/data/n2o_fapar/",
  method_interpol   = "loess",
  keep              = TRUE,
  overwrite_raw     = FALSE,
  overwrite_interpol= TRUE
)

siteinfo_final<- tibble(sitemame=c("s1","s2","s3"),lon=c(-104.69,-105.05,-105.12),lat=c(53.92,19.50,53.99),elv=c(10,20,30),
                  year_start=c(2009,2009,2010),year_end=c(2010,2011,2011))

NumberOfCluster <- 8
cl <- makeCluster(NumberOfCluster, type='SOCK')
registerDoSNOW(cl)
x0 <- foreach(i = c(1:3),.combine = "rbind") %dopar% {
  devtools::load_all("/Users/yunpeng/yunkepeng/latest_packages/ingestr/")
  library(dplyr)
  df_modis_fpar <- ingestr::ingest_bysite(
    sitename  = siteinfo_final[i,c("sitename")],  
    source    = "modis",
    year_start = siteinfo_final[i,c("year_start")],
    year_end  = siteinfo_final[i,c("year_end")],
    lon       = siteinfo_final[i,c("lon")],
    lat       = siteinfo_final[i,c("lat")],
    settings  = settings_modis,
    verbose   = FALSE
  )
}

stopCluster(cl)



