This file is the work flow of the whole processing:

1. preprocessing/obs_field_combinations.R (combines dataset of all field N2O data, ingest its environmental predictors, and also run LPX simulations in those sites). It outputs the observational dataset of field n2o: /data/archive/n2o_Yunke/final_obs_dataset/obs_field_dataset.csv


2. pre_processing/obs_co2_combinations.R (combines dataset of all experimental N2O data, ingest its environmental predictors, and also run LPX simulations in those sites). 
	- It outputs the eCO2 observed dataset to: /data/archive/n2o_Yunke/final_obs_dataset/obs_eCO2_dataset.csv.
	- It outputs the warming observed dataset to: /data/archive/n2o_Yunke/final_obs_dataset/obs_warming_dataset.csv


#these outputs are the main data that will be used in https://github.com/yunkepeng/n2o_MS to produce figures of Yunke's chapter 3.