This file is the work flow of the whole processing:

1. obs_field_combinations.R (combines dataset of all field N2O data, ingest its environmental predictors, and also run LPX simulations in those sites). Steps are:

- Input field data from below sources:
	- Liao et al. field-data: ~/data/n2o_liao/org/Global_change_biology_GCB-22-1572_primary_field_data.csv
	- Liao et al. pot-data (not used in paper): ~/data/n2o_liao/org/Global_change_biology_GCB-22-1572_primary_pot_data.csv 
	- Cui et al. Emission factor without fallow (not used in paper): ~/data/n2o_cui_naturefood/43016_2021_384_MOESM3_ESM.csv
	- Cui et al. Emission factor with fallow (not used in paper): ~/data/n2o_cui_naturefood/43016_2021_384_MOESM3_ESM_fallow.csv
	- Hortnagl et al. gcb field-data (9 grassland sites): ~/data/n2o_hortnagl_gcb/n2o_hortnagl_gcb.csv
	- Xu-Ri et al.: ~/data/n2o_xuri/xuri_newphy.csv

- output data for Emission factor database: ~/data/n2o_Yunke/final_forcing/EF_database.csv 

- output dataset coordinates (lon, lat, z, start_yr, end_yr, pft) for all field dataset: ~/data/n2o_Yunke/final_forcing/n2o_siteinfo.csv

- Code 1: field_predictors.R (for ingest all environmental predictors: Tg, vpd, PPFD_total, PPFD...)
	- Input data from ~/data/n2o_Yunke/final_forcing/n2o_siteinfo.csv
	- Output data to ~/data/n2o_Yunke/final_forcing/siteinfo_predictors.csv

- Code 2: field_fapar.R (for ingest fAPAR3g at 1/12 resolutions)
	- Input data from: ~/data/n2o_Yunke/final_forcing/n2o_siteinfo.csv
	- Output data to: ~/data/n2o_Yunke/final_forcing/siteinfo_measurementyear_fapar3g_zhu.csv

- Code 3: field_LPX.R (run LPX simulations for all these field sites, for annual data from 1980 to 2016)
	- Input data from: ~/data/n2o_Yunke/final_forcing/n2o_siteinfo.csv
	- Output data to n2o (ug/m2/h): ~/data/n2o_Yunke/final_forcing/LPX_annual_n2o.csv
	- Output data to min monthly fapar: ~/data/n2o_Yunke/final_forcing/LPX_annual_actual_minfapar.csv
	- Output data to mean monthly fapar: ~/data/n2o_Yunke/final_forcing/LPX_annual_actual_meanfapar.csv
	- Output data to max monthly fapar: ~/data/n2o_Yunke/final_forcing/LPX_annual_actual_maxfapar.csv
	- Output data to volumetric soil water content (soil moisture, unitless): ~/data/n2o_Yunke/final_forcing/LPX_annual_moisture.csv
	- Output data to soil nitrogen fertilisation (kg/ha): ~/data/n2o_Yunke/final_forcing/LPX_annual_nfer.csv
	- Output data to growing-season temperature (degree celcius; i.e. average temperature of months where T > 0): ~/data/n2o_Yunke/final_forcing/LPX_annual_T.csv
	- Output data to total PPFD over growing season (mol/m2): ~/data/n2o_Yunke/final_forcing/LPX_annual_PPFD.csv
	- Output data to forest cover (pft 1-8/pft 1-10 from LPX): ~/data/n2o_Yunke/final_forcing/forestcover_site.csv

- Output the field-based dataset: ~/data/n2o_Yunke/final_obs_dataset/obs_field_dataset.csv

2. obs_co2_combinations.R (combines dataset of all experimental N2O data, ingest its environmental predictors, and also run LPX simulations in those sites).

- Input field data from below sources:
	- Wang et al. Oikos eCO2 database: df1 <- read.csv(~/data/n2o_wang_oikos/n2o_tables1.csv)
	- Wang et al. Oikos warming database: df1 <- read.csv(~/data/n2o_wang_oikos/n2o_tables2.csv)

- Code 1: co2_fapar.R (for ingest fAPAR3g at 1/12 resolutions):
    - input data directly from df1 and df2 above
    - output data to: ~/data/n2o_Yunke/final_forcing/co2_siteinfo_measurementyear_fapar3g_zhu.csv

- Code 2: co2_predictors.R (for ingest all environmental predictors: Tg, vpd, PPFD_total, PPFD...)
    - input data directly from df1 and df2 above
    - output data to: ~/data/n2o_Yunke/final_forcing/co2_siteinfo_predictors.csv

- Code 3: CO2_LPX.R (basing on step experiment for ingesting eCO2 and warming model's variables)
Conditions include: "dT0_C380","dT0.39_C380","dT3.95_C380","dT7.5_C380","dT0_C416","dT0_C582","dT0_C813"
	- input data directly from df1 and df2 above
	- Output data to eCO2 and warming effects on N2O: ~/data/n2o_Yunke/final_forcing/eCO2_LPX_annual_n2o.csv
	- Output data to N fertilisation (annual data from 1850 to 2020): ~/data/n2o_Yunke/final_forcing/eCO2_LPX_annual_nfer.csv
	- Output data to temperature (annual data from 1850 to 2020): ~/data/n2o_Yunke/final_forcing/eCO2_LPX_annual_T.csv
	- Output data to total PPFD (annual data from 1850 to 2020): ~/data/n2o_Yunke/final_forcing/eCO2_LPX_annual_PPFD.csv
	- Output data to vegetation cover: ~/data/n2o_Yunke/final_forcing/co2_forestcover_site.csv

-  Output the eCO2 observed dataset to: ~/data/n2o_Yunke/final_obs_dataset/obs_eCO2_dataset.csv
-  Output the warming observed dataset to: ~/data/n2o_Yunke/final_obs_dataset/obs_warming_dataset.csv

3. Input three maps that will be used for upscalling:
	- nfer_map.R  - N fertilisation (g/m2) - map output at ~/data/n2o_Yunke/final_map/nfer.nc
	- ppfd_total.Rmd - total PPFD over growing season (mol/m2) - map output at ~/data/n2o_Yunke/final_map/PPFD_total.nc
	- SOC_map.R - soil organic carbon (g/kg) - map output at ~/data/n2o_Yunke/final_map/ORGC.nc

4. LPX_feedback_calculation.R (calculate total N2O estimation Tg/yr at "dT0_C380","dT0.39_C380","dT3.95_C380","dT7.5_C380","dT0_C416","dT0_C582","dT0_C813") (will be used to calculate feedback and gains from LPX)
	- Data output to: ~/data/n2o_Yunke/final_forcing/eCO2_warming_LPX_total_n2o.csv

5. EF_analysis.R (Emission factor validations, not used in paper)

6. Final code running to produce all figures in paper
- Either use final_MS_code.R
- Or use n2o_MS.Rmd


