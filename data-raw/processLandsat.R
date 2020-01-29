# Process Landsat info for Fdi sites

library(tidyverse)
library(lubridate)

# Read in Landsat indices produced by GEE
# Script path for GEE: https://code.earthengine.google.com/?scriptPath=users%2Fhardygriesbauer%2Ftrial%3AfdiPoints_indices

X<-
  read_csv("data-raw/Pixel_extract_50cl_clMask_20200128_collect_fdiPoints.csv") %>% 

# Data cleaning/wrangling
  drop_na(blue) %>%  # drop all rows that are missing spectral values
  mutate(date=ymd(date)) %>%  # convert to date format

# Create indices
  mutate(ndvi=(nir-red)/(nir+red)) %>% # vegetation and biomass index
  mutate(ndwi=(green-nir)/(green+nir)) %>%  # water body index
  mutate(ndwi2=(green-swir1)/(green+swir1)) %>%  # improved water body index (Xu,2006)
  mutate(ndmi=(nir-swir1)/(nir+swir1)) %>%  # canopy and soil moisture index
  mutate(evi=2.5*((nir-red)/(1+nir+6*red-7.5*blue))) %>% # enhanced vegetation index
  
# rearrange
  dplyr::select(site,date,blue,green,red,nir,swir1,swir2,tir,ndvi,ndwi,ndwi2,
                ndmi,evi,everything()) %>% 
  arrange(desc(site),date)

# save into data-raw folder
# still some processing required to come up with final LS dataset
fdiLS<-X
save(fdiLS,file="data-raw/fdiLS_raw.RData")



