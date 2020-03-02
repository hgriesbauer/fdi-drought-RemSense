# Process Landsat info for Fdi sites

library(tidyverse)
library(lubridate)

# Read in Landsat indices produced by GEE
# Script path for GEE: https://code.earthengine.google.com/?scriptPath=users%2Fhardygriesbauer%2Ftrial%3AfdiPoints_indices

X<-
  read_csv("data-raw/Pixel_extract_50cl_clMask_20200218_collect_fdiPoints.csv") %>% 

# Data cleaning/wrangling
  drop_na(blue) %>%  # drop all rows that are missing spectral values
  mutate(date=ymd(date)) %>%  # convert to date format
  
# Screen for outliers
  filter(blue<1.5) %>% # took a look at extreme outliers in boxplot
  filter(green<1.5) %>% # took a look at extreme outliers in boxplot
  filter(nir<1.5) %>% # took a look at extreme outliers in boxplot
  filter(swir1<1.5) %>% # took a look at extreme outliers in boxplot
  
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

# Create monthly indices
# Using max or min
fdiMonthLS<-
  fdiLS %>% 
  mutate(month=lubridate::month(date)) %>% 
  mutate(year=lubridate::year(date)) %>% 
  filter(year<=2007) %>% 
  group_by(site,year,month) %>% 
  summarise(ndvi=max(ndvi,na.rm=TRUE),
            ndwi=min(ndwi,na.rm=TRUE),
            ndwi2=min(ndwi2,na.rm=TRUE),
            evi=max(evi,na.rm=TRUE),
            ndmi=max(ndmi,na.rm=TRUE))

save(fdiMonthLS,file=here::here("data","fdiMonthLS.RData"))
            
            
                     
  
  





