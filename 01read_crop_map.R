# Read in crop map data and aggregate it to the PWS level
# sandysum@ucsb.edu
# 2022/02/06
library(raster)
library(rgdal)
library(sp)
library(future.apply)
library(tidyverse)
library(readr)
library(readxl)
library(readr)
library(lubridate)
library(ggplot2)
library(DescTools)
library(sf)
library(lfe)

crop <- read_sf("../Data/shp_cropping_2018/i15_Crop_Mapping_2018.shp")
pws <- read_sf("../Data/shp_PWS_SABL_Public_080221/SABL_Public_080221.shp") %>% filter(SHAPE_AREA > 0, SHAPE_LEN > 0) %>% st_geometry()
# pws_sf_zip is mutually exclusive of pws_sf
pws_sf_zip <- read_sf("../Data/shp_PWS_SABL_Public_080221/PWS_by_zip.shp") %>% st_geometry()
# pws_sub <- st_geometry(pws[1:3,])

crop_sim_SB <- crop_sim %>% filter(COUNTY == 'Santa Barbara')

crop_sim_SB %>% ggplot() +
  geom_sf(aes(fill = CLASS2), size = .1) +
  theme_minimal()
