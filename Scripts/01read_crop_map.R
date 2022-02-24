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

crop <- read_sf("../Data/shp_cropping_2018/i15_Crop_Mapping_2018.shp")

# change everything to a circle with a fixed radius for computing ag land
pws <- read_sf("../Data/shp_PWS_SABL_Public_080221/SABL_Public_080221.shp") %>% 
  filter(SHAPE_AREA > 0, SHAPE_LEN > 0) %>% 
  st_centroid() %>% 
  st_buffer(dist = 1500)

# pws_sf_zip is mutually exclusive of pws_sf
pws_sf_zip <- read_sf("../Data/shp_PWS_SABL_Public_080221/PWS_by_zip.shp") %>%
  st_transform(crs = st_crs(pws)) %>% 
  # filter(SHAPE_AREA > 0, SHAPE_LEN > 0) %>% 
  st_centroid() %>% 
  st_buffer(dist = 1500)

crop_codes = c('G', 'R', 'F', 'P', 'T', 'D', 'C','V', 'S')

# Drop all polygons that are not ag land code
crop_only <- crop %>% filter(CLASS2 %in% crop_codes)

# Clean up crop only 
crop_only_buff <- sf::st_buffer(crop_only, dist = 0)

# this returns the area intersections between all crop polygons and pws polygons. all I need to do is to 
# compute the total areas by summing over all pws similar shapefiles
int <- sf::st_intersection(crop_only_buff, pws) %>% 
  mutate(intersect_area = st_area(.) %>% as.vector()) %>%  
  as_tibble() %>% 
  dplyr::select(SABL_PWSID, intersect_area) %>% 
  ungroup() %>% 
  dplyr::group_by(SABL_PWSID) %>% 
  summarise(agArea = sum(intersect_area))

int_zip <- sf::st_intersection(crop_only_buff, pws_sf_zip) %>% 
mutate(intersect_area = st_area(.) %>% as.vector()) %>%  
  as_tibble() %>% 
  dplyr::select(PWSID, intersect_area) %>% 
  ungroup() %>% 
  dplyr::group_by(PWSID) %>% 
  summarise(agArea = sum(intersect_area))

pws_zip_ag <- pws_sf_zip %>% 
  mutate(SHAPE_AREA = st_area(.) %>% as.vector()) %>% 
  as_tibble() %>% 
  select(SYSTEM_NO = PWSID, SHAPE_AREA) %>% 
  left_join(int_zip, c('SYSTEM_NO' = 'PWSID')) %>% 
  mutate(agArea = if_else(is.na(agArea), 0, agArea),
         percAgArea = agArea/SHAPE_AREA)

pws_ag <- pws %>% 
  mutate(SHAPE_AREA = st_area(.) %>% as.vector()) %>% 
  as_tibble() %>% 
  select(SYSTEM_NO = SABL_PWSID, SHAPE_AREA) %>% 
  left_join(int, c('SYSTEM_NO' = 'SABL_PWSID')) %>% 
  mutate(agArea = if_else(is.na(agArea), 0, agArea),
         percAgArea = agArea/SHAPE_AREA)
  
pws_ag_all <- bind_rows(pws_ag, pws_zip_ag)

saveRDS(pws_ag_all, "../Data/1int/pws_area_ag.rds")

pws_ag_all <- readRDS("../Data/1int/pws_area_ag.rds")
# Do some spatial analysis for checking that area is correctly assigned

# select only 3090
pchk <- pws[287,]

cchk <- crop_only_buff %>% st_crop(st_bbox(pchk))

int <- readRDS("../Data/1int/pws_ind.rds")

ggplot(pchk) + 
  geom_sf(color = 'blue', size = 1.2) +
  geom_sf(data = cchk, aes(fill = CLASS2), alpha = .7) +
  geom_sf(data = int, color = 'black', size = .7, alpha = .1)

# combine with PWS clay/pH/social

lat <- readRDS("../Data/1int/pws_ej_ind.rds")
pws_info <- read_excel("../Data/ca_water_qual/siteloc.xlsx") 
pws_add <- read_excel("../Data/ca_water_qual/watsys.xlsx")

out <- pws_ag_all %>% mutate(SYSTEM_NO = str_extract(SYSTEM_NO, '\\d+')) %>% 
  left_join(lat) %>% 
  # left_join(pws_info) %>% 
  left_join(pws_add) %>% 
  mutate(b_majority_latino = if_else(percent_hispanic >= .5, 1, 0),
         b_low_income = if_else(median_hh_income <=46000, 1, 0),
         log_hh_income = log(median_hh_income),
         b_ag_area = if_else(percAgArea >=.4, 1, 0)
         )

# combine with meta data and clay

clay <- readRDS("../Data/1int/pws_clay_merged.rds")
ph <- readRDS("../Data/1int/pws_ph_merged.rds")
ownership <- read_csv("../Data/ca_drinkingwatersystems_meta.csv")

names(ownership) <- names(ownership) %>% str_remove_all("\\s")

ownership <-mutate(ownership, SYSTEM_NO = str_extract(WaterSystemNo, '\\d+'))

ind <- out %>% left_join(ownership) %>% left_join(clay) %>% left_join(ph)

names(ind) <- names(ind) %>% str_replace("-CODE",'C')

ind <- ind %>% mutate(logpop = log(TotalPopulation),
                      log_hh_income = log(median_hh_income),
                      percent_latino = percent_hispanic*100)

saveRDS(ind, "../Data/1int/pws_ind.rds")

ggplot(out, aes(percAgArea, percent_hispanic)) +
  geom_point() +
  geom_smooth() +
  theme_classic()
