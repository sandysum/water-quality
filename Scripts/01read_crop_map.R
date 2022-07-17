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
setwd(home)
library(stringr)
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

# decided to compute % ag area by crop type as given in the shape files
# have to then join all of them together.
# do this for all together and then individually.

crop_codes = c('G', 'R', 'F', 'P', 'T', 'D', 'C', 'V', 'S')

for (x in crop_codes) {
# Drop all polygons that are not ag land code
crop_only <- crop %>% filter(CLASS2 %in% x)

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
  dplyr::select(SYSTEM_NO = PWSID, SHAPE_AREA) %>% 
  left_join(int_zip, c('SYSTEM_NO' = 'PWSID')) %>% 
  mutate(agArea = if_else(is.na(agArea), 0, agArea),
         !!paste0("percent_ag_", x) := agArea/SHAPE_AREA) %>% 
  dplyr::select(-agArea)

pws_ag <- pws %>% 
  mutate(SHAPE_AREA = st_area(.) %>% as.vector()) %>% 
  as_tibble() %>% 
  dplyr::select(SYSTEM_NO = SABL_PWSID, SHAPE_AREA) %>% 
  left_join(int, c('SYSTEM_NO' = 'SABL_PWSID')) %>% 
  mutate(agArea = if_else(is.na(agArea), 0, agArea),
         !!paste0("percent_ag_", x) := agArea/SHAPE_AREA) %>% 
  dplyr::select(-agArea)
  
pws_ag_all <- bind_rows(pws_ag, pws_zip_ag) %>% 
  distinct(SYSTEM_NO, .keep_all = TRUE)

saveRDS(pws_ag_all, file.path(home,paste0("../Data/1int/pws_area", x, ".rds")))
}

# this part of the strip reads in all of the files and join them together into one dataset.
main  <- readRDS("1int/pws_area_ag.rds") %>% 
  rename(percent_ag_all = percAgArea) %>% 
  dplyr::select(-SHAPE_AREA, -agArea)
  
fp <- list.files("1int", full.names = TRUE) %>% str_subset('pws_area[A-Z]')

for (x in fp) {
  df <- readRDS(x) %>% 
    dplyr::select(-SHAPE_AREA)
  main <- main %>% left_join(df)
}

main <- main %>% distinct(SYSTEM_NO, .keep_all = TRUE) %>% 
  mutate(SYSTEM_NO = str_extract(SYSTEM_NO, '\\d+'))

saveRDS(main, file.path(home,paste0("../Data/1int/pws_ag.rds")))

int <- readRDS("../Data/1int/pws_ind.rds") %>% 
  dplyr::select(-matches("percent_ag_"))

int <- int %>% left_join(main)

saveRDS(int, "../Data/1int/pws_ind.rds")


# Do some spatial analysis for checking that area is correctly assigned

# check what crop type correlated with high percentage of percent hispanic.



# select only 3090
pchk <- pws[287,]

cchk <- crop_only_buff %>% st_crop(st_bbox(pchk))

ggplot(pchk) + 
  geom_sf(color = 'blue', size = 1.2) +
  geom_sf(data = cchk, aes(fill = CLASS2), alpha = .7) +
  geom_sf(data = int, color = 'black', size = .7, alpha = .1)

# combine with PWS clay/pH/social: old only, if need to redo and construct ind from scratch.

# lat <- readRDS(file.path(home, "/1int/pws_ind.rds")) %>% distinct(.keep_all = TRUE)
# nrow(lat)
# dups <- lat %>% group_by(SYSTEM_NO) %>%  filter(n()>1)
# 
# pws_info <- read_excel(file.path(home, "/ca_water_qual/siteloc.xlsx"))
# pws_add <- read_excel(file.path(home, "/ca_water_qual/watsys.xlsx"))
# 
# out <- main %>% mutate(SYSTEM_NO = str_extract(SYSTEM_NO, '\\d+')) %>% 
#   left_join(lat) %>% 
#   # left_join(pws_info) %>% 
#   left_join(pws_add) %>% 
#   mutate_at(vars(matches("percent_ag_")), ~(.*100)) %>% 
#   mutate(b_majority_latino = if_else(percent_hispanic >= 50, 1, 0),
#          b_low_income = if_else(median_hh_income <=46000, 1, 0),
#          log_hh_income = log(median_hh_income),
#          b_ag_area = if_else(percent_ag_all >=.4, 1, 0)
#          )
# 
# # combine with meta data and clay
# 
# clay <- readRDS(file.path(home,"/1int/pws_clay_merged.rds"))
# ph <- readRDS(file.path(home,"1int/pws_ph_merged.rds"))
# ownership <- read_csv(file.path(home,"ca_drinkingwatersystems_meta.csv"))
# 
# names(ownership) <- names(ownership) %>% str_remove_all("\\s")
# 
# ownership <-mutate(ownership, SYSTEM_NO = str_extract(WaterSystemNo, '\\d+'))
# 
# ind <- out %>% left_join(ownership) %>% left_join(clay) %>% left_join(ph)
# 
# names(ind) <- names(ind) %>% str_replace("-CODE",'C')
# 
# ind <- ind %>% 
#   # mutate(log_pop = log(TotalPopulation),
#   #                     log_pop_caswrb = log(POP_SERV), 
#   #                     log_hh_income = log(median_hh_income)) %>% 
#   dplyr::select(-PWSID, -matches("nections")) %>% 
#   na_if("#N/A") %>% 
#   distinct(.keep_all = TRUE)
# 
# # dups <- ind %>% group_by(SYSTEM_NO)%>% filter(n()>1)
# #   
# # pws_ag_all %>% group_by(SYSTEM_NO) %>% filter(n()>1)
# 
# saveRDS(ind, file.path(home,"1int/pws_ind.rds"))
# 
# ggplot(out, aes(percent_hispanic, percent_spanish_speaker)) +
#   geom_point() +
#   geom_smooth() +
#   theme_classic()
#   geom_density()
