# Heading -----------------------------------------------------------------
# A script to read in PWSs shapefiles
# sandysum@ucsb.edu
# 2021/09/17

library(tidyverse)
library(sf)
library(tigris)
library(rgeos)
library(cowplot)
library(lubridate)

# Read in datasets --------------------------------------------------------

home <- "/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/Data/"
pws <- read_sf("../Data/SABL_Public_080221/SABL_Public_080221.shp")
cv_counties <-  c('Butte', 'Colusa', 'Glenn', 'Fresno', 'Kern', 'Kings', 'Madera', 'Merced', 'Placer', 'San Joaquin', 'Sacramento', 'Shasta', 'Solano', 'Stanislaus', 'Sutter', 'Tehama', 'Tulare', 'Yolo', 'Yuba') %>% str_to_lower()
ca <- counties(state = "ca") %>% st_simplify(dTolerance = 10)
object.size(pws)

# really big shapefile 

# try to make it smaller
pwss <- pws %>% st_simplify(dTolerance = 50)
rm(pws)
# check if we have all the PWSs in the 
ar <- read_rds(file.path(home, "1int/caswrb_ar_1998-2021.rds")) %>% 
  # drop destroyed and wastewater wells
  filter(!(STATUS %in% c('DS', 'WW', 'PN'))) %>%
  mutate(WATER_TYPE = if_else(WATER_TYPE == "g", "G", WATER_TYPE),
         sampleDate = as_date(sampleDate),
         # raw, untreated, monitoring well, and agriculture well considered raw
         raw = if_else(map(str_extract_all(STATUS, "."),2) %in% c('R', 'U', 'W', 'G'), 1, 0))

ni <-read_rds(file.path(home, "1int/caswrb_n_1998-2021.rds")) %>% 
  # drop destroyed and wastewater wells
  filter(!(STATUS %in% c('DS', 'WW', 'PN'))) %>%
  mutate(WATER_TYPE = if_else(WATER_TYPE == "g", "G", WATER_TYPE),
         sampleDate = as_date(sampleDate),
         # raw, untreated, monitoring well, and agriculture well considered raw
         raw = if_else(map(str_extract_all(STATUS, "."),2) %in% c('R', 'U', 'W', 'G'), 1, 0))

caswrb_mon_names <- paste0("CA", ar$SYSTEM_NO %>% unique())

in_pwss <- caswrb_mon_names[which(caswrb_mon_names%in% pwss$SABL_PWSID)]

# only 3787 of them are in the shapefile though.

ar %>% filter(SYSTEM_NO %in% (in_pwss %>% str_extract("\\d+")))
ar %>% filter(!(SYSTEM_NO %in% (in_pwss %>% str_extract("\\d+"))))

# do a quick analysis to check if PWS not in the shapefile has higher ar values

ar <- ar %>% mutate(has_shapefile = SYSTEM_NO %in% (in_pwss %>% str_extract("\\d+")))

# Explore Ar by PWS

ar_my48 <- ar %>% 
  # filter only to groundwater
  filter(WATER_TYPE=="G", year %in% 2005:2009) %>%
  mutate(month = month(sampleDate),
         SABL_PWSID = paste0('CA', SYSTEM_NO),
         cv = if_else(countyName %in% cv_counties, 1, 0)) %>% 
  group_by(SYSTEM_NO, countyName, cv, SABL_PWSID, raw) %>%
  summarise(median_ar = median(ar_ugl, na.rm = TRUE),
            mean_ar = mean(ar_ugl, na.rm = TRUE)) %>% 
  ungroup() %>% mutate(ar2006 = case_when(
    mean_ar <= 5 ~ 'low',
    mean_ar > 5 & mean_ar < 10 ~ 'med',
    mean_ar >= 10 ~ 'high',
    TRUE ~ NA_character_
  ),
  ar2006 = factor(ar2006, levels = c('low', 'med', 'high'))) 

ar_2006_categories <- ar_my48 %>% mutate(
  ar2006 = case_when(
    mean_ar <= 5 ~ 'low',
    mean_ar > 5 & mean_ar <= 10 ~ 'med',
    mean_ar > 10 ~ 'high',
    TRUE ~ NA_character_
  )
) %>%
  select(c(1, 2), ar2006)

saveRDS(ar_2006_categories, file = '../Data/ca_water_qual/ar_2006_levels.rds')

pws_ar <- pwss %>% left_join(ar_my48 %>% filter(raw==1)) %>% 
  mutate(mean_ar = Winsorize(mean_ar, na.rm = TRUE)) %>% 
  left_join(dww, c('SABL_PWSID' = 'WaterSystemNo'))

pl<-ggplot() + geom_sf(data = pws_ar,
                   # color = "black",
                   aes(fill = factor(ar2006)),
                   size = 0.1) +
  geom_sf(data = st_geometry(ca), size = 0.1, color = 'red', alpha = 0) +
  theme_light() 
+
  scale_fill_continuous(type = "viridis")

save_plot("Plots/ar_pws_2017_2021_counties.png", pl, base_asp = .8, scale = 2.5)



# Explore patterns of higher Arsenic --------------------------------------

ar_my_all <- ar %>% 
  # filter only to groundwater
  filter(WATER_TYPE=="G") %>%
  mutate(month = month(sampleDate),
         SABL_PWSID = paste0('CA', SYSTEM_NO),
         cv = if_else(countyName %in% cv_counties, 1, 0)) %>% 
  group_by(SYSTEM_NO, countyName, cv, SABL_PWSID, raw) %>%
  summarise(median_ar = median(ar_ugl, na.rm = TRUE),
            mean_ar = mean(ar_ugl, na.rm = TRUE)) %>% 
  ungroup() %>% mutate(ar_level = case_when(
    mean_ar <= 5 ~ 'low',
    mean_ar > 5 & mean_ar < 10 ~ 'med',
    mean_ar >= 10 ~ 'high',
    TRUE ~ NA_character_
  ),
  ar_level = factor(ar_level, levels = c('low', 'med', 'high')))

pws_ar <- pwss %>% left_join(ar_my_all %>% filter(raw==0)) %>% 
  mutate(mean_ar = Winsorize(mean_ar, na.rm = TRUE)) %>% 
  left_join(dww, c('SABL_PWSID' = 'WaterSystemNo'))

pl <- ggplot() + geom_sf(data = pws_ar,
                       color = "black",
                       aes(fill = mean_ar),
                       size = 0.1) +
  geom_sf(data = st_geometry(ca), size = 0.1, color = 'red', alpha = 0) +
  theme_light() +
  scale_fill_continuous(type = "viridis")

save_plot("Plots/ar_pws_all_counties.png", pl, base_asp = .8, scale = 2.5)

# any correlation with size?

ggplot(pws_ar, aes(POPULATION, mean_ar)) +
  geom_point() +
  theme_minimal_vgrid()

ggplot(pws_ar, aes(POPULATION, mean_ar)) +
  geom_point() +
  theme_minimal_vgrid() +
  lims(x = c(0, 500000))

ggplot(pws_ar, aes(POPULATION, mean_ar)) +
  geom_point() +
  theme_minimal_vgrid() +
  lims(x = c(0, 5000))
