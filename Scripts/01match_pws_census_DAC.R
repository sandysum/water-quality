#################################
# This script downloads census tract level social equality indicators from the 2015-2019 ACS
# It matches these indicators to PWS level
# There are more PWSs than census tract so many PWS get the same value
# 2022/01/04
# sandysum@ucsb.edu
#################################

rm(list = ls())
home <- "/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/Data"
# home <- "G:/My Drive/0Projects/1Water/2Quality/Data"

# set up census api key: only required for first time

# census_api_key("f2fc44567f6b5fdeede7ead43f70f3853ccb8508", overwrite = TRUE ,install = TRUE)

# load package

library(tidyverse)
library(tidycensus)
library(readxl)
library(readr)
library(lfe)
library(sf)
library(sp)

# Match PWS ZIPS to census tract and disadvantaged community --------------
n_reg <- readRDS("../Data/1int/caswrb_n_reg.rds")
ca_crs <- 3488
sys <- read_xlsx(file.path(home, "ca_water_qual/watsys.xlsx")) 
pws_shp <- read_sf("../Data/shp_PWS_SABL_Public_080221/SABL_Public_080221.shp") %>% 
  st_transform(ca_crs)
# luckily I have ensured that this dataset of pws_zip contains all the zip shape files of all the pws that ever appeared in the monitoring dataset 

pws_zip <- read_sf("../Data/shp_PWS_SABL_Public_080221/PWS_by_zip.shp") %>% 
  st_transform(ca_crs)
loc <- read_xlsx(file.path(home, "ca_water_qual/siteloc.xlsx")) %>% 
  mutate(STATUS = str_to_upper(STATUS))
# This crosswalk is a horrible aggregation to match PWS to census tract
# today to start the matching of PWS to census tract by spatial overlaps

# cw <- read_xlsx(file.path(home, "shp_PWS_SABL_Public_080221/ZIP_TRACT_cross.xlsx")) %>%
#   filter(USPS_ZIP_PREF_STATE == 'CA')

# # Investigate crosswalk or lowest common denominator in PWS / trac --------
# 
# zip_to_pws <- sys %>% group_by(ZIP) %>% 
#   summarise(n_pws = unique(SYSTEM_NO) %>% length()) %>% 
#   arrange(desc(n_pws))
# 
# pws_to_zip <- sys %>% group_by(SYSTEM_NO) %>% 
#   summarise(n_zip = unique(ZIP) %>% length()) %>% 
#   arrange(desc(n_zip))
# 
# # 2450 ZIP CODES that match to 1 to 35 individual census tracts
# 
# zip_to_tract <- sys %>% group_by(ZIP) %>% 
#   summarise(n_tracts = unique(TRACT) %>% length())

# Download and clean census tract data for socio-economic 

ca_stats <- get_acs(
  geography = "tract",
  # Median household income in the past 12 months (in 2019 inflation-adjusted dollars)
  # https://api.census.gov/data/2019/acs/acs5/groups/B19013.html
  # median income, total population, total hispanic or latino, total white
  variables = c("B19013_001", "B01003_001", "B03001_003E", "B02001_002"),
  state = "CA", 
  geometry = TRUE
)

# I am not sure if the variable name is correct. Seems like total pop latino and total pop is giving me the same thing 2
ca_stats <- ca_stats %>% mutate(variable_nm = case_when(
  variable == "B19013_001" ~ "median_hh_income",
  variable == "B01003_001" ~ "total_pop",
  variable == "B03001_003" ~ "total_pop_latino",
  variable == "B02001_002" ~ "total_pop_white",
  TRUE ~ NA_character_
)) %>% select(-moe, -variable)

# generating some social equality metrics
ca_stats <- ca_stats %>% spread(key = variable_nm, value = estimate)
ca_stats <- ca_stats %>% mutate(percent_non_white = (total_pop - total_pop_white)/total_pop,
                                percent_his = total_pop_latino/total_pop,
                                income_category = cut_number(median_hh_income, 8, labels = paste0('income_percentile', 1:8))) %>% st_transform(ca_crs)

write_sf(ca_stats, "../Data/shp_ej.shp")

# try for one PWS to aggregate census tract

x <- aggregate(ca_stats[c('median_hh_income', 'percent_his', 'total_pop', 'percent_non_white')], by = st_geometry(pws_shp), mean, areaWeighted = TRUE) 

pws_shp <- pws_shp %>% bind_cols(x %>% st_drop_geometry())

pws_shp[,1] %>% st_drop_geometry() %>% duplicated() %>% which()

pws_shp$SABL_PWSID[c(2021, 3274)]
# "CA5510009" "CA1010007" duplicated shapefiles!
dups <- pws_shp %>% filter(SABL_PWSID %in% pws_shp$SABL_PWSID[c(2021, 3274)])

# just select row 1 and 3
merge_dups <- dups[c(1,3),]

pws_shp <- pws_shp %>% filter(!(SABL_PWSID %in% c("CA5510009", "CA1010007"))) %>% 
  bind_rows(merge_dups)

class(pws_shp)
write_sf(pws_shp, "../Data/shp_PWS_SABL_Public_080221/shp_Pws")

sys_with_ej <- sys %>% left_join(pws_shp %>% st_drop_geometry() %>% 
  mutate(SYSTEM_NO = str_extract(SABL_PWSID, '\\d+'))) %>% 
  filter(SYSTEM_NO %in% str_extract(pws_shp$SABL_PWSID, '\\d+'))

# for those without shapefile use zip

x <- aggregate(ca_stats[c('median_hh_income', 'percent_his', 'total_pop', 'percent_non_white')], by = st_geometry(pws_zip), mean, areaWeighted = TRUE) 

# # plot relationship between percent hispanic and income
# 
# ggplot(data = ca_stats, aes(percent_non_white, median_hh_income)) +
#   geom_point() +
#   theme_minimal() +
#   geom_smooth()
# 
# # plot income categories
# quartz()
# ggplot(data = ca_stats, aes(fill = income_category)) +
#   geom_sf() +
#   scale_fill_brewer(palette = 'YlGnBu')
# 
# # plot percent latino/hispanic
# quartz()
# ggplot(data = ca_stats, aes(fill = percent_his)) +
#   geom_sf() 

# Join PWS system information to cross walk to ID GEOID
# there are some overlaps -- a lot of PWS have similar census tract

# Now: to match PWS to census tract... to compute overlap by areas.
