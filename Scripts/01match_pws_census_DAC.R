#################################
# This script downloads census tract level social equality indicators from the 2015-2019 ACS
# It matches these indicators to PWS level
# There are more PWSs than census tract so many PWS get the same value
# 2021/12/18
# sandysum@ucsb.edu
#################################

rm(list = ls())
home <- "/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/Data"
# home <- "G:/My Drive/0Projects/1Water/2Quality/Data"
source("Scripts/helper_functions_models.R")

# set up census api key: only required for first time

# census_api_key("f2fc44567f6b5fdeede7ead43f70f3853ccb8508", overwrite = TRUE ,install = TRUE)

# load package

library(tidyverse)
library(tidycensus)
library(readxl)
library(readr)
library(lfe)
library(sf)

# Match PWS ZIPS to census tract and disadvantaged community --------------

sys <- read_xlsx(file.path(home, "ca_water_qual/watsys.xlsx")) 
loc <- read_xlsx(file.path(home, "ca_water_qual/siteloc.xlsx")) %>% 
  mutate(STATUS = str_to_upper(STATUS))
cw <- read_xlsx(file.path(home, "shp_PWS_SABL_Public_080221/ZIP_TRACT_cross.xlsx"))
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

# what is the median family income in california?

# generating some social equality metrics
ca_stats <- ca_stats %>% spread(key = variable_nm, value = estimate)
ca_stats <- ca_stats %>% mutate(percent_non_white = (total_pop - total_pop_white)/total_pop,
                                percent_his = total_pop_latino/total_pop,
                                income_category = cut_number(median_hh_income, 8, labels = paste0('income_percentile', 1:8)))

# plot income categories
quartz()
ggplot(data = ca_stats, aes(fill = income_category)) +
  geom_sf() +
  scale_fill_brewer(palette = 'YlGnBu')

# plot percent latino/hispanic
quartz()
ggplot(data = ca_stats, aes(fill = percent_his)) +
  geom_sf() 

# Join PWS system information to cross walk to ID GEOID
# there are some overlaps -- a lot of PWS have similar census tract

sys <- sys %>% left_join(cw, by ="ZIP") %>% 
  left_join(ca_stats %>% st_drop_geometry(), by = c('TRACT'='GEOID'))

write.csv(sys, file.path(home, "ca_water_qual/watsys_tract_socialeq_ind.csv")) 

# 1485 is not in the census tract
(!(sys$TRACT %in% ca_income$GEOID)) %>% sum()