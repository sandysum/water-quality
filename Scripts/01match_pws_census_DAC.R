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


# Run some correlational analysis of social indicators --------------------

ar_reg <- read_rds("../Data/1int/caswrb_ar_reg.rds")
ni_reg <- read_rds("../Data/1int/caswrb_n_reg.rds")

# Q1 are DAC getting worst raw water?
ar_pws_10yr <- ar_reg %>% filter(raw==1, year %in% 2010:2020) %>% 
  group_by(SYSTEM_NO, CITY, ZIP, POP_SERV) %>% 
  summarize(pws_ar_mean = mean(mean_ar, na.rm = TRUE),
            pws_ar_median = median(mean_ar, na.rm = TRUE)) %>% 
  left_join(sys %>% select(SYSTEM_NO, TRACT, matches('USPS'), NAME, median_hh_income, 
                           matches('total_'), matches('percent_'), income_category)) %>% 
  ungroup() %>% 
  mutate(pws_ar_mean = Winsorize(pws_ar_mean, probs = c(0, 0.99)),
         log_hh_income = log(median_hh_income)) %>% 
  group_by(TRACT) %>% 
  mutate(n_pws = 1/n())

ar_pws_10yr %>% ggplot(aes(percent_his, pws_ar_median)) +
  geom_point(alpha = .5) +
  geom_smooth() +
  theme_minimal()

mod_ar_ej_income <- felm(pws_ar_median ~ log_hh_income | 0 | 0 | TRACT, data = ar_pws_10yr, weights = ar_pws_10yr$n_pws)

summary(mod_ar_ej_income)

mod_ar_ej_percent_his <- felm(pws_ar_median ~ percent_his | 0 | 0 | TRACT, data = ar_pws_10yr, weights = ar_pws_10yr$n_pws)

summary(mod_ar_ej_percent_his)

# Q1 are DAC getting worst raw water?
n_pws_10yr <- ni_reg %>% filter(raw==1, year %in% 2010:2020) %>% 
  group_by(SYSTEM_NO, CITY, ZIP, POP_SERV) %>% 
  summarize(pws_n_mean = mean(mean_n, na.rm = TRUE),
            pws_n_median = median(mean_n, na.rm = TRUE)) %>% 
  left_join(sys %>% select(SYSTEM_NO, TRACT, matches('USPS'), NAME, median_hh_income, 
                           matches('total_'), matches('percent_'), income_category)) %>% 
  ungroup() %>% 
  mutate(pws_n_mean = Winsorize(pws_n_mean, probs = c(0, 0.99)),
         log_hh_income = log(median_hh_income)) %>% 
  group_by(TRACT) %>% 
  mutate(n_pws = 1/n())

n_pws_10yr %>% ungroup() %>% ggplot(aes(percent_his, pws_n_median)) +
  geom_point(alpha = .5) +
  geom_smooth() +
  theme_minimal()

mod_n_ej_income <- felm(pws_n_median ~ log_hh_income | 0 | 0 | TRACT, data = n_pws_10yr, weights = n_pws_10yr$n_pws)

summary(mod_n_ej_income)

mod_n_ej_percent_his <- felm(pws_n_median ~ percent_his | 0 | 0 | TRACT, data = n_pws_10yr, weights = n_pws_10yr$n_pws)

summary(mod_n_ej_percent_his)

stargazer::stargazer(mod_ar_ej_income, mod_ar_ej_percent_his,
                     mod_n_ej_income, mod_n_ej_percent_his, 
                     )