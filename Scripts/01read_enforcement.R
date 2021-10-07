#################################
# Read and clean SDWA water violations data
# 
# 5/7/2021
# sandysum@ucsb.edu
#################################

rm(list = ls())
home <- "/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/Data"

# load package

library(tidyverse)
library(readxl)
library(readr)

pws <- read_csv(file.path(home, "/SDWA_downloads/SDWA_PUB_WATER_SYSTEMS.csv")) %>% filter(STATE == "CA")

violations <- read_csv(file.path(home, "/SDWA_downloads/SDWA_VIOLATIONS.csv"))
c_nm <- read_xlsx(file.path(home, "ca_water_qual/county_nms.xlsx"))
loc <- read_xlsx(file.path(home, "ca_water_qual/siteloc.xlsx")) %>%
  mutate(NUMBER = as.numeric(COUNTY),
         WATER_TYPE = str_to_lower(WATER_TYPE)) %>%
  dplyr::select(-COUNTY) %>%
  left_join(c_nm) %>%
  mutate(COUNTY = str_to_upper(COUNTY)) %>%
  group_by(SYSTEM_NO) %>%
  summarise(water_type = WATER_TYPE[1],
            station = STATION_TY[1],
            county = COUNTY[1]) 

pws_count <- pws %>% group_by(PWSID, SOURCE_WATER) %>% summarise(count = n()) 
pws %>% group_by(PWSID) %>% summarise(count = n()) 

vio <- violations %>% 
  filter(STATE=="CA" & RULE_NAME == "Nitrates" & HEALTH_BASED == "Y") %>% 
  mutate(SYSTEM_NO = str_extract(PWSID, "(?<=CA)\\d+")) %>% 
  left_join(loc) 
# %>% 
#   left_join(blue %>% select(-county), by = c('county' = 'COUNTY', 'FISCAL_YEAR' ='year'))

# vio2020 <- vio %>% 
#   filter(FISCAL_YEAR == 2020) %>% 
#   group_by(PWSID) %>% 
#   summarize(number_vio = n(),
#             served = POPULATION_SERVED_COUNT[1])

# coding names of infant deaths not in violations data
# I think we should include these in the dataset and be conscious of the missing values/data
unique(blue$COUNTY)[which(!(unique(blue$COUNTY) %in% unique(vio$county)))]

sum_vio <- vio %>% group_by(county, FISCAL_YEAR, VIOLATION_NAME) %>% 
  summarise(mean_inf_deaths = mean(death_counts, na.rm = TRUE), total_violations = n())

ggplot(sum_vio, aes(x = total_violations, mean_inf_deaths, color = VIOLATION_NAME)) +
  geom_point() +
  theme_minimal()

#### 7/6/2021 Exploration

vio %>% group_by(water_type) %>% summarise(n_violations = n())
  

