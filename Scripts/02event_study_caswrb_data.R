# Heading -----------------------------------------------------------------
# A script to generate event study plots for both Arsenic and Nitrate.
# sandysum@ucsb.edu
# 2021/10/08

library(sp)
library(tidyverse)
library(readr)
library(readxl)
library(readr)
library(lubridate)
library(ggplot2)
library(kableExtra)
library(tigris)
library(DescTools)
library(Hmisc)
library(lfe)

home <- "../Data/"

# Read data in ------------------------------------------------------------

sys <- read_xlsx(file.path(home, "ca_water_qual/watsys.xlsx")) 
loc <- read_xlsx(file.path(home, "ca_water_qual/siteloc.xlsx")) %>% 
  mutate(STATUS = str_to_upper(STATUS))
# read arsenic and nitrate data from the CA SWRB portal 
ar <- read_rds(file.path(home, "1int/caswrb_ar_1998-2021.rds")) %>% 
  # drop destroyed and wastewater wells
  filter(!(STATUS %in% c('DS', 'WW', 'PN'))) %>%
  mutate(WATER_TYPE = if_else(WATER_TYPE == "g", "G", WATER_TYPE),
         sampleDate = as_date(sampleDate),
         # raw, untreated, monitoring well, and agriculture well considered raw
         raw = if_else(map(str_extract_all(STATUS, "."),2) %in% c('R', 'U', 'W', 'G'), 1, 0),
         countyName = str_to_lower(countyName))

ni <-read_rds(file.path(home, "1int/caswrb_n_1998-2021.rds")) %>% 
  # drop destroyed and wastewater wells
  filter(!(STATUS %in% c('DS', 'WW', 'PN'))) %>%
  mutate(WATER_TYPE = if_else(WATER_TYPE == "g", "G", WATER_TYPE),
         sampleDate = as_date(sampleDate),
         # raw, untreated, monitoring well, and agriculture well considered raw
         raw = if_else(map(str_extract_all(STATUS, "."),2) %in% c('R', 'U', 'W', 'G'), 1, 0),
         countyName = str_to_lower(countyName))

# For Ar and Ni, generate event study coefficients

glimpse(ar)
ar_mod <- ar %>% 
  mutate()