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
ar <- read_rds(file.path(home, "1int/caswrb_ar_1974-2021.rds")) 

ni <-read_rds(file.path(home, "1int/caswrb_n_1998-2021.rds")) %>% 
  # drop destroyed and wastewater wells
  filter(!(STATUS %in% c('DS', 'WW', 'PN'))) %>%
  mutate(WATER_TYPE = if_else(WATER_TYPE == "g", "G", WATER_TYPE),
         sampleDate = as_date(sampleDate),
         # raw, untreated, monitoring well, and agriculture well considered raw
         raw = if_else(map(str_extract_all(STATUS, "."),2) %in% c('R', 'U', 'W', 'G'), 1, 0),
         countyName = str_to_lower(countyName))


# Generate event study coefficients: Arsenic ------------------------------

glimpse(ar)
ar_mod <- ar %>%
  # look from 1991 and only at raw sources from groundwater
  # important!!!
  filter(year >= 1994, raw == 1, WATER_TYPE=='G') %>% 
  mutate(
    td = str_extract(sampleTime, "\\d{2}") %>% as.numeric()
    %>% if_else((. == 44 | . == 80), NA_real_, .),
    dy = yday(sampleDate),
    year = factor(year),
    ar_ugl = Winsorize(ar_ugl, probs = c(0,.99))
  ) 

# drop rows with no time of sample
ar_mod <- ar_mod %>% tidyr::drop_na(td, dy)

# create polynomials
poly_td <- poly(ar_mod$td, degree = 3) %>% as_tibble()
names(poly_td) <- paste0('td_', 1:3)
poly_dy <- poly(ar_mod$dy, degree = 3) %>% as_tibble()
names(poly_dy) <- paste0('dy_', 1:3)

ar_mod <- ar_mod %>% bind_cols(poly_td, poly_dy)

es_ar <- felm(ar_ugl ~ year + td_1 + td_2 + td_3 + dy_1 + dy_2 + dy_3 | SYSTEM_NO | 0 | ZIP,
              data = ar_mod)
quartz()
plot_es(es_ar, ar_mod, what = 'year')
