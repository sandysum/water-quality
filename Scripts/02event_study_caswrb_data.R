# Heading -----------------------------------------------------------------
# A script to generate event study plots for both Arsenic and Nitrate.
# sandysum@ucsb.edu
# 2022/01/06

library(tidyverse)
library(readr)
library(readxl)
library(readr)
library(lubridate)
library(ggplot2)
library(DescTools)
library(cowplot)
library(lfe)
source("Scripts/helper_functions_models.R")

home <- "../Data/"

# Read data in ------------------------------------------------------------

# read arsenic and nitrate data from the CA SWRB portal 
ar <- read_rds(file.path(home, "1int/caswrb_ar_1974-2021.rds")) 

ni <-read_rds(file.path(home, "1int/caswrb_n_1974-2021.rds")) 

# Generate event study coefficients: Arsenic ------------------------------

glimpse(ar)

# 1. Raw groundwater
ar_mod <- ar %>%
  mutate(ar_ugl = Winsorize(ar_ugl, probs = c(0,.99))) %>% 
  # look from 1991 and only at raw sources from groundwater
  # important!!!
  filter(year >= 1995, raw == 1, WATER_TYPE=='G') %>% 
  mutate(
    td = str_extract(sampleTime, "\\d{2}") %>% as.numeric()
    %>% if_else((. == 44 | . == 80), NA_real_, .),
    dy = yday(sampleDate),
    year = factor(year)
  ) 

# drop rows with no time of sample
ar_mod <- ar_mod %>% tidyr::drop_na(td, dy)

# create polynomials
poly_td <- poly(ar_mod$td, degree = 3) %>% as_tibble()
names(poly_td) <- paste0('td_', 1:3)
poly_dy <- poly(ar_mod$dy, degree = 3) %>% as_tibble()
names(poly_dy) <- paste0('dy_', 1:3)

ar_mod <- ar_mod %>% bind_cols(poly_td, poly_dy)

es_ar <- felm(ar_ugl ~ year + td_1 + td_2 + td_3 + dy_1 + dy_2 + dy_3 | samplePointID | 0 | SYSTEM_NO,
              data = ar_mod)

p1 <- plot_es(es_ar, ar_mod, contaminant = 'ar', 
              main  = "Raw groundwater sources arsenic trends, 1995-2021", ylm = c(3,5))

save_plot("Plots/ES_raw_gw_ar_95-21.png", p1, base_asp = 2.5, scale = 1.2)

# 2. Raw surface water
ar_mod <- ar %>%
  mutate(ar_ugl = Winsorize(ar_ugl, probs = c(0,.99))) %>% 
  # look from 1991 and only at raw sources from groundwater
  # important!!!
  filter(year >= 1995, raw == 1, WATER_TYPE=='S') %>% 
  mutate(
    td = str_extract(sampleTime, "\\d{2}") %>% as.numeric()
    %>% if_else((. == 44 | . == 80), NA_real_, .),
    dy = yday(sampleDate),
    year = factor(year)
  ) 

# drop rows with no time of sample
ar_mod <- ar_mod %>% tidyr::drop_na(td, dy)

# create polynomials
poly_td <- poly(ar_mod$td, degree = 3) %>% as_tibble()
names(poly_td) <- paste0('td_', 1:3)
poly_dy <- poly(ar_mod$dy, degree = 3) %>% as_tibble()
names(poly_dy) <- paste0('dy_', 1:3)

ar_mod <- ar_mod %>% bind_cols(poly_td, poly_dy)

es_ar <- felm(ar_ugl ~ year + td_1 + td_2 + td_3 + dy_1 + dy_2 + dy_3 | samplePointID | 0 | SYSTEM_NO,
              data = ar_mod)
p1 <- plot_es(es_ar, ar_mod, contaminant = 'ar', ylm=c(1,3.5),
              main  = "Raw surface sources arsenic trends, 1995-2021")

save_plot("Plots/ES_raw_s_ar_95-21.png", p1, base_asp = 2.5, scale = 1.2)

# 3. Treated water
ar_mod <- ar %>%
  mutate(ar_ugl = Winsorize(ar_ugl, probs = c(0,.99))) %>%
  filter(year >= 1996, raw == 0) %>% 
  mutate(
    td = str_extract(sampleTime, "\\d{2}") %>% as.numeric()
    %>% if_else((. == 44 | . == 80), NA_real_, .),
    dy = yday(sampleDate),
    year = factor(year)
  ) 

# drop rows with no time of sample
ar_mod <- ar_mod %>% tidyr::drop_na(td, dy)

# create polynomials
poly_td <- poly(ar_mod$td, degree = 3) %>% as_tibble()
names(poly_td) <- paste0('td_', 1:3)
poly_dy <- poly(ar_mod$dy, degree = 3) %>% as_tibble()
names(poly_dy) <- paste0('dy_', 1:3)

ar_mod <- ar_mod %>% bind_cols(poly_td, poly_dy)

es_ar <- felm(ar_ugl ~ year + td_1 + td_2 + td_3 + dy_1 + dy_2 + dy_3 | samplePointID | 0 | SYSTEM_NO,
              data = ar_mod)

p1 <- plot_es(es_ar, ar_mod, contaminant = 'ar', ylm = c(0,6),
              main  = "Treated arsenic trends, 1995-2021")

save_plot("Plots/ES_treated_ar_95-21.png", p1, base_asp = 2.5, scale = 1.2)

# Generate event study coefficients: Nitrate ------------------------------

glimpse(ni)

# ni <- ni %>% group_by_all() %>% filter(n()==1)

# 1. Raw groundwater
ni_mod <- ni %>%
  mutate(n_mgl = Winsorize(n_mgl, probs = c(0, .99))) %>%
  filter(raw == 1, WATER_TYPE == 'G',
         year >= 1991) %>%
  mutate(
    td = str_extract(sampleTime, "\\d{2}") %>% as.numeric()
    %>% if_else((. == 44 | . == 80), NA_real_, .),
    dy = yday(sampleDate),
    year = factor(year),
    n() == 1 # tells me if this is a duplicate
  ) 

# drop rows with no time of sample
ni_mod <- ni_mod %>% tidyr::drop_na(td, dy)

# create polynomials
poly_td <- poly(ni_mod$td, degree = 3) %>% as_tibble()
names(poly_td) <- paste0('td_', 1:3)
poly_dy <- poly(ni_mod$dy, degree = 3) %>% as_tibble()
names(poly_dy) <- paste0('dy_', 1:3)

ni_mod <- ni_mod %>% bind_cols(poly_td, poly_dy)

es_ni <- felm(n_mgl ~ year + td_1 + td_2 + td_3 + dy_1 + dy_2 + dy_3 | samplePointID | 0 | SYSTEM_NO,
              data = ni_mod)

p1 <- plot_es(es_ni, ni_mod, contaminant = 'n', 
              main  = "Raw groundwater sources nitrate trends, 1991-2021", ylm = c(3,6))

save_plot("Plots/ES_raw_gw_ni_91-21.png", p1, base_asp = 2.5, scale = 1.2)

# 2. Raw surface water
ni_mod <- ni %>%
  mutate(n_mgl = Winsorize(n_mgl, probs = c(0, .99))) %>%
  filter(year >= 1991, raw == 1, WATER_TYPE=='S') %>% 
  mutate(
    td = str_extract(sampleTime, "\\d{2}") %>% as.numeric()
    %>% if_else((. == 44 | . == 80), NA_real_, .),
    dy = yday(sampleDate),
    year = factor(year)
  ) 

# drop rows with no time of sample
ni_mod <- ni_mod %>% tidyr::drop_na(td, dy)

# create polynomials
poly_td <- poly(ni_mod$td, degree = 3) %>% as_tibble()
names(poly_td) <- paste0('td_', 1:3)
poly_dy <- poly(ni_mod$dy, degree = 3) %>% as_tibble()
names(poly_dy) <- paste0('dy_', 1:3)

ni_mod <- ni_mod %>% bind_cols(poly_td, poly_dy)

es_ni <- felm(n_mgl ~ year + td_1 + td_2 + td_3 + dy_1 + dy_2 + dy_3 | samplePointID | 0 | SYSTEM_NO,
              data = ni_mod)

p1 <- plot_es(es_ni, ni_mod, contaminant = 'n', 
              main  = "Raw surface sources nitrate trends, 1991-2021",
              ylm = c(-.2, 1.2))

save_plot("Plots/ES_raw_s_ni_91-21.png", p1, base_asp = 2.5, scale = 1.2)

# 3. Treated water
ni_mod <- ni %>%
  mutate(n_mgl = Winsorize(n_mgl, probs = c(0, .99))) %>%
  filter(year >= 1991, raw == 0) %>% 
  mutate(
    td = str_extract(sampleTime, "\\d{2}") %>% as.numeric()
    %>% if_else((. == 44 | . == 80), NA_real_, .),
    dy = yday(sampleDate),
    year = factor(year)
  ) 

# drop rows with no time of sample
ni_mod <- ni_mod %>% tidyr::drop_na(td, dy)

# create polynomials
poly_td <- poly(ni_mod$td, degree = 3) %>% as_tibble()
names(poly_td) <- paste0('td_', 1:3)
poly_dy <- poly(ni_mod$dy, degree = 3) %>% as_tibble()
names(poly_dy) <- paste0('dy_', 1:3)

ni_mod <- ni_mod %>% bind_cols(poly_td, poly_dy)

es_ni <- felm(n_mgl ~ year + td_1 + td_2 + td_3 + dy_1 + dy_2 + dy_3 | samplePointID | 0 | SYSTEM_NO,
              data = ni_mod)

p1 <- plot_es(es_ni, ni_mod, contaminant = 'n', 
              main  = "Treated nitrate trend, 1991-2021",
              ylm = c(2.2, 3.6))

save_plot("Plots/ES_treated_ni_91-21.png", p1, base_asp = 2.5, scale = 1.2)

# ES for latino vs not latino majority
ej <- readRDS(file.path(home, "1int/pws_ej_ind.rds")) %>% 
  mutate(b_majority_latino = if_else(percent_his >= .5, 1, 0),
         b_low_income = if_else(median_hh_income <=46000, 1, 0),
         log_hh_income = log(median_hh_income))
ni_mod_hlat <- ni %>%
  mutate(n_mgl = Winsorize(n_mgl, probs = c(0,.99))) %>% 
  filter(year >=1991, raw == 1, WATER_TYPE == "G") %>% 
  mutate(
    td = str_extract(sampleTime, "\\d{2}") %>% as.numeric()
    %>% if_else((. == 44 | . == 80), NA_real_, .),
    dy = yday(sampleDate),
    year = factor(year)
  ) %>% 
  left_join(ej) %>% 
  filter(b_majority_latino==1)
  

# drop rows with no time of sample
ni_mod_hlat <- ni_mod_hlat %>% tidyr::drop_na(td, dy)

# create polynomials
poly_td <- poly(ni_mod_hlat$td, degree = 3) %>% as_tibble()
names(poly_td) <- paste0('td_', 1:3)
poly_dy <- poly(ni_mod_hlat$dy, degree = 3) %>% as_tibble()
names(poly_dy) <- paste0('dy_', 1:3)

ni_mod_hlat <- ni_mod_hlat %>% bind_cols(poly_td, poly_dy)

es_ni_hlat <- felm(n_mgl ~ year + td_1 + td_2 + td_3 + dy_1 + dy_2 + dy_3 | samplePointID | 0 | SYSTEM_NO,data = ni_mod_hlat)

p1 <- plot_es(es_ni_hlat, ni_mod_hlat, contaminant = 'n', 
              main  = "Raw groundwater sources nitrate trends for \nPWS serving majority Latino population, 1991-2021", ylm = c(3,6))

save_plot("Plots/ES_raw_gw_ni_hlat_91-21.png", p1, base_asp = 2.5, scale = 1.2)
 
#---

ni_mod_llat <- ni %>%
  mutate(n_mgl = Winsorize(n_mgl, probs = c(0,.99))) %>% 
  filter(year >=1991, raw == 1, WATER_TYPE == "G") %>% 
  mutate(
    td = str_extract(sampleTime, "\\d{2}") %>% as.numeric()
    %>% if_else((. == 44 | . == 80), NA_real_, .),
    dy = yday(sampleDate),
    year = factor(year)
  ) %>% 
  left_join(ej) %>% 
  filter(b_majority_latino==0)


# drop rows with no time of sample
ni_mod_llat <- ni_mod_llat %>% tidyr::drop_na(td, dy)

# create polynomials
poly_td <- poly(ni_mod_llat$td, degree = 3) %>% as_tibble()
names(poly_td) <- paste0('td_', 1:3)
poly_dy <- poly(ni_mod_llat$dy, degree = 3) %>% as_tibble()
names(poly_dy) <- paste0('dy_', 1:3)

ni_mod_llat <- ni_mod_llat %>% bind_cols(poly_td, poly_dy)

es_ni_llat <- felm(n_mgl ~ year + td_1 + td_2 + td_3 + dy_1 + dy_2 + dy_3 | samplePointID | 0 | SYSTEM_NO,data = ni_mod_llat)

p1 <- plot_es(es_ni_llat, ni_mod_llat, contaminant = 'n', 
              main  = "Raw groundwater sources nitrate trends for \nPWS serving non-majority Latino population, 1991-2021", ylm = c(3,6))

save_plot("Plots/ES_raw_gw_ni_llat_91-21.png", p1, base_asp = 2.5, scale = 1.2)

ej_n <- plot_es2(es_ni_llat, es_ni_hlat, ni_mod_llat, ni_mod_hlat, contaminant = 'n', ylm = c(3,6))
save_plot("Plots/ES_n_ej.png", ej_n, base_asp = 2.5, scale = 1.2)
