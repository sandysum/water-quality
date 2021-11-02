# Heading -----------------------------------------------------------------
# A script to generate event study plots for both Arsenic and Nitrate.
# sandysum@ucsb.edu
# 2021/10/10

library(tidyverse)
library(readr)
library(readxl)
library(readr)
library(lubridate)
library(ggplot2)
library(DescTools)
library(cowplot)
library(lfe)
source("Google Drive/My Drive/0Projects/1Water/2Quality/water-quality/Scripts/helper_functions_models.R")

home <- "Google Drive/My Drive/0Projects/1Water/2Quality/Data/"

# Read data in ------------------------------------------------------------

# read arsenic and nitrate data from the CA SWRB portal 
ar <- read_rds(file.path(home, "1int/caswrb_ar_1974-2021.rds")) 

ni <-read_rds(file.path(home, "1int/caswrb_n_1974-2021.rds")) 

# Generate event study coefficients: Arsenic ------------------------------

glimpse(ar)

# 1. Raw groundwater
ar_mod <- ar %>%
  # look from 1991 and only at raw sources from groundwater
  # important!!!
  filter(year >= 1996, raw == 1, WATER_TYPE=='G') %>% 
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

es_ar <- felm(ar_ugl ~ year + td_1 + td_2 + td_3 + dy_1 + dy_2 + dy_3 | SYSTEM_NO | 0 | CITY,
              data = ar_mod)
p1 <- plot_es(es_ar, ar_mod, contaminant = 'ar', 
              main  = "Raw groundwater sources arsenic trends, 1996-2021")

save_plot("Plots/ES_raw_gw_ar_96-21.png", p1, base_asp = 2, scale = 1.5)

# 2. Raw surface water
ar_mod <- ar %>%
  # look from 1991 and only at raw sources from groundwater
  # important!!!
  filter(year >= 1996, raw == 1, WATER_TYPE=='S') %>% 
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
p1 <- plot_es(es_ar, ar_mod, contaminant = 'ar',
              main  = "Raw surface sources arsenic trends, 1996-2021")

save_plot("Plots/ES_raw_s_ar_96-21.png", p1, base_asp = 2, scale = 1.5)

# 3. Treated water
ar_mod <- ar %>%
  # look from 1991 and only at raw sources from groundwater
  # important!!!
  filter(year >= 1996, raw == 0) %>% 
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

p1 <- plot_es(es_ar, ar_mod, contaminant = 'ar',
              main  = "Treated arsenic trends, 1996-2021")


save_plot("Plots/ES_treated_ar_96-21.png", p1, base_asp = 2, scale = 1.5)

# Generate event study coefficients: NItrate ------------------------------

glimpse(ni)

# ni <- ni %>% group_by_all() %>% filter(n()==1)

# 1. Raw groundwater
ni_mod <- ni %>%
# There is not a lot of nitrate measurements! only started ramping up in 2010
  filter(raw == 1, WATER_TYPE=='G',
    year > 1984
    ) %>% 
  mutate(
    td = str_extract(sampleTime, "\\d{2}") %>% as.numeric()
    %>% if_else((. == 44 | . == 80), NA_real_, .),
    dy = yday(sampleDate),
    year = factor(year),
    n_mgl = Winsorize(n_mgl, probs = c(0,.99)),
    n() ==1 # tells me if this is a duplicate
  ) 

# drop rows with no time of sample
ni_mod <- ni_mod %>% tidyr::drop_na(td, dy)

# create polynomials
poly_td <- poly(ni_mod$td, degree = 3) %>% as_tibble()
names(poly_td) <- paste0('td_', 1:3)
poly_dy <- poly(ni_mod$dy, degree = 3) %>% as_tibble()
names(poly_dy) <- paste0('dy_', 1:3)

ni_mod <- ni_mod %>% bind_cols(poly_td, poly_dy)

es_ni <- felm(n_mgl ~ year + td_1 + td_2 + td_3 + dy_1 + dy_2 + dy_3 | SYSTEM_NO | 0 | CITY,
              data = ni_mod)

p1 <- plot_es(es_ni, ni_mod, contaminant = 'n', 
              main  = "Raw groundwater sources nitrate trends, 1985-2021")

save_plot("Plots/ES_raw_gw_ni_85-21.png", p1, base_asp = 2, scale = 1.5)

# 2. Raw surface water
ni_mod <- ni %>%
  # look from 1991 and only at raw sources from groundwater
  # important!!!
  filter(year > 1995, raw == 1, WATER_TYPE=='S') %>% 
  mutate(
    td = str_extract(sampleTime, "\\d{2}") %>% as.numeric()
    %>% if_else((. == 44 | . == 80), NA_real_, .),
    dy = yday(sampleDate),
    year = factor(year),
    n_mgl = Winsorize(n_mgl, probs = c(0,.99))
  ) 

# drop rows with no time of sample
ni_mod <- ni_mod %>% tidyr::drop_na(td, dy)

# create polynomials
poly_td <- poly(ni_mod$td, degree = 3) %>% as_tibble()
names(poly_td) <- paste0('td_', 1:3)
poly_dy <- poly(ni_mod$dy, degree = 3) %>% as_tibble()
names(poly_dy) <- paste0('dy_', 1:3)

ni_mod <- ni_mod %>% bind_cols(poly_td, poly_dy)

es_ni <- felm(n_mgl ~ year + td_1 + td_2 + td_3 + dy_1 + dy_2 + dy_3 | SYSTEM_NO | 0 | ZIP,
              data = ni_mod)

p1 <- plot_es(es_ni, ni_mod, contaminant = 'n', 
              main  = "Raw surface sources nitrate trends, 1996-2021")

save_plot("Plots/ES_raw_s_ni_96-21.png", p1, base_asp = 2, scale = 1.5)

# 3. Treated water
ni_mod <- ni %>%
  # look from 1991 and only at raw sources from groundwater
  # important!!!
  filter(year >= 1996, raw == 0) %>% 
  mutate(
    td = str_extract(sampleTime, "\\d{2}") %>% as.numeric()
    %>% if_else((. == 44 | . == 80), NA_real_, .),
    dy = yday(sampleDate),
    year = factor(year),
    n_mgl = Winsorize(n_mgl, probs = c(0,.99))
  ) 

# drop rows with no time of sample
ni_mod <- ni_mod %>% tidyr::drop_na(td, dy)

# create polynomials
poly_td <- poly(ni_mod$td, degree = 3) %>% as_tibble()
names(poly_td) <- paste0('td_', 1:3)
poly_dy <- poly(ni_mod$dy, degree = 3) %>% as_tibble()
names(poly_dy) <- paste0('dy_', 1:3)

ni_mod <- ni_mod %>% bind_cols(poly_td, poly_dy)

es_ni <- felm(n_mgl ~ year + td_1 + td_2 + td_3 + dy_1 + dy_2 + dy_3 | SYSTEM_NO | 0 | ZIP,
              data = ni_mod)
p1 <- plot_es(es_ni, ni_mod, contaminant = 'n', 
              main  = "Treated nitrate trend, 1996-2021")

save_plot("Plots/ES_treated_ni_96-21.png", p1, base_asp = 2, scale = 1.5)


# 4. Raw water only in Central Valley
# 1. Raw groundwater/Surface water
ni_mod_cv <- ni %>%
  # There is not a lot of nitrate measurements! only started ramping up in 2010
  filter(year > 1995, raw == 1, WATER_TYPE == "G") %>% 
  mutate(
    td = str_extract(sampleTime, "\\d{2}") %>% as.numeric()
    %>% if_else((. == 44 | . == 80), NA_real_, .),
    dy = yday(sampleDate),
    year = factor(year),
    n_mgl = Winsorize(n_mgl, probs = c(0,.99)),
    cv = if_else(countyName %in% cv_counties, 1, 0)
  ) %>% 
  filter(cv==1)
  

# drop rows with no time of sample
ni_mod_cv <- ni_mod_cv %>% tidyr::drop_na(td, dy)

# create polynomials
poly_td <- poly(ni_mod_cv$td, degree = 3) %>% as_tibble()
names(poly_td) <- paste0('td_', 1:3)
poly_dy <- poly(ni_mod_cv$dy, degree = 3) %>% as_tibble()
names(poly_dy) <- paste0('dy_', 1:3)

ni_mod_cv <- ni_mod_cv %>% bind_cols(poly_td, poly_dy)

es_ni_cv <- felm(n_mgl ~ year + td_1 + td_2 + td_3 + dy_1 + dy_2 + dy_3 | SYSTEM_NO | 0 | ZIP,
              data = ni_mod_cv)

p1 <- plot_es(es_ni_cv, ni_mod_cv, contaminant = 'n', 
              main  = "Raw groundwater sources nitrate trends not in Central Valley, 1995-2021")

save_plot("Plots/ES_raw_gw_ni_not_cv_95-21.png", p1, base_asp = 2, scale = 1.5)
