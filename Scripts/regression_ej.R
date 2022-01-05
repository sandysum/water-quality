#################################
# This script runs the regression to characterize the uneven distribution of 
# drinking water quality across income and racial lines
# I investigate long term medians of nitrate, arsenic at the raw level and violations of
# PWS serving communities with different 
# 2021/12/28
# sandysum@ucsb.edu
#################################

rm(list = ls())
home <- "/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/Data"

library(tidyverse)
library(DescTools)
library(readxl)
library(readr)
library(lfe)
library(sf)

# Read in water systems information with social equity indicators ---------

# https://cacensus.maps.arcgis.com/apps/webappviewer/index.html?id=48be59de0ba94a3dacff1c9116df8b37

# Good website to double check if the census average numbers are correct
sys <- read_csv(file.path(home, "ca_water_qual/watsys_tract_socialeq_ind.csv"))

# some PWS serve many tracts and some tracts are served by many PWS... gonna summarize everything to the PWS level and then weight by number of tracts served?

# Run some correlational analysis of social indicators --------------------

ar_reg <- read_rds("../Data/1int/caswrb_ar_reg.rds")
ni_reg <- read_rds("../Data/1int/caswrb_n_reg.rds")

# Read in the violations data

violations <- read_csv(file.path(home, "/SDWA-data/SDWA_VIOLATIONS_CA.csv"))

# Clean violations data for nitrates ----------------

ni_vio_new <- violations %>% 
  filter(STATE=="CA" & RULE_NAME == "Nitrates" & HEALTH_BASED == "Y") %>% 
  mutate(SYSTEM_NO = str_extract(PWSID, "(?<=CA)\\d+")) %>% 
  dplyr::select(SYSTEM_NO, PWSID, year = BEGIN_YEAR, VIOLATION_ID, VIOLATION_NAME, RULE_NAME) %>% 
  group_by(SYSTEM_NO, PWSID) %>% 
  # summarise(n_new_violations = unique(VIOLATION_ID) %>% length())
  summarise(n_violations = n())

# Clean violations data for arsenic ----------------

as_vio_new <- violations %>%
  filter(STATE == "CA" & RULE_NAME == "Arsenic" & HEALTH_BASED == "Y") %>%
  mutate(SYSTEM_NO = str_extract(PWSID, "(?<=CA)\\d+")) %>% 
  dplyr::select(SYSTEM_NO, PWSID, year = BEGIN_YEAR, VIOLATION_ID, VIOLATION_NAME, RULE_NAME) %>% 
  group_by(SYSTEM_NO, PWSID) %>% 
  # summarise(n_new_violations = unique(VIOLATION_ID) %>% length())
  summarise(as_violations = n()) 

sys_vio <- sys %>% left_join(as_vio_new) %>% 
  left_join(ni_vio_new) %>% 
  drop_na(TRACT) %>% 
  mutate_at(vars(matches("_violations")), ~replace(.,is.na(.), 0)) %>% 
  mutate(log_hh_income = log(median_hh_income)) %>% 
  group_by(TRACT) %>% 
  mutate(n_pws = 1/n())


# Run regression for violations -------------------------------------------

mod_v_as_ej_income <- felm(as_violations ~ log_hh_income | 0 | 0 | TRACT, data = sys_vio, weights = sys_vio$n_pws)

summary(mod_v_as_ej_income)

mod_v_as_ej_percent_his <- felm(as_violations ~ percent_his | 0 | 0 | TRACT, data = sys_vio, weights = sys_vio$n_pws)

summary(mod_v_as_ej_percent_his)

mod_v_n_ej_income <- felm(n_violations ~ log_hh_income | 0 | 0 | TRACT, data = sys_vio, weights = sys_vio$n_pws)

summary(mod_v_n_ej_income)

mod_v_n_ej_percent_his <- felm(n_violations ~ percent_his | 0 | 0 | TRACT, data = sys_vio, weights = sys_vio$n_pws)

summary(mod_v_n_ej_percent_his)

stargazer::stargazer(mod_v_as_ej_income, mod_v_as_ej_percent_his,
                     mod_v_n_ej_income, mod_v_n_ej_percent_his, 
                     dep.var.labels.include = FALSE,
                     title = "Corr. of PWS median contaminants and TRACT-level EJ 
                     indicators",
                     column.separate = c(2,2,2,2),
                     column.labels = c("# Violations As", "# Violations N",
                                       "Median As", "Median N"),
                     omit = "Constant",
                     single.row = TRUE,
                     omit.stat = c("adj.rsq", "ser"))

# Q1 are DAC PWS getting more violations?

ggplot(sys, aes(median_hh_income, n_violations)) +
  geom_point() +
  geom_smooth() +
  theme_minimal()

# Q1 are DAC getting worst raw water?
# filter to raw and in the last 10 years
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
                     dep.var.labels.include = FALSE,
                     title = "Corr. of PWS median contaminants and TRACT-level EJ 
                     indicators",
                     column.separate = c(2,2),
                     column.labels = c("Median As", "Median N"),
                     omit = "Constant",
                     single.row = TRUE,
                     omit.stat = c("adj.rsq", "ser"))






