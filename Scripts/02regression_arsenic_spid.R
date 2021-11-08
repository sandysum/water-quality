
# Arsenic regression ------------------------------------------------------

# 2021/11/4
# sandysum@ucsb.edu


# Load packages -----------------------------------------------------------

library(tidyverse)
library(lfe)
library(DescTools)
library(future.apply)
library(cowplot)
source("Scripts/helper_functions_models.R")

# Read in data ------------------------------------------------------------

pdsi <- readRDS("../Data/drought/pdsi_pws_monthyear.rds") %>% 
  mutate(month = as.numeric(str_extract(my, "\\d{2}")),
         year = as.numeric(str_extract(my, "\\d{4}$"))) %>% 
  group_by(SABL_PWSID, year) %>% 
  dplyr::summarise(mean_pdsi = mean(mean_pdsi, na.rm = TRUE))

ar <-read_rds(file.path(home, "1int/caswrb_ar_1974-2021.rds"))

soil <- sf::read_sf("../Data/1int/pws_sf_clay_ph.shp") %>% as_data_frame() %>% 
  select(SYSTEM_NO = SABL_PWSID, clay, ph) %>% 
  mutate(SYSTEM_NO = str_extract(SYSTEM_NO, "\\d+"),
         ph_grp = cut_interval(ph, 3),
         clay_grp = cut_interval(clay, 3)) 
levels(soil$ph_grp) <- c('low', 'med', 'high')
levels(soil$clay_grp) <- c('low', 'med', 'high')

cv_counties <-
  c(
    'Butte',
    'Colusa',
    'Glenn',
    'Fresno',
    'Kern',
    'Kings',
    'Madera',
    'Merced',
    'Placer',
    'San Joaquin',
    'Sacramento',
    'Shasta',
    'Solano',
    'Stanislaus',
    'Sutter',
    'Tehama',
    'Tulare',
    'Yolo',
    'Yuba'
  ) %>%
  str_to_lower()
gold <-
  c(
    'Butte',
    "Amador",
    'Calaveras',
    'El Dorado',
    'Mariposa',
    'Nevada',
    'Placer',
    'Plumas',
    'Sierra',
    'Tuolumne',
    'Yuba'
  ) %>%
  str_to_lower()

# Regression at the monitor month year level ------------------------------

# 1. Prep data for regression at the monitoring ID level

# drop the duplicates! and keep only ground or surface water type. 

ar_reg <- ar %>% 
  distinct(samplePointID, SYSTEM_NO, sampleDate, sampleTime, ar_ugl, .keep_all = TRUE) %>% 
  filter(WATER_TYPE %in% c("G", "S"), year > 1995, !is.na(ar_ugl)) %>% 
  mutate(groundwater = if_else(WATER_TYPE == "G", 1, 0),
         cv = if_else(countyName %in% cv_counties, 1, 0)) %>% 
  group_by(groundwater, samplePointID, year, SYSTEM_NO, cv, countyName, 
           SYSTEM_NAM, STATUS, ZIP, POP_SERV, raw, CITY) %>% 
  summarise(mean_ar = mean(ar_ugl, na.rm = TRUE),
            median_ar = median(ar_ugl, na.rm = TRUE)) %>% 
  mutate(mean_ar = Winsorize(mean_ar, probs = c(0, .99))) 

# 2. Filter to balanced panel for year 1996 to 2021

# this function subsets to only balanced panels
ar_reg_balanced <- subset_years(2010, pollutant = ar_reg, 2020, 1)

ar_drought <- ar_reg_balanced %>% 
  ungroup() %>% 
  left_join(pdsi %>% mutate(SYSTEM_NO = str_extract(SABL_PWSID, "\\d+")), c("year", "SYSTEM_NO")) %>% 
  left_join(soil) %>% 
  group_by(samplePointID) %>% 
  mutate(
    d = mean_pdsi, 
    dlead = lead(d),
    dlead2 = lead(dlead),
    dlag1 = lag(d),
    dlag2 = lag(dlag1),
    dlag3 = lag(dlag2),
    dlag4 = lag(dlag3),
    dlag5 = lag(dlag4),
    dlag6 = lag(dlag5),
    gXr = groundwater*raw) %>% 
  mutate(groundwater = factor(groundwater),
         raw = factor(raw),
         SYSTEM_NO = factor(SYSTEM_NO)) %>% 
  ungroup()

# 3. Run regressions

mod_ar_stacked <- 
  felm(mean_ar ~ d + d:groundwater + d:raw | samplePointID + groundwater:SYSTEM_NO + raw:SYSTEM_NO | 0 | SYSTEM_NO, data = ar_drought)

summary(mod_ar_stacked)

stargazer::stargazer(mod_ni_stacked)

mod_ar_stacked2 <- 
  felm(mean_ar ~ d + dlag1 + dlag2 + dlag3 + dlag4 + dlag5 +
         d:groundwater + d:raw
       + dlag1:groundwater
       + dlag2:groundwater
       + dlag3:groundwater
       + dlag4:groundwater
       + dlag5:groundwater
       + dlag1:raw
       + dlag2:raw
       + dlag3:raw 
       + dlag4:raw 
       + dlag5:raw| samplePointID + raw:SYSTEM_NO + groundwater:SYSTEM_NO | 0 | SYSTEM_NO, data = ar_drought)

summary(mod_ar_stacked2)

stargazer::stargazer(mod_ni_stacked, mod_ni_stacked2)

# Try running regression at the piecewise level ---------------------------

ar_gw_raw <- ar_drought %>% filter(groundwater ==1, raw == 1, year > 1995)  

mod_gw <- felm(mean_ar ~ d + d:clay_grp 
               + dlag1 + dlag1:clay_grp 
               + dlag2 + dlag2:clay_grp 
               + dlag3 + dlag3:clay_grp 
               + dlag4 + dlag4:clay_grp| samplePointID + SYSTEM_NO | 0 | SYSTEM_NO, data = ar_gw_raw)

ar_s_raw <- ar_drought %>% filter(groundwater ==0, raw == 1, year > 1995)  

mod_s <- felm(mean_ar ~ d + dlag1 + dlag2 + dlag3 + dlag4 | samplePointID + SYSTEM_NO | 0 | SYSTEM_NO, data = ar_s_raw)

ar_tr <- ar_drought %>% filter(raw == 0, year > 1995)  

mod_tr <- felm(mean_ar ~ d + dlag1 + dlag2 + dlag3 + dlag4 | samplePointID + SYSTEM_NO | 0 | SYSTEM_NO, data = ar_tr)

plot_reg(mod_tr, contaminant = "ar", 
         main = "Arsenic (ug/L) response to +1 in PDSI, \nRegression at the sample point level", nleads = 0, nlags = 5, ylm = c(-.3, .3))

# Plot and save -----------------------------------------------------------

plist <- map2(list(mod_gw, mod_s, mod_tr), c("Raw groundwater", "Raw surface water", "Treated water"), plot_reg, contaminant = "ar", nleads = 0, nlags = 4, ylm = c(-.3, .3))

save_plot("Plots/ar_pdsi_coefs_spid.png", plot_grid(plotlist = plist, ncol = 1), base_asp = .5, scale = 4)
