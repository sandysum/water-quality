# Arsenic regression
# sandysum@ucsb.edu
# 2021/10/24

library(tidyverse)
library(readr)
library(readxl)
library(readr)
library(lubridate)
library(ggplot2)
library(lfe)
library(zoo)
library(cowplot)
library(DescTools)
source("Scripts/helper_functions_models.R")

home <- "../Data/"

# Read data in ------------------------------------------------------------

ar <- read_rds(file.path(home, "1int/caswrb_ar_1974-2021.rds"))

dww <- read_csv(file.path(home, "ca_drinkingwatersystems_meta.csv"))
names(dww) <- names(dww) %>% str_remove_all("\\s")

# read in gridded drought data

pdsi <- readRDS(file.path(home,"/drought/pdsi_pws_monthyear.rds")) %>% 
  mutate(month = as.numeric(str_extract(my, "\\d{2}")),
         year = as.numeric(str_extract(my, "\\d{4}$"))) %>% 
  group_by(SABL_PWSID, year) %>% 
  dplyr::summarise(mean_pdsi = mean(mean_pdsi, na.rm = TRUE))

soil <- read_sf("../Data/1int/pws_sf_clay_ph.shp") %>% as_data_frame() %>% 
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

# Regression PDSI on raw groundwater ----------------------------------------

# drop the duplicates!

ar <- ar %>% distinct(samplePointID, SYSTEM_NO, sampleDate, sampleTime, ar_ugl, .keep_all = TRUE)

ar_py <- ar %>%
  # filter only to groundwater and raw sources
  # star from year 1984, when there are more data points..
  filter(WATER_TYPE == "G", raw == 1, year > 1985) %>%
  mutate(month = month(sampleDate),
         cv = if_else(countyName %in% cv_counties, 1, 0),
         gold = if_else(countyName %in% gold, 1, 0)) %>%
  group_by(
    SYSTEM_NO,
    year,
    countyName,
    gold,
    cv,
    DISTRICT,
    CITY,
    POP_SERV,
    ZIP,
    ZIP_EXT,
    CONNECTION,
    AREA_SERVE
  ) %>%
  # Winsorize as per Shapiro (2021 paper)
  dplyr::summarise(
    median_ar = median(ar_ugl, na.rm = TRUE),
    mean_ar = mean(ar_ugl, na.rm = TRUE)
  ) %>%
  # keep PWS with at least 10 observations
  group_by(SYSTEM_NO) %>%
  filter(n()>5) %>%
  ungroup() %>%
  mutate(mean_ar = Winsorize(mean_ar, probs = c(0, .99)))

# prepping data for interpolation
comb <- expand_grid(unique(ar_py$SYSTEM_NO), 1986:2021) 
names(comb) <- c('SYSTEM_NO', 'year')

ar_py <- left_join(comb, ar_py) %>% 
  group_by(SYSTEM_NO) %>% 
  fill_(c("countyName", "cv", "gold", "DISTRICT" , "CITY", "POP_SERV", "ZIP", "ZIP_EXT", "CONNECTION" ,"AREA_SERVE"), "downup")

# now we interpolate the data

ar_py_int <- ar_py %>% 
  arrange(SYSTEM_NO, year) %>% 
  group_by(SYSTEM_NO) %>%
  mutate(true = if_else(!is.na(mean_ar), "true value", "interpolated"),
         mean_ar = na.spline(mean_ar, 
                             maxgap = 2,
                             na.rm = FALSE),
         n_obs = sum(is.na(mean_ar)))

# what if we drop those with less than 10 observations: 42615

ar_drought <- ar_py_int %>% 
  ungroup() %>% 
  left_join(pdsi %>% mutate(SYSTEM_NO = str_extract(SABL_PWSID, "\\d+")), c("year", "SYSTEM_NO")) %>% 
  group_by(SYSTEM_NO) %>% 
  mutate(
    d = mean_pdsi, 
    dlead = lead(d),
    dlead2 = lead(dlead),
    dlag1 = lag(d),
    dlag2 = lag(dlag1),
    dlag3 = lag(dlag2),
    dlag4 = lag(dlag3),
    dlag5 = lag(dlag4),
    dlag6 = lag(dlag5)) %>% 
  # left_join(soil) %>% 
  # mutate(high_clay = if_else(clay_grp=="high", 1,))

# drop data / pws 

# run regressions

# can I believe this model???

mod_gw <-
  felm(mean_ar ~ dlead + d + dlag1 + dlag2 + dlag3 + dlag4 + dlag5 | SYSTEM_NO + year |
         0 | SYSTEM_NO, data = ar_drought)

summary(mod_gw)
plot_reg(mod_gw, contaminant = "ar", main = "Raw groundwater sources", nleads = 1, nlags = 5)

# mean(ar[(ar$raw==1&ar$WATER_TYPE=="G"),]$ar_ugl, na.rm = TRUE)
# 
# .1/8.9

# Regression PDSI on treated water ----------------------------------------

ar_py <- ar %>%
  # filter only to groundwater and raw sources
  # star from year 1984, when there are more data points..
  filter(raw == 0, year > 1985) %>%
  mutate(month = month(sampleDate),
         cv = if_else(countyName %in% cv_counties, 1, 0),
         gold = if_else(countyName %in% gold, 1, 0)) %>%
  group_by(
    SYSTEM_NO,
    year,
    countyName,
    gold,
    cv,
    DISTRICT,
    CITY,
    POP_SERV,
    ZIP,
    ZIP_EXT,
    CONNECTION,
    AREA_SERVE
  ) %>%
  # Winsorize as per Shapiro (2021 paper)
  dplyr::summarise(
    median_ar = median(ar_ugl, na.rm = TRUE),
    mean_ar = mean(ar_ugl, na.rm = TRUE)
  ) %>%
  # keep PWS with at least 10 observations
  group_by(SYSTEM_NO) %>%
  filter(n()>5) %>%
  ungroup() %>%
  mutate(mean_ar = Winsorize(mean_ar, probs = c(0, .99)))

# 38 years
2021-1984 +1

# prepping data for interpolation
comb <- expand_grid(unique(ar_py$SYSTEM_NO), 1986:2021) 
names(comb) <- c('SYSTEM_NO', 'year')

ar_py <- left_join(comb, ar_py) %>% 
  group_by(SYSTEM_NO) %>% 
  fill_(c("countyName", "cv", "gold", "DISTRICT" , "CITY", "POP_SERV", "ZIP", "ZIP_EXT", "CONNECTION" ,"AREA_SERVE"), "downup")

# now we interpolate the data

ar_py_int <- ar_py %>% 
  arrange(SYSTEM_NO, year) %>% 
  group_by(SYSTEM_NO) %>%
  mutate(true = if_else(!is.na(mean_ar), "true value", "interpolated"),
         mean_ar = na.spline(mean_ar, 
                             maxgap = 2,
                             na.rm = FALSE),
         n_obs = sum(is.na(mean_ar)))

# what if we drop those with less than 10 observations: 42615

ar_drought <- ar_py_int %>% 
  ungroup() %>% 
  left_join(pdsi %>% mutate(SYSTEM_NO = str_extract(SABL_PWSID, "\\d+")), c("year", "SYSTEM_NO")) %>% 
  group_by(SYSTEM_NO) %>% 
  mutate(
    d = mean_pdsi, 
    dlead = lead(d),
    dlead2 = lead(dlead),
    dlag1 = lag(d),
    dlag2 = lag(dlag1),
    dlag3 = lag(dlag2),
    dlag4 = lag(dlag3),
    dlag5 = lag(dlag4),
    dlag6 = lag(dlag5))

# if the previous model is to be believed, then there should be no relationship here

mod_tr <-
  felm(mean_ar ~ dlead + d + dlag1 + dlag2 + dlag3 + dlag4 + dlag5 | SYSTEM_NO + year | 0 | SYSTEM_NO,
       data = ar_drought)

summary(mod_tr)

# Regression PDSI on surface water ----------------------------------------

ar_py <- ar %>%
  # filter only to groundwater and raw sources
  # star from year 1984, when there are more data points..
  filter(raw == 1, WATER_TYPE == "S", year > 1985) %>%
  mutate(month = month(sampleDate),
         cv = if_else(countyName %in% cv_counties, 1, 0),
         gold = if_else(countyName %in% gold, 1, 0)) %>%
  group_by(
    SYSTEM_NO,
    year,
    countyName,
    gold,
    cv,
    DISTRICT,
    CITY,
    POP_SERV,
    ZIP,
    ZIP_EXT,
    CONNECTION,
    AREA_SERVE
  ) %>%
  # Winsorize as per Shapiro (2021 paper)
  dplyr::summarise(
    median_ar = median(ar_ugl, na.rm = TRUE),
    mean_ar = mean(ar_ugl, na.rm = TRUE)
  ) %>%
  # keep PWS with at least 10 observations
  group_by(SYSTEM_NO) %>%
  filter(n()>5) %>%
  ungroup() %>%
  mutate(mean_ar = Winsorize(mean_ar, probs = c(0, .99)))

# 38 years
2021-1984 +1

# prepping data for interpolation
comb <- expand_grid(unique(ar_py$SYSTEM_NO), 1986:2021) 
names(comb) <- c('SYSTEM_NO', 'year')

ar_py <- left_join(comb, ar_py) %>% 
  group_by(SYSTEM_NO) %>% 
  fill_(c("countyName", "cv", "gold", "DISTRICT" , "CITY", "POP_SERV", "ZIP", "ZIP_EXT", "CONNECTION" ,"AREA_SERVE"), "downup")

# now we interpolate the data

ar_py_int <- ar_py %>% 
  arrange(SYSTEM_NO, year) %>% 
  group_by(SYSTEM_NO) %>%
  mutate(true = if_else(!is.na(mean_ar), "true value", "interpolated"),
         mean_ar = na.spline(mean_ar, 
                             maxgap = 2,
                             na.rm = FALSE),
         n_obs = sum(is.na(mean_ar)))

# what if we drop those with less than 10 observations: 42615

ar_drought <- ar_py_int %>% 
  ungroup() %>% 
  left_join(pdsi %>% mutate(SYSTEM_NO = str_extract(SABL_PWSID, "\\d+")), c("year", "SYSTEM_NO")) %>% 
  group_by(SYSTEM_NO) %>% 
  mutate(
    d = mean_pdsi, 
    dlead = lead(d),
    dlead2 = lead(dlead),
    dlag1 = lag(d),
    dlag2 = lag(dlag1),
    dlag3 = lag(dlag2),
    dlag4 = lag(dlag3),
    dlag5 = lag(dlag4),
    dlag6 = lag(dlag5))

# there is not a lot of arsenic in groundwater

mod_s <-
  felm(mean_ar ~ dlead + d + dlag1 + dlag2 + dlag3 + dlag4 + dlag5 | SYSTEM_NO + year | 0 | SYSTEM_NO,
       data = ar_drought)

summary(mod_s)


# Plot and save -----------------------------------------------------------

plist <- map2(list(mod_gw, mod_s, mod_tr), c("Raw groundwater", "Raw surface water", "Treated water"), plot_reg, nleads = 1, nlags = 5)

save_plot("Plots/ar_pdsi_coefs.png", plot_grid(plotlist = plist, ncol = 1), base_asp = 0.5, scale = 3.8)

