
# Nitrate regression ------------------------------------------------------

# 2021/10/24
# sandysum@ucsb.edu


# Load packages -----------------------------------------------------------

library(tidyverse)
library(lfe)
library(DescTools)
library(future.apply)

# Read in data ------------------------------------------------------------

pdsi <- readRDS("../Data/drought/pdsi_pws_monthyear.rds") %>% 
  mutate(month = as.numeric(str_extract(my, "\\d{2}")),
         year = as.numeric(str_extract(my, "\\d{4}$"))) %>% 
  group_by(SABL_PWSID, year) %>% 
  dplyr::summarise(mean_pdsi = mean(mean_pdsi, na.rm = TRUE))

ni <-read_rds(file.path(home, "1int/caswrb_n_1974-2021.rds"))

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

ni_reg <- ni %>% 
  distinct(samplePointID, SYSTEM_NO, sampleDate, sampleTime, n_mgl, .keep_all = TRUE) %>% 
  filter(WATER_TYPE %in% c("G", "S"), year > 1995, !is.na(n_mgl)) %>% 
  mutate(groundwater = if_else(WATER_TYPE == "G", 1, 0) %>% as.factor(),
         cv = if_else(countyName %in% cv_counties, 1, 0)) %>% 
  group_by(groundwater, samplePointID, year, SYSTEM_NO, cv, countyName, 
           SYSTEM_NAM, STATUS, ZIP, POP_SERV, raw) %>% 
  summarise(mean_n = mean(n_mgl, na.rm = TRUE),
            median_n = median(n_mgl, na.rm = TRUE)) %>% 
  mutate(mean_n = Winsorize(mean_n, probs = c(0, .99))) 

# 2. Filter to balanced panel for year 1996 to 2021

# this function subsets to only balanced panels
ni_reg_balanced <- subset_years(1996, pollutant = ni_reg, 2020, 1)

ni_drought <- ni_reg_balanced %>% 
  ungroup() %>% 
  left_join(pdsi %>% mutate(SYSTEM_NO = str_extract(SABL_PWSID, "\\d+")), c("year", "SYSTEM_NO")) %>% 
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
    dlag6 = lag(dlag5))

# 

# 2. Interpolate between max gap of two years -----------------------------

comb <- expand_grid(unique(ni_reg$samplePointID), 1996:2021) 
names(comb) <- c('samplePointID', 'year')

ni_full <- left_join(comb, ni_reg) %>% 
  group_by(samplePointID) %>% 
  fill_(c("countyName", "cv", "gold", "DISTRICT" , "CITY", "POP_SERV", "ZIP", "ZIP_EXT", "CONNECTION" ,"AREA_SERVE"), "downup")

# now we interpolate the data

ni_int <- ni_py %>% 
  arrange(SYSTEM_NO, year) %>% 
  group_by(SYSTEM_NO) %>%
  mutate(true = if_else(!is.na(mean_n), "true value", "interpolated"),
         mean_n = na.spline(mean_n, 
                            maxgap = 2,
                            na.rm = FALSE),
         n_obs = sum(is.na(mean_n)))


# Regression at the PWS year level --------------------------------------

# Regression PDSI on raw groundwater ----------------------------------------

ni_py <- ni %>%
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
    median_n = median(n_mgl, na.rm = TRUE),
    mean_n = mean(n_mgl, na.rm = TRUE)
  ) %>%
  # keep PWS with at least 10 observations
  group_by(SYSTEM_NO) %>%
  filter(n()>5) %>%
  ungroup() %>%
  mutate(mean_n = Winsorize(mean_n, probs = c(0, .99))) %>% 
  filter(cv==1)

# prepping data for interpolation
comb <- expand_grid(unique(ni_py$SYSTEM_NO), 1986:2021) 
names(comb) <- c('SYSTEM_NO', 'year')

ni_py <- left_join(comb, ni_py) %>% 
  group_by(SYSTEM_NO) %>% 
  fill_(c("countyName", "cv", "gold", "DISTRICT" , "CITY", "POP_SERV", "ZIP", "ZIP_EXT", "CONNECTION" ,"AREA_SERVE"), "downup")

# now we interpolate the data

ni_py_int <- ni_py %>% 
  arrange(SYSTEM_NO, year) %>% 
  group_by(SYSTEM_NO) %>%
  mutate(true = if_else(!is.na(mean_n), "true value", "interpolated"),
         mean_n = na.spline(mean_n, 
                             maxgap = 2,
                             na.rm = FALSE),
         n_obs = sum(is.na(mean_n)))

# 137532-118138 = 19394 is the number interpolated....

ni_drought <- ni_py_int %>% 
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

# drop data / pws 

# run regressions

# can I believe this model???

mod_gw_noCV <-
  felm(mean_n ~ dlead + d + dlag1 + dlag2 + dlag3 + dlag4 + dlag5 + dlag6 | SYSTEM_NO + year | 0 | SYSTEM_NO,
       data = ni_drought)
mod_gw_CV <-
  felm(mean_n ~ dlead + d + dlag1 + dlag2 + dlag3 + dlag4 + dlag5 + dlag6 | SYSTEM_NO + year | 0 | SYSTEM_NO,
       data = ni_drought)

cv <- plot_reg(mod_gw_CV, contaminant = "n", 
               main = "Groundwater response to unit increase in PDSI, \nCentral Valley CA only", nleads = 1, nlags = 6)
noCV <- plot_reg(mod_gw_noCV, contaminant = "n", 
                       main = "Groundwater response to unit increase in PDSI, \nall other CA", nleads = 1, nlags = 6)

save_plot(filename = "Plots/n_pdsi_coefs_CV.png", plot_grid(cv, noCV, nrow = 1), base_asp = 2, nrow = 1, scale = 2.5)

# mean(ar[(ar$raw==1&ar$WATER_TYPE=="G"),]$ar_ugl, na.rm = TRUE)

.1/8.9

# Regression PDSI on treated water ----------------------------------------

ni_py <- ni %>%
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
    median_n = median(n_mgl, na.rm = TRUE),
    mean_n = mean(n_mgl, na.rm = TRUE)
  ) %>%
  # keep PWS with at least 10 observations
  group_by(SYSTEM_NO) %>%
  filter(n()>5) %>%
  ungroup() %>%
  mutate(mean_ar = Winsorize(mean_n, probs = c(0, .99)))

# 38 years
2021-1984 +1

# prepping data for interpolation
comb <- expand_grid(unique(ni_py$SYSTEM_NO), 1986:2021) 
names(comb) <- c('SYSTEM_NO', 'year')

ar_py <- left_join(comb, ni_py) %>% 
  group_by(SYSTEM_NO) %>% 
  fill_(c("countyName", "cv", "gold", "DISTRICT" , "CITY", "POP_SERV", "ZIP", "ZIP_EXT", "CONNECTION" ,"AREA_SERVE"), "downup")

# now we interpolate the data

ni_py_int <- ni_py %>% 
  arrange(SYSTEM_NO, year) %>% 
  group_by(SYSTEM_NO) %>%
  mutate(true = if_else(!is.na(mean_ar), "true value", "interpolated"),
         mean_n = na.spline(mean_n, 
                             maxgap = 2,
                             na.rm = FALSE),
         n_obs = sum(is.na(mean_n)))

# what if we drop those with less than 10 observations: 42615

ni_drought <- ni_py_int %>% 
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
  felm(mean_n ~ dlead + d + dlag1 + dlag2 + dlag3 + dlag4 + dlag5 + dlag6 | SYSTEM_NO + year | 0 | SYSTEM_NO,
       data = ni_drought)

summary(mod_tr)

# Regression PDSI on surface water ----------------------------------------

ni_py <- ni %>%
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
    median_n = median(n_mgl, na.rm = TRUE),
    mean_n = mean(n_mgl, na.rm = TRUE)
  ) %>%
  # keep PWS with at least 10 observations
  group_by(SYSTEM_NO) %>%
  filter(n()>5) %>%
  ungroup() %>%
  mutate(mean_ar = Winsorize(mean_n, probs = c(0, .99)))

# 38 years
2021-1984 +1

# prepping data for interpolation
comb <- expand_grid(unique(ni_py$SYSTEM_NO), 1986:2021) 
names(comb) <- c('SYSTEM_NO', 'year')

ar_py <- left_join(comb, ni_py) %>% 
  group_by(SYSTEM_NO) %>% 
  fill_(c("countyName", "cv", "gold", "DISTRICT" , "CITY", "POP_SERV", "ZIP", "ZIP_EXT", "CONNECTION" ,"AREA_SERVE"), "downup")

# now we interpolate the data

ni_py_int <- ni_py %>% 
  arrange(SYSTEM_NO, year) %>% 
  group_by(SYSTEM_NO) %>%
  mutate(true = if_else(!is.na(mean_n), "true value", "interpolated"),
         mean_ar = na.spline(mean_n, 
                             maxgap = 2,
                             na.rm = FALSE),
         n_obs = sum(is.na(mean_n)))

# what if we drop those with less than 10 observations: 42615

ni_drought <- ni_py_int %>% 
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
  felm(mean_n ~ dlead + d + dlag1 + dlag2 + dlag3 + dlag4 + dlag5 + dlag6 | SYSTEM_NO + year | 0 | SYSTEM_NO,
       data = ni_drought)

summary(mod_s)

# Plot and save -----------------------------------------------------------


plist <- map2(list(mod_gw, mod_s, mod_tr), c("Raw groundwater", "Raw surface water", "Treated water"), plot_reg, contaminant = "n", nleads = 1, nlags = 6)

save_plot("Plots/n_pdsi_coefs.png", plot_grid(plotlist = plist, ncol = 1), base_asp = .5, scale = 3.9)

# yay I think it works because I am having year FE and system SE and clustering at the city level

# ni_my <- ni %>% 
#   filter(!is.na(countyName), WATER_TYPE == "G") %>% 
#   mutate(month = month(sampleDate)) %>% 
#   group_by(countyName, year, month, raw) %>%
#   dplyr::summarise(median_ni = median(n_mgl, na.rm = TRUE),
#             mean_ni = mean(n_mgl, na.rm = TRUE)) 
# 
# ni_my_drought <- ni_my %>% 
#   mutate(countyName = str_to_lower(countyName)) %>% 
#   left_join(climdiv_cw, by = c('countyName' = 'NAME')) %>% 
#   mutate(climdiv = as.integer(climdiv_assigned),
#          in_cv = factor(countyName%in%cv_counties)) %>% 
#   left_join(pdsi, by = c("year", "month", "climdiv")) %>% 
#   mutate(pdsi2 = (pdsi^2)*sign(pdsi))
# 
# # visualize
# 
# ni_my_drought %>% 
#   filter(countyName == "tulare", raw == 1) %>% 
#   ggplot(aes(date, median_ni)) +
#   geom_line() +
#   geom_line(aes(date, pdsi), color = 'blue') +
#   theme_minimal_hgrid() +
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 0.9))
# 
# mod <-
#   felm(
#     mean_ni ~ pdsi |
#       month + countyName |
#       0 | countyName + year,
#     data = ni_my_drought %>% filter(raw == 1)
#   )
# 
# summary(mod)  

