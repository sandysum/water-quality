# Heading -----------------------------------------------------------------
# A script to expore CA SWRB contaminant monitoring data
# sandysum@ucsb.edu
# 2021/09/04

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

dww <- read_csv(file.path(home, "ca_drinkingwatersystems_meta.csv"))
names(dww) <- names(dww) %>% str_remove_all("\\s")
# c_nm <- read_xlsx(file.path(home, "ca_water_qual/county_nms.xlsx")) %>% 
  # rename(countyName = COUNTY, countyID = `USER ID`, countyNumber = NUMBER)

baseline <- readRDS("../Data/ca_water_qual/ar_2006_levels.rds")

# read in gridded drought data

pdsi <- readRDS("../Data/drought/pdsi_pws_monthyear.rds") %>% 
  mutate(month = as.numeric(str_extract(my, "\\d{2}")),
         year = as.numeric(str_extract(my, "\\d{4}$")))

# sanity check
pdsi %>% group_by(month, year) %>% 
  summarise(pdsi = mean(mean_pdsi, na.rm = TRUE)) %>% 
  mutate(date = lubridate::dmy(paste0("01-", month, "-", year))) %>% 
  ggplot(aes(x = date, y = pdsi)) +
  geom_line() +
  theme_light() +
  geom_hline(yintercept = 0, color = 'red') +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  theme(axis.text.x = element_text(angle = 45))
  
# climdiv_cw <- read_csv("../Data/drought/ca_climdiv_crosswalk.csv") 
#   
cv_counties <-  c('Butte', 'Colusa', 'Glenn', 'Fresno', 'Kern', 'Kings', 'Madera', 'Merced', 'Placer', 'San Joaquin', 'Sacramento', 'Shasta', 'Solano', 'Stanislaus', 'Sutter', 'Tehama', 'Tulare', 'Yolo', 'Yuba') %>% str_to_lower()

# Explore Arsenic -----------------------------------------------------------------

# note that samplePointID is within each PWSID- there may be more than 1 samplePoint for each PWSID

ar %>% 
  mutate(ar_ugl = DescTools::Winsorize(ar_ugl)) %>% 
  ggplot(aes(x=ar_ugl, fill = factor(raw)))+
  # geom_density() +
  geom_histogram(alpha = .7, bins = 20, position = position_dodge()) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") 

# a lot of the median were just 2 because that is the non-detectable limit...
set.seed(123)
ar %>%
  filter(SYSTEM_NO %in% sample(unique(ar$SYSTEM_NO), 6)) %>%
  # group_by(year, raw, SYSTEM_NAM) %>%
  # summarise(mean_ar = mean(ar_ugl, na.rm = TRUE),
  #           median_ar = median(ar_ugl, na.rm = TRUE),
  #           stdd = sd(ar_ugl, na.rm = TRUE),
  #           n_obs = n()) %>%
  ggplot(aes(
    year,
    ar_ugl,
    group = samplePointID,
    color = factor(SYSTEM_NAM),
    shape = factor(raw)
  )) +
  geom_line() +
  geom_point(aes(
    year,
    ar_ugl,
    group = samplePointID,
    color = factor(SYSTEM_NAM),
    shape = factor(raw)
  ), size = 2) +
  geom_vline(xintercept = 2006) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1975, 2021, 2)) +
  theme(axis.text.x = element_text(angle = 45)) 

# a lot of the median were just 2 because that is the non-detectable limit...


# Exploring Nitrate -------------------------------------------------------

# how many monitoring station has data from 1990-2021?

ni %>% 
  # filter(samplePointID==)
  group_by(samplePointID) %>% 
  summarise(counts = n()) %>% 
  select(counts) %>% 
  table()

ni %>% 
  # filter(CITY=="MADERA") %>%
  group_by(year, raw) %>%
  summarise(mean_n = mean(n_mgl, na.rm = TRUE),
            median_n = median(n_mgl, na.rm = TRUE),
            stdd = sd(n_mgl, na.rm = TRUE),
            n_obs = n()) %>%
  ggplot(aes(year, median_n, color = factor(raw))) +
  geom_line() +
  geom_point(aes(year, mean_n, color = factor(raw))) +
  geom_vline(xintercept = 2006) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1975,2021,2)) +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_color_discrete(type = c("lightblue", "orange"))

# create sample-year data; need to do some kind of event study

# Exploring Arsenic, Running Regressions -------------------------------------------------

ar_my <- ar %>% 
  # filter only to groundwater
  filter(WATER_TYPE=="G", year >= 1996) %>%
  mutate(month = month(sampleDate),
         cv = if_else(countyName %in% cv_counties, 1, 0)) %>% 
  group_by(SYSTEM_NO, countyName, cv, year, month, raw) %>%
  summarise(median_ar = median(ar_ugl, na.rm = TRUE),
            # should I winsorize or not? Other people just check if you 
            mean_ar = mean(ar_ugl, na.rm = TRUE)) %>% 
  ungroup()

# at this point probably gotta thing about dropping the pwss with only one observations

ar_my %>%
  ungroup() %>%
  dplyr::filter(SYSTEM_NO %in% sample(ar_my$SYSTEM_NO, 6)) %>%
  mutate(date = lubridate::dmy(paste0("01-", month, "-", year))) %>%
  ggplot(aes(
    date,
    mean_ar,
    color = factor(SYSTEM_NO),
    shape = factor(raw)
  )) +
  geom_line() +
  geom_point(aes(
    date,
    mean_ar,
    color = factor(SYSTEM_NO),
    shape = factor(raw)
  ), size = 2) +
  geom_vline(xintercept = lubridate::dmy('1-1-2006')) +
  theme_minimal() +
  scale_x_date(date_breaks = '6 months') +
  theme(axis.text.x = element_text(angle = 45))

ar_my_drought <- ar_my %>% 
  ungroup() %>% 
  left_join(pdsi %>% mutate(SYSTEM_NO = str_extract(SABL_PWSID, "\\d+")), c("year", "month", "SYSTEM_NO")) %>% 
  left_join(baseline) %>% 
  mutate(mean_ar = Winsorize(mean_ar, na.rm = TRUE),
         mean_pdsi2 = (mean_pdsi^2)*sign(mean_pdsi),
         ar2006 = factor(ar2006, levels = c('low', 'med', 'high')))

# run regressions

mod <-
  felm(mean_ar ~ mean_pdsi | 0 | 0 | countyName + month,
       data = ar_my_drought %>% filter(raw == 1)
       )

summary(mod)

mod1 <-
  felm(mean_ar ~ mean_pdsi | countyName | 0 | countyName + month,
       data = ar_my_drought %>% filter(raw == 1)
  )

summary(mod1)

mod2 <-
  felm(mean_ar ~ mean_pdsi | countyName + year | 0 | countyName + month,
       data = ar_my_drought %>% filter(raw == 1)
  )

summary(mod2)

mod3 <-
  felm(mean_ar ~ mean_pdsi | countyName + month | 0 | countyName + month,
       data = ar_my_drought %>% filter(raw == 1)
  )

summary(mod3)

mod3 <-
  felm(
    mean_ar ~ mean_pdsi + cv + mean_pdsi:cv | 0 | 0 | countyName + month,
    data = ar_my_drought %>% filter(raw == 1)
  )

summary(mod3)

mod4 <-
  felm(
    mean_ar ~ mean_pdsi + ar2006 + mean_pdsi:ar2006 |
      countyName + month | 0 | countyName + month,
    data = ar_my_drought %>% filter(raw == 1)
  )

summary(mod4)

# mod5 <-
#   felm(
#     mean_ar ~ mean_pdsi + ar2006 + mean_pdsi:ar2006 |
#       SYSTEM_NO | 0 | SYSTEM_NO + month,
#     data = ar_my_drought %>% filter(raw == 1)
#   )
# 
# summary(mod5)
# 
# mod6 <-
#   felm(
#     mean_ar ~ mean_pdsi + ar2006 + mean_pdsi:ar2006 |
#       SYSTEM_NO | 0 | countyName + month,
#     data = ar_my_drought %>% filter(raw == 1)
#   )
# 
# summary(mod6)

# oh gosh, I am so dejected, there is nothing here at all

ni_my <- ni %>% 
  filter(!is.na(countyName), WATER_TYPE == "G") %>% 
  mutate(month = month(sampleDate)) %>% 
  group_by(countyName, year, month, raw) %>%
  summarise(median_ni = median(n_mgl, na.rm = TRUE),
            mean_ni = mean(n_mgl, na.rm = TRUE)) 

ni_my_drought <- ni_my %>% 
  mutate(countyName = str_to_lower(countyName)) %>% 
  left_join(climdiv_cw, by = c('countyName' = 'NAME')) %>% 
  mutate(climdiv = as.integer(climdiv_assigned),
         in_cv = factor(countyName%in%cv_counties)) %>% 
  left_join(pdsi, by = c("year", "month", "climdiv")) %>% 
  mutate(pdsi2 = (pdsi^2)*sign(pdsi))

# visualize

ni_my_drought %>% 
  filter(countyName == "tulare", raw == 1) %>% 
  ggplot(aes(date, median_ni)) +
  geom_line() +
  geom_line(aes(date, pdsi), color = 'blue') +
  theme_minimal_hgrid() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9))

mod <-
  felm(
    mean_ni ~ pdsi |
      month + countyName |
      0 | countyName + year,
    data = ni_my_drought %>% filter(raw == 1)
  )

summary(mod)  
