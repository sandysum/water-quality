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
ar <- read_rds(file.path(home, "1int/caswrb_ar_1974-2021.rds"))

ni <-read_rds(file.path(home, "1int/caswrb_n_1974-2021.rds"))

dww <- read_csv(file.path(home, "ca_drinkingwatersystems_meta.csv"))
names(dww) <- names(dww) %>% str_remove_all("\\s")

# read in gridded drought data

pdsi <- readRDS("../Data/drought/pdsi_pws_monthyear.rds") %>% 
  mutate(month = as.numeric(str_extract(my, "\\d{2}")),
         year = as.numeric(str_extract(my, "\\d{4}$"))) %>% 
  group_by(SABL_PWSID, year) %>% 
  dplyr::summarise(mean_pdsi = mean(mean_pdsi, na.rm = TRUE))

# sanity check
# can see reassuring patterns of drought that are congruent with historical california drought.
set.seed(1028928)
q <- sample(pdsi$SABL_PWSID, 6)
quartz()
pdsi %>% 
  filter(SABL_PWSID %in% q) %>% 
  ggplot(aes(x = year, y = mean_pdsi)) +
  geom_line(aes(color = SABL_PWSID)) +
  theme_light() +
  scale_x_continuous(breaks = seq(1980, 2022, 2)) +
  geom_hline(yintercept = 0, color = 'red') +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_color_brewer(palette = "Greens")
  
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

# drop the duplicates!

ar <- ar %>% distinct(samplePointID, SYSTEM_NO, sampleDate, sampleTime, ar_ugl, .keep_all = TRUE)

ar_py <- ar %>%
  # filter only to groundwater and raw sources
  # star from year 1984, when there are more data points..
  filter(WATER_TYPE == "G", raw == 1, year > 1983) %>%
  mutate(month = month(sampleDate),
         cv = if_else(countyName %in% cv_counties, 1, 0)) %>%
  group_by(
    SYSTEM_NO,
    year,
    countyName,
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
  # drop PWSs with only one observation
  group_by(SYSTEM_NO) %>%
  filter(n()>1) %>%
  ungroup() %>%
  mutate(mean_ar = Winsorize(mean_ar, probs = c(0, .99)))

# prepping data for interpolation
comb <- expand_grid(unique(ar_py$SYSTEM_NO), 1984:2021) 
names(comb) <- c('SYSTEM_NO', 'year')

ar_py <- left_join(comb, ar_py) %>% 
  group_by(SYSTEM_NO) %>% 
  fill_(c("countyName", "cv", "DISTRICT" , "CITY", "POP_SERV", "ZIP", "ZIP_EXT", "CONNECTION" ,"AREA_SERVE"), "downup")

# now we interpolate the data

ar_py_int <- ar_py %>% 
  arrange(SYSTEM_NO, year) %>% 
  group_by(SYSTEM_NO) %>%
  mutate(true = if_else(is.na(mean_ar), "true value", "interpolated"),
         mean_ar = na.spline(mean_ar, 
                             maxgap = 2,
                             na.rm = FALSE))

ar_drought <- ar_py_int %>% 
  ungroup() %>% 
  left_join(pdsi %>% mutate(SYSTEM_NO = str_extract(SABL_PWSID, "\\d+")), c("year", "SYSTEM_NO"))

# run regressions

mod <-
  felm(mean_ar ~ mean_pdsi | SYSTEM_NO | 0 | 0,
       data = ar_drought)

summary(mod)

mod1 <-
  felm(mean_ar ~ mean_pdsi | SYSTEM_NO | 0 | CITY,
       data = ar_drought
  )

summary(mod1)

mod2 <-
  felm(mean_ar ~ mean_pdsi + mean_pdsi:ar2006 + ar2006 | 0 | 0 | 0 , data = ar_drought)

summary(mod2)

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
