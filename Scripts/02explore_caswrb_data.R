# Heading -----------------------------------------------------------------
# A script to expore CA SWRB contaminant monitoring data
# sandysum@ucsb.edu
# 2021/09/04

library(sf)
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
library(zoo)

home <- "/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/Data/"

# Read data in ------------------------------------------------------------

# read arsenic and nitrate data from the CA SWRB portal 
ar <- read_rds(file.path(home, "1int/caswrb_ar_1974-2021.rds"))

ni <-read_rds(file.path(home, "1int/caswrb_n_1974-2021.rds"))

dww <- read_csv(file.path(home, "ca_drinkingwatersystems_meta.csv"))
names(dww) <- names(dww) %>% str_remove_all("\\s")
soil <- read_sf("../Data/1int/pws_sf_clay_ph.shp") %>% as_tibble()

# read in gridded drought data

pdsi <- readRDS(file.path(home, "drought/pdsi_pws_monthyear.rds")) %>% 
  mutate(month = as.numeric(str_extract(my, "\\d{2}")),
         year = as.numeric(str_extract(my, "\\d{4}$"))) %>% 
  # summarize to PWS - year
  group_by(SABL_PWSID, year) %>% 
  dplyr::summarise(mean_pdsi = mean(mean_pdsi, na.rm = TRUE))


# Look at a sub sample of drought time series for some PWS ----------------

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


# How many sample point does each PWS have? -------------------------------

pws_stat <- ar %>% 
  filter(!is.na(SYSTEM_NO)) %>% 
  group_by(SYSTEM_NO, WATER_TYPE, STATUS) %>% 
  dplyr::summarise(num_spid = length(unique(samplePointID))) %>% 
  arrange(SYSTEM_NO, desc(num_spid))

# look at PWS with over 20 unique sample points
pws_g_20 <- pws_stat %>% filter(WATER_TYPE == "G", num_spid > 19)

set.seed(50927)
q <- sample(pws_g_20$SYSTEM_NO, 6)
quartz()
ar %>% 
  filter(SYSTEM_NO %in% q) %>% 
  ggplot(aes(x = sampleDate, y = ar_ugl, group = samplePointID)) +
  geom_line() +
  theme_light() +
  scale_x_continuous(breaks = seq(1980, 2022, 2)) +
  geom_hline(yintercept = 10, color = 'red') +
  theme(axis.text.x = element_text(angle = 45)) +
  facet_wrap(vars(SYSTEM_NO), scales = "free")

# Look at a sub sample of Arsenic time series for some PWS ---------------

# RAW 
set.seed(1028928)
q <- sample(unique(ar$samplePointID), 1000)
ar_sub <- ar %>% filter(samplePointID %in% q)

spid_with_20_years <- subset_years(ar_sub, 2010, 2020, by =1)

# 50 spid with 20 years of data
set.seed(243)
ar %>% 
  filter(samplePointID %in% sample(spid_with_20_years, 12)) %>% 
  ggplot(aes(x = year, y = ar_ugl)) +
  geom_line() +
  geom_point(size = .5) +
  theme_light() +
  scale_x_continuous(breaks = seq(1990, 2020, 2)) +
  geom_hline(yintercept = 10, color = 'red') +
  theme(axis.text.x = element_text(angle = 45)) +
  facet_wrap(vars(samplePointID), nrow = 4, ncol = 3, scales = "free")


  
# climdiv_cw <- read_csv("../Data/drought/ca_climdiv_crosswalk.csv") 
#   
cv_counties <-  c('Butte', 'Colusa', 'Glenn', 'Fresno', 'Kern', 'Kings', 'Madera', 'Merced',
                  'Placer', 'San Joaquin', 'Sacramento', 'Shasta', 'Solano', 'Stanislaus', 'Sutter', 'Tehama', 'Tulare', 'Yolo', 'Yuba') %>% 
  str_to_lower()
gold <- c('Butte', "Amador", 'Calaveras', 'El Dorado', 'Mariposa', 'Nevada', 'Placer', 'Plumas', 'Sierra', 'Tuolumne', 'Yuba') %>% 
  str_to_lower()


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

