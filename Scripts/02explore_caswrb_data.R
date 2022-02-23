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
options(scipen=999)
rm(list = ls())

home <- "/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/Data/"
home <- "G:/My Drive/0Projects/1Water/2Quality/Data"
# Read data in ------------------------------------------------------------

# read arsenic and nitrate data from the CA SWRB portal 
ar <- read_rds(file.path(home, "1int/caswrb_ar_1974-2021.rds"))
ni <-read_rds(file.path(home, "1int/caswrb_n_1974-2021.rds"))
ind <- read_rds(file.path(home, "1int/pws_ind.rds"))

# explore and clean ind
# there are duplicates in ind

# which are the duplicates?
# keep only the PWS that are relevant for my analysis
active <- ind %>% filter()
ind %>% table()
# saveRDS(distinct, file.path(home, "1int/pws_ind.rds"))

ind$ResidentialPopulation %>% sum(na.rm = TRUE)

# This dataset covers almost all of California 38168200

ind %>%
  ggplot(aes(POP_SERV, TotalPopulation)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

ind$TotalPopulation %>% sum(na.rm = TRUE)


# look at one water system

paramount <- ni %>% filter(SYSTEM_NO=='1910105') 

paramount$samplePointID %>% table()


# Generate dataset for data collection ------------------------------------

large <- ind %>% filter(ResidentialPopulation>20000)
sources <- read_xlsx("../Data/ca_water_qual/siteloc.xlsx")
sources <- sources %>% 
  mutate(STATUS = str_to_upper(STATUS))

rel.source <- sources %>% 
  filter(SYSTEM_NO %in% large$SYSTEM_NO,
         !(STATUS %in% c('AB', 'AG', 'CM', 'SR', 'ST', 'SU', 'PN', 'MW', 
                         'WW', 'IT', 'IR', 'IS', 'DS')))

write_csv(rel.source, file = '../Data/1int/pws_source_prop2.csv')

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


# Investigate distribution sample points -----------------------------------

ni_dist <- ni_reg_balanced %>% filter(STATUS == 'DT' | STATUS == 'DR' | STATUS == 'DU')

ar_dt %>% 
  filter(samplePointID %in% sample(ar_dt$samplePointID, 6)) %>% 
  ggplot(aes(x = sampleDate, y = ar_ugl)) +
  geom_line(aes(color = samplePointID, group = samplePointID)) +
  theme_light() +
  scale_x_continuous(breaks = seq(1980, 2022, 2)) +
  geom_hline(yintercept = 0, color = 'red') +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_color_brewer(palette = "Greens")

# How many PWS just have one sample point
# Keep only the active ones

dist <- ni %>% 
  filter(!(STATUS %in% c('AB', 'AG', 'CM', 'SR', 'ST', 'SU', 'PN', 'MW', 
                         'WW', 'IT', 'IR', 'IS', 'DS'))) %>% 
  group_by(SYSTEM_NO, year) %>%
  # distinct() %>% 
  count(sort = TRUE)
  
dist$n %>% table()

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


# Relationship between nitrate and arsenic --------------------------------
# 2021/11/30

all <- ni %>% select(samplePointID, sampleDate, n_mgl) %>% left_join(ar, by = c('samplePointID', 'sampleDate')) %>% drop_na(n_mgl, ar_ugl) %>% 
  mutate(n_mgl_w = Winsorize(n_mgl, probs = c(0, .95)),
         ar_ugl_w = Winsorize(ar_ugl, probs = c(0, .95))) %>% 
  filter(WATER_TYPE=="G", raw==1)

all %>% ggplot(aes(n_mgl_w, ar_ugl_w)) +
  geom_point() +
  geom_smooth()


# New SWDIS data ----------------------------------------------------------

df <- read_csv("../Data/ca_water_qual/01012019 to present.csv")
names(df) <- names(df) %>% str_remove_all('\\s')

df_n <- df %>% filter(FacilityType == 'DS', FacilityStatus == 'A',
                      AnalyteName == 'NITRATE')
df2 <- read_csv("../Data/ca_water_qual/01012015 to 12312018.csv")

df2 %>% group

names(df2) <- names(df2) %>% str_remove_all('\\s')

df2_n <- df2 %>% filter(FacilityType == 'DS', FacilityStatus == 'A',
                      AnalyteName == 'NITRATE') %>% 
  select(samplePointID, sampleDate, n_mgl) %>% left_join(ar, by = c('samplePointID', 'sampleDate')) %>% drop_na(n_mgl, ar_ugl) %>% 
  mutate(n_mgl_w = Winsorize(n_mgl, probs = c(0, .95)),
         ar_ugl_w = Winsorize(ar_ugl, probs = c(0, .95))) %>% 
  filter(WATER_TYPE=="G", raw==1)
