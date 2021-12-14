
#################################
# Read and clean monitoring data
# 
# 5/8/2021
# sandysum@ucsb.edu
#################################

# home <- "/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/Data"

home <- "G:/My Drive/0Projects/1Water/2Quality/Data/"

# load package

library(tidyverse)
library(readr)
library(lubridate)
library(readxl)
library(rdrobust)
library(cowplot)

ar_reg <-read_rds("../Data/1int/caswrb_ar_1974-2021.rds")
n_reg <- read_rds("../Data/1int/caswrb_n_1974-2021.rds")
fe_reg <- read_rds("../Data/1int/caswrb_fe_1974-2021.rds")

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

################### CLEAN AND SAVE FOR ARSENIC ######################
# filter only code 01002 for ARSENIC
chem74_99 <- read_csv(file.path(home, "ca_water_qual/chem_1974_1999.csv")) %>% filter(STORE_NUM == "01002")
chem08_14 <- read_csv(file.path(home, "ca_water_qual/chem_2008_2014.csv")) %>% filter(STORE_NUM == "01002")
chem15_20 <- read_csv(file.path(home, "ca_water_qual/chem_2015_2020.csv")) %>% filter(STORE_NUM == "01002")
chem00_07 <- read_csv(file.path(home, "ca_water_qual/chem_2000_2007.csv")) %>% filter(STORE_NUM == "01002")

sys <- read_xlsx(file.path(home, "ca_water_qual/watsys.xlsx")) 
loc <- read_xlsx(file.path(home, "ca_water_qual/siteloc.xlsx")) %>% 
  mutate(STATUS = str_to_upper(STATUS))
c_nm <- read_xlsx(file.path(home, "ca_water_qual/county_nms.xlsx")) %>% 
  rename(countyName = COUNTY, countyID = `USER ID`, countyNumber = NUMBER) 

# Save and clean for Arsenic Data

all_arsenic <- future.apply::future_lapply(list(chem00_07, chem08_14, chem15_20, chem74_99), function(x){
  x %>% mutate_at(vars(matches("DATE")), ~(mdy_hms(.))) %>% 
    dplyr::select(samplePointID = PRIM_STA_C, sampleDate = SAMP_DATE, LAB_NUM, ar_ugl = FINDING, sampleTime = SAMP_TIME) %>% 
    mutate(year = year(sampleDate),
           labNum = as.integer(LAB_NUM)) %>% 
    select(-LAB_NUM) %>% 
    left_join(loc, by = c("samplePointID" = "PRI_STA_C")) %>%
    mutate(countyNumber = as.numeric(COUNTY), .keep = "unused") %>%
    left_join(c_nm)
}) %>% bind_rows() %>%
  left_join(sys) %>%
  select(-COMMENT_1) %>%
  # filter out destroyed waste water well and pending status
  filter(!(STATUS %in% c('DS', 'WW', 'PN'))) %>%
  mutate(
    WATER_TYPE = if_else(WATER_TYPE == "g", "G", WATER_TYPE),
    sampleDate = as_date(sampleDate),
    sampleHour = str_extract(sampleTime, "\\d{2}"),
    # raw, untreated, monitoring well, and agriculture well considered raw
    raw = if_else(map(str_extract_all(STATUS, "."), 2) %in% c('R', 'U', 'W', 'G'), 1, 0),
    countyName = str_to_lower(countyName)
  )

# check for duplicates
all_arsenic %>% 
  group_by_all() %>% 
  filter(n()>1)

rm(list = ls(pattern = 'chem'))

saveRDS(all_arsenic, "../Data/1int/caswrb_ar_1974-2021.rds")


# Clean and save Arsenic data for regression -----------------------------

# Regression at the monitor month year level ------------------------------

# 1. Prep data for regression at the monitoring ID level

# drop the duplicates! and keep only ground or surface water type. 

ar_reg <- ar_reg %>% 
  distinct(samplePointID, SYSTEM_NO, sampleDate, sampleTime, ar_ugl, .keep_all = TRUE) %>% 
  filter(WATER_TYPE %in% c("G", "S"), !is.na(ar_ugl)) %>% 
  mutate(gw = if_else(WATER_TYPE == "G", 1, 0),
         cv = if_else(countyName %in% cv_counties, 1, 0),
         gold = if_else(countyName %in% gold, 1, 0)) %>% 
  group_by(gw, gold, samplePointID, year, SYSTEM_NO, cv, countyName, 
           SYSTEM_NAM, STATUS, ZIP, POP_SERV, raw, CITY, WATER_TYPE) %>% 
  summarise(mean_ar = mean(ar_ugl, na.rm = TRUE),
            median_ar = median(ar_ugl, na.rm = TRUE)) %>% 
  mutate(mean_ar = Winsorize(mean_ar, probs = c(0, .99))) 

saveRDS(ar_reg, "../Data/1int/caswrb_ar_reg.rds")

################### CLEAN AND SAVE FOR NITRATES --------------------------------------

# filter all only 00618 for nitrate as N
# these 3 are useless
# chem74_99 <- read_csv(file.path(home, "ca_water_qual/chem_1974_1999.csv")) %>% filter(STORE_NUM == "00618")
# chem00_07 <- read_csv(file.path(home, "ca_water_qual/chem_2000_2007.csv")) %>% filter(STORE_NUM == "00618")
# chem08_14 <- read_csv(file.path(home, "ca_water_qual/chem_2008_2014.csv")) %>% filter(STORE_NUM == "00618")
chem15_20 <- read_csv(file.path(home, "ca_water_qual/chem_2015_2020.csv")) %>% filter(STORE_NUM == "00618")

# for the years prior to 2015, use N as NO3 as that used to be the old standard
chemno374_99 <- read_csv(file.path(home, "ca_water_qual/chem_1974_1999.csv")) %>% filter(STORE_NUM == "71850") %>% mutate(FINDING = 0.2259*FINDING)
chemno308_14 <- read_csv(file.path(home, "ca_water_qual/chem_2008_2014.csv")) %>% filter(STORE_NUM == "71850") %>% mutate(FINDING = 0.2259*FINDING)
chemno300_07 <- read_csv(file.path(home, "ca_water_qual/chem_2000_2007.csv")) %>% filter(STORE_NUM == "71850") %>% mutate(FINDING = 0.2259*FINDING)
chemno315_20 <- read_csv(file.path(home, "ca_water_qual/chem_2015_2020.csv")) %>% filter(STORE_NUM == "71850") %>% mutate(FINDING = 0.2259*FINDING)

all_nitrate <- future.apply::future_lapply(list(chemno300_07, chemno308_14, chemno315_20, chemno374_99, chem15_20), function(x){
  x %>% mutate_at(vars(matches("DATE")), ~(mdy_hms(.))) %>%
    dplyr::select(samplePointID = PRIM_STA_C, sampleDate = SAMP_DATE, LAB_NUM, n_mgl =FINDING, sampleTime = SAMP_TIME) %>%
    mutate(year = year(sampleDate),
           labNum = as.integer(LAB_NUM)) %>%
    select(-LAB_NUM)
}) %>% bind_rows()

# Investigate if there are duplicates in N as NO3 and N in the 201 --------

# Join them together and see how many overlapping... not a lot! means that they are different data
chemno315_20 <-
  chemno315_20 %>% mutate_at(vars(matches("DATE")), ~ (mdy_hms(.))) %>%
  dplyr::select(
    samplePointID = PRIM_STA_C,
    sampleDate = SAMP_DATE,
    LAB_NUM,
    n_mgl = FINDING,
    sampleTime = SAMP_TIME
  ) %>%
  mutate(year = year(sampleDate),
         labNum = as.integer(LAB_NUM)) %>%
  select(-LAB_NUM)

chem15_20 <-
  chem15_20 %>% mutate_at(vars(matches("DATE")), ~ (mdy_hms(.))) %>%
  dplyr::select(
    samplePointID = PRIM_STA_C,
    sampleDate = SAMP_DATE,
    LAB_NUM,
    n_mgl = FINDING,
    sampleTime = SAMP_TIME
  ) %>%
  mutate(year = year(sampleDate),
         labNum = as.integer(LAB_NUM)) %>%
  select(-LAB_NUM)

# these are the overlapping observations
both <- full_join(
  chemno315_20 %>% rename(no3 = n_mgl),
  chem15_20 %>% rename(n = n_mgl)
) %>% 
  mutate(discrep = no3 - n) %>% 
  filter(!is.na(discrep)) %>% 
  mutate(n_mgl = if_else(abs(discrep)<1, n, no3))

# drop all observation in both in the main dataset

all_nitrate_tmp <- all_nitrate %>% anti_join(both %>% select(samplePointID, sampleDate, sampleTime, year, labNum)) %>% 
  bind_rows(both %>% select(-c(n, no3, discrep)))

# > 327063-324789 
# [1] 2274 <- the number of overlapping observation

all_nitrate <- all_nitrate_tmp %>%
  left_join(loc, by = c("samplePointID" = "PRI_STA_C")) %>%
  mutate(countyNumber = as.numeric(COUNTY), .keep = "unused") %>%
  left_join(c_nm) %>% 
  left_join(sys) %>%
  select(-COMMENT_1) %>%
  # filter out destroyed waste water well and pending status
  filter(!(STATUS %in% c('DS', 'WW', 'PN'))) %>%
  mutate(
    WATER_TYPE = if_else(WATER_TYPE == "g", "G", WATER_TYPE),
    sampleDate = as_date(sampleDate),
    sampleHour = str_extract(sampleTime, "\\d{2}"),
    # raw, untreated, monitoring well, and agriculture well considered raw
    raw = if_else(map(str_extract_all(STATUS, "."), 2) %in% c('R', 'U', 'W', 'G'), 1, 0),
    countyName = str_to_lower(countyName)
  )

# check if there are duplicates

dups <- all_nitrate %>% 
  group_by_all() %>% 
  filter(n()>1)

rm(list = ls(pattern = 'chem.+'))

saveRDS(all_nitrate, "../Data/1int/caswrb_n_1974-2021.rds")

n_reg <- n_reg %>% 
  distinct(samplePointID, SYSTEM_NO, sampleDate, sampleTime, n_mgl, .keep_all = TRUE) %>% 
  filter(WATER_TYPE %in% c("G", "S"), !is.na(n_mgl)) %>% 
  mutate(gw = if_else(WATER_TYPE == "G", 1, 0),
         cv = if_else(countyName %in% cv_counties, 1, 0)) %>% 
  group_by(gw, samplePointID, year, SYSTEM_NO, cv, countyName, 
           SYSTEM_NAM, STATUS, ZIP, POP_SERV, raw, CITY) %>% 
  summarise(mean_n = mean(n_mgl, na.rm = TRUE),
            median_n = median(n_mgl, na.rm = TRUE)) %>% 
  mutate(mean_n = Winsorize(mean_n, probs = c(0, .99))) 

saveRDS(n_reg, "../Data/1int/caswrb_n_reg.rds")


################### CLEAN AND SAVE FOR IRON ######################
# filter only code 01045 for IRON
chem74_99 <- read_csv(file.path(home, "ca_water_qual/chem_1974_1999.csv")) %>% filter(STORE_NUM == "01045")
chem08_14 <- read_csv(file.path(home, "ca_water_qual/chem_2008_2014.csv")) %>% filter(STORE_NUM == "01045")
chem15_20 <- read_csv(file.path(home, "ca_water_qual/chem_2015_2020.csv")) %>% filter(STORE_NUM == "01045")
chem00_07 <- read_csv(file.path(home, "ca_water_qual/chem_2000_2007.csv")) %>% filter(STORE_NUM == "01045")

sys <- read_xlsx(file.path(home, "ca_water_qual/watsys.xlsx")) 
loc <- read_xlsx(file.path(home, "ca_water_qual/siteloc.xlsx")) %>% 
  mutate(STATUS = str_to_upper(STATUS))
c_nm <- read_xlsx(file.path(home, "ca_water_qual/county_nms.xlsx")) %>% 
  rename(countyName = COUNTY, countyID = `USER ID`, countyNumber = NUMBER) 

# Save and clean for Iron Data

all_iron <- future.apply::future_lapply(list(chem00_07, chem08_14, chem15_20, chem74_99), function(x){
  x %>% mutate_at(vars(matches("DATE")), ~(mdy_hms(.))) %>% 
    dplyr::select(samplePointID = PRIM_STA_C, sampleDate = SAMP_DATE, LAB_NUM, fe_ugl = FINDING, sampleTime = SAMP_TIME) %>% 
    mutate(year = year(sampleDate),
           labNum = as.integer(LAB_NUM)) %>% 
    dplyr::select(-LAB_NUM) %>% 
    left_join(loc, by = c("samplePointID" = "PRI_STA_C")) %>%
    mutate(countyNumber = as.numeric(COUNTY), .keep = "unused") %>%
    left_join(c_nm)
}) %>% bind_rows() %>%
  left_join(sys) %>%
  dplyr::select(-COMMENT_1) %>%
  # filter out destroyed waste water well and pending status
  filter(!(STATUS %in% c('DS', 'WW', 'PN'))) %>%
  mutate(
    WATER_TYPE = if_else(WATER_TYPE == "g", "G", WATER_TYPE),
    sampleDate = as_date(sampleDate),
    sampleHour = str_extract(sampleTime, "\\d{2}"),
    # raw, untreated, monitoring well, and agriculture well considered raw
    raw = if_else(map(str_extract_all(STATUS, "."), 2) %in% c('R', 'U', 'W', 'G'), 1, 0),
    countyName = str_to_lower(countyName)
  )

# check for duplicates
all_iron %>% 
  group_by_all() %>% 
  filter(n()>1)

all_iron <- all_iron %>% distinct()

rm(list = ls(pattern = 'chem'))

saveRDS(all_iron, "../Data/1int/caswrb_fe_1974-2021.rds")

fe_reg <- fe_reg %>% 
  distinct(samplePointID, SYSTEM_NO, sampleDate, sampleTime, fe_ugl, .keep_all = TRUE) %>% 
  filter(WATER_TYPE %in% c("G", "S"), !is.na(fe_ugl)) %>% 
  mutate(gw = if_else(WATER_TYPE == "G", 1, 0),
         cv = if_else(countyName %in% cv_counties, 1, 0),
         gold = if_else(countyName %in% gold, 1, 0)) %>% 
  group_by(gw, gold, samplePointID, year, SYSTEM_NO, cv, countyName, 
           SYSTEM_NAM, STATUS, ZIP, POP_SERV, raw, CITY, WATER_TYPE) %>% 
  summarise(mean_fe = mean(fe_ugl, na.rm = TRUE),
            median_fe = median(fe_ugl, na.rm = TRUE)) %>% 
  mutate(mean_fe = Winsorize(mean_fe, probs = c(0, .99))) 

saveRDS(fe_reg, "../Data/1int/caswrb_fe_reg.rds")

