
#################################
# Read and clean monitoring data
# 
# 5/8/2021
# sandysum@ucsb.edu
#################################

home <- "/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/Data"

home <- "G:/My Drive/0Projects/1Water/2Quality/Data/"

# load package

library(tidyverse)
library(readr)
library(lubridate)
library(readxl)
library(rdrobust)
library(cowplot)

ar <-read_rds("../Data/1int/caswrb_ar_1974-2021.rds")
n <- read_rds("../Data/1int/caswrb_n_1974-2022.rds")
ind <- readRDS("../Data/1int/pws_ind.rds")

dups <- ind %>% 
  group_by(SYSTEM_NO) %>% 
  filter(n()>1)

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
# read file from the new format 2019-2022
chem19_21 <- read_csv(file.path(home, "ca_water_qual/01012019 to present.csv")) %>% 
  filter(`Analyte Name` == "ARSENIC")

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

# Clean and join 2019-2022 new data ---------------------------------------

names(chem19_21) <- names(chem19_21) %>% str_replace_all('\\s', '')

clean <- chem19_21 %>% 
  mutate(SYSTEM_NO = str_extract(WaterSystemNumber, '\\d+'),
         samplePointID = str_extract(PSCode, '\\d+_\\d{3}') %>% str_replace("_", "-")) %>% 
  select(samplePointID, sampleTime = SampleTime, sampleDate = SampleDate, ar_ugl = Result) %>% 
  mutate(sampleDate = mdy(sampleDate),
         year = year(sampleDate),
         sampleTime = as.character(sampleTime))

((clean$samplePointID %>% unique()) %in% unique(ar$samplePointID)) %>% sum()

all_arsenic <- ar %>% select(names(clean)) %>% 
  bind_rows(clean) %>% 
  # distinct(samplePointID, sampleDate, n_mgl, .keep_all = TRUE) %>% 
  # all_nitrate_tmp %>%
  left_join(loc, by = c("samplePointID" = "PRI_STA_C")) %>%
  mutate(countyNumber = as.numeric(COUNTY), .keep = "unused") %>%
  left_join(c_nm) %>% 
  # left_join(sys) %>%
  select(-COMMENT_1) %>%
  # filter out destroyed waste water well and pending status
  filter(!(STATUS %in% c('DS', 'WW', 'PN'))) %>%
  mutate(
    WATER_TYPE = if_else(WATER_TYPE == "g", "G", WATER_TYPE),
    sampleHour = str_extract(sampleTime, "\\d{2}"),
    # raw, untreated, monitoring well, and agriculture well considered raw
    raw = if_else(map(str_extract_all(STATUS, "."), 2) %in% c('R', 'U', 'W', 'G'), 1, 0),
    countyName = str_to_lower(countyName)
  )  %>% distinct(samplePointID, sampleDate, ar_ugl, .keep_all = TRUE) %>% 
  rename(as_ugl = ar_ugl) 
  

saveRDS(all_nitrate, "../Data/1int/caswrb_n_1974-2022.rds")
saveRDS(all_arsenic, "../Data/1int/caswrb_ar_1974-2022.rds")

# Clean and save Arsenic data for regression -----------------------------

# Regression at the monitor month year level ------------------------------

# 1. Prep data for regression at the monitoring ID level

# drop the duplicates! and keep only ground or surface water type. 
ar <- readRDS("../Data/1int/caswrb_ar_1974-2021.rds")
ar_reg <- all_arsenic %>% 
  # I think that it is more correct to winsorize before taking means and medians
  mutate(as_ugl = Winsorize(as_ugl, probs = c(0, .99), na.rm = TRUE)) %>% 
  distinct(samplePointID, SYSTEM_NO, sampleDate, sampleTime, as_ugl, .keep_all = TRUE) %>% 
  filter(WATER_TYPE %in% c("G", "S"), !is.na(as_ugl)) %>% 
  mutate(gw = if_else(WATER_TYPE == "G", 1, 0),
         cv = if_else(countyName %in% cv_counties, 1, 0),
         gold = if_else(countyName %in% gold, 1, 0)) %>% 
  group_by(gw, gold, samplePointID, year, SYSTEM_NO, cv, countyName, raw, WATER_TYPE) %>% 
  summarise(mean_as = mean(as_ugl, na.rm = TRUE),
            median_as = median(as_ugl, na.rm = TRUE)) 

saveRDS(ar_reg, "../Data/1int/caswrb_ar_reg.rds")

################### CLEAN AND SAVE FOR NITRATES --------------------------------------

# filter all only 00618 for nitrate as N
# these 3 are useless
# chem74_99 <- read_csv(file.path(home, "ca_water_qual/chem_1974_1999.csv")) %>% filter(STORE_NUM == "00618")
# chem00_07 <- read_csv(file.path(home, "ca_water_qual/chem_2000_2007.csv")) %>% filter(STORE_NUM == "00618")
# chem08_14 <- read_csv(file.path(home, "ca_water_qual/chem_2008_2014.csv")) %>% filter(STORE_NUM == "00618")

# load this because it is a full compliance cycle and will give me the regulating agency for all other 
pws_ind <- readRDS("../Data/1int/pws_ind.rds") %>% 
  distinct(SYSTEM_NO, .keep_all = TRUE)

chem15_18 <- read_csv(file.path(home, "ca_water_qual/01012015 to 12312018.csv")) 
names(chem15_18) <- names(chem15_18) %>% str_replace_all('\\s', '')

chem15_18 <- chem15_18 %>% 
  select(RegulatingAgency, WaterSystemNumber) %>% 
mutate(SYSTEM_NO = str_extract(WaterSystemNumber, '\\d+'))

regulating.match <- chem15_18 %>% select(RegulatingAgency, SYSTEM_NO) %>% 
  distinct()
saveRDS(pws_ind, '../Data/1int/pws_ind.rds')

chem15_20 <- read_csv(file.path(home, "ca_water_qual/chem_2015_2020.csv")) %>% filter(STORE_NUM == "00618")
chem19_21 <- read_csv(file.path(home, "ca_water_qual/01012019 to present.csv")) %>% 
  filter(`Analyte Name` == "NITRATE")
# for the years prior to 2015, use N as NO3 as that used to be the old standard
chemno374_99 <- read_csv(file.path(home, "ca_water_qual/chem_1974_1999.csv")) %>% filter(STORE_NUM == "71850") %>% mutate(FINDING = 0.2259*FINDING)
chemno308_14 <- read_csv(file.path(home, "ca_water_qual/chem_2008_2014.csv")) %>% filter(STORE_NUM == "71850") %>% mutate(FINDING = 0.2259*FINDING)
chemno300_07 <- read_csv(file.path(home, "ca_water_qual/chem_2000_2007.csv")) %>% filter(STORE_NUM == "71850") %>% mutate(FINDING = 0.2259*FINDING)
chemno315_20 <- read_csv(file.path(home, "ca_water_qual/chem_2015_2020.csv")) %>% filter(STORE_NUM == "71850") %>% mutate(FINDING = 0.2259*FINDING)

# Generate regulating agency dataset which we never had before. Match it to ind

all_nitrate <- future.apply::future_lapply(list(chemno300_07, chemno308_14, chemno315_20, chemno374_99, chem15_20), function(x){
  x %>% mutate_at(vars(matches("DATE")), ~(mdy_hms(.))) %>%
    dplyr::select(samplePointID = PRIM_STA_C, sampleDate = SAMP_DATE, LAB_NUM, n_mgl =FINDING, sampleTime = SAMP_TIME) %>%
    mutate(year = year(sampleDate),
           labNum = as.integer(LAB_NUM)) %>%
    select(-LAB_NUM)
}) %>% bind_rows()

# Clean and join the new data
# > ((clean$samplePointID %>% unique()) %in% unique(all_nitrate$samplePointID)) %>% sum()
# [1] 15842
# > clean$samplePointID %>% unique() %>% length()
# [1] 16505

# most of them can be matched!!!

# all_nitrate <- readRDS( "../Data/1int/caswrb_n_1974-2021.rds")
names(chem19_21) <- names(chem19_21) %>% str_replace_all('\\s', '')

clean <- chem19_21 %>% 
  mutate(SYSTEM_NO = str_extract(WaterSystemNumber, '\\d+'),
         samplePointID = str_extract(PSCode, '\\d+_\\d{3}') %>% str_replace("_", "-")) %>% 
  select(samplePointID, sampleTime = SampleTime, sampleDate = SampleDate, n_mgl = Result) %>% 
  mutate(sampleDate = mdy(sampleDate),
         year = year(sampleDate),
         sampleTime = as.character(sampleTime))

((clean$samplePointID %>% unique()) %in% unique(all_nitrate$samplePointID)) %>% sum()
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
all_nitrate <- readRDS("../Data/1int/caswrb_n_1974-2022.rds")

all_nitrate <- all_nitrate %>% select(names(clean)) %>% 
  bind_rows(clean) %>% 
  # distinct(samplePointID, sampleDate, n_mgl, .keep_all = TRUE) %>% 
  # all_nitrate_tmp %>%
  left_join(loc, by = c("samplePointID" = "PRI_STA_C")) %>%
  mutate(countyNumber = as.numeric(COUNTY), .keep = "unused") %>%
  left_join(c_nm) %>% 
  left_join(sys) %>%
  select(-COMMENT_1) %>%
  # filter out destroyed waste water well and pending status
  filter(!(STATUS %in% c('DS', 'WW', 'PN'))) %>%
  mutate(
    WATER_TYPE = if_else(WATER_TYPE == "g", "G", WATER_TYPE),
    # sampleDate = as_date(sampleDate),
    sampleHour = str_extract(sampleTime, "\\d{2}"),
    # raw, untreated, monitoring well, and agriculture well considered raw
    raw = if_else(map(str_extract_all(STATUS, "."), 2) %in% c('R', 'U', 'W', 'G'), 1, 0),
    countyName = str_to_lower(countyName)
  )  distinct(samplePointID, sampleDate, n_mgl, .keep_all = TRUE) %>% 

# check if there are duplicates

dups <- all_nitrate %>% 
  group_by_all() %>% 
  filter(n()>1)
all_nitrate <- all_nitrate %>%  distinct(samplePointID, sampleDate, n_mgl, .keep_all = TRUE) %>% 
  drop_na(SYSTEM_NO, sampleDate, n_mgl)
rm(list = ls(pattern = 'chem.+'))

saveRDS(all_nitrate, "../Data/1int/caswrb_n_1974-2022.rds")

# Tidy save N data at the spid and year level -----------------------------

n_reg <- readRDS("../Data/1int/caswrb_n_1974-2022.rds")

n_reg <- n_reg %>% 
  mutate(n_mgl = Winsorize(n_mgl, probs = c(0, .99))) %>% 
  filter(WATER_TYPE %in% c("G", "S"), !is.na(n_mgl)) %>% 
  mutate(gw = if_else(WATER_TYPE == "G", 1, 0)) %>% 
  group_by(gw, samplePointID, year, SYSTEM_NO, countyName, 
           SYSTEM_NAM, STATUS, ZIP, POP_SERV, raw, CITY) %>% 
  summarise(mean_n = mean(n_mgl, na.rm = TRUE),
            median_n = median(n_mgl, na.rm = TRUE))

saveRDS(n_reg, "../Data/1int/caswrb_n_reg.rds")

# 2. Create a dataset that is at the system no. year for delivered water to household or non-transient

# Split nitrate observations into only one and more then 1

# ind <- readRDS("../Data/1int/pws_ind.rds") %>% 
#   distinct(SYSTEM_NO, .keep_all = TRUE)
# 
# n_list <- n_reg %>%
#   # mutate(n_mgl = Winsorize(n_mgl, probs = c(0, .99))) %>% 
#   # left_join(ind) %>%
#   filter(!(
#     STATUS %in% c(
#       'AB',
#       'AG',
#       'CM',
#       'SR',
#       'ST',
#       'SU',
#       'PN',
#       'MW',
#       'WW',
#       'IT',
#       'IR',
#       'IU',
#       'IS',
#       'DS'
#     )
#   )) %>%
#   group_by(SYSTEM_NO, year) %>%
#   add_count() %>%
#   mutate(only_one_obs = if_else(n == 1, 1, 0),
#          has_treated = any(
#            unique(STATUS) %in% c('AT', 'AU', 'CT', 'CU', 'DR', 'DT', 'PT', 'PU')
#          )) 
# 
# # View(n_list %>% right_join(n.list.index[209,]))
# 
# # the second list is the spidXy that has only one observations
# # I split the spid x year data into cases to assess which spid should be considered 'delivered'
# # the first one is delivered as there is only 1 sample from 1 spid in that year
# 
# n.list <- split(n_list, f = n_list$only_one_obs)
# set.seed(38780)
# 
# # n.list[[1]] %>% ungroup() %>% filter(SYSTEM_NO==sample(n_reg$SYSTEM_NO, 1), year %in% 2012:2019) %>% 
# #   dplyr::select(1:4, STATUS, only_one_obs, has_treated)
# 
# # just take plain old mean of the spid years with only one observation
# delivered1 <- n.list[[2]] %>%
#   dplyr::select(SYSTEM_NO, year, mean_n) %>%
#   group_by(SYSTEM_NO, year) %>%
#   summarise(n = mean(mean_n, na.rm = TRUE),
#             max_n = mean_n, 
#             min_n = mean_n)
# 
# # further split the ones with more than 1 sources into treated and raw
# 
# # first is FALSE (no treated)
# # second is TRUE (has treated)
# 
# n.list.many <- split(n.list[[1]], f = n.list[[1]]$has_treated)
# 
# # generate function
# delivered2 <- n.list.many[[1]] %>% 
#   group_by(SYSTEM_NO, year) %>%
#   summarise(n = mean(mean_n, na.rm = TRUE),
#             max_n = max(mean_n, na.rm = TRUE),
#             min_n = min(mean_n, na.rm = TRUE))
# 
# delivered3 <- n.list.many[[2]] %>% 
#   filter(STATUS %in% c('AT', 'AU', 'CT', 'CU', 'DR', 'DT', 'PT', 'PU')) %>% 
#   group_by(SYSTEM_NO, year) %>%
#   summarise(n = mean(mean_n, na.rm = TRUE),
#             max_n = max(mean_n, na.rm = TRUE),
#             min_n = min(mean_n, na.rm = TRUE))
# 
# delivered_n_all <- bind_rows(delivered1, delivered2, delivered3) %>%
#   arrange(SYSTEM_NO, year)
# 
# delivered_n_all <- delivered_n_all %>% distinct()
# 
# write_rds(delivered_n_all, "../Data/1int/caswrb_n_delivered.rds")

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

