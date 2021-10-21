
#################################
# Read and clean monitoring data
# 
# 5/8/2021
# sandysum@ucsb.edu
#################################

home <- "/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/Data"

# load package

library(tidyverse)
library(readr)
library(lubridate)
library(readxl)
library(rdrobust)
library(cowplot)

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


# Read, clean, and save Nitrate data --------------------------------------

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

# chem %>% 
#   group_by(year) %>% 
#   summarize(med_n = median(N_no3_mgL, na.rm = TRUE)) %>% 
#   ggplot(aes(x=year, y = med_n)) +
#   geom_line() +
#   geom_point() +
#   geom_vline(xintercept = c(2012, 2016), color = 'red') +
#   scale_x_continuous(breaks = seq(2004, 2020, 2)) +
#   theme_minimal()
# 
# chem_med <- chem %>%
#   group_by(year) %>% 
#   summarise(med_N = median(N_no3_mgL, na.rm = TRUE)) %>% 
#   mutate(source = "cswrb")
# 
# #### join to NWIS data and see discrepencies
# 
# all <- chem_med %>% bind_rows(nwis_med)
# 
# all %>% 
#   ggplot(aes(year, med_N, color = source)) +
#   geom_point() + 
#   geom_line() +
#   geom_vline(xintercept = c(2012, 2016), color = 'red') +
#   # facet_wrap(vars(AquiferName)) +
#   theme_minimal() +
#   # scale_x_continuous(breaks = seq(2004, 2020, 2)) +
#   scale_color_brewer(palette = "Set2")
# 
# #### 7/6/2021 Exploration
# 
# chem_med %>% 
#   ungroup() %>% 
#   filter(WATER_TYPE %in% c("G", "S")) %>% 
#   ggplot(aes(year, med_N, color = WATER_TYPE)) +
#   geom_line() +
#   geom_point() +
#   scale_x_continuous(breaks = 2000:2015) +
#   theme_minimal()
# 
# chem %>% 
#   filter(N_no3_mgL < 80, WATER_TYPE %in% c("G", "S", NA)) %>% 
#   ungroup() %>% 
#   ggplot(aes(factor(year), N_no3_mgL)) +
#   geom_boxplot() +
#   facet_wrap(vars(WATER_TYPE), ncol = 1) +
#   theme_minimal()
# 
# chem %>% filter(SYSTEM_NO =="4210004", STATUS %in% c("AR", "AT")) %>%
#   ggplot(aes(x = sample_date, y = N_no3_mgL, color = SOURCE_NAM)) +
#   geom_line() + 
#   theme_minimal()
# 
# chem %>% filter(year == max(year)) %>% 
#   group_by(SYSTEM_NO, POP_SERV) %>% 
#   summarise(n = max(N_no3_mgL>10)) %>% 
#   ungroup() %>% 
#   summarise(n = sum(n),
#             total_pop = sum(POP_SERV, na.rm = TRUE))
#   
# # check that all county names are accounted for
# 
# chem$COUNTY %>% unique()  
# blue$COUNTY %>% unique()
# (blue$COUNTY %>% unique()) %in% (chem08_14_clean$COUNTY %>% unique())
# 
# # look at distribution: might be indicative of manipulation
# 
# dist_p <- chem %>% 
#   filter(N_no3_mgL < 200) %>% 
#   ggplot(aes(x = N_no3_mgL)) +
#   geom_histogram(fill = NA, col = "black", breaks = seq(0, 200, by = 2)) +
#   scale_x_continuous(breaks = seq(0, 200, by = 10)) +
#   geom_vline(xintercept = 10, col = 'red') +
#   theme_minimal()
# 
# save_plot(filename = "Google Drive/My Drive/0Projects/1Water/Plots/N_dist.png", dist_p, scale = 1.2)
#   
# # clean monitoring data and merge with infant death data, IGNORING THAT TREATMENT MIGHT BE FUZZY FOR NOW
# 
# n_df <- chem %>% 
#   filter(N_no3_mgL < 500) %>% 
#   # what if I don't group by county year for now
#   # group_by(COUNTY, year) %>% 
#   # summarise(max_N = max(N_no3_mgL, na.rm = TRUE),
#   #           avg_N = mean(N_no3_mgL, na.rm = TRUE)) %>% 
#   mutate(N_norm = N_no3_mgL - 10) %>%
#   left_join(blue %>% select(COUNTY, year, death_counts), by =c('COUNTY', 'year')) 
# 
# n_df_i <- chem %>% 
#   filter(N_no3_mgL < 500) %>% 
#   # what if I don't group by county year for now
#   group_by(COUNTY, year) %>%
#   summarise(max_N = max(N_no3_mgL, na.rm = TRUE),
#             avg_N = mean(N_no3_mgL, na.rm = TRUE)) %>%
#   left_join(blue %>% select(COUNTY, year, death_counts), by =c('COUNTY', 'year')) 
# 
# rdmod <- rdrobust(y=n_df$death_counts, x=n_df$N_norm, all=TRUE)
# 
# rdplot(y=n_df$death_counts, x=n_df$N_norm,
#        x.lim = c(-10,40),
#        y.lim = c(0,80000),
#        x.lab="Normalized NO3 at 10 mg/L",
#        y.lab="Total infant methemoglobinemia (count)", title = "")
# 
# ggplot(n_df_i, aes(max_N, death_counts)) +
#   geom_point() +
#   lims(x=c(0, 500)) +
#   theme_minimal() +
#   labs(y = "Infant methemoglobinemia\n (counts per county-year)", x = "\nMaximum annual \n nitrate level reported (mg/L)")
# 
# ggplot(n_df, aes(death_counts, avg_N)) +
#   geom_point() +
#   theme_bw()

