
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
    dplyr::select(samplePointID = PRIM_STA_C, sampleDate = SAMP_DATE, LAB_NUM, ar_ugl =FINDING) %>% 
    mutate(year = year(sampleDate),
           labNum = as.integer(LAB_NUM)) %>% 
    select(-LAB_NUM) %>% 
    left_join(loc, by = c("samplePointID"="PRI_STA_C")) %>%
    mutate(countyNumber = as.numeric(COUNTY), .keep = "unused") %>%
    left_join(c_nm) %>% 
    select(-COMMENT_1)
}) %>% bind_rows() %>% 
  left_join(sys)

rm(list = ls(pattern = 'chem'))

saveRDS(all_arsenic, "1int/caswrb_ar_1998-2021.rds")

################################# SUMMARY STATS AND EXPLORATION FOR ARSENIC ################################# 

rawSources <- c("AR", "AU", "CR", "PR")

# should I drop the good measurements or interpolate?
raw <- ar %>% 
  # group_by(samplePointID) %>% 
  filter(STATUS %in% rawSources) 

raw %>%  group_by(samplePointID) %>% 
  dplyr::summarize(n_obs = n()) %>%
  select(n_obs) %>%
  as_vector() %>%
  quantile() 

# 75 % of the monitoring stations (total 13,612) has at least 14 observations... but how frequent are they? Filter to locations with at least 8 ...

raw <- raw %>% 
  group_by(samplePointID) %>% filter(n()>8)

# filter to at least 10 observations, and then what data range? Should at lease cover the duration of the drought
set.seed(24); 
set.seed(4)
set.seed(3080)
raw %>%
  group_by(samplePointID) %>% 
  filter(samplePointID %in% sample(raw$samplePointID, size = 6, replace = FALSE)) %>% 
  ggplot(aes(sampleDate, ar_ugl, color = samplePointID)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y")

# Save and clean for Nitrate Data

# filter all only 00618 for nitrate as N
chem74_99 <- read_csv(file.path(home, "ca_water_qual/chem_1974_1999.csv")) %>% filter(STORE_NUM == "00618")
chem08_14 <- read_csv(file.path(home, "ca_water_qual/chem_2008_2014.csv")) %>% filter(STORE_NUM == "00618")
chem15_20 <- read_csv(file.path(home, "ca_water_qual/chem_2015_2020.csv")) %>% filter(STORE_NUM == "00618")
chem00_07 <- read_csv(file.path(home, "ca_water_qual/chem_2000_2007.csv")) %>% filter(STORE_NUM == "00618")

all_nitrate <- future.apply::future_lapply(list(chem00_07, chem08_14, chem15_20, chem74_99), function(x){
  x %>% mutate_at(vars(matches("DATE")), ~(mdy_hms(.))) %>% 
    dplyr::select(samplePointID = PRIM_STA_C, sampleDate = SAMP_DATE, LAB_NUM, n_mgl =FINDING) %>% 
    mutate(year = year(sampleDate),
           labNum = as.integer(LAB_NUM)) %>% 
    select(-LAB_NUM) %>% 
    left_join(loc, by = c("samplePointID"="PRI_STA_C")) %>%
    mutate(countyNumber = as.numeric(COUNTY), .keep = "unused") %>%
    left_join(c_nm) %>% 
    select(-COMMENT_1)
}) %>% bind_rows() %>% 
  left_join(sys)

rm(list = ls(pattern = 'chem'))

saveRDS(all_nitrate, "../Data/1int/caswrb_n_1998-2021.rds")

chem %>% 
  group_by(year) %>% 
  summarize(med_n = median(N_no3_mgL, na.rm = TRUE)) %>% 
  ggplot(aes(x=year, y = med_n)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = c(2012, 2016), color = 'red') +
  scale_x_continuous(breaks = seq(2004, 2020, 2)) +
  theme_minimal()

chem_med <- chem %>%
  group_by(year) %>% 
  summarise(med_N = median(N_no3_mgL, na.rm = TRUE)) %>% 
  mutate(source = "cswrb")

#### join to NWIS data and see discrepencies

all <- chem_med %>% bind_rows(nwis_med)

all %>% 
  ggplot(aes(year, med_N, color = source)) +
  geom_point() + 
  geom_line() +
  geom_vline(xintercept = c(2012, 2016), color = 'red') +
  # facet_wrap(vars(AquiferName)) +
  theme_minimal() +
  # scale_x_continuous(breaks = seq(2004, 2020, 2)) +
  scale_color_brewer(palette = "Set2")

#### 7/6/2021 Exploration

chem_med %>% 
  ungroup() %>% 
  filter(WATER_TYPE %in% c("G", "S")) %>% 
  ggplot(aes(year, med_N, color = WATER_TYPE)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 2000:2015) +
  theme_minimal()

chem %>% 
  filter(N_no3_mgL < 80, WATER_TYPE %in% c("G", "S", NA)) %>% 
  ungroup() %>% 
  ggplot(aes(factor(year), N_no3_mgL)) +
  geom_boxplot() +
  facet_wrap(vars(WATER_TYPE), ncol = 1) +
  theme_minimal()

chem %>% filter(SYSTEM_NO =="4210004", STATUS %in% c("AR", "AT")) %>%
  ggplot(aes(x = sample_date, y = N_no3_mgL, color = SOURCE_NAM)) +
  geom_line() + 
  theme_minimal()

chem %>% filter(year == max(year)) %>% 
  group_by(SYSTEM_NO, POP_SERV) %>% 
  summarise(n = max(N_no3_mgL>10)) %>% 
  ungroup() %>% 
  summarise(n = sum(n),
            total_pop = sum(POP_SERV, na.rm = TRUE))
  
# check that all county names are accounted for

chem$COUNTY %>% unique()  
blue$COUNTY %>% unique()
(blue$COUNTY %>% unique()) %in% (chem08_14_clean$COUNTY %>% unique())

# look at distribution: might be indicative of manipulation

dist_p <- chem %>% 
  filter(N_no3_mgL < 200) %>% 
  ggplot(aes(x = N_no3_mgL)) +
  geom_histogram(fill = NA, col = "black", breaks = seq(0, 200, by = 2)) +
  scale_x_continuous(breaks = seq(0, 200, by = 10)) +
  geom_vline(xintercept = 10, col = 'red') +
  theme_minimal()

save_plot(filename = "Google Drive/My Drive/0Projects/1Water/Plots/N_dist.png", dist_p, scale = 1.2)
  
# clean monitoring data and merge with infant death data, IGNORING THAT TREATMENT MIGHT BE FUZZY FOR NOW

n_df <- chem %>% 
  filter(N_no3_mgL < 500) %>% 
  # what if I don't group by county year for now
  # group_by(COUNTY, year) %>% 
  # summarise(max_N = max(N_no3_mgL, na.rm = TRUE),
  #           avg_N = mean(N_no3_mgL, na.rm = TRUE)) %>% 
  mutate(N_norm = N_no3_mgL - 10) %>%
  left_join(blue %>% select(COUNTY, year, death_counts), by =c('COUNTY', 'year')) 

n_df_i <- chem %>% 
  filter(N_no3_mgL < 500) %>% 
  # what if I don't group by county year for now
  group_by(COUNTY, year) %>%
  summarise(max_N = max(N_no3_mgL, na.rm = TRUE),
            avg_N = mean(N_no3_mgL, na.rm = TRUE)) %>%
  left_join(blue %>% select(COUNTY, year, death_counts), by =c('COUNTY', 'year')) 

rdmod <- rdrobust(y=n_df$death_counts, x=n_df$N_norm, all=TRUE)

rdplot(y=n_df$death_counts, x=n_df$N_norm,
       x.lim = c(-10,40),
       y.lim = c(0,80000),
       x.lab="Normalized NO3 at 10 mg/L",
       y.lab="Total infant methemoglobinemia (count)", title = "")

ggplot(n_df_i, aes(max_N, death_counts)) +
  geom_point() +
  lims(x=c(0, 500)) +
  theme_minimal() +
  labs(y = "Infant methemoglobinemia\n (counts per county-year)", x = "\nMaximum annual \n nitrate level reported (mg/L)")

ggplot(n_df, aes(death_counts, avg_N)) +
  geom_point() +
  theme_bw()

