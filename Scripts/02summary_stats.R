
# Create summary statistic tables -----------------------------------------
# 2022-06-15
# this script creates basic summary statistics for PWS in the dataset

library(tidyverse)
library(fixest)
library(DescTools)
library(future.apply)
library(did)
library(Hmisc)
library(cowplot)
source("/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/water-quality/Scripts/helper_functions_models.R")
source("/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/water-quality/Scripts/helper_functions_es.R")
options(digits=3)
# Read in data ------------------------------------------------------------
# home <- "G:/My Drive/0Projects/1Water/2Quality/Data/"
home <- "/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/Data/"
pdsi <- readRDS(file.path(home, "../Data/drought/pdsi_pws_year.rds"))

ind <- readRDS(file.path(home, "1int/pws_ind.rds"))
ni <-read_rds(file.path(home, "1int/caswrb_n_reg.rds")) %>% left_join(ind) %>% 
  left_join(pdsi)
geog <- read_csv("../Data/SDWA-DL/SDWA_GEOGRAPHIC_AREAS.csv") %>% 
  distinct()

ni_drought <- subset_years(2007, pollutant = ni , 2021, 1) %>% 
  prep_reg() %>% 
  mutate(b_majority_latino = factor(b_majority_latino),
         b_low_income = factor(b_low_income)) %>% 
  filter(STATUS %in% c('AT', 'AR', 'AU', 'CM', 'CR', 'CT', 'DT', 'DR', 'SR',
                       'SU', 'ST', 'CU')) 

ni_drought$samplePointID %>% unique() %>% length()

facilities <- read_csv(file.path(home, "SDWA-DL/SDWA_FACILITIES.csv")) %>% 
  filter(PWSID %in% ni$SABL_PWSID, FACILITY_ACTIVITY_CODE == 'A') %>% 
  # to match facilities dataset from SWDA, we have to paste the numerical value of PWSID to the state facility id 
  # and this correspond to the samplePointID in the CA SWRB
  mutate(samplePointID = paste0(str_extract(PWSID, '\\d+'), '-', STATE_FACILITY_ID)) %>% 
  filter(samplePointID %in% ni_drought$samplePointID) %>% 
  left_join(ind, by =c('PWSID'='WaterSystemNo'))

df <- facilities %>% 
  mutate(prop_latino = cut_interval(percent_hispanic, 4)) %>% 
  group_by(SYSTEM_NO, prop_latino, agArea, median_hh_income, percent_hispanic,
           percent_non_white, percent_not_fluent_english, percent_low_edu, POP_SERV, percent_ag,
           OwnerType, PrimaryWaterSourceType, ResidentialPopulation, FACILITY_TYPE_CODE, FeeCodeDescription) %>% 
  summarise(number_source = length(unique(samplePointID)),
            number_source_treated = sum(IS_SOURCE_TREATED_IND=='Y'),
            number_gw_source = sum(WATER_TYPE_CODE%in%c('GU', 'GW')),
            number_sw_source = sum(WATER_TYPE_CODE%in%c('SW')),
            number_tp = sum(FACILITY_TYPE_CODE == 'TP')) %>% 
  mutate(prop_latino = forcats::fct_explicit_na(prop_latino))

df %>% ggplot(aes(number_source_treated))

df %>% ungroup() %>% select(-1) %>% tbl_summary(by = prop_latino, 
                                                statistic = list(all_continuous() ~ "{mean}, {median} ({min}, {max})", 
                                                all_categorical() ~ "{n} ({p}%)"))
df %>% describeBy(group = 'prop_latino')


