###########################
# This script explores the grant data from CA SWRB
# There is a 1) dataset for the universe (?) of water system that are out of compliance (not sure if universe)
# or is it just the PWS that has submitted a grant proposal
# There is another dataset that tells me the PWSs that had receive a grant for drinking water
# 2022-06-17


home <- "/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/Data"

# home <- "G:/My Drive/0Projects/1Water/2Quality/Data/"

# load package

library(tidyverse)
library(readr)
library(lubridate)
library(readxl)
library(janitor)
library(cowplot)


# Read the grant data in  -------------------------------------------------

ind <- readRDS(file.path(home, "1int/pws_ind.rds")) %>% 
  mutate(SYSTEM_NAM = str_to_lower(SYSTEM_NAM) %>% clean_strings(remove_char = '\\s') %>% 
           str_replace('north fork union', 'north fork elementary')) 

ni <-read_rds(file.path(home, "1int/caswrb_n_reg.rds")) 
grants <- read_csv(file.path(home, "UDWN_Projects.csv")) %>% 
  clean_names() %>% 
  mutate(SYSTEM_NO = str_pad(pws, 7, side = 'left', pad = '0'))

grants.ind <- left_join(grants, ind, by ='SYSTEM_NO') %>% 
  mutate(date = mdy(date_approved),
         year = year(date))

grants.ind %>% ggplot(aes(year, grant_amount, color = project_type)) +
  stat_summary(geom = 'line', fun = 'sum') +
  theme_minimal() +
  facet_wrap(vars(b_majority_latino))


# Read out of compliant data in  ------------------------------------------
ooc <- read_xlsx(file.path(home, "out_of_compliance.xlsx")) %>% clean_names()

ooc1 <- filter(ooc, water_system_key != 'WATER SYSTEM KEY', 
               !str_detect(water_system_key, '1Type of solution')) %>% 
  mutate(water_system_nm = water_system %>% str_to_lower() %>% 
           str_replace('\r\n', ' ') %>% str_replace('\'', ' '),
         nitrate_problem = str_detect(analyte_s, "NITRATE|NITRITES"))
  

fuzzy_result <- merge_plus(data1 = ind, 
                           data2 = ooc1,
                           by.x = "SYSTEM_NAM",
                           by.y = "water_system_nm", match_type = "fuzzy", 
                           unique_key_1 = "SYSTEM_NO",
                           unique_key_2 = "water_system")

match <- fuzzy_result$matches %>% as_tibble() %>% filter(!is.na(nitrate_problem))
in_compliance <- fuzzy_result$data1_nomatch %>% as_tibble() %>% filter(!is.na(nitrate_problem))

ggplot(match, aes(x=percent_hispanic, color = nitrate_problem)) +
  # geom_density(data = in_compliance, aes(x=percent_hispanic), color = 'red') +
  geom_density() +
  theme_cowplot()

ggplot(match, aes(x=percent_ag, color = nitrate_problem)) +
  # geom_density(data = in_compliance, aes(x=percent_hispanic), color = 'red') +
  geom_density() +
  theme_cowplot()

ggplot(match, aes(x=POP_SERV, color = nitrate_problem)) +
  # geom_density(data = in_compliance, aes(x=percent_hispanic), color = 'red') +
  geom_density() +
  theme_cowplot()

median_hh_income

# investigate match for nitrate type 


