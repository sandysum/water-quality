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

indx <- readRDS(file.path(home, "1int/pws_ind.rds")) %>% 
  mutate(SYSTEM_NAM = str_to_lower(SYSTEM_NAM) %>% clean_strings(remove_char = '\\s') %>% 
           str_replace('north fork union', 'north fork elementary') %>% 
           str_remove_all("\\,")) 

ni <-read_rds(file.path(home, "1int/caswrb_n_reg.rds")) 
grants <- read_csv(file.path(home, "UDWN_Projects.csv")) %>% 
  clean_names() %>% 
  mutate(SYSTEM_NO = str_pad(pws, 7, side = 'left', pad = '0'))

grants.ind <- left_join(grants, indx, by ='SYSTEM_NO') %>% 
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
           str_replace('\r\n', ' ') %>% str_replace('\'', ' ') %>% 
           str_remove_all('\\,|-|\\.|\\s\\(puc\\)|#|\\[|\\]'),
         nitrate_problem = str_detect(analyte_s, "NITRATE|NITRITES"),
         planning_1 = as.double(planning_1),
         construction = as.double(construction),
         interim = as.double(interim_1_3),
         techassist = as.double(ta_1_2)) %>% 
  replace_na(list(planning_1 = 0, construction = 0, interim = 0, techassist = 0)) %>% 
  left_join(indx, by = c('water_system_nm' = 'SYSTEM_NAM'))
quartz()
ggplot(ooc1, aes(x=percent_not_fluent_english, y = planning_1)) +
  # geom_density(data = in_compliance, aes(x=percent_hispanic), color = 'red') +
  geom_point(size = 2) +
  theme_cowplot()

ggplot(ooc1, aes(x=percent_not_fluent_english, y = techassist)) +
  # geom_density(data = in_compliance, aes(x=percent_hispanic), color = 'red') +
  geom_point(size = 2) +
  theme_cowplot()

ggplot(ooc1, aes(x=percent_hispanic, y = planning_1, color = factor(b_majority_latino))) +
  # geom_density(data = in_compliance, aes(x=percent_hispanic), color = 'red') +
  geom_point(size = 2) +
  theme_cowplot()

# Try fuzzy matching package ----------------------------------------------

fuzzy_result <- merge_plus(data1 = indx, 
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

ggplot(match, aes(x=percent_not_fluent_english)) +
  geom_density(data = in_compliance, aes(x=percent_not_fluent_english), color = 'red') +
  geom_density() +
  theme_cowplot()

median_hh_income

# investigate match for nitrate type 


# Now investigate grants data from  ---------------------------------------
# https://www.waterboards.ca.gov/safer/dw_systems_violations_tool.html

hr2w <- read_csv("../Data/ca_grants/pws_violation_tool_status.csv") %>% clean_names() %>% 
  mutate(SYSTEM_NO = str_extract(water_system_number, "\\d+")) %>% 
  left_join(ind) %>% 
  mutate(prop_latino = cut_interval(percent_hispanic, 6),
         prop_not_eng_fluent = cut_interval(percent_not_fluent_english, 3),
         nitrate_problem = str_detect(analyte_name_s, "NITRATE|NITRITES")) %>% 
  drop_na(prop_latino, prop_not_eng_fluent)

nhr2w <- read_csv("../Data/ca_grants/pws_violation_tool_status.csv") %>% clean_names() %>% 
  mutate(SYSTEM_NO = str_extract(water_system_number, "\\d+")) %>% 
  left_join(ind) %>% 
  mutate(prop_latino = cut_interval(percent_hispanic, 6),
         prop_not_eng_fluent = cut_interval(percent_not_fluent_english, 5),
         nitrate_problem = str_detect(analyte_name_s, "NITRATE|NITRITES")) %>% 
  drop_na(prop_latino, prop_not_eng_fluent) %>% 
  filter(nitrate_problem)

ggplot(nhr2w, aes(x = prop_not_eng_fluent, y = POP_SERV, fill = receiving_financial_assistance)) +
  geom_bar(position = "dodge", stat = "identity")

ggplot(nhr2w %>% mutate(total_financial_assistance= Winsorize(total_financial_assistance, probs = c(0, .97))), 
       aes(x = percent_hispanic, y = total_financial_assistance, 
                 size = POP_SERV,
           color = financial_assistance_project_type)) +
  geom_point() +
  theme_cowplot()

ggplot(hr2w %>% mutate(total_financial_assistance= Winsorize(total_financial_assistance, 
                                                             probs = c(0, .99))), 
       aes(x = percent_not_fluent_english, y = total_financial_assistance, 
           size = POP_SERV,
           color = financial_assistance_status)) +
  geom_point() +
  theme_cowplot()

ggplot(hr2w %>% mutate(total_financial_assistance= Winsorize(total_financial_assistance, 
                                                             probs = c(0, .99))), 
       aes(x = percent_hispanic, y = total_financial_assistance, 
           size = population,
           color = financial_assistance_status)) +
  geom_point() +
  theme_cowplot()

ggplot(
  hr2w %>% mutate(
    total_financial_assistance = Winsorize(total_financial_assistance, probs = c(0, 1))
  ),
  aes(x = percent_not_fluent_english, y = total_financial_assistance,
      size = POP_SERV)
) +
  geom_point() +
  theme_cowplot()

ggplot(
  hr2w %>% mutate(
    total_financial_assistance = Winsorize(total_financial_assistance, probs = c(0, .99))
  ),
  aes(x = percent_not_fluent_english, y = total_financial_assistance,
      size = POP_SERV)
) +
  geom_point() +
  theme_cowplot()
