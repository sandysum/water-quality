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

source("Scripts/helper_functions_models.R")
home <- "/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/Data/"
# home <- "G:/My Drive/0Projects/1Water/2Quality/Data"
# Read data in ------------------------------------------------------------

# read arsenic and nitrate data from the CA SWRB portal 
ar <- read_rds(file.path(home, "1int/caswrb_ar_1974-2022.rds"))
ni <-read_rds(file.path(home, "1int/caswrb_n_1974-2022.rds"))
ind <- read_rds(file.path(home, "1int/pws_ind.rds")) %>% filter(
  PrincipalCountyServed %in% str_to_upper(c('san luis obispo', 'santa barbara', 'ventura'))
)

# explore and clean ind
# there are duplicates in ind

# which are the duplicates?
# keep only the PWS that are relevant for my analysis

ni.cc <- ni %>% filter(countyName %in% c('san luis obispo', 'santa barbara', 'ventura')) %>% 
  left_join(ind)

p <- ni.cc %>% 
  mutate(n_mgl = Winsorize(n_mgl, probs = c(0, 0.99))) %>%
  group_by(b_majority_latino, year) %>% 
  drop_na(b_majority_latino) %>% 
  summarise(mean_n = mean(n_mgl, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = mean_n, color = factor(b_majority_latino))) +
  geom_line() +
  theme_cowplot() +
  scale_color_brewer(palette = 'Dark2')

p %>% add_drought()

p <- ni.cc %>% 
  drop_na(median_hh_income) %>% 
  mutate(income_bins = cut_number(median_hh_income, 3)) %>% 
  group_by(income_bins, year) %>% 
  # drop_na(b_majority_latino) %>% 
  summarise(mean_n = median(n_mgl, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = mean_n, color = factor(income_bins))) +
  geom_smooth() +
  theme_cowplot() +
  scale_color_brewer(palette = 'Dark2')


# Look at social ind for CC -----------------------------------------------

ca_stats <- get_acs(
  geography = "tract",
  # Median household income in the past 12 months (in 2019 inflation-adjusted dollars)
  # https://api.census.gov/data/2019/acs/acs5/groups/B19013.html
  # median income, total population, total hispanic or latino, total white
  # percent speak spanish, speak other language and english not well, less than HS graduates
  variables = c("B19013_001", "B01003_001", "B03001_003E", "B02001_002", "B06007_003", "B06007_008",
                "B16010_002"),
  state = "CA", 
  geometry = TRUE
)

cc <- ca_stats %>% filter(str_detect(NAME, 'Barbara|Obispo|Ventura')) %>% st_simplify(dTolerance = 10)
# I am not sure if the variable name is correct. Seems like total pop latino and total pop is giving me the same thing 2
cc <- cc %>% mutate(variable_nm = case_when(
  variable == "B19013_001" ~ "median_hh_income",
  variable == "B01003_001" ~ "total_pop",
  variable == "B03001_003" ~ "total_pop_latino",
  variable == "B02001_002" ~ "total_pop_white",
  variable == 'B06007_003' ~ "total_pop_speak_spanish",
  variable == 'B06007_008' ~ "total_pop_english_not_well",
  variable == 'B16010_002' ~ "total_pop_low_edu",
  TRUE ~ NA_character_
)) %>% dplyr::select(-moe, -variable) %>% spread(key = variable_nm, value = estimate) %>%
  mutate(
    percent_non_white = ((total_pop - total_pop_white) / total_pop) *100,
    percent_hispanic = (total_pop_latino / total_pop)*100,
    percent_low_edu = (total_pop_low_edu / total_pop)*100,
    percent_spanish_speaker = (total_pop_speak_spanish / total_pop)*100,
    percent_not_fluent_english = (total_pop_english_not_well / total_pop)*100
  ) 

cc %>% 
  ggplot(aes(fill = percent_hispanic)) +
  geom_sf() +
  scale_fill_gradient(low = 'mistyrose', high = 'orchid4') +
  theme_minimal()
