#################################
# Read and clean preterm birth 
# 
# 2022/05/23
# sandysum@ucsb.edu
#################################
# Heading -----------------------------------------------------------------
# A script to expore CA SWRB contaminant monitoring data
# sandysum@ucsb.edu
# 2021/09/04

library(sf)
library(janitor)
library(tidyverse)
library(tigris)
library(DescTools)
options(scipen=999)
rm(list = ls())

home <- "/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/Data/"
# home <- "G:/My Drive/0Projects/1Water/2Quality/Data"
# Read data in ------------------------------------------------------------

# read arsenic and nitrate data from the CA SWRB portal 
pt <- read_csv("/Users/sandysum/Downloads/preterm-and-very-preterm-births-by-county-2010-2018-3.csv") %>% 
  clean_names() %>% 
  replace_na(list(events=0, percent=0, lower_95_percent_ci=0, upper_95_percent_ci=0))
  
pt.race <- read_csv("/Users/sandysum/Downloads/preterm-and-very-preterm-births-by-raceethnicity-2010-2018.csv") %>% 
  clean_names() %>% 
  replace_na(list(events=0, percent=0, lower_95_percent_ci=0, upper_95_percent_ci=0))

pt.race %>% ggplot(aes(year, percent, color=race_ethnicity))+
  geom_line() +
  facet_wrap(vars(birth_type)) +
  scale_color_brewer(palette = 'Dark2') +
  theme_minimal()

pt %>% ggplot(aes(year, percent))+
  stat_summary(fun = 'mean', geom = 'line') +
  # geom_path(aes(group = county), color = 'grey70', alpha = .6) +
  facet_wrap(vars(birth_type)) +
  scale_color_brewer(palette = 'Dark2') +
  theme_minimal()
