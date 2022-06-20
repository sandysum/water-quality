
# Explore Arsenic after FAR -----------------------------------------------
# 2022/05/02
# this scripts explores arsenic levels in CA after FAR
# test for heterogeneous effects by violations and also SW / GW

library(sf)
library(tidyverse)
library(readxl)
library(readr)
library(lubridate)
library(tigris)
library(DescTools)
library(fixest)
library(tidycensus)
options(scipen=999)
rm(list = ls())

home <- "/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/Data/"
ind <- readRDS("../Data/1int/pws_ind.rds")
home <- "G:/My Drive/0Projects/1Water/2Quality/Data"
# Read data in ------------------------------------------------------------

# read arsenic and nitrate data from the CA SWRB portal 
ar <- read_rds(file.path(home, "1int/caswrb_ar_1974-2022.rds")) %>% 
  left_join(ind) %>% 
  mutate(Raw = if_else(raw==1, 'Untreated', 'Treated'))

as.usgs <- read_csv("../Data/1int/usgs_counties_as.csv")

county_stats <- get_acs(
  geography = "county",
  # Median household income in the past 12 months (in 2019 inflation-adjusted dollars)
  # https://api.census.gov/data/2019/acs/acs5/groups/B19013.html
  # median income, total population, total hispanic or latino, total white
  # percent speak spanish, speak other language and english not well, less than HS graduates
  variables = c("B19013_001", "B01003_001", "B03001_003E", "B02001_002", "B06007_003", "B06007_008",
                "B16010_002"),
  state = "CA",
  geometry = TRUE
) %>% st_simplify(dTolerance = 20)

# I am not sure if the variable name is correct. Seems like total pop latino and total pop is giving me the same thing 2
county_stats <- county_stats %>% mutate(variable_nm = case_when(
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

df <- county_stats %>% filter(GEOID %in% as.usgs$GEOID) %>% 
  left_join(as.usgs %>% dplyr::select(usgs_as, GEOID), by = 'GEOID')

ggplot(ar.did, aes(x=year, y=median_as, color = factor(Raw))) +
  stat_summary(fun = 'mean', geom = 'line', size = 2) +
  facet_grid(rows = vars(WATER_TYPE)) +
  geom_vline(xintercept = c(2001, 2006), color = 'red') +
  # geom_hline(yintercept = 10, color = 'blue') +
  theme_minimal() +
  scale_color_brewer(palette = 'Dark2') +
  labs(x= '\nYear', y='Arsenic concentration (ug/l)', color = ' ')


# Did ---------------------------------------------------------------------

ar.did <- ar %>% 
  filter(year>1998, year < 2020, WATER_TYPE%in%c('S', 'G'), FederalWaterSystemTypeC %in% c('C', 'NTNC')) %>% 
  group_by(year, samplePointID) %>% 
  mutate(median_as = median(as_ugl, na.rm = TRUE), 
         violate_as = if_else(max(as_ugl)>10, 1, 0)) %>% 
  ungroup() %>% 
  mutate(treat = if_else(raw==0&WATER_TYPE=='G', 2006, 0),
         id = as.numeric(str_remove_all(samplePointID, '-')))

example_attgt <- att_gt(yname = "median_as",
                        tname = "year",
                        idname = "id",
                        gname = "treat",
                        xformla = ~1,
                        data = ar.did,
                        panel = FALSE
)

ggdid(example_attgt)

df.reg <- df3 %>% drop_na(death.rates, PM25_conc, county_fips, as_bins, tmax, O3_conc, year, state)

