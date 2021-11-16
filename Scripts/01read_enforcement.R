#################################
# Read and clean SDWA water violations data
# 
# 5/7/2021
# sandysum@ucsb.edu
#################################

rm(list = ls())
home <- "/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/Data"
home <- "G:/My Drive/0Projects/1Water/2Quality/Data"

# load package

library(tidyverse)
library(readxl)
library(readr)

# read files in 

pws <- read_csv(file.path(home, "/SDWA-DL/SDWA_PUB_WATER_SYSTEMS.csv"))
dww <- read.csv("../Data/ca_drinkingwatersystems_meta.csv")
violations <- read_csv(file.path(home, "/SDWA-data/SDWA_VIOLATIONS.csv"))

pws_ni_vio <- violations %>% 
  filter(STATE=="CA" & RULE_NAME == "Nitrates" & HEALTH_BASED == "Y") %>% 
  mutate(SYSTEM_NO = str_extract(PWSID, "(?<=CA)\\d+"))

pws_ar_vio <- violations %>%
  filter(STATE == "CA" & RULE_NAME == "Arsenic" & HEALTH_BASED == "Y") %>%
  mutate(SYSTEM_NO = str_extract(PWSID, "(?<=CA)\\d+"))

rm(violations)

pdsi <- readRDS("../Data/drought/pdsi_pws_year.rds")

# the goal is to get things to the PWS-year level

pws_count <- pws %>% group_by(PWSID, SOURCE_WATER) %>% summarise(count = n()) 
pws %>% group_by(PWSID) %>% summarise(count = n()) 

vio <- violations %>% 
  filter(STATE=="CA" & RULE_NAME == "Nitrates" & HEALTH_BASED == "Y") %>% 
  mutate(SYSTEM_NO = str_extract(PWSID, "(?<=CA)\\d+")) %>% 
  left_join(loc)

# Nitrate ----------------

ni_vio_new <- pws_ni_vio %>% dplyr::select(SYSTEM_NO, PWSID, year = BEGIN_YEAR, VIOLATION_ID, VIOLATION_NAME, RULE_NAME) %>% 
  group_by(SYSTEM_NO, year, PWSID) %>% 
  # summarise(n_new_violations = unique(VIOLATION_ID) %>% length())
  summarise(n_new_violations = n())

# some violation ID span many years # if RTC year is NA means that the system never returned to compliant?
# to make it easy look at new violations issued

# investigate if begin_year or fiscal_year is the actual year in which a PWS gets a violation
# compare to stats on the dashboard
# https://echo.epa.gov/trends/comparative-maps-dashboards/drinking-water-dashboard?state=California&view=activity&criteria=adv&yearview=FY
# nope, this is not the same number as that on the dashboard

violations %>%  filter(STATE=="CA") %>% dplyr::select(BEGIN_YEAR, VIOLATION_ID) %>% distinct() %>% select(BEGIN_YEAR) %>% table()
violations %>%  filter(STATE=="CA") %>% dplyr::select(FISCAL_YEAR, VIOLATION_ID) %>% distinct() %>% select(FISCAL_YEAR) %>% table()

# unique(pws_ni_vio$PWSID)[which(!unique(pws_ni_vio$PWSID) %in% unique(pdsi$SABL_PWSID))] %>% length()
# ughhh no shapefile, no pdsi for 

# The fiscal year might be the year in which these data were added into the system; yeah I think so 
  
reg_ni <- pdsi %>% 
  ungroup() %>% 
  filter(year %in% ni_vio_new$year) %>% 
  left_join(ni_vio_new, by = c('SABL_PWSID' = 'PWSID', 'year', 'SYSTEM_NO')) %>% 
  left_join(pws, by = c('SABL_PWSID' = 'PWSID')) %>% 
  left_join(dww %>% dplyr::select(Water.System.No, Total.Population), by = c('SABL_PWSID' = 'Water.System.No')) %>% 
  mutate(n_new_violations = replace_na(n_new_violations, 0)) %>% 
  group_by(SABL_PWSID) %>% 
  arrange(SABL_PWSID, year) %>% 
  mutate(
    d = mean_pdsi, 
    dlead = lead(d),
    dlead2 = lead(dlead),
    dlag1 = lag(d),
    dlag2 = lag(dlag1),
    dlag3 = lag(dlag2),
    dlag4 = lag(dlag3),
    dlag5 = lag(dlag4),
    dlag6 = lag(dlag5)) %>% 
  mutate(
    PWSID = factor(SABL_PWSID),
    CITY_NAME = factor(CITY_NAME),
    gw = if_else((PRIMARY_SOURCE_CODE == "GW"|PRIMARY_SOURCE_CODE == "GWP"), 1, 0),
    gw = factor(gw, levels = c('1', '0'))
  )

# run regressions

mod_vio_ni <- felm(n_new_violations ~ d + dlag1 + dlag2 + dlag3 | SABL_PWSID + year | 0 | SABL_PWSID, data = reg_ni)
summary(mod_vio_ni)


mod_vio_ni2 <- felm(n_new_violations ~ d + dlag1 + dlag2 + dlag3
                   + d:gw + dlag1:gw + dlag2:gw + dlag3:gw | SABL_PWSID + year | 0 | SABL_PWSID, data = reg_ni)
summary(mod_vio_ni2)

stargazer::stargazer(mod_vio_ni, mod_vio_ni2, omit = c('year'), single.row = TRUE,
                     dep.var.labels   = "Number of new Nitration violations", dep.var.caption = "Outcome:", omit.stat = c("adj.rsq", "ser"))

# Arsenic -----------------------------------------------------------------
ar_vio_new <- pws_ar_vio %>% dplyr::select(SYSTEM_NO, PWSID, year = BEGIN_YEAR, VIOLATION_ID, VIOLATION_NAME, RULE_NAME) %>% 
  group_by(SYSTEM_NO, year, PWSID) %>% 
  # summarise(n_new_violations = unique(VIOLATION_ID) %>% length())
  summarise(n_new_violations = n())

# The fiscal year might be the year in which these data were added into the system; yeah I think so 

reg_ar <- pdsi %>% 
  ungroup() %>% 
  filter(year %in% ar_vio_new$year) %>% 
  left_join(ar_vio_new, by = c('SABL_PWSID' = 'PWSID', 'year', 'SYSTEM_NO')) %>% 
  left_join(pws, by = c('SABL_PWSID' = 'PWSID')) %>% 
  left_join(dww %>% dplyr::select(Water.System.No, Total.Population), by = c('SABL_PWSID' = 'Water.System.No')) %>% 
  mutate(n_new_violations = replace_na(n_new_violations, 0)) %>% 
  group_by(SABL_PWSID) %>% 
  arrange(SABL_PWSID, year) %>% 
  mutate(
    d = mean_pdsi, 
    dlead = lead(d),
    dlead2 = lead(dlead),
    dlag1 = lag(d),
    dlag2 = lag(dlag1),
    dlag3 = lag(dlag2),
    dlag4 = lag(dlag3),
    dlag5 = lag(dlag4),
    dlag6 = lag(dlag5)) %>% 
  mutate(
    PWSID = factor(SABL_PWSID),
    CITY_NAME = factor(CITY_NAME),
    gw = if_else((PRIMARY_SOURCE_CODE == "GW"|PRIMARY_SOURCE_CODE == "GWP"), 1, 0),
    gw = factor(gw, levels = c('1', '0'))
  )

# run regressions

mod_vio_ar <- felm(n_new_violations ~ d + dlag1 + dlag2 + dlag3 + dlag4 | SABL_PWSID + year | 0 | SABL_PWSID, data = reg_ar)
summary(mod_vio_ar)

# mod_vio_ar_pop <- felm(n_new_violations ~ d + dlag1 + dlag2 + dlag3 | SABL_PWSID + year | 0  | SABL_PWSID, data = reg_ar %>% drop_na(Total.Population), weights = reg_ar$Total.Population[!is.na(reg_ar$Total.Population)])
# summary(mod_vio_ar_pop)


mod_vio_ar2 <- felm(n_new_violations ~ d + dlag1 + dlag2 + dlag3
                   + d:gw + dlag1:gw + dlag2:gw + dlag3:gw | SABL_PWSID + CITY_NAME + year | 0 | SABL_PWSID, data = reg_ar)
summary(mod_vio_ar2)
stargazer::stargazer(mod_vio_ar, mod_vio_ar2, omit = c('year'), single.row = TRUE,
                     dep.var.labels   = "Number of new Arsenic violations", dep.var.caption = "Outcome:", omit.stat = c("adj.rsq", "ser"))








