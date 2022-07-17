#################################
# Read and clean SDWA water violations data
# 
# 5/7/2021
# sandysum@ucsb.edu
#################################

rm(list = ls())
home <- "/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/Data"
home <- "G:/My Drive/0Projects/1Water/2Quality/Data"
source("Scripts/helper_functions_models.R")
# load package

library(tidyverse)
library(readxl)
library(readr)
library(lfe)

# read files in 

pws <- read_csv(file.path(home, "/SDWA-DL/SDWA_PUB_WATER_SYSTEMS.csv"))
dww <- read.csv("../Data/ca_drinkingwatersystems_meta.csv")
violations <- read_csv(file.path(home, "/SDWA-data/SDWA_VIOLATIONS_CA.csv"))
# write_csv(violations %>%  filter(STATE=="CA" & HEALTH_BASED == 'Y'), file.path(home, "/SDWA-data/SDWA_VIOLATIONS_CA.csv"))
ind <- readRDS("../Data/1int/pws_ind.rds")
# read in PWS level monitoring data/to filter only the PWS that we have monitoring data for!
ni_reg <-read_rds(file.path(home, "1int/caswrb_n_reg.rds"))

ni_reg_balanced <- subset_years(2001, pollutant = ni_reg, 2020, 1)

ar_reg <-read_rds("../Data/1int/caswrb_ar_reg.rds")

ar_reg_balanced <- subset_years(2010, pollutant = ar_reg, 2020, 1)

violations$RULE_NAME %>% table()

pws_ni_vio <- violations %>% 
  filter(STATE=="CA" & RULE_NAME == "Nitrates" & HEALTH_BASED == "Y") %>% 
  mutate(SYSTEM_NO = str_extract(PWSID, "(?<=CA)\\d+"))

pws_ar_vio <- violations %>%
  filter(STATE == "CA" & RULE_NAME == "Arsenic" & HEALTH_BASED == "Y") %>%
  mutate(SYSTEM_NO = str_extract(PWSID, "(?<=CA)\\d+"))

rm(violations)

pdsi <- readRDS("../Data/drought/pdsi_pws_year.rds")

# pdsi_year <- pdsi %>% group_by(year) %>% summarise(mean_pdsi = mean(mean_pdsi, na.rm = TRUE))
# the goal is to get things to the PWS-year level

pws_count <- pws %>% group_by(PWSID, SOURCE_WATER) %>% summarise(count = n()) 
pws %>% group_by(PWSID) %>% summarise(count = n()) 

vio <- violations %>% 
  filter(STATE=="CA" & RULE_NAME == "Nitrates" & HEALTH_BASED == "Y") %>% 
  mutate(SYSTEM_NO = str_extract(PWSID, "(?<=CA)\\d+")) %>% 
  left_join(ind)

# Nitrate ----------------

ni_vio_new <- pws_ni_vio %>% dplyr::select(SYSTEM_NO, PWSID, year = BEGIN_YEAR, VIOLATION_ID, VIOLATION_NAME, RULE_NAME) %>% 
  group_by(SYSTEM_NO, year, PWSID) %>% 
  # summarise(n_new_violations = unique(VIOLATION_ID) %>% length())
  summarise(n_new_violations = n())


# Plot nitrate violations -------------------------------------------------

# some violation ID span many years # if RTC year is NA means that the system never returned to compliant?
# to make it easy look at new violations issued

# investigate if begin_year or fiscal_year is the actual year in which a PWS gets a violation
# compare to stats on the dashboard
# https://echo.epa.gov/trends/comparative-maps-dashboards/drinking-water-dashboard?state=California&view=activity&criteria=adv&yearview=FY
# nope, this is not the same number as that on the dashboard

violations %>%  filter(STATE=="CA") %>% dplyr::select(BEGIN_YEAR, VIOLATION_ID) %>% distinct() %>% select(BEGIN_YEAR) %>% table()
violations %>%  filter(STATE=="CA") %>% dplyr::select(FISCAL_YEAR, VIOLATION_ID) %>% distinct() %>% select(FISCAL_YEAR) %>% table()

# Community water systems shall conduct monitoring to determine compliance with the maximum contaminant levels specified in § 141.62 in accordance with this section. Non-transient, non-community water systems shall conduct monitoring to determine compliance with the maximum contaminant levels specified in § 141.62 in accordance with this section. Transient, non-community water systems shall conduct monitoring to determine compliance with the nitrate and nitrite maximum contaminant levels in §§ 141.11 and 141.62 (as appropriate) in accordance with this section.
p <- ni_reg_balanced %>% 
  left_join(pdsi, c("year", "SYSTEM_NO"))

# use this to normalize pdsi
mean.d <- mean(p$mean_pdsi, na.rm = TRUE)
sd.d <- sd(p$mean_pdsi, na.rm = TRUE)
reg_ni <- pdsi %>% 
  ungroup() %>% 
  # filter to year in relevant years and to pws in the main monitoring dataset
  # might want to consider poisson for modeling counts
  left_join(ni_vio_new, by = c('SABL_PWSID' = 'PWSID', 'year', 'SYSTEM_NO')) %>% 
  left_join(ind) %>% 
  # left_join(dww %>% dplyr::select(Water.System.No, Total.Population), by = c('SABL_PWSID' = 'Water.System.No')) %>%
  # we only want to include PWS that are monitored under the EPA see section above
  filter(year %in% 2001:2021) %>% 
  mutate(n_new_violations = replace_na(n_new_violations, 0)) %>% 
  group_by(SYSTEM_NO) %>% 
  arrange(SYSTEM_NO, year) %>% 
  mutate(
    # d = mean_pdsi, 
    d = ((mean_pdsi-mean.d)*-1)/sd.d,
    dlead = lead(d),
    dlead2 = lead(dlead),
    dlag1 = lag(d),
    dlag2 = lag(dlag1),
    dlag3 = lag(dlag2),
    dlag4 = lag(dlag3),
    dlag5 = lag(dlag4),
    dlag6 = lag(dlag5)) 

# run regressions
# xvar = list('d', paste(c('d', 'd:b_majority_latino'), collapse = '+'), 
#             paste(c('d', 'd:b_majority_latino', 'd:log_hh_income'), collapse = '+'),
#             paste(c('d', 'd:b_majority_latino', 'd:log_hh_income', 'd:percent_ag'), collapse = '+'),
#             paste(c('d', 'd:b_majority_latino', 'd:log_hh_income', 'd:percent_ag', 'd:avg_percent_clay'), collapse = '+'),
#             paste(c('d', 'd:b_majority_latino', 'd:log_hh_income', 'd:percent_not_fluent_english'), collapse = '+'),
#             paste(c('d', 'd:b_majority_latino', 'd:log_hh_income', 'd:log_pop'), collapse = '+'))
# 
# mod_vio_ni <- felm(n_new_violations ~ d | SYSTEM_NO + year | 0 | 0, data = reg_ni)
# summary(mod_vio_ni)

mod_vio_ni2 <- felm(n_new_violations ~ d + dlag1 + dlag2 + dlag3
                   + d:gw + dlag1:gw + dlag2:gw + dlag3:gw + year:CITY_NAME | PWSID | 0 | PWSID, data = reg_ni)
summary(mod_vio_ni2)

stargazer::stargazer(mod_vio_ni, mod_vio_ni2, omit = c('year'), single.row = TRUE,
                     dep.var.labels   = "Number of new Nitration violations", dep.var.caption = "Outcome:", 
                     omit.stat = c("adj.rsq", "ser"))

# Arsenic -----------------------------------------------------------------
ar_vio_new <- pws_ar_vio %>% dplyr::select(SYSTEM_NO, PWSID, year = BEGIN_YEAR, VIOLATION_ID, VIOLATION_NAME, RULE_NAME) %>% 
  group_by(SYSTEM_NO, year, PWSID) %>% 
  # summarise(n_new_violations = unique(VIOLATION_ID) %>% length())
  summarise(n_new_violations = n())

pws_ar_vio %>% dplyr::select(SYSTEM_NO, PWSID, year = BEGIN_YEAR, VIOLATION_ID, VIOLATION_NAME, RULE_NAME) %>% 
  group_by(year) %>% 
summarise(n_new_violations = n()) %>% 
  ggplot(aes(year, n_new_violations)) +
  geom_col() +
  theme_minimal()

# The fiscal year might be the year in which these data were added into the system; yeah I think so 

reg_ar <- pdsi %>% 
  ungroup() %>% 
  left_join(ar_vio_new, by = c('SABL_PWSID' = 'PWSID', 'year', 'SYSTEM_NO')) %>% 
  left_join(pws, by = c('SABL_PWSID' = 'PWSID')) %>% 
  # Only CWS and NTNCWS are subject to the FAR;
  # we only want to include PWS that are monitored under the EPA see section above
  filter(year %in% 2010:2021, PWS_TYPE_CODE %in% c('CWS', 'NTNCWS'), !is.na(PWS_TYPE_CODE)) %>% 
  mutate(n_new_violations = replace_na(n_new_violations, 0)) %>% 
  group_by(SABL_PWSID) %>% 
  arrange(SABL_PWSID, year) %>% 
  mutate(
    # d = mean_pdsi, 
    d = if_else(mean_pdsi<=-1, 1, 0),
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

mod_vio_ar <- felm(n_new_violations ~ d + dlag1 + dlag2 + dlag3 + CITY_NAME:year| PWSID | 0 | PWSID, data = reg_ar)

summary(mod_vio_ar)

# mod_vio_ar_pop <- felm(n_new_violations ~ d + dlag1 + dlag2 + dlag3 | SABL_PWSID + year | 0  | SABL_PWSID, data = reg_ar %>% drop_na(Total.Population), weights = reg_ar$Total.Population[!is.na(reg_ar$Total.Population)])
# summary(mod_vio_ar_pop)


mod_vio_ar2 <- felm(n_new_violations ~ d + dlag1 + dlag2 + dlag3
                   + d:gw + dlag1:gw + dlag2:gw + dlag3:gw + CITY_NAME:year| PWSID | 0 | PWSID, data = reg_ar)
summary(mod_vio_ar2)

stargazer::stargazer(mod_vio_ar, mod_vio_ar2, omit = c('year'), single.row = TRUE,
                     dep.var.labels   = "Number of new Arsenic violations", dep.var.caption = "Outcome:", omit.stat = c("adj.rsq", "ser"))








