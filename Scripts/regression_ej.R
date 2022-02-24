#################################
# This script runs the regression to characterize the uneven distribution of 
# drinking water quality across income and racial lines
# I investigate long term medians of nitrate, arsenic at the raw level and violations of
# PWS serving communities with different 
# 2021/12/28
# sandysum@ucsb.edu
#################################

rm(list = ls())
home <- "/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/Data"

library(tidyverse)
library(DescTools)
library(readxl)
library(readr)
library(lfe)
library(sf)
library(cowplot)
library(stargazer)
library(gtsummary)
library(gt)
# Read in water systems information with social equity indicators ---------

# https://cacensus.maps.arcgis.com/apps/webappviewer/index.html?id=48be59de0ba94a3dacff1c9116df8b37

# Good website to double check if the census average numbers are correct
ind <- readRDS(file.path(home, "1int/pws_ind.rds")) %>% 
  mutate(OwnerType = ifelse(OwnerType=='#N/A', NA, OwnerType))

# %>% 
#   filter(SYSTEM_NO %in% (str_extract(pws_shp$SABL_PWSID, "\\d+"))) %>% 
#   drop_na()

# some PWS serve many tracts and some tracts are served by many PWS... gonna summarize everything to the PWS level and then weight by number of tracts served?

# Run some correlational analysis of social indicators --------------------

ar_reg <- read_rds("../Data/1int/caswrb_ar_reg.rds")
ni_reg <- read_rds("../Data/1int/caswrb_n_reg.rds")

# Read in the violations data

violations <- read_csv(file.path(home, "/SDWA-data/SDWA_VIOLATIONS_CA.csv"))

# Clean violations data for nitrates ----------------

ni_vio_new <- violations %>% 
  filter(STATE=="CA" & RULE_NAME == "Nitrates" & HEALTH_BASED == "Y") %>% 
  mutate(SYSTEM_NO = str_extract(PWSID, "(?<=CA)\\d+")) %>% 
  dplyr::select(SYSTEM_NO, PWSID, year = BEGIN_YEAR, VIOLATION_ID, VIOLATION_NAME, RULE_NAME) %>% 
  group_by(SYSTEM_NO) %>% 
  # summarise(n_new_violations = unique(VIOLATION_ID) %>% length())
  summarise(n_violations = n())

# Clean violations data for arsenic ----------------

as_vio_new <- violations %>%
  filter(STATE == "CA" & RULE_NAME == "Arsenic" & HEALTH_BASED == "Y") %>%
  mutate(SYSTEM_NO = str_extract(PWSID, "(?<=CA)\\d+")) %>% 
  dplyr::select(SYSTEM_NO, PWSID, year = BEGIN_YEAR, VIOLATION_ID, VIOLATION_NAME, RULE_NAME) %>% 
  group_by(SYSTEM_NO) %>% 
  # summarise(n_new_violations = unique(VIOLATION_ID) %>% length())
  summarise(as_violations = n()) 

pws_vio <- ind %>% left_join(as_vio_new, by = "SYSTEM_NO") %>% 
  left_join(ni_vio_new, by = "SYSTEM_NO") %>% 
  mutate_at(vars(matches("_violations")), ~replace(.,is.na(.), 0)) %>% 
  mutate(log_hh_income = log(median_hh_income),
         perc_latino = percent_his*100,
         median_hh_income_cat = cut_interval(median_hh_income, 10, labels = 1:10),
         perc_latino_cat = cut_interval(perc_latino, 10, labels = 1:10),
         arcsinh_n_vio = log(n_violations+sqrt((n_violations^2)+1)),
         arcsinh_as_vio = log(as_violations+sqrt((as_violations^2)+1)))


# Create summary table for export -----------------------------------------

summ <- pws_vio %>% select(percAgArea, TotalPopulation, 
                           TransientPopulation, ResidentialPopulation, 
                           NumberofServiceConnectionsAgricultural,
                           NumberofResidentialServiceConnections,
                           `NumberofCommercial(CM)ServiceConnections`,
                           NumerofInstitutionalServiceConections,
                           TotalNumberofServiceConnections,
                           avg_percent_clay,
                           avg_percent_ph,
                           percent_latino, median_hh_income, 
                           n_violations, as_violations, PrimaryWaterSourceType)

stargazer(as.data.frame(summ), digits = 0)

summc <- ind %>% select(OwnerType, FeeCodeDescription, FederalWaterSystemType,
                        PrimaryWaterSourceType, TreatmentPlantClass,
                        DistributionSystemClass)

tbl_summary(summc) %>% as_kable_extra(format = 'latex')

# Create shapefile centroids of pws / to plot # violations ----------------
# saved for plotting in ArcMap on grotto
# pws_zip <- read_sf("../Data/shp_PWS_SABL_Public_080221/PWS_by_zip.shp") %>% 
#   mutate(SYSTEM_NO = str_extract(PWSID, '\\d+')) %>% 
#   select(SYSTEM_NO) %>% 
#   st_transform(crs = 3488)
# pws_shp <- read_sf("../Data/shp_PWS_SABL_Public_080221/SABL_Public_080221.shp") %>% 
#   mutate(SYSTEM_NO = str_extract(SABL_PWSID, '\\d+')) %>% 
# select(SYSTEM_NO) %>% 
#   st_transform(crs = 3488)
# pws_points <- st_centroid(pws_zip) 
# pws_points2 <- st_centroid(pws_shp) 
# pws_points <- rbind(pws_points, pws_points2)
# pws_points2  <- pws_points %>% 
#   left_join(ni_vio_new, by = "SYSTEM_NO") %>% 
#   mutate_at(vars(matches("_violations")), ~replace(.,is.na(.), 0)) %>% 
#   filter(n_violations > 0)
# 
# write_sf(pws_points2, "../Data/vio_n_pws.shp")

# Run regression for violations -------------------------------------------

mod_v_as_ej_income <- felm(as_violations ~ log_hh_income | 0 | 0 | 0, data = pws_vio)

summary(mod_v_as_ej_income)

mod_v_as_ej_percent_his <- felm(as_violations ~ perc_latino | 0 | 0 | 0, data = pws_vio)

summary(mod_v_as_ej_percent_his)

mod_v_n_ej_income <- felm(n_violations ~ log_hh_income | 0 | 0 | 0, data = pws_vio)

summary(mod_v_n_ej_income)

mod_v_n_ej_percent_his <- felm(n_violations ~ perc_latino | 0 | 0 | 0, data = pws_vio)

summary(mod_v_n_ej_percent_his)

stargazer::stargazer(mod_v_as_ej_income, mod_v_as_ej_percent_his,
                     mod_v_n_ej_income, mod_v_n_ej_percent_his, 
                     dep.var.labels.include = FALSE,
                     title = "Corr. of PWS median contaminants and sociodemographic 
                     variables",
                     column.separate = c(2,2,2,2),
                     column.labels = c("# Violations As", "# Violations N",
                                       "Median As", "Median N"),
                     omit = "Constant",
                     single.row = TRUE,
                     omit.stat = c("adj.rsq", "ser"))

# Q1 are DAC PWS getting more violations?

ggplot(pws_vio, aes(perc_latino, percAgArea)) +
  geom_point() +
  geom_smooth() +
  theme_minimal()

# Q1 are DAC getting worst raw water?
# filter to raw and in the last 10 years
as_pws_10yr <- ar_reg %>% filter(raw==1, year %in% 2010:2020) %>% 
  group_by(SYSTEM_NO, CITY, ZIP, POP_SERV) %>% 
  summarize(pws_as_mean = mean(mean_as, na.rm = TRUE),
            pws_as_median = median(mean_as, na.rm = TRUE)) %>% 
  left_join(ind) %>% 
  ungroup() %>% 
  mutate(log_hh_income = log(median_hh_income),
         perc_latino = percent_his*100)

as_pws_10yr %>% ggplot(aes(perc_latino, pws_as_median)) +
  geom_point(alpha = .5) +
  geom_smooth() +
  theme_minimal()

mod_as_ej_income <- felm(pws_as_median ~ log_hh_income + percAgArea | 0 | 0 | 0, data = as_pws_10yr)

summary(mod_as_ej_income)

mod_as_ej_percent_his <- felm(pws_as_median ~ perc_latino + percAgArea + log_hh_income | 0 | 0 | 0, data = as_pws_10yr)

summary(mod_as_ej_percent_his)

# Q1 are DAC getting worst raw water?
n_pws_10yr <- ni_reg %>% filter(raw==1, year %in% 2010:2020) %>% 
  group_by(SYSTEM_NO, CITY, ZIP, POP_SERV) %>% 
  summarize(pws_n_mean = mean(mean_n, na.rm = TRUE),
            pws_n_median = median(mean_n, na.rm = TRUE)) %>% 
  left_join(ind) %>% 
  ungroup() %>% 
  mutate(log_hh_income = log(median_hh_income),
         perc_latino = percent_his*100)
quartz()
n_pws_10yr %>% ungroup() %>% ggplot(aes(percent_his, pws_n_median)) +
  geom_point(alpha = .5) +
  geom_smooth() +
  theme_minimal()

mod_n_ej_income <- felm(pws_n_median ~ log_hh_income | 0 | 0 | 0, data = n_pws_10yr)

summary(mod_n_ej_income)

mod_n_ej_percent_his <- felm(pws_n_median ~ perc_latino + percAgArea + log_hh_income | 0 | 0, data = n_pws_10yr)

summary(mod_n_ej_percent_his)

stargazer::stargazer(mod_as_ej_income, mod_as_ej_percent_his,
                     mod_n_ej_income, mod_n_ej_percent_his, 
                     dep.var.labels.include = FALSE,
                     title = "Corr. of PWS median contaminants and TRACT-level EJ 
                     indicators",
                     column.separate = c(2,2),
                     column.labels = c("Median As", "Median N"),
                     omit = "Constant",
                     single.row = TRUE,
                     omit.stat = c("adj.rsq", "ser"))


# Plot data for violations ------------------------------------------------

# not sure if it is worth showing these.

# pws_c <- n_pws_10yr %>% left_join(as_pws_10yr) %>% 
#   mutate(median_hh_income_cat = cut_interval(median_hh_income, 10, labels = 1:10),
#           perc_latino_cat = cut_interval(perc_latino, 10, labels = 1:10))
# 
# pws_boxpl_perclat_c <- pws_c %>% drop_na() %>% gather("Contaminant", "Level", matches("_median")) %>%
#   ggplot(aes(perc_latino_cat, Level, fill = Contaminant)) +
#   geom_boxplot() +
#   theme_minimal() +
#   scale_fill_brewer(palette = 'Dark2') +
#   labs(x = '% Latino', y = "Median contaminant level")
# 
# pws_boxpl_hhincome_c <- pws_c %>% drop_na() %>% gather("Contaminant", "Level", matches("_median")) %>%
#   ggplot(aes(median_hh_income_cat, Level, fill = Contaminant)) +
#   geom_boxplot() +
#   theme_minimal() +
#   scale_fill_brewer(palette = 'Dark2') +
#   labs(x = 'Median hh income decile', y = "Median contaminant level")
# 
# save_plot("Plots/ej_perclat_vios.png", pws_boxpl_perclat_vios, base_asp = 3, scale = 1.2)



