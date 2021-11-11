
# Arsenic regression ------------------------------------------------------

# 2021/11/4
# sandysum@ucsb.edu


# Soil exploration --------------------------------------------------------

ggplot(soil, aes(ph, clay)) +
  geom_point(size = 1, alpha = .7) +
  theme_minimal_hgrid() +
  geom_hline(aes(yintercept = 33), color = 'red')

# Load packages -----------------------------------------------------------

library(tidyverse)
library(lfe)
library(DescTools)
library(future.apply)
library(cowplot)
source("Scripts/helper_functions_models.R")

# Read in data ------------------------------------------------------------

pdsi <- readRDS("../Data/drought/pdsi_pws_monthyear.rds") %>% 
  mutate(month = as.numeric(str_extract(my, "\\d{2}")),
         year = as.numeric(str_extract(my, "\\d{4}$"))) %>% 
  group_by(SABL_PWSID, year) %>% 
  dplyr::summarise(mean_pdsi = mean(mean_pdsi, na.rm = TRUE))

ar <-read_rds(file.path(home, "1int/caswrb_ar_1974-2021.rds"))

soil <- sf::read_sf("../Data/1int/pws_sf_clay_ph.shp") %>% as_data_frame() %>% 
  select(SYSTEM_NO = SABL_PWSID, clay, ph) %>% 
  mutate(SYSTEM_NO = str_extract(SYSTEM_NO, "\\d+"),
         ph_grp = cut_interval(ph, 3),
         clay_grp = cut_interval(clay, 2)) 
levels(soil$ph_grp) <- c('low', 'med', 'high')
levels(soil$clay_grp) <- c('low', 'high')

cv_counties <-
  c(
    'Butte',
    'Colusa',
    'Glenn',
    'Fresno',
    'Kern',
    'Kings',
    'Madera',
    'Merced',
    'Placer',
    'San Joaquin',
    'Sacramento',
    'Shasta',
    'Solano',
    'Stanislaus',
    'Sutter',
    'Tehama',
    'Tulare',
    'Yolo',
    'Yuba'
  ) %>%
  str_to_lower()
gold <-
  c(
    'Butte',
    "Amador",
    'Calaveras',
    'El Dorado',
    'Mariposa',
    'Nevada',
    'Placer',
    'Plumas',
    'Sierra',
    'Tuolumne',
    'Yuba'
  ) %>%
  str_to_lower()

# Regression at the monitor month year level ------------------------------

# 1. Prep data for regression at the monitoring ID level

# drop the duplicates! and keep only ground or surface water type. 

ar_reg <- ar %>% 
  distinct(samplePointID, SYSTEM_NO, sampleDate, sampleTime, ar_ugl, .keep_all = TRUE) %>% 
  filter(WATER_TYPE %in% c("G", "S"), year > 1995, !is.na(ar_ugl)) %>% 
  mutate(gw = if_else(WATER_TYPE == "G", 1, 0),
         cv = if_else(countyName %in% cv_counties, 1, 0)) %>% 
  group_by(gw, samplePointID, year, SYSTEM_NO, cv, countyName, 
           SYSTEM_NAM, STATUS, ZIP, POP_SERV, raw, CITY, WATER_TYPE) %>% 
  summarise(mean_ar = mean(ar_ugl, na.rm = TRUE),
            median_ar = median(ar_ugl, na.rm = TRUE)) %>% 
  mutate(mean_ar = Winsorize(mean_ar, probs = c(0, .99))) 

# 2. Filter to balanced panel for year 1996 to 2021

# this function subsets to only balanced panels
ar_reg_balanced <- subset_years(2010, pollutant = ar_reg, 2020, 1)

ar_drought <- ar_reg_balanced %>% 
  ungroup() %>% 
  left_join(pdsi %>% mutate(SYSTEM_NO = str_extract(SABL_PWSID, "\\d+")), c("year", "SYSTEM_NO")) %>% 
  left_join(soil) %>% 
  group_by(samplePointID) %>% 
  mutate(
    d = mean_pdsi, 
    dlead = lead(d),
    dlead2 = lead(dlead),
    dlag1 = lag(d),
    dlag2 = lag(dlag1),
    dlag3 = lag(dlag2),
    dlag4 = lag(dlag3),
    dlag5 = lag(dlag4),
    dlag6 = lag(dlag5),
    gXr = gw*raw) %>% 
  mutate(gw = factor(gw),
         raw = factor(raw),
         SYSTEM_NO = factor(SYSTEM_NO)) %>% 
  group_by(SYSTEM_NO, year, gw) %>% 
  mutate(n_spid = 1/(unique(samplePointID) %>% length())) %>% 
  ungroup()


# Visualize annual trends within PWS --------------------------------------
set.seed(2)
q <- sample(unique(ar_drought$SYSTEM_NO), 8)
quartz()
ar_drought %>% 
  filter(SYSTEM_NO %in% q) %>% 
  ggplot(aes(x = year, y = mean_ar, group = samplePointID, color = raw)) +
  geom_line() +
  theme_light() +
  geom_smooth(aes(group = SYSTEM_NO), method = 'lm') +
  scale_x_continuous(breaks = seq(2010, 2022, 2)) +
  # geom_hline(yintercept = 10, color = 'red') +
  theme(axis.text.x = element_text(angle = 45)) +
  facet_wrap(vars(SYSTEM_NO), scales = "free")

# Run stacked regressions -------------------------------------------------

mod_ar_stacked <- 
  felm(mean_ar ~ d + d:gXr | samplePointID + SYSTEM_NO:year | 0 | SYSTEM_NO, data = ar_drought, weights = ar_drought$n_spid)

summary(mod_ar_stacked)

stargazer::stargazer(mod_ni_stacked)

mod_ar_stacked2 <- 
  felm(mean_ar ~ d + dlag1 + dlag2 + dlag3 + dlag4 + dlag5 +
         d:gw + d:raw
       + dlag1:gw
       + dlag2:gw
       + dlag3:gw
       + dlag4:gw
       + dlag5:gw
       + dlag1:raw
       + dlag2:raw
       + dlag3:raw 
       + dlag4:raw 
       + dlag5:raw| samplePointID + SYSTEM_NO:year | 0 | SYSTEM_NO, data = ar_drought, weights = ar_drought$n_spid)

summary(mod_ar_stacked2)

stargazer::stargazer(mod_ni_stacked, mod_ni_stacked2)

# Try running regression at the piecewise level ---------------------------

# Why does result change SO much when I add in pws-level linear trends and non-pws-level linear trends??? 

# PWS level linear trends soak up all the effects of drought, suggest that drought's effect on Arsenic is driven by PWS level heterogeneity

# I can see that this heterogeneity is driven by claygroups

ar_gw_raw <- ar_drought %>% filter(gw ==1, raw == 1, year >= 2010)  

mod_gw <- felm(mean_ar ~ d + dlag1 + dlag2 + dlag3 + dlag4 + dlag5 | samplePointID + year | 0 | SYSTEM_NO, data = ar_gw_raw, weights = ar_gw_raw$n_spid)

mod_gw2 <- felm(mean_ar ~ d + dlag1 + dlag2 + dlag3 + dlag4 + dlag5 | samplePointID + year | 0 | SYSTEM_NO, data = ar_gw_raw, weights = ar_gw_raw$n_spid)

ar_s_raw <- ar_drought %>% filter(gw ==0, raw == 1, year >= 2010)  

mod_s <- felm(mean_ar ~ d + dlag1 + dlag2 + dlag3 + dlag4 + dlag5 | samplePointID + SYSTEM_NO:year | 0 | SYSTEM_NO, data = ar_s_raw, weights = ar_s_raw$n_spid)

ar_tr <- ar_drought %>% filter(raw == 0, year >= 2010)  

mod_tr <- felm(mean_ar ~ d + dlag1 + dlag2 + dlag3 + dlag4 + dlag5 | samplePointID + SYSTEM_NO:year  | 0 | SYSTEM_NO, data = ar_tr, weights = ar_tr$n_spid)

plot_reg(mod_tr, contaminant = "ar", 
         main = "Arsenic (ug/L) response to +1 in PDSI, \nRegression at the sample point level", nleads = 0, nlags = 5, ylm = c(-.5, .5))

# Plot and save -----------------------------------------------------------

plist <- pmap(list(list(mod_gw, mod_s, mod_tr), c("Raw groundwater", "Raw surface water", "Treated water"), list(c(-.8, .8), c(-.5, .5), c(-.5, .5))), plot_reg, contaminant = "ar", nleads = 0, nlags = 5)

save_plot("Plots/ar_pdsi_coefs_spid.png", plot_grid(plotlist = plist, ncol = 1), base_asp = .5, scale = 4)


# Explore clay group ------------------------------------------------------

mod_gw_clay_nodrought <- felm(mean_ar ~ clay_grp | year | 0 | SYSTEM_NO, data = ar_gw_raw, weights = ar_gw_raw$n_spid)

summary(mod_gw_clay_nodrought)

# differential trends are segmented by clay groups

mod_gw_clay <- felm(mean_ar ~ d + d:clay_grp 
                    + dlag1 + dlag1:clay_grp 
                    + dlag2 + dlag2:clay_grp 
                    + dlag3 + dlag3:clay_grp 
                    + dlag4 + dlag4:clay_grp
                    + dlag5 + dlag5:clay_grp | samplePointID + year | 0 | SYSTEM_NO, data = ar_gw_raw, weights = ar_gw_raw$n_spid)

summary(mod_gw_clay)
stargazer::stargazer(mod_gw_clay)
