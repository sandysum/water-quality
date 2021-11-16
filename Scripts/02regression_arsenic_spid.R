
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
pdsi <- readRDS("../Data/drought/pdsi_pws_year.rds") 

ar_reg <-read_rds(file.path(home, "1int/caswrb_ar_reg.rds"))

soil <- sf::read_sf("../Data/1int/pws_sf_clay_ph.shp") %>% as_data_frame() %>% 
  dplyr::select(SYSTEM_NO = SABL_PWSID, clay, ph) %>% 
  mutate(SYSTEM_NO = str_extract(SYSTEM_NO, "\\d+"),
         ph_grp = cut_interval(ph, 2),
         clay_grp = cut_interval(clay, 2)) 
levels(soil$ph_grp) <- c('low', 'high')
levels(soil$clay_grp) <- c('low', 'high')
# 2. Filter to balanced panel for year 1996 to 2021

# this function subsets to only balanced panels
ar_reg_balanced <- subset_years(2010, pollutant = ar_reg, 2020, 1)

ar_drought <- ar_reg_balanced %>% 
  ungroup() %>% 
  left_join(pdsi, c("year", "SYSTEM_NO")) %>% 
  left_join(soil) %>% 
  group_by(samplePointID) %>% 
  mutate(
    highclay = if_else(clay_grp=="high", 1, 0),
    highph = if_else(ph_grp=="high", 1, 0),
    d = mean_pdsi, 
    dlead = lead(d),
    dlead2 = lead(dlead),
    dlag1 = lag(d),
    dlag2 = lag(dlag1),
    dlag3 = lag(dlag2),
    dlag4 = lag(dlag3),
    dlag5 = lag(dlag4),
    dlag6 = lag(dlag5),
    gXr = gw*raw,
    gXrXc = gw*raw*highclay,
    gXrXg = gw*raw*gold,
    gXrXph = gw*raw*highph) %>% 
  mutate(gw = factor(gw),
         raw = factor(raw),
         # gXr = factor(gXr),
         SYSTEM_NO = factor(SYSTEM_NO)) %>% 
  group_by(SYSTEM_NO, year, gw) %>% 
  mutate(n_spid = 1/(unique(samplePointID) %>% length())) %>% 
  ungroup()


# Visualize annual trends within PWS --------------------------------------
set.seed(8997)
q <- sample(unique(ar_drought$SYSTEM_NO), 100)
quartz()
p <- ar_drought %>% 
  filter(SYSTEM_NO %in% q) %>% 
  ggplot(aes(x = year, y = mean_ar, group = samplePointID, color = raw)) +
  geom_line() +
  theme_light() +
  geom_smooth(aes(group = SYSTEM_NO), method = 'lm') +
  scale_x_continuous(breaks = seq(2010, 2022, 2)) +
  # geom_hline(yintercept = 10, color = 'red') +
  theme(axis.text.x = element_text(angle = 45)) +
  facet_wrap(vars(SYSTEM_NO), scales = "free")

save_plot("Google Drive/My Drive/0Projects/1Water/2Quality/water-quality/Plots/pws_linear_ar.png", p, base_asp = 1.2, scale = 5)

# 1. CONTEMPORANEOUS EFFECTS  -------------------------------------------------

mod_ar_1 <- 
  felm(mean_ar ~ d + d:gw + d:raw | samplePointID + factor(year) | 0 | SYSTEM_NO, data = ar_drought, weights = ar_drought$n_spid)

summary(mod_ar_1)

mod_ar_2 <- 
  felm(mean_ar ~ d + d:gw + d:raw + year | samplePointID | 0 | SYSTEM_NO, data = ar_drought, weights = ar_drought$n_spid)

summary(mod_ar_2)

mod_ar_3 <- 
  felm(mean_ar ~ d + d:gw + d:raw + SYSTEM_NO:year | samplePointID | 0 | SYSTEM_NO, data = ar_drought, weights = ar_drought$n_spid)

summary(mod_ar_3)

mod_ar_4 <- 
  felm(mean_ar ~ d + d:gw + d:raw + d:gXr + SYSTEM_NO:year | samplePointID | 0 | SYSTEM_NO, data = ar_drought, weights = ar_drought$n_spid)

summary(mod_ar_4)

stargazer::stargazer(mod_ar_1, mod_ar_2, mod_ar_3, mod_ar_4, omit = c('year'), single.row = TRUE,
                     dep.var.labels   = "Mean arsenic level (ug/L)", dep.var.caption = "Outcome:", omit.stat = c("adj.rsq", "ser"))


# LAGGED EFFECTS ---------------------------------------------------

mod_ar_lag1 <- 
  felm(mean_ar ~ d + dlag1 + dlag2 + dlag3
       + d:gw 
       + dlag1:gw
       + dlag2:gw
       + dlag3:gw
       + d:raw 
       + dlag1:raw
       + dlag2:raw
       + dlag3:raw
| samplePointID + factor(year) | 0 | SYSTEM_NO, data = ar_drought, weights = ar_drought$n_spid)

summary(mod_ar_lag1)


mod_ar_lag2 <- 
  felm(mean_ar ~ d + dlag1 + dlag2 + dlag3
       + d:gw 
       + dlag1:gw
       + dlag2:gw
       + dlag3:gw
       + d:raw 
       + dlag1:raw
       + dlag2:raw
       + dlag3:raw + year
       | samplePointID | 0 | SYSTEM_NO, data = ar_drought, weights = ar_drought$n_spid)

summary(mod_ar_lag2)

mod_ar_lag3 <- 
  felm(mean_ar ~ d + dlag1 + dlag2 + dlag3
       + d:gw 
       + dlag1:gw
       + dlag2:gw
       + dlag3:gw
       + d:raw 
       + dlag1:raw
       + dlag2:raw
       + dlag3:raw+ year:SYSTEM_NO
       | samplePointID | 0 | SYSTEM_NO, data = ar_drought, weights = ar_drought$n_spid)

summary(mod_ar_lag3)

stargazer::stargazer(mod_ar_lag1, mod_ar_lag2, mod_ar_lag3, omit = c('year'), single.row = TRUE,
                     dep.var.labels   = "Mean arsenic level (ug/L)", dep.var.caption = "Outcome:", omit.stat = c("adj.rsq", "ser"))

# mod4 include interaction terms for being groundwater and raw; 
# might want to recode everything to factors or binary variable for easier interpretation.

mod_ar_lag4 <- 
  felm(mean_ar ~ d + dlag1 + dlag2 + dlag3
       + d:gw 
       + dlag1:gw
       + dlag2:gw
       + dlag3:gw
       + d:raw 
       + dlag1:raw
       + dlag2:raw
       + dlag3:raw 
       + d:gXr
       + dlag1:gXr
       + dlag2:gXr
       + dlag3:gXr + year:SYSTEM_NO
       | samplePointID | 0 | SYSTEM_NO, data = ar_drought, weights = ar_drought$n_spid)

summary(mod_ar_lag4)

# this function outputs the following effects
# raw gw, raw sw, treated gw, treated sw
# only raw gw is impacted; increase in arsenic level!
df <- sum_lags(mod_ar_lag4, int_terms = c('gXr', 'gw0', 'raw0', 'raw0|gw0'))


# plot lagged effects

df <- sum_lags(mod_ar_lag3)
save_plot("Plots/cumulative_lagged_effects_ar.png", plot_coeff(df), scale = 1, base_asp = 2)

# GW X RAW X HIGHCLAY -----------------------------------------------------

mod_ar_lag_clay1 <- 
  felm(mean_ar ~ d + dlag1 + dlag2 + dlag3 + dlag4 + dlag5
       + d:gXrXc 
       + dlag1:gXrXc
       + dlag2:gXrXc
       + dlag3:gXrXc
       + dlag4:gXrXc
       + dlag5:gXrXc
       | samplePointID + factor(year) | 0 | SYSTEM_NO, data = ar_drought, weights = ar_drought$n_spid)

summary(mod_ar_lag_clay1)


mod_ar_lag_clay2 <- 
  felm(mean_ar ~ d + dlag1 + dlag2 + dlag3 + dlag4 + dlag5
       + d:gXrXc 
       + dlag1:gXrXc
       + dlag2:gXrXc
       + dlag3:gXrXc
       + dlag4:gXrXc
       + dlag5:gXrXc + year
       | samplePointID | 0 | SYSTEM_NO, data = ar_drought, weights = ar_drought$n_spid)

summary(mod_ar_lag_clay2)

mod_ar_lag_clay3 <- 
  felm(mean_ar ~ d + dlag1 + dlag2 + dlag3 + dlag4 + dlag5
       + d:gXrXc 
       + dlag1:gXrXc
       + dlag2:gXrXc
       + dlag3:gXrXc
       + dlag4:gXrXc
       + dlag5:gXrXc + SYSTEM_NO:year
       | samplePointID | 0 | SYSTEM_NO, data = ar_drought, weights = ar_drought$n_spid)

summary(mod_ar_lag_clay3)

stargazer::stargazer(mod_ar_lag_clay1, mod_ar_lag_clay2, mod_ar_lag_clay3, omit = c('year'), single.row = TRUE,
                     dep.var.labels   = "Mean arsenic level (ug/L)", dep.var.caption = "Outcome:", omit.stat = c("adj.rsq", "ser"))

# GW X RAW X HIGHCLAY -----------------------------------------------------

# in central valley

mod_ar_lag_hiclay <- 
  felm(mean_ar ~ d
       + dlag1
       + dlag2
       + dlag3
       + d:gw
       + dlag1:gw
       + dlag2:gw
       + dlag3:gw
       + d:raw
       + dlag1:raw
       + dlag2:raw
       + dlag3:raw + SYSTEM_NO:year
       | samplePointID | 0 | SYSTEM_NO, data = ar_drought %>% filter(highclay==1), weights = ar_drought[ar_drought$highclay==1,]$n_spid)

summary(mod_ar_lag_hiclay)

# non-central valley 

mod_ni_lag_cv0 <- 
  felm(mean_n ~ d
       + dlag1
       + dlag2
       + dlag3
       + d:gw
       + dlag1:gw
       + dlag2:gw
       + dlag3:gw
       + d:raw
       + dlag1:raw
       + dlag2:raw
       + dlag3:raw + SYSTEM_NO:year
       | samplePointID | 0 | SYSTEM_NO, data = ni_drought %>% filter(cv==0), weights = ni_drought[ni_drought$cv==0,]$n_spid)


summary(mod_ni_lag_cv0)

stargazer::stargazer(mod_ni_lag_cv1, mod_ni_lag_cv0, omit = c('year'), single.row = TRUE,
                     dep.var.lab

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
