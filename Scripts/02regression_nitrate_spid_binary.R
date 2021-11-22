
# Nitrate regression ------------------------------------------------------

# 2021/11/4
# sandysum@ucsb.edu


# Load packages -----------------------------------------------------------

library(tidyverse)
library(lfe)
library(DescTools)
library(future.apply)
library(cowplot)
source("Scripts/helper_functions_models.R")

# Read in data ------------------------------------------------------------

pdsi <- readRDS("../Data/drought/pdsi_pws_year.rds") 

ni_reg <-read_rds(file.path(home, "1int/caswrb_ni_reg.rds"))

# 1. CLEAN DATA FOR: Regression at the monitor month year level ------------------------------

# 2. Filter to balanced panel for year 1996 to 2021

# this function subsets to only balanced panels that has the

ni_reg_balanced <- subset_years(2001, pollutant = ni_reg, 2020, 1)

ni_drought <- ni_reg_balanced %>% 
  ungroup() %>% 
  left_join(pdsi, c("year", "SYSTEM_NO")) %>% 
  group_by(samplePointID) %>% 
  mutate(
    d = if_else(mean_pdsi <= -1, 1, 0), 
    dlead = lead(d),
    dlead2 = lead(dlead),
    dlag1 = lag(d),
    dlag2 = lag(dlag1),
    dlag3 = lag(dlag2),
    dlag4 = lag(dlag3),
    dlag5 = lag(dlag4),
    dlag6 = lag(dlag5),
    gXraw0 = factor(gw*(raw==0), levels = c('0', '1')),
    rXcv = raw*cv) %>% 
  mutate(
    gw = factor(gw, levels = c("1", "0")),
    raw = factor(raw),
    SYSTEM_NO = factor(SYSTEM_NO)
  ) %>%
  group_by(SYSTEM_NO, year) %>%
  mutate(n_spid = 1 / (unique(samplePointID) %>% length())) %>%
  ungroup()

ni_drought %>% drop_na()
# A tibble: 17,232 × 29
# A tibble: 18,276 × 29
# Visualize annual trends within PWS --------------------------------------
set.seed(1297)
q <- sample(unique(ni_drought$SYSTEM_NO), 100)
quartz()
p <- ni_drought %>% 
  filter(SYSTEM_NO %in% q) %>% 
  ggplot(aes(x = year, y = mean_n, color = raw, group = samplePointID)) +
  geom_line() +
  geom_smooth(aes(group = SYSTEM_NO), method = 'lm') +
  theme_light() +
  scale_x_continuous(breaks = seq(2000, 2022, 2)) +
  # geom_hline(yintercept = 10, color = 'red') +
  theme(axis.text.x = element_text(angle = 45)) +
  facet_wrap(vars(SYSTEM_NO), scales = "free")

save_plot("Google Drive/My Drive/0Projects/1Water/2Quality/water-quality/Plots/pws_linear_n.png", p, base_asp = 1.2, scale = 5)

# NO LAGGED EFFECTS  -------------------------------------------------

# instantaneous effect is on the surface
# mean contemporaneous effect of precip on raw GW, mean contemporaneous effect of precip on raw S, mean contemporaneous effect of precip on treated water
mod_ni_1 <- 
  felm(mean_n ~ d + d:gw + d:raw | samplePointID + factor(year) | 0 | SYSTEM_NO, data = ni_drought, weights = ni_drought$n_spid)

summary(mod_ni_1)

mod_ni_2 <- 
  felm(mean_n ~ d + d:gw + d:raw + year | samplePointID | 0 | SYSTEM_NO, data = ni_drought, weights = ni_drought$n_spid)

summary(mod_ni_2)

mod_ni_3 <- 
  felm(mean_n ~ d + d:gw + d:raw + SYSTEM_NO:year | samplePointID | 0 | SYSTEM_NO, data = ni_drought, weights = ni_drought$n_spid)

summary(mod_ni_3)

mod_ni_4 <- 
  felm(mean_n ~ d + d:gw + d:raw + d:gXraw0 + SYSTEM_NO:year | samplePointID | 0 | SYSTEM_NO, data = ni_drought, weights = ni_drought$n_spid)

summary(mod_ni_4)

stargazer::stargazer(mod_ni_1, mod_ni_2, mod_ni_3, mod_ni_4, omit = c(':year'), single.row = TRUE,
                     dep.var.labels   = "Mean Nitrate level (ug/L)", dep.var.caption = "Outcome:", omit.stat = c("adj.rsq", "ser"))


# ADDING LAGGED EFFECTS ---------------------------------------------------

mod_ni_lag1 <- 
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
       + dlag3:raw
       | samplePointID + factor(year) | 0 | SYSTEM_NO, data = ni_drought, weights = ni_drought$n_spid)

summary(mod_ni_lag1)

mod_ni_lag2 <- 
  felm(mean_n ~ year + d + dlag1 + dlag2 + dlag3
       + d:gw
       + dlag1:gw
       + dlag2:gw
       + dlag3:gw
       + d:raw
       + dlag1:raw
       + dlag2:raw
       + dlag3:raw + year
       | samplePointID | 0 | SYSTEM_NO, data = ni_drought, weights = ni_drought$n_spid)

summary(mod_ni_lag2)

mod_ni_lag3 <- 
  felm(mean_n ~ d + dlag1 + dlag2 + dlag3 
       + d:gw
       + dlag1:gw
       + dlag2:gw
       + dlag3:gw
       + d:raw
       + dlag1:raw
       + dlag2:raw
       + dlag3:raw + year:SYSTEM_NO
       | samplePointID | 0 | SYSTEM_NO, data = ni_drought, weights = ni_drought$n_spid)

summary(mod_ni_lag3)

mod_ni_lag4 <-
  felm(mean_n ~ d + dlag1 + dlag2 + dlag3 
       + d:gw
       + dlag1:gw
       + dlag2:gw
       + dlag3:gw
       + d:raw
       + dlag1:raw
       + dlag2:raw
       + dlag3:raw 
       + d:gXraw0
       + dlag1:gXraw0
       + dlag2:gXraw0
       + dlag3:gXraw0 + year:SYSTEM_NO
       | samplePointID | 0 | SYSTEM_NO, data = ni_drought, weights = ni_drought$n_spid)

summary(mod_ni_lag4)
# this function outputs the following effects
# raw gw, raw sw, treated gw, treated sw
# only raw gw is impacted; increase in arsenic level!

df_int <- sum_lags(mod_ni_lag4, int_terms = c('gw0', ':raw0|gXraw0', ':raw0|gw0'))
plot_coeff(df_int, contaminant = 'ni')
# plot lagged effects

df <- sum_lags(mod_ni_lag3, int_terms = c('gw0', 'raw0', 'gw0|raw0'))

save_plot("Plots/cumulative_lagged_effects_ni_interations.png", plot_coeff(df_int, contaminant = 'ni', drought_measure = "moderate drought year"), scale = 1, base_asp = 2)

stargazer::stargazer(mod_ni_lag1, mod_ni_lag2, mod_ni_lag3, mod_ni_lag4, omit = c('year'), single.row = TRUE,
                     dep.var.labels   = "Mean Nitrate level (ug/L)", dep.var.caption = "Outcome:", omit.stat = c("adj.rsq", "ser"))

# GW X RAW X HIGHCLAY -----------------------------------------------------

# in central valley

mod_ni_lag_cv1 <- 
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
       + dlag3:raw 
       + d:gXraw0
       + dlag1:gXraw0
       + dlag2:gXraw0
       + dlag3:gXraw0 + SYSTEM_NO:year
       | samplePointID | 0 | SYSTEM_NO, data = ni_drought %>% filter(cv==1), weights = ni_drought[ni_drought$cv==1,]$n_spid)

summary(mod_ni_lag_cv1)

x <- sum_lags(mod_ni_lag_cv1, int_terms = c('gw0', ':raw0|gXraw0', ':raw0|gw0'))
plot_coeff(x, contaminant = 'ni')

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
                     dep.var.labels   = "Mean Nitrate level (ug/L)", dep.var.caption = "Outcome:", omit.stat = c("adj.rsq", "ser"),
                     column.labels = c('In Central Valley', 'Outside of Central Valley'))


