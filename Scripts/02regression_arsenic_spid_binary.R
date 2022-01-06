
# Arsenic regression ------------------------------------------------------

# 2021/11/21
# this is my attempt to simplify things and run regression with just an indicator for moderate drought on the RHS
# sandysum@ucsb.edu

# Load packages -----------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(lfe)
library(DescTools)
library(future.apply)
library(cowplot)
source("Scripts/helper_functions_models.R")

# Read in data ------------------------------------------------------------



home <- "/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/Data"

# home <- "G:/My Drive/0Projects/1Water/2Quality/Data/"

pdsi <- readRDS("../Data/drought/pdsi_pws_year.rds") 

ar_reg <-read_rds("../Data/1int/caswrb_ar_reg.rds")

soil <- readRDS("../Data/1int/pws_clay_merged.rds") %>% 
  mutate(clay_grp = cut_interval(avg_percent_clay, 2)) %>% 
  drop_na()
# levels(soil$ph_grp) <- c('low', 'high')
levels(soil$clay_grp) <- c('low', 'high')

# Read in and join to social eq ind ---------------------------------------
# Added this part to run some regression to join social eq indicator

ej <- readRDS(file.path(home, "1int/pws_ej_ind.rds")) %>% 
  mutate(b_majority_latino = if_else(percent_his >= .5, 1, 0),
         b_low_income = if_else(median_hh_income <=46000, 1, 0),
         log_hh_income = log(median_hh_income))

# 2. Filter to balanced panel for year 1996 to 2021

# this function subsets to only balanced panels
as_reg_balanced <- subset_years(2010, pollutant = ar_reg, 2020, 1)

as_drought <- as_reg_balanced %>% 
  ungroup() %>% 
  left_join(pdsi, c("year", "SYSTEM_NO")) %>% 
  left_join(soil) %>% 
  group_by(samplePointID) %>% 
  mutate(
    highclay = if_else(clay_grp=="high", 1, 0),
    # highph = if_else(ph_grp=="high", 1, 0),
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
    gXrXc = gw*raw*highclay) %>% 
  mutate(gw = factor(gw, levels = c("1", "0")),
         raw = factor(raw),
         SYSTEM_NO = factor(SYSTEM_NO)) %>% 
  group_by(SYSTEM_NO, year) %>% 
  mutate(n_spid = 1/(unique(samplePointID) %>% length())) %>% 
  ungroup() %>% 
  dplyr::left_join(ej, by = "SYSTEM_NO")

# Drought on As, status, lags, EJ ------------------------------------------

# in majority latino areas

df_h_perclat <- as_drought %>% filter(b_majority_latino==1 & !is.na(b_majority_latino))

mod_as_3_perclat_h <-
  felm(mean_as ~ d + d:gw + d:raw + SYSTEM_NO:year | samplePointID | 0 | SYSTEM_NO, data = df_h_perclat, weights = df_h_perclat$n_spid)

summary(mod_as_3_perclat_h)

mod_as_lag3_perclat_h <-
  felm(mean_as ~ d + dlag1 + dlag2
       # + dlag3
       + d:gw
       + dlag1:gw
       + dlag2:gw
       # + dlag3:gw
       + d:raw
       + dlag1:raw
       + dlag2:raw
       # + dlag3:raw
       + SYSTEM_NO:year
       | samplePointID | 0 | SYSTEM_NO, data = df_h_perclat, weights = df_h_perclat$n_spid)

summary(mod_as_lag3_perclat_h)

# only 8 SPID with treated GW in this category... do not add that?

df_l_perclat <- as_drought %>% filter(b_majority_latino==0 & !is.na(b_majority_latino))

mod_as_3_perclat_l <-
  felm(mean_as ~ d + d:gw + d:raw + SYSTEM_NO:year |  samplePointID | 0 | SYSTEM_NO, data = df_l_perclat, weights = df_l_perclat$n_spid)

mod_as_lag3_perclat_l <-
  felm(mean_as ~ d + dlag1 + dlag2
       # + dlag3
       + d:gw
       + dlag1:gw
       + dlag2:gw
       # + dlag3:gw
       + d:raw
       + dlag1:raw
       + dlag2:raw
       # + dlag3:raw
       + SYSTEM_NO:year
       | samplePointID | 0 | SYSTEM_NO, data = df_l_perclat, weights = df_l_perclat$n_spid)

summary(mod_as_lag3_perclat_l)

# df_int_perclat_h <- sum_marginal(mod_as_4_perclat_h, nlags = 0, int_terms = c('gw0', ':raw0'), contaminant = 'n')
# df_int_perclat_l <- sum_marginal(mod_as_4_perclat_l, nlags = 0, int_terms = c('gw0', ':raw0'), contaminant = 'n')
# 
# plot_coeff_lags(df_int_cv[1:4,], contaminant = 'n', ylm =c(-0.1, 0.25))

# for household income

df_h_income <- as_drought %>% filter(b_low_income==0 & !is.na(b_low_income))

mod_as_3_income_h <-
  felm(mean_as ~ d + d:gw + d:raw + SYSTEM_NO:year | samplePointID | 0 | SYSTEM_NO, data = df_h_income, weights = df_h_income$n_spid)

summary(mod_as_3_income_h)

mod_as_lag3_income_h <-
  felm(mean_as ~ d + dlag1 + dlag2
       # + dlag3
       + d:gw
       + dlag1:gw
       + dlag2:gw
       # + dlag3:gw
       + d:raw
       + dlag1:raw
       + dlag2:raw
       # + dlag3:raw
       + year:SYSTEM_NO
       | samplePointID | 0 | SYSTEM_NO, data = df_h_income, weights = df_h_income$n_spid)

summary(mod_as_lag3_income_h)

# only 8 SPID with treated GW in this category... do not add that?

df_l_income <- as_drought %>% filter(b_low_income==1 & !is.na(b_low_income))

mod_as_3_income_l <-
  felm(mean_as ~ d + d:gw + d:raw + SYSTEM_NO:year |  samplePointID | 0 | SYSTEM_NO, data = df_l_income, weights = df_l_income$n_spid)

mod_as_lag3_income_l <-
  felm(mean_as ~ d + dlag1 + dlag2
       # + dlag3
       + d:gw
       + dlag1:gw
       + dlag2:gw
       # + dlag3:gw
       + d:raw
       + dlag1:raw
       + dlag2:raw
       # + dlag3:raw
       + year:SYSTEM_NO
       | samplePointID | 0 | SYSTEM_NO, data = df_l_income, weights = df_l_income$n_spid)

summary(mod_as_lag3_income_l)

stargazer::stargazer(mod_as_3_perclat_l, mod_as_3_perclat_h, 
                     mod_as_3_income_h, mod_as_3_income_l,
                     omit = c(':year'), single.row = TRUE,
                     dep.var.labels   = "Mean arsenic (As) (ug/L)", dep.var.caption = "Outcome:", omit.stat = c("adj.rsq", "ser"))

stargazer::stargazer(mod_as_lag3_perclat_l, mod_as_lag3_perclat_h, 
                     mod_as_lag3_income_h, mod_as_lag3_income_l,
                     omit = c('year'), single.row = TRUE,
                     dep.var.labels   = "Mean arsenic (As) (ug/L)", dep.var.caption = "Outcome:", omit.stat = c("adj.rsq", "ser"))


# MOD GRP 1: Drought on As -------------------------------------------------

# instantaneous effect is on the surface
# mean contemporaneous effect of precip on raw GW, mean contemporaneous effect of precip on raw S, mean contemporaneous effect of precip on treated water

# mod 1: spid FEs and year FE
mod_as1 <- 
  felm(mean_as ~ d | samplePointID + factor(year) | 0 | SYSTEM_NO, data = as_drought, weights = as_drought$n_spid)

summary(mod_as1)

# mod 2: spid FEs and linear year 
mod_as2 <- 
  felm(mean_as ~ d + year | samplePointID | 0 | SYSTEM_NO, data = as_drought, weights = as_drought$n_spid)

summary(mod_as2)

# mod 3: spid FEs and PWS specific linear year 
mod_as3 <- 
  felm(mean_as ~ d + SYSTEM_NO:year | samplePointID | 0 | SYSTEM_NO, data = as_drought, weights = as_drought$n_spid)

summary(mod_as3)

# mod 3 + perc latino: spid FEs and PWS specific linear year 
mod_as_perclat <- 
  felm(mean_as ~ d + d:b_majority_latino + SYSTEM_NO:year | samplePointID | 0 | SYSTEM_NO, data = as_drought, weights = as_drought$n_spid)

summary(mod_as_perclat)

# mod 3 + hh income: spid FEs and PWS specific linear year 
mod_as_lowincome <- 
  felm(mean_as ~ d + d:b_low_income + SYSTEM_NO:year | samplePointID | 0 | SYSTEM_NO, data = as_drought, weights = as_drought$n_spid)

summary(mod_as_lowincome)

# mod 3 + hh income + perc latino: spid FEs and PWS specific linear year 
mod_as_both <- 
  felm(mean_as ~ d + d:b_low_income + d:b_majority_latino + SYSTEM_NO:year | samplePointID | 0 | SYSTEM_NO, data = as_drought, weights = as_drought$n_spid)

summary(mod_as_both)

stargazer::stargazer(mod_as1, mod_as2, mod_as3, mod_as_perclat, mod_as_lowincome, mod_as_both, omit = c(':year'), single.row = TRUE,
                     dep.var.labels   = "Mean arsenic level (ug/L)", dep.var.caption = "Outcome:", omit.stat = c("adj.rsq", "ser"))

# MOD GRP 2: Drought on As, status ----------------------------------------

# instantaneous effect is on the surface
# mean contemporaneous effect of precip on raw GW, mean contemporaneous effect of precip on raw S, mean contemporaneous effect of precip on treated water

# mod 1: spid FEs and year FE
mod_as_1 <- 
  felm(mean_as ~ d + d:gw + d:raw | samplePointID + factor(year) | 0 | SYSTEM_NO, data = as_drought, weights = as_drought$n_spid)

summary(mod_as_1)

# mod 2: spid FEs and linear year 
mod_as_2 <- 
  felm(mean_as ~ d + d:gw + d:raw + year | samplePointID | 0 | SYSTEM_NO, data = as_drought, weights = as_drought$n_spid)

summary(mod_as_2)

# mod 3: spid FEs and PWS specific linear year 
mod_as_3 <- 
  felm(mean_as ~ d + d:gw + d:raw + SYSTEM_NO:year | samplePointID | 0 | SYSTEM_NO, data = as_drought, weights = as_drought$n_spid)

summary(mod_as_3)

stargazer::stargazer(mod_as_1, mod_as_2, mod_as_3,
                     omit = c(':year'), single.row = TRUE,
                     dep.var.labels   = "Mean arsenic (As) (ug/L)", dep.var.caption = "Outcome:", omit.stat = c("adj.rsq", "ser"))


# MOD GRP 3: Drought on As, status, lags -----------------------------------
mod_as_lag1 <- 
  felm(mean_as ~ d
       + dlag1
       + dlag2
       # + dlag3
       + d:gw
       + dlag1:gw
       + dlag2:gw
       # + dlag3:gw
       + d:raw
       + dlag1:raw
       + dlag2:raw
       # + dlag3:raw
       | samplePointID + factor(year) | 0 | SYSTEM_NO, data = as_drought, weights = as_drought$n_spid)

summary(mod_as_lag1)

mod_as_lag2 <- 
  felm(mean_as ~ year + d + dlag1 + dlag2 
       # + dlag3
       + d:gw
       + dlag1:gw
       + dlag2:gw
       # + dlag3:gw
       + d:raw
       + dlag1:raw
       + dlag2:raw
       # + dlag3:raw 
       + year
       | samplePointID | 0 | SYSTEM_NO, data = as_drought, weights = as_drought$n_spid)

summary(mod_as_lag2)

mod_as_lag3 <- 
  felm(mean_as ~ d + dlag1 + dlag2 
       # + dlag3 
       + d:gw
       + dlag1:gw
       + dlag2:gw
       # + dlag3:gw
       + d:raw
       + dlag1:raw
       + dlag2:raw
       # + dlag3:raw 
       + year:SYSTEM_NO
       | samplePointID | 0 | SYSTEM_NO, data = as_drought, weights = as_drought$n_spid)

summary(mod_as_lag3)

summary(mod_as_lag4)
saveRDS(mod_as_lag4, "../Data/1int/ni_mod_lag4.rds")

stargazer::stargazer(mod_as_lag1, mod_as_lag2, mod_as_lag3, 
                     omit = c('year:'), single.row = TRUE,
                     dep.var.labels   = "Mean arsenic (As) (ug/L)", dep.var.caption = "Outcome:", omit.stat = c("adj.rsq", "ser"))
