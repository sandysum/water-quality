
# Nitrate regression ------------------------------------------------------

# 2021/11/4
# sandysum@ucsb.edu

# Load packages -----------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(lfe)
library(DescTools)
library(future.apply)
library(cowplot)
source("G:/My Drive/0Projects/1Water/2Quality/water-quality/Scripts/helper_functions_models.R")
source("/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/water-quality/Scripts/helper_functions_models.R")
options(digits=3)

# Read in data ------------------------------------------------------------
home <- "G:/My Drive/0Projects/1Water/2Quality/Data/"
home <- "/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/Data/"
pdsi <- readRDS(file.path(home,"/drought/pdsi_pws_year.rds"))

as_reg <-read_rds(file.path(home, "1int/caswrb_as_delivered.rds")) %>% 
  dplyr::select(1:4)
# ni_reg <-read_rds(file.path(home, "1int/caswrb_n_1974-2021.rds"))

# Read in and join to social eq ind ---------------------------------------
# Added this part to run some regression to join social eq indicator

ind <- readRDS(file.path(home, "1int/pws_ind.rds"))

# 1. CLEAN DATA FOR: Regression at the monitor month year level ------------------------------

# 2. Filter to balanced panel for year 1996 to 2021

# this function subsets to only balanced panels that has the

as_reg_balanced <- subset_years_cws(2010, pollutant = as_reg, 2020, 1)
p <- as_reg_balanced %>% 
  left_join(pdsi, c("year", "SYSTEM_NO"))

# use this to normalize pdsi
mean.d <- mean(p$mean_pdsi, na.rm = TRUE)
sd.d <- sd(p$mean_pdsi, na.rm = TRUE)
rm(p)

as_drought <- as_reg_balanced %>% 
  left_join(pdsi, c("year", "SYSTEM_NO")) %>% 
  group_by(SYSTEM_NO) %>% 
  # d is normalized!
  mutate(
    d = ((mean_pdsi-mean.d)*-1)/sd.d,
    # d = if_else(mean_pdsi <= -3, 1, 0), 
    dlead = lead(d),
    dlead2 = lead(dlead),
    dlag1 = lag(d),
    dlag2 = lag(dlag1),
    dlag3 = lag(dlag2),
    dlag4 = lag(dlag3)) %>% 
  ungroup() %>% 
  mutate(SYSTEM_NO = factor(SYSTEM_NO)
  ) %>% left_join(ind)


# Baseline specifications for the effect of drought on nitrates: FE is SYSTEM NO x YEAR -----------

# Overall + CWS & Year FE

# list of xvariables to explore
xvar = list('d', paste(c('d', 'd:b_majority_latino'), collapse = '+'), 
            paste(c('d', 'd:b_majority_latino', 'd:log_hh_income'), collapse = '+'),
            paste(c('d', 'd:b_majority_latino', 'd:log_hh_income', 'd:percent_ag'), collapse = '+'),
            paste(c('d', 'd:b_majority_latino', 'd:log_hh_income', 'd:percent_ag', 'd:avg_percent_clay'), collapse = '+'),
            paste(c('d', 'd:b_majority_latino', 'd:log_hh_income', 'd:percent_not_fluent_english'), collapse = '+'),
            paste(c('d', 'd:b_majority_latino', 'd:log_hh_income', 'd:log_pop'), collapse = '+'))

# fe is cws x year
fe = paste(c('SYSTEM_NO', 'factor(year)'), collapse = '+')

reg_delivered(df = as_drought, xvar = xvar, yvar = 'mean_as', fe=fe, clust=0, plot = FALSE)
reg_delivered(df = as_drought, xvar = xvar, yvar = 'mean_as', fe=fe, clust=0, plot = TRUE)

# Now try for linear year specification

xvar = list('d + SYSTEM_NO:year', paste(c('d', 'd:b_majority_latino', 'SYSTEM_NO:year'), collapse = '+'), 
            paste(c('d', 'd:b_majority_latino', 'd:log_hh_income', 'SYSTEM_NO:year'), collapse = '+'),
            paste(c('d', 'd:b_majority_latino', 'd:log_hh_income', 'd:percent_ag', 'SYSTEM_NO:year'), collapse = '+'),
            paste(c('d', 'd:b_majority_latino', 'd:log_hh_income', 'd:percent_not_fluent_english', 'SYSTEM_NO:year'), collapse = '+'),
            paste(c('d', 'd:b_majority_latino', 'd:log_hh_income', 'd:log_pop', 'SYSTEM_NO:year'), collapse = '+'))


reg_delivered(df = as_drought, xvar = xvar, yvar = 'mean_as', fe=0, clust=0, plot = FALSE)

reg_delivered(df = ni_drought, xvar = xvar, yvar = 'mean_n', fe=0, clust=0, plot = TRUE)
stargazer(mod_ni1, mod_ni2, mod_ni_l1, mod_ni_l2, omit = ':year|Constant', 
          title = 'Impacts of a unit increase in drought measure',
          dep.var.labels = 'Mean concentration of N (mg/l)', type = 'html',
          style = 'qje', digits = 3,
          add.lines = list(c("Fixed effects?", "CWS and year",  "CWS and year", "CWS linear trends", "CWS linear trends")),
          single.row = TRUE)

form = paste(c('d', 'd:b_majority_latino', 'd:log_hh_income', 
        'd:percent_ag', 'd:avg_percent_clay', 'd:avg_percent_ph'), collapse = '+')

# Make spatial varying graphs

ni.cs <- ni_reg %>% group_by(SYSTEM_NO) %>% 
  filter(year > 2010) %>%
  summarise(pws_n_mean = mean(mean_n, na.rm = TRUE),
            pws_n_median = median(mean_n, na.rm = TRUE)) %>% 
  left_join(ind) %>% 
  mutate(bins = cut_number(log_hh_income, 10)) %>% 
  drop_na(bins)

# Be careful with scale_y_continuous(limits=...) 
# This will remove data that fall outside the limits and then perform the statistical calculations.
# Be careful with scale_y_continuous(limits=...) This will remove data that fall outside the limits and then perform the statistical calculations. In other words the mean and other summaries will be affected. If this is what you want, then great. 
# The alternative is to use coord_cartesian(limits=...) - 
# this 'zooms' in without removing data or affecting the summaries.

ni.cs %>% ggplot(aes(x = bins, y = pws_n_median)) +
  geom_boxplot() +
  # coord_cartesian(ylim = c(0,30)) +
  theme_cowplot()


# summary(mod_ni_perclat)
# 
# # mod 3 + hh income: spid FEs and PWS specific linear year 
# mod_ni_lowincome <- 
#   felm(mean_n ~ d + d:b_low_income + SYSTEM_NO:year | samplePointID | 0 | SYSTEM_NO, data = ni_drought, weights = ni_drought$n_spid)
# 
# summary(mod_ni_lowincome)
# 
# # mod 3 + hh income + perc latino: spid FEs and PWS specific linear year 
# mod_ni_both <- 
#   felm(mean_n ~ d + d:b_low_income + d:b_majority_latino + SYSTEM_NO:year | samplePointID | 0 | SYSTEM_NO, data = ni_drought, weights = ni_drought$n_spid)
# 
# summary(mod_ni_both)
# 
# stargazer::stargazer(mod_ni1, mod_ni2, mod_ni3, mod_ni_perclat, mod_ni_lowincome, mod_ni_both, omit = c(':year'), single.row = TRUE,
#                      dep.var.labels   = "Mean Nitrate level (ug/L)", dep.var.caption = "Outcome:", omit.stat = c("adj.rsq", "ser"))
# 
# # MOD GRP 2: Drought on N,  status ----------------------------------------
# 
# # instantaneous effect is on the surface
# # mean contemporaneous effect of precip on raw GW, mean contemporaneous effect of precip on raw S, mean contemporaneous effect of precip on treated water
# 
# # mod 1: spid FEs and year FE
# mod_ni_1 <- 
#   felm(mean_n ~ d + d:gw + d:raw | samplePointID + factor(year) | 0 | SYSTEM_NO, data = ni_drought, weights = ni_drought$n_spid)
# 
# summary(mod_ni_1)
# 
# # mod 2: spid FEs and linear year 
# mod_ni_2 <- 
#   felm(mean_n ~ d + d:gw + d:raw + year | samplePointID | 0 | SYSTEM_NO, data = ni_drought, weights = ni_drought$n_spid)
# 
# summary(mod_ni_2)
# 
# # mod 3: spid FEs and PWS specific linear year 
# mod_ni_3 <- 
#   felm(mean_n ~ d + d:gw + d:raw + SYSTEM_NO:year | samplePointID | 0 | SYSTEM_NO, data = ni_drought, weights = ni_drought$n_spid)
# 
# summary(mod_ni_3)
# 
# stargazer::stargazer(mod_ni_1, mod_ni_2, mod_ni_3,
#                      omit = c(':year'), single.row = TRUE,
#                      dep.var.labels   = "Mean Nitrate level (ug/L)", dep.var.caption = "Outcome:", omit.stat = c("adj.rsq", "ser"))
# 
# 
# 
# # MOD GRP 3: Drought on N, status, lags -----------------------------------
# mod_ni_lag1 <- 
#   felm(mean_n ~ d
#        + dlag1
#        + dlag2
#        # + dlag3
#        + d:gw
#        + dlag1:gw
#        + dlag2:gw
#        # + dlag3:gw
#        + d:raw
#        + dlag1:raw
#        + dlag2:raw
#        # + dlag3:raw
#        | samplePointID + factor(year) | 0 | SYSTEM_NO, data = ni_drought, weights = ni_drought$n_spid)
# 
# summary(mod_ni_lag1)
# 
# mod_ni_lag2 <- 
#   felm(mean_n ~ year + d + dlag1 + dlag2 
#        # + dlag3
#        + d:gw
#        + dlag1:gw
#        + dlag2:gw
#        # + dlag3:gw
#        + d:raw
#        + dlag1:raw
#        + dlag2:raw
#        # + dlag3:raw 
#        + year
#        | samplePointID | 0 | SYSTEM_NO, data = ni_drought, weights = ni_drought$n_spid)
# 
# summary(mod_ni_lag2)
# 
# mod_ni_lag3 <- 
#   felm(mean_n ~ d + dlag1 + dlag2 
#        # + dlag3 
#        + d:gw
#        + dlag1:gw
#        + dlag2:gw
#        # + dlag3:gw
#        + d:raw
#        + dlag1:raw
#        + dlag2:raw
#        # + dlag3:raw 
#        + year:SYSTEM_NO
#        | samplePointID | 0 | SYSTEM_NO, data = ni_drought, weights = ni_drought$n_spid)
# 
# summary(mod_ni_lag3)
# 
# # mod_ni_lag4 <-
# #   felm(mean_n ~ d + dlag1 + dlag2 + dlag3 
# #        + d:gw
# #        + dlag1:gw
# #        + dlag2:gw
# #        + dlag3:gw
# #        + d:raw
# #        + dlag1:raw
# #        + dlag2:raw
# #        + dlag3:raw 
# #        + d:gXraw0
# #        + dlag1:gXraw0
# #        + dlag2:gXraw0
# #        + dlag3:gXraw0 + year:SYSTEM_NO
# #        | samplePointID | 0 | SYSTEM_NO, data = ni_drought, weights = ni_drought$n_spid)
# 
# saveRDS(mod_ni_lag3, "../Data/1int/ni_mod_lag3.rds")
# 
# stargazer::stargazer(mod_ni_lag1, mod_ni_lag2, mod_ni_lag3, 
#                      omit = c('year:'), single.row = TRUE,
#                      dep.var.labels   = "Mean Nitrate level (ug/L)", dep.var.caption = "Outcome:", omit.stat = c("adj.rsq", "ser"))
# 
# # GW X RAW X EJ  -----------------------------------------------------
# 
# # in majority latino areas
# 
# df_h_perclat <- ni_drought %>% filter(b_majority_latino==1 & !is.na(b_majority_latino))
# 
# mod_ni_3_perclat_h <-
#   felm(mean_n ~ d + d:gw + d:raw + SYSTEM_NO:year | samplePointID | 0 | SYSTEM_NO, data = df_h_perclat, weights = df_h_perclat$n_spid)
# 
# summary(mod_ni_4_perclat_h)
# 
# mod_ni_lag3_perclat_h <-
#   felm(mean_n ~ d + dlag1 + dlag2
#        # + dlag3
#        + d:gw
#        + dlag1:gw
#        + dlag2:gw
#        # + dlag3:gw
#        + d:raw
#        + dlag1:raw
#        + dlag2:raw
#        # + dlag3:raw
#        + year:SYSTEM_NO
#        | samplePointID | 0 | SYSTEM_NO, data = df_h_perclat, weights = df_h_perclat$n_spid)
# 
# summary(mod_ni_lag3_perclat_h)
# 
# # only 8 SPID with treated GW in this category... do not add that?
# 
# df_l_perclat <- ni_drought %>% filter(b_majority_latino==0 & !is.na(b_majority_latino))
# 
# mod_ni_3_perclat_l <-
#   felm(mean_n ~ d + d:gw + d:raw + SYSTEM_NO:year |  samplePointID | 0 | SYSTEM_NO, data = df_l_perclat, weights = df_l_perclat$n_spid)
# 
# mod_ni_lag3_perclat_l <-
#   felm(mean_n ~ d + dlag1 + dlag2
#        # + dlag3
#        + d:gw
#        + dlag1:gw
#        + dlag2:gw1
#        # + dlag3:gw
#        + d:raw
#        + dlag1:raw
#        + dlag2:raw
#        # + dlag3:raw
#        + year:SYSTEM_NO
#        | samplePointID | 0 | SYSTEM_NO, data = df_l_perclat, weights = df_l_perclat$n_spid)
# 
# summary(mod_ni_lag3_perclat_l)
# 
# df_int_perclat_h <- sum_marginal(mod_ni_4_perclat_h, nlags = 0, int_terms = c('gw0', ':raw0'), contaminant = 'n')
# df_int_perclat_l <- sum_marginal(mod_ni_4_perclat_l, nlags = 0, int_terms = c('gw0', ':raw0'), contaminant = 'n')
# 
# plot_coeff_lags(df_int_cv[1:4,], contaminant = 'n', ylm =c(-0.1, 0.25))
# 
# 
