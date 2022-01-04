
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
options(digits=3)
# Read in data ------------------------------------------------------------

pdsi <- readRDS("../Data/drought/pdsi_pws_year.rds") 

ni_reg <-read_rds(file.path(home, "1int/caswrb_n_reg.rds"))
# ni_reg <-read_rds(file.path(home, "1int/caswrb_n_1974-2021.rds"))

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
    gXraw = factor(gw*raw, levels = c('0', '1'))) %>% 
  mutate(
    gw = factor(gw, levels = c("1", "0")),
    raw = factor(raw),
    SYSTEM_NO = factor(SYSTEM_NO)
  ) %>%
  group_by(SYSTEM_NO, year) %>%
  mutate(n_spid = 1 / (unique(samplePointID) %>% length())) %>%
  ungroup()


# Read in and join to social eq ind ---------------------------------------
# Added this part to run some regression to join social eq indicator

sys <- read_csv(file.path(home, "ca_water_qual/watsys_tract_socialeq_ind.csv")) %>% 
  mutate(b_majority_latino = if_else(percent_his >= .5, 1, 0),
         b_low_income = if_else(median_hh_income <=46000, 1, 0))

ni_reg_balanced <- ni_reg_balanced %>% dplyr::left_join(sys, by = "SYSTEM_NO")
# Visualize annual trends within PWS --------------------------------------
# set.seed(1297)
# q <- sample(unique(ni_drought$SYSTEM_NO), 100)
# quartz()
# p <- ni_drought %>% 
#   filter(SYSTEM_NO %in% q) %>% 
#   ggplot(aes(x = year, y = mean_n, color = raw, group = samplePointID)) +
#   geom_line() +
#   geom_smooth(aes(group = SYSTEM_NO), method = 'lm') +
#   theme_light() +
#   scale_x_continuous(breaks = seq(2000, 2022, 2)) +
#   # geom_hline(yintercept = 10, color = 'red') +
#   theme(axis.text.x = element_text(angle = 45)) +
#   facet_wrap(vars(SYSTEM_NO), scales = "free")
# 
# save_plot("Google Drive/My Drive/0Projects/1Water/2Quality/water-quality/Plots/pws_linear_n.png", p, base_asp = 1.2, scale = 5)

# NO LAGGED EFFECTS  -------------------------------------------------

# instantaneous effect is on the surface
# mean contemporaneous effect of precip on raw GW, mean contemporaneous effect of precip on raw S, mean contemporaneous effect of precip on treated water

# mod 1: spid FEs and year FE
mod_ni_1 <- 
  felm(mean_n ~ d + d:gw + d:raw | samplePointID + factor(year) | 0 | SYSTEM_NO, data = ni_drought, weights = ni_drought$n_spid)

summary(mod_ni_1)

# mod 2: spid FEs and linear year FE
mod_ni_2 <- 
  felm(mean_n ~ d + d:gw + d:raw + year | samplePointID | 0 | SYSTEM_NO, data = ni_drought, weights = ni_drought$n_spid)

summary(mod_ni_2)

# mod 3: spid FEs and PWS specific linear year FEs
mod_ni_3 <- 
  felm(mean_n ~ d + d:gw + d:raw + SYSTEM_NO:year | samplePointID | 0 | SYSTEM_NO, data = ni_drought, weights = ni_drought$n_spid)

summary(mod_ni_3)

# mod 3: spid FEs and PWS specific year FEs
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
saveRDS(mod_ni_lag4, "../Data/1int/ni_mod_lag4.rds")

stargazer::stargazer(mod_ni_lag1, mod_ni_lag2, mod_ni_lag3, mod_ni_lag4, omit = c('year'), single.row = TRUE,
                     dep.var.labels   = "Mean Nitrate level (ug/L)", dep.var.caption = "Outcome:", omit.stat = c("adj.rsq", "ser"))

# GW X RAW X CV -----------------------------------------------------

# in central valley
# since there is not a lot of surface water nor treated GW observations in the filtered dataset in the CV, I look at the effects of being raw and groundwater compared to all other water types
mod_ni_lag4_cv <-
  felm(mean_n ~ d + dlag1 + dlag2 + dlag3 
       + d:raw
       + dlag1:raw
       + dlag2:raw
       + dlag3:raw + year:SYSTEM_NO
       | samplePointID | 0 | SYSTEM_NO, data = ni_drought %>% filter(cv==1), weights = ni_drought[ni_drought$cv==1,]$n_spid)

summary(mod_ni_lag4_cv)

df_int_cv <- sum_marginal(mod_ni_lag_cv1, nlags = 3, int_terms = c('raw0'), contaminant = 'n')

plot_coeff_lags(df_int_cv[1:4,], contaminant = 'n', ylm =c(-0.1, 0.25))

# non-central valley 

mod_ni_lag4_cv0 <-
  felm(mean_n ~ d + dlag1 + dlag2 + dlag3 
       + d:raw
       + dlag1:raw
       + dlag2:raw
       + dlag3:raw + year:SYSTEM_NO
       | samplePointID | 0 | SYSTEM_NO, data = ni_drought %>% filter(cv==0), weights = ni_drought[ni_drought$cv==0,]$n_spid)

summary(mod_ni_lag4_cv0)

df_int_cv0 <- sum_lags(mod_ni_lag4_cv0, nlags = 3, int_terms = c('raw0'), contaminant = 'n')

df_lag_cv0 <- sum_marginal(mod_ni_lag4_cv0, int_terms = c('raw0'), contaminant = 'n')

plot_coeff(df_int_cv, contaminant = 'ni')
plot_coeff_lags(df_lag_cv0[1:4,], type = 'raw ground/surface water', drought_measure = 'a drought year', contaminant = 'n', ylm = c(-0.1, 0.2))

stargazer::stargazer(mod_ni_lag_cv1, mod_ni_lag_cv0, omit = c('year'), single.row = TRUE,
                     dep.var.labels   = "Mean Nitrate level (ug/L)", dep.var.caption = "Outcome:", omit.stat = c("adj.rsq", "ser"),
                     column.labels = c('In Central Valley', 'Outside of Central Valley'))


