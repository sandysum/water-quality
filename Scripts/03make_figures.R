
# Make plots for optimal regressions --------------------------------------
rm(list = ls())
library(tidyverse)
source("../water-quality/Scripts/helper_functions_models.R")


# Read in the ideal model output for arsenic and nitrate ------------------

ar <- read_rds("../Data/1int/ar_mod_lag4.rds")
ni<- read_rds("../Data/1int/ni_mod_lag4.rds")

# this function outputs the following effects
# raw gw, raw sw, treated gw, treated sw
# only raw gw is impacted; increase in arsenic level!

# Arsenic -----------------------------------------------------------------

# this returns the cumulative effects 
df_int <- sum_lags(ar, int_terms = c('gw0', ':raw0|gXraw0', ':raw0|gw0'))
df_int_lags <- sum_marginal(ar, contaminant = 'ar', )

ar_reg_balanced %>% group_by(raw, gw) %>% 
  summarize(mean_n_ = mean(mean_ar, na.rm = TRUE),
            n = n(),
            sd_n = sd(mean_ar, na.rm = TRUE))

plot_coeff(df_int,  drought_measure = 'a drought-year')
# map the effects to 4 water source type and status
a <- plot_coeff_lags(df_int_lags[1:4,], type = 'raw groundwater', drought_measure = 'a drought year', contaminant = 'ar')
b <- plot_coeff_lags(df_int_lags[5:8,], type = 'raw surface water', drought_measure = 'a drought year', contaminant = 'ar')
c <- plot_coeff_lags(df_int_lags[9:12,], type = 'treated groundwater', drought_measure = 'a drought year', contaminant = 'ar', ylm = c(-1.5, 1.2))
d <- plot_coeff_lags(df_int_lags[13:16,], type = 'treated surface water', drought_measure = 'a drought year', contaminant = 'ar', ylm = c(-1.5, 1.2))

save_plot("Plots/20211202_lagged_as.png", plot_grid(plotlist = list(a,b,c,d), nrow = 2, labels = c('A', 'B', 'C', 'D')), base_asp = 1.6, scale = 1.6)

# Nitrates ----------------------------------------------------------------

# this returns the cumulative effects 
df_int <- sum_lags(ni, int_terms = c('gw0', ':raw0|gXraw0', ':raw0|gw0'))
# the second term in this function helps name the output contaminant effect
df_int_lags <- sum_marginal(ni, contaminant = 'n')

plot_coeff(df_int, contaminant = 'ni', drought_measure = 'a drought-year')
# map the effects to 4 water source type and status
a <- plot_coeff_lags(df_int_lags[1:4,], type = 'raw groundwater', drought_measure = 'a drought year', contaminant = 'n', ylm = c(-.1, .2))
b <- plot_coeff_lags(df_int_lags[5:8,], type = 'raw surface water', drought_measure = 'a drought year', contaminant = 'n', ylm = c(-.1, .2))
c <- plot_coeff_lags(df_int_lags[9:12,], type = 'treated groundwater', drought_measure = 'a drought year', contaminant = 'n', ylm = c(-1.2, .6))
d <- plot_coeff_lags(df_int_lags[13:16,], type = 'treated surface water', drought_measure = 'a drought year', contaminant = 'n', ylm = c(-1.2, .6))

save_plot("Plots/20211202_lagged_n.png", plot_grid(plotlist = list(a,b,c,d), nrow = 2, labels = c('A', 'B', 'C', 'D')), base_asp = 1.6, scale = 1.6)

ni_reg_balanced %>% group_by(raw, gw) %>% 
  summarize(mean_n_ = mean(mean_n, na.rm = TRUE),
            n = n(),
            sd_n = sd(mean_n, na.rm = TRUE))


