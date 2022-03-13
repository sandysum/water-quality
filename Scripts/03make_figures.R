
# Make plots for optimal regressions --------------------------------------
rm(list = ls())
library(tidyverse)
source("../water-quality/Scripts/helper_functions_models.R")


# Read in the ideal model output for arsenic and nitrate ------------------

ar <- read_rds("../Data/1int/as_mod_lag3.rds")
ni <- read_rds("../Data/1int/ni_mod_lag3.rds")

ni_ej <- read_rds("../Data/1int/n_mod_ej.rds")
as_ej <- read_rds("../Data/1int/as_mod_ej.rds")

# this function outputs the following effects
# raw gw, raw sw, treated gw, treated sw
# only raw gw is impacted; increase in arsenic level!

# Arsenic -----------------------------------------------------------------

# this returns the cumulative effects 
df_int <- sum_lags(ar, nlags = 2, int_terms = c('gw0', 'raw0')) %>% 
  mutate(mod = 'All California')
df_int_lags <- sum_marginal(ar, nlags = 2, contaminant = 'as', int_terms = c('gw0', 'raw0'))
as_ej_cum <- map(as_ej, sum_lags, nlags = 2, int_terms = c('gw0', 'raw0')) %>%
  bind_rows() %>%
  mutate(mod = c(rep('non-majority-Latino', 3),rep('majority-Latino', 3),
                 rep('non-low-income', 3), rep('low-income', 3))) %>%
  bind_rows(df_int) %>% 
  mutate(x_ticks = rep(c('Groundwater', 'Surface water', 'Treated water'),5))

dodge <- position_dodge(.6)

ggplot(as_ej_cum, aes(x = x_ticks, y = est, color = mod)) +
  geom_errorbar(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se),
                width = .1, position = dodge) +
  geom_hline(yintercept = 0, color = 'red') +
  geom_point(position = dodge) +
  theme_minimal_vgrid() +
  ylim(c(-1, 1)) +
  scale_y_continuous(n.breaks = 11) +
  coord_flip() +
  labs(x = ' ', y = 'Cumulative drought effects on As (ug/l)') +
  scale_color_brewer(palette = 'Set2')

as_reg_balanced %>% group_by(raw, gw) %>% 
  summarize(mean = mean(mean_as, na.rm = TRUE),
            n = n(),
            sd_n = sd(mean_as, na.rm = TRUE))

plot_coeff(df_int, drought_measure = 'a drought-year')
# map the effects to 4 water source type and status
a <- plot_coeff_lags(df_int_lags[1:3,], type = 'raw groundwater', drought_measure = 'a drought year', contaminant = 'as')
b <- plot_coeff_lags(df_int_lags[4:6,], type = 'raw surface water', drought_measure = 'a drought year', contaminant = 'as')
c <- plot_coeff_lags(df_int_lags[7:9,], type = 'treated water', drought_measure = 'a drought year', contaminant = 'as', ylm = c(-1.5, 1.2))

save_plot("Plots/20211202_lagged_as.png", plot_grid(plotlist = list(a,b,c,d), nrow = 2, labels = c('A', 'B', 'C', 'D')), base_asp = 1.6, scale = 1.6)

# Nitrates ----------------------------------------------------------------

# this returns the cumulative effects 
df_int <- sum_lags(ni, nlags = 2, int_terms = c('gw0', 'raw0')) %>% 
  mutate(mod = 'All California')
# the second term in this function helps name the output contaminant effect
df_int_lags <- sum_marginal(ni, contaminant = 'n')
# perc lat l, per lat h, non-low income, low income

n_ej_cum <- map(ni_ej, sum_lags, nlags = 2, int_terms = c('gw0', 'raw0')) %>%
  bind_rows() %>%
  mutate(mod = c(rep('non-majority-Latino', 3),rep('majority-Latino', 3),
                 rep('non-low-income', 3), rep('low-income', 3))) %>%
  bind_rows(df_int) %>% 
  mutate(x_ticks = rep(c('Groundwater', 'Surface water', 'Treated water'),5))

dodge <- position_dodge(.6)

ggplot(n_ej_cum, aes(x = x_ticks, y = est, color = mod)) +
  geom_errorbar(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se),
                width = .1, position = dodge) +
  geom_hline(yintercept = 0, color = 'red') +
  geom_point(position = dodge) +
  theme_minimal_vgrid() +
  ylim(c(-1, 1)) +
  scale_y_continuous(n.breaks = 11) +
  coord_flip() +
  labs(x = ' ', y = 'Cumulative drought effects on N') +
  scale_color_brewer(palette = 'Set2')

plot_coeff(df_int, contaminant = 'ni', drought_measure = 'a drought-year')

plot_coeff(df_int, contaminant = 'n', drought_measure = 'a drought-year')
plot_coeff(n_ej_cum[[2]], contaminant = 'n', drought_measure = 'a drought-year')
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


# Make figures for delivered N and As ----------------------------------------------

home <- "/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/Data"
pdsi <- readRDS(file.path(home,"/drought/pdsi_pws_year.rds"))
ind <- readRDS(file.path(home, "1int/pws_ind.rds"))

ni_reg = c()

for (j in 1:3) {
  ni_reg[[j]] <- readRDS(str_subset(list.files(file.path(home, "1int"), full.names = TRUE), 
                                    "caswrb_n_(de|op|pe)")[j]) %>% 
    dplyr::select(1:4) %>% 
    ungroup() %>% 
    mutate(scenario = c('equal', 'optimistic', 'pessimistic')[j],
           mean_n = Winsorize(mean_n, probs = c(0, 0.95))) %>% 
    left_join(ind)
}

as_reg = c()

for (j in 1:3) {
  as_reg[[j]] <- readRDS(str_subset(list.files(file.path(home, "1int"), full.names = TRUE), 
                                    "caswrb_as_(de|op|pe)")[j]) %>% 
    dplyr::select(1:4) %>% 
    ungroup() %>% 
    mutate(scenario = c('equal', 'optimistic', 'pessimistic')[j],
           mean_as = Winsorize(mean_as, probs = c(0, 0.95))) %>% 
    left_join(ind)
}

# ni_reg <-read_rds(file.path(home, "1int/caswrb_n_1974-2021.rds"))

# Read in and join to social eq ind ---------------------------------------
# Added this part to run some regression to join social eq indicator

plist <- map(ni_reg, plot_es_delivered, by = 'b_low_income', ylm=c(1, 4)) %>% map(add_drought)
out <- plot_grid(plotlist = plist, nrow = 1, labels = c('Equal', 'Optimistic', 'Pessimistic'))

save_plot('Plots/0delivered_scenario_income.png', out, base_height = 3.2, scale = 3)

plist <- map(as_reg, plot_es_delivered, pollutant = 'mean_as', by = 'b_low_income',ylm=c(1, 8), ylab = 'Mean As conc. (ug/l)')

out <- plot_grid(plotlist = plist, nrow = 1, labels = c('Equal', 'Optimistic', 'Pessimistic'))

save_plot('Plots/0delivered_scenario_drought_as_income.png', out, base_height = 3.2, scale = 3)

