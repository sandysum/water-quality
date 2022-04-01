
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

# Make figures for raw GW SW and TW

home <- "/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/Data"

ind <- readRDS(file.path(home, "1int/pws_ind.rds")) %>% 
  distinct(SYSTEM_NO, .keep_all = TRUE)

ni.d <- readRDS("../Data/1int/caswrb_n_reg.rds") %>% 
  left_join(ind) 

ni.d.means <- ni.d %>% 
  mutate(status = case_when(
    gw==1&raw==1 ~ 'Raw groundwater',
    gw==0&raw==1 ~ 'Raw surface water',
    raw == 0 ~ 'Treated water'
  )) %>% 
  group_by(b_majority_latino, year, status) %>% 
  summarise(mean_n = mean(mean_n, na.rm = TRUE),
            se = sd(mean_n, na.rm = TRUE)) %>% 
  drop_na(b_majority_latino, year, status, mean_n) %>% 
  filter(year>1995, year<2022) %>% 
  ggplot() +
  geom_line(aes(x=year, y=mean_n, color = factor(b_majority_latino)), size = 1) +
  facet_grid(cols = vars(status)) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = .9)) + 
  scale_colour_manual(" ", values=c("darkturquoise", "black"),
                      breaks=c('1', '0'), 
                      labels=c('Majority Latino', "All other")) 

pl <- ni.d.means +
  labs(x = '\nYear', y = 'Mean N conc, mg/l') +
  theme_bw()

pl<-add_drought(pl)

save_plot("Plots/0source_trends_plain.png", add_drought(pl), base_asp = 2.8, scale = 1)

# Make figures for delivered N and As ----------------------------------------------

home <- "/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/Data"

ind <- readRDS(file.path(home, "1int/pws_ind.rds")) %>% 
  distinct(SYSTEM_NO, .keep_all = TRUE)

ni.d <- readRDS("../Data/1int/caswrb_n_delivered.rds") %>% 
  left_join(ind) %>% 
  mutate(mean_n=n)

ni.optimistic <- mutate(ni.d, mean_n = if_else(b_majority_latino==1, min_n, max_n))
ni.pessimistic <- mutate(ni.d, mean_n = if_else(b_majority_latino==1, max_n, min_n))

ni <- bind_rows('Equal' = ni.d, 'Optimistic' = ni.optimistic, 'Pessimistic' = ni.pessimistic, .id = 'Scenario')
ni.d.means <- ni %>% 
  group_by(b_majority_latino, year, Scenario) %>% 
  summarise(`Mean N conc.` = mean(mean_n, na.rm = TRUE),
            se = sd(mean_n, na.rm = TRUE)) %>% 
  drop_na() %>% 
  filter(year>1995, year<2022) %>% 
  ggplot() +
  geom_line(aes(x=year, y=`Mean N conc.` , color = factor(b_majority_latino)), size = 1) +
  # geom_ribbon(aes(x = year, ymin = meanN-se, ymax = meanN + se, group = factor(b_majority_latino)), 
  #                 fill = 'grey70', alpha=.5) +
  facet_grid(cols = vars(Scenario)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = .9)) + 
  scale_colour_manual(" ", values=c("darkturquoise", "black"),
                      breaks=c('1', '0'), 
                      labels=c('Majority Latino', "All other")) 

add_drought(ni.d.means)

save_plot('Plots/0ni_scenarios_ml.png', ni.d.means, base_height = 3.4, scale = 1.2)
save_plot('Plots/0nid_scenarios_ml.png', add_drought(ni.d.means), base_height = 3.4, scale = 1.2)

plist <- map(list(ni.d, ni.optimistic, ni.pessimistic),
             plot_es_delivered, by = 'b_majority_latino', ylm=c(0, 5), 
             years = 1998:2020) %>% map(add_drought)

out <- plot_grid(plotlist = plist, nrow = 1, labels = c('Equal', 'Optimistic', 'Pessimistic'))

# Read in and join to social eq ind ---------------------------------------
# Added this part to run some regression to join social eq indicator

plist <- map(ni_reg, plot_es_delivered, by = 'b_low_income', ylm=c(1, 4)) %>% map(add_drought)

out <- plot_grid(plotlist = plist, nrow = 1, labels = c('Equal', 'Optimistic', 'Pessimistic'))

save_plot('Plots/0delivered_scenario_income.png', out, base_height = 3.2, scale = 3)

as.d <- readRDS("../Data/1int/caswrb_as_delivered.rds") %>% left_join(ind)

plist <- map(as_reg, plot_es_delivered, pollutant = 'mean_as', by = 'b_low_income',ylm=c(1, 8), ylab = 'Mean As conc. (ug/l)')

out <- plot_grid(plotlist = plist, nrow = 1, labels = c('Equal', 'Optimistic', 'Pessimistic'))

save_plot('Plots/0delivered_scenario_drought_as_income.png', out, base_height = 3.2, scale = 3)


# Plot maps ---------------------------------------------------------------

rm(list = ls())
home <- "G:/My Drive/0Projects/1Water/2Quality/Data"
home <- "/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/Data"

library(tidyverse)
library(DescTools)
library(readxl)
library(readr)
library(lfe)
library(sf)
library(cowplot)

# Read in water systems information with social equity indicators ---------

# https://cacensus.maps.arcgis.com/apps/webappviewer/index.html?id=48be59de0ba94a3dacff1c9116df8b37

# Good website to double check if the census average numbers are correct
ind <- readRDS(file.path(home, "1int/pws_ind.rds")) 
# shp_zip <- read_sf("../Data/shp_PWS_SABL_Public_080221/PWS_by_zip.shp")
shp_ej <- read_sf("../Data/shp_ej.shp")

head(shp_ej)

pws_c <- st_centroid(pws) %>% mutate(SYSTEM_NO = str_extract(SABL_PWSID, '\\d+')) %>% 
left_join(n_pws_10yr, by  = 'SYSTEM_NO') %>% 
 filter(!is.na(pws_n_mean)) %>% 
mutate(high_n = pws_n_mean > 5) %>% st_transform(3488) %>% filter(high_n)

ggplot(shp_ej) +
  geom_sf(aes(fill = prcnt_h), color = 'grey70', size = .5) +
  geom_sf(data = pws_c, alpha = .6) +
  theme_minimal() +
  scale_fill_distiller(palette = "RdPu", trans = 'reverse', name = 'Percent Latino')
