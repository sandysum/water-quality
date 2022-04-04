
# Nitrate regression source ------------------------------------------------------

# 2022/03/15
# sandysum@ucsb.edu

# Load packages -----------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(fixest)
library(DescTools)
library(future.apply)
library(did)
library(cowplot)
source("Scripts/helper_functions_models.R")
options(digits=3)
# Read in data ------------------------------------------------------------
# home <- "G:/My Drive/0Projects/1Water/2Quality/Data/"
home <- "/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/Data/"
pdsi <- readRDS("../Data/drought/pdsi_pws_year.rds") 
ind <- readRDS(file.path(home, "1int/pws_ind.rds"))
ni <-read_rds(file.path(home, "1int/caswrb_n_reg.rds")) %>% left_join(ind) %>% 
  left_join(pdsi)

ni_drought <- subset_years(2007, pollutant = ni , 2021, 1) %>% 
  prep_reg() %>% 
  mutate(b_majority_latino = factor(b_majority_latino),
         b_low_income = factor(b_low_income),
         raXy = factor(paste0(RegulatingAgency, year)),
         cXy = factor(paste0(CITY, year)),
         ctXy = factor(paste0(countyName, year))) %>% 
  filter(STATUS %in% c('AT', 'AR', 'AU', 'CM', 'CR', 'CT', 'DT', 'DR', 'SR',
                       'SU', 'ST', 'CU'))

ni_split <- ni_drought %>% 
  split(ni_drought$gw) %>% 
  map(~(filter(., raw==1)))

table <- ni %>% group_by(SYSTEM_NO, TotalPopulation, b_majority_latino, b_low_income, 
                         PrimaryWaterSourceType) %>%
  filter(STATUS %in% c('AT', 'AR', 'AU', 'CM', 'CR', 'CT', 'DT', 'DR', 'SR',
                       'SU', 'ST', 'CU')) %>% 
  summarize(num_source = length(unique(samplePointID)),
            groundwater = sum(str_detect(PrimaryWaterSourceType, 'Groundwater')),
            surface = sum(str_detect(PrimaryWaterSourceType, 'Surface')))

table$num_source %>% table()
  

  group_by(b_majority_latino) %>%
  summarize(
    total_pop_served = sum(TotalPopulation[n==1]),
    total_pop_served_gw = sum(TotalPopulation[(n==1&gw==1)]),
    one_source = sum(n == 1),
    one_source_gw = sum(n==1 & gw==1),
    n = n()
  ) %>%
  mutate(frac_one_source = one_source / n)

kbl(table, 'latex', booktabs = TRUE)

# need to find a way to visualize this 
sys <- ni_drought %>% distinct(b_majority_latino, raw, gw, SYSTEM_NO, samplePointID)
table(sys$raw, sys$gw, sys$b_majority_latino)

sys %>% 
  drop_na() %>% 
  group_by(b_majority_latino, gw, raw) %>% 
  summarize(n = n())

ni_drought %>% group_by(year) %>% summarise(mean_d = mean(d, na.rm = TRUE))

# Main regression GWxML

gwml <- source_reg(ni_split[[1]], by = '+d:b_majority_latino')

# Main regressions GWxLI group --------------------------------------------

gwli <- source_reg(ni_split[[1]], by = "+d:b_low_income")

# Final regressions 2022 spring:
  
gw0 <- felm(as.formula(paste0('mean_n ~ d ', ' ', '| factor(year) | 0 | 0')), 
                   data = ni_split[[1]], weights = ni_split[[1]]$n_spid)

summary(gw0)

# should I include b_majority_latino in the controls?
gw1 <- felm(as.formula(paste0('mean_n ~ d ', ' +d:b_majority_latino + b_majority_latino', '| factor(year) | 0 | 0')), 
            data = ni_split[[1]], weights = ni_split[[1]]$n_spid)

summary(gw1)

# should I include b_majority_latino in the controls?
gw1 <- felm(as.formula(paste0('mean_n ~ d ', ' +d:b_majority_latino + d:log_hh_income + b_majority_latino + log_pop + log_hh_income + 
                              percent_ag + avg_percent_clay', '| countyName + factor(year) + OwnerType | 0 | 0')), 
            data = ni_split[[1]], weights = ni_split[[1]]$n_spid)

summary(gw1)

# should I include b_majority_latino in the controls?
# gw2 <- felm(as.formula(paste0('mean_n ~ d ', ' +d:b_majority_latino + b_majority_latino + log_pop + log_hh_income + 
#                               percent_ag + avg_percent_clay + SYSTEM_NO:year', '| samplePointID + factor(year) | 0 | 0')), 
#             data = ni_split[[1]], weights = ni_split[[1]]$n_spid)
# 
# summary(gw2)

gw3 <- felm(as.formula(paste0('mean_n ~ d ', ' + d:b_majority_latino', '| samplePointID + factor(year) | 0 | 0')), 
            data = ni_split[[1]], weights = ni_split[[1]]$n_spid)
summary(gw3)

sum_marginal(gw3, nlags=0, int_terms = c('b_majority_latino'), pollutant = 'n')

gw4 <- felm(as.formula(paste0('mean_n ~ d ', ' + d:b_majority_latino + SYSTEM_NO:year', '| samplePointID | 0 | 0')), 
            data = ni_split[[1]], weights = ni_split[[1]]$n_spid)

# For plotting results ----------------------------------------------------

gw_main <- feols(as.formula(paste0('mean_n ~ d ', ' + d:b_majority_latino + d:b_low_income', '| samplePointID + SYSTEM_NO[year]')), 
            data = ni_split[[1]], weights = ni_split[[1]]$n_spid, vcov = ~SYSTEM_NO)

sum_marginal(gw_main, nlags=0, int_terms = c('b_majority_latino', 'b_low_income'), pollutant = 'n')

sw_main <- feols(as.formula(paste0('mean_n ~ d ', ' + d:b_majority_latino + d:b_low_income', '| samplePointID + SYSTEM_NO[year]')), 
                 data = ni_split[[2]], weights = ni_split[[2]]$n_spid, vcov = ~SYSTEM_NO)

sum_marginal(sw_main, nlags=0, int_terms = c('b_majority_latino', 'b_low_income'), pollutant = 'n')

ni.tr <- ni_drought %>% filter(raw == 0)
tw_main <- feols(as.formula(paste0('mean_n ~ d ', ' + d:b_majority_latino + d:b_low_income', '| samplePointID + SYSTEM_NO[year]')), 
                 data = ni.tr, weights = ni.tr$n_spid, vcov = ~SYSTEM_NO)

sum_marginal(tw_main, nlags=0, int_terms = c('b_majority_latino', 'b_low_income'), pollutant = 'n')

ls <- map(list(gw_main, sw_main, tw_main), sum_marginal, 
          nlags=0, int_terms = c('b_majority_latino', 'b_low_income'), pollutant = 'n') %>% 
  bind_rows(.id = 'model') %>%
  rename(estimate = mean_n, 
         std.error = se, 
         p.value = pval, 
         statistic = t_val) %>% 
  mutate(term = c(rep('Groundwater', 3), rep('Surface water', 3), rep('Treated water', 3)),
         model = int_terms,
         estimate = estimate*2, 
         std.error = std.error*2)

out <- dwplot(ls,
              vline = geom_vline(
                xintercept = 0,
                colour = "grey60",
                linetype = 2
              )) + theme_bw() +
  scale_color_manual(
    name = ' ',
    values = c(' ' = 'darkgoldenrod3', 'b_majority_latino' = 'darkturquoise', 'b_low_income' = 'black'),
    labels = c("All California", "Majority latino", "Low income")
  ) +
  scale_x_continuous(n.breaks = 8) 

save_plot("Plots.spring2022/final_reg_n.png", out, base_asp = 1.3, scale = 1)

# Stargazer

x <- append(gwml, gwli)
stargazer(x,  omit = c(':year'), single.row = TRUE,
          add.lines = list(c("Fixed effects?", "Yr", "Yr+Site", 
                             "Site+ CWS linear trends","Fixed effects?", "Yr", "Yr+Site", 
                             "Site+ CWS linear trends")),
          # column.labels = c("All California", "Majority Latino", "Low income"),
          dep.var.labels   = "Mean Nitrate level (ug/L)", 
          dep.var.caption = "Outcome:", omit.stat = c("adj.rsq", "ser"),
          type = 'html', style = 'qje')

# Main regression GWxML

swml <- source_reg(ni_split[[2]], by = '+d:b_majority_latino')

# Main regressions GWxLI group --------------------------------------------

swli <- source_reg(ni_split[[2]], by = "+d:b_low_income")

# Stargazer

x <- append(swml, swli)
stargazer(x,  omit = c(':year'), single.row = TRUE,
          add.lines = list(c("Fixed effects?", "Yr", "Yr+Site", 
                             "Site+ CWS linear trends","Fixed effects?", "Yr", "Yr+Site", 
                             "Site+ CWS linear trends")),
          # column.labels = c("All California", "Majority Latino", "Low income"),
          dep.var.labels   = "Mean Nitrate level (ug/L)", 
          dep.var.caption = "Outcome:", omit.stat = c("adj.rsq", "ser"),
          type = 'html', style = 'qje')


# Main regression TWxML

ni.tr <- ni_drought %>% filter(raw == 0)

tw <- source_reg(ni.tr, by = " ")
twml <- source_reg(ni.tr, by = '+d:b_majority_latino')

# Main regressions TWxLI group --------------------------------------------

twli <- source_reg(ni.tr, by = "+d:b_low_income")

x <- append(twml, twli)
stargazer(x,  omit = c(':year'), single.row = TRUE,
          add.lines = list(c("Fixed effects?", "Yr", "Yr+Site", 
                             "Site+ CWS linear trends","Fixed effects?", "Yr", "Yr+Site", 
                             "Site+ CWS linear trends")),
          # column.labels = c("All California", "Majority Latino", "Low income"),
          dep.var.labels   = "Mean Nitrate level (ug/L)", 
          dep.var.caption = "Outcome:", omit.stat = c("adj.rsq", "ser"),
          type = 'html', style = 'qje')

gwcoeffs <- gwml %>% .[[2]] %>% tidy() %>% bind_rows(gwli %>% .[[2]] %>% tidy()) %>% 
  slice(1, 2, 4) %>% mutate(model = term, term = 'Raw groundwater')
swcoeffs <- swml %>% .[[2]] %>% tidy() %>% bind_rows(swli %>% .[[2]] %>% tidy()) %>% 
  slice(1, 2, 4) %>% mutate(model = term, term = 'Raw surface water')
twcoeffs <- twml %>% .[[2]] %>% tidy() %>% bind_rows(twli %>% .[[2]] %>% tidy()) %>% 
  slice(1, 2, 4) %>% mutate(model = term, term = 'Treated water')

coeffs <- bind_rows(gwcoeffs, swcoeffs, twcoeffs)

out <- dwplot(coeffs,
       vline = geom_vline(
         xintercept = 0,
         colour = "grey60",
         linetype = 2
       )) + theme_bw() +
  scale_color_manual(
    values = c('d' = 'darkgoldenrod3', 'd:b_majority_latino1' = 'darkturquoise', 'd:b_low_income' = 'black'),
    labels = c("All California", "Majority latino", "Low income")
  ) +
  scale_x_continuous(breaks = seq(-.8, 2, 0.2))

save_plot("Plots/0source_regression.png", out, base_height = 1.2, scale = 4)

# Baseline specifications for the effect of drought on nitrates -----------
# fe is cws x year

fe = paste(c('samplePointID', 'factor(year)'), collapse = '+')
fe = 'samplePointID + SYSTEM_NO[year]'
x <-  paste(c('d', 'd:percent_hispanic', 'd:log_hh_income'), collapse = '+')
form = as.formula(paste0('mean_n', " ~ ", x,   " | ", fe))

# run optimal model on 3 different dataset with different scenarios
mod <- feols(fml = form, data = ni_split[[1]], weights = ni_split[[1]]$n_spid)

newdata0 <- ni_split[[1]] %>% 
  ungroup() %>% 
  select(SYSTEM_NO, samplePointID, percent_hispanic, log_hh_income) %>% 
  distinct() %>% 
  mutate(year=2019,
         d=-0.8)

newdata1 <- ni_split[[1]] %>% 
  ungroup() %>% 
  select(SYSTEM_NO, samplePointID, percent_hispanic, log_hh_income) %>% 
  distinct() %>% 
  mutate(year=2019,
         d=2.5)

pred0 <- predict(mod, newdata = newdata0) %>% bind_cols(newdata0) 
pred1 <- predict(mod, newdata = newdata1) %>% bind_cols(newdata1) 

pred <- pred0 %>% bind_rows(pred1) %>% 
  spread(key = d, value = `...1`) %>%
  rename(Predrought = `-0.8`,
         Drought = `2.5`) %>% 
  mutate(drought_effect = Drought - Predrought,
         over5_pre = Predrought>5, 
         over5_post = Drought > 5,
         over5_causal_drought = if_else((!over5_pre)&over5_post, 1, 0)) %>% 
  select(-percent_hispanic, -log_hh_income) %>% 
  left_join(ind)

pred$over5_post %>% sum(na.rm = TRUE)

ggplot(pred, aes(percent_hispanic, median_hh_income)) +
  geom_point(aes(color= drought_effect), shape = 15, size = 4) +
  scale_color_distiller(palette = 'YlOrRd', direction = 1, name = 'Severe drought effect') +
  theme_minimal() +
  labs(x = '% Latino PWS Area Served', y = 'Median Household Income ($)\n')
  
  

pred_long <- pred %>% gather("When", "meanN", Predrought, Drought)

ggplot(pred_long, aes(meanN, color = When)) +
  geom_density() +
  theme_minimal() +
  facet_wrap(facets = vars(b_majority_latino))
