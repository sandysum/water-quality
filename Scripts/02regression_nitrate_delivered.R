
# Nitrate regression ------------------------------------------------------

# 2021/11/4
# sandysum@ucsb.edu

# Load packages -----------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(stargazer)
library(lfe)
library(DescTools)
library(future.apply)
library(cowplot)
source("../water-quality/Scripts/helper_functions_models.R")
options(digits=3)

# Read in data ------------------------------------------------------------
home <- "G:/My Drive/0Projects/1Water/2Quality/Data/"
home <- "/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/Data/"
pdsi <- readRDS(file.path(home,"/drought/pdsi_pws_year.rds"))

ni_reg <-read_rds(file.path(home, "1int/caswrb_n_delivered.rds")) %>% 
  dplyr::select(1:4)
# ni_reg <-read_rds(file.path(home, "1int/caswrb_n_1974-2021.rds"))

# Read in and join to social eq ind ---------------------------------------
# Added this part to run some regression to join social eq indicator

ind <- readRDS(file.path(home, "1int/pws_ind.rds"))

# 1. CLEAN DATA FOR: Regression at the monitor month year level ------------------------------

# 2. Filter to balanced panel for year 1996 to 2021

# this function subsets to only balanced panels that has the
ni.d <- readRDS("../Data/1int/caswrb_n_delivered.rds") %>% 
  left_join(ind) %>% 
  left_join(pdsi) %>% 
  mutate(mean_n=n)

ni.optimistic <- mutate(ni.d, mean_n = if_else(b_majority_latino==1, min_n, max_n))
ni.pessimistic <- mutate(ni.d, mean_n = if_else(b_majority_latino==1, max_n, min_n))

ni_equal <- subset_years_cws(2005, pollutant = ni.d , 2021, 1) %>% 
  prep_reg_cws()

ni.optimistic <- subset_years_cws(2005, pollutant = ni.optimistic , 2021, 1) %>% 
  prep_reg_cws()

ni.pessimistic <- subset_years_cws(2005, pollutant = ni.pessimistic , 2021, 1) %>% 
  prep_reg_cws()

fe = paste(c('SYSTEM_NO', 'factor(year)'), collapse = '+')

x <-  paste(c('d', 'd:b_majority_latino', 'd:log_hh_income'), collapse = '+')
form = as.formula(paste0('mean_n', " ~ ", x,   " | ", fe, " | 0 | 0 "))

# run optimal model on 3 different dataset with different scenarios
mod <- map(list(ni.optimistic, ni_equal, ni.pessimistic), ~( smallerMod(felm(formula = form, data = .))) )

x <- map(mod, ~(sum_coeffs(.))) %>% 
  bind_rows() %>% 
  mutate(model = c('Optimistic', 'Pessimistic', 'Equal'),
         term = 'Drought')

mod %>% map(summary)
# To put these numbers in context, I simulated drought 
# impacts on majority-latino serving CWSs earning around 2% 
#less than the mean household income for each scenario using model 3

dwplot(x, vline = geom_vline(
  xintercept = 0,
  colour = "grey60",
  linetype = 2
)) + scale_colour_grey(start = .3,
                    end = .7,
                    name = "Scenario",
                    breaks = c('Optimistic', 'Pessimistic', 'Equal'),
                    labels = c('Optimistic', 'Pessimistic', 'Equal')) +
  theme_bw() 
  

# if opt to output plot then return the plot as from d


# Exploratory regressions tossing in a bunch of things --------------------
# 1. xvars with CWS and years FE
xvar = list('d', paste(c('d', 'd:b_majority_latino'), collapse = '+'),
            paste(c('d', 'd:b_majority_latino', 'd:log_hh_income'), collapse = '+'),
            paste(c('d', 'd:b_majority_latino', 'd:log_hh_income', 'd:percent_ag'), collapse = '+'),
            paste(c('d', 'd:b_majority_latino', 'd:log_hh_income', 'd:percent_ag', 'd:avg_percent_clay'), collapse = '+'),
            paste(c('d', 'd:b_majority_latino', 'd:log_hh_income', 'd:percent_not_fluent_english'), collapse = '+'),
            paste(c('d', 'd:b_majority_latino', 'd:log_hh_income', 'd:log_pop'), collapse = '+'))
# 2. xvars with lags
xvar = list('d', paste(c('d', 'd:b_majority_latino', 'dlag1', 'dlag1:b_majority_latino'), collapse = '+'),
            paste(c('d', 'd:b_majority_latino', 'd:log_hh_income',
                    'dlag1', 'dlag1:b_majority_latino', 'dlag1:log_hh_income'), collapse = '+'))

# fe is cws x year

fe = paste(c('SYSTEM_NO', 'factor(year)'), collapse = '+')

reg_delivered(df = ni.optimistic, xvar = xvar, yvar = 'mean_n', fe=fe, clust=0, plot = FALSE)

# 2. xvars with linear trends

xvar = list('d', paste(c('d', 'd:b_majority_latino', 'SYSTEM_NO:year'), collapse = '+'), 
            paste(c('d', 'd:b_majority_latino', 'd:log_hh_income', 'SYSTEM_NO:year'), collapse = '+'),
            paste(c('d', 'd:b_majority_latino', 'd:log_hh_income', 'd:percent_ag', 'SYSTEM_NO:year'), collapse = '+'),
            paste(c('d', 'd:b_majority_latino', 'd:log_hh_income', 'd:percent_ag', 'd:avg_percent_clay', 'SYSTEM_NO:year'), collapse = '+'),
            paste(c('d', 'd:b_majority_latino', 'd:log_hh_income', 'd:percent_not_fluent_english', 'SYSTEM_NO:year'), collapse = '+'),
            paste(c('d', 'd:b_majority_latino', 'd:log_hh_income', 'd:log_pop', 'SYSTEM_NO:year'), collapse = '+'))

reg_delivered(df = ni_drought, xvar = xvar, yvar = 'mean_n', fe=0, clust=0, plot = FALSE,
              save.reg = '../Data/1int/mod.n.deliveredop.rds')

# Overall + CWS & Year FE

mod_ni1 <-
  felm(
    mean_n ~ d | SYSTEM_NO + factor(year) | 0 | 0,
    data = ni_drought
  )

summary(mod_ni1)

# SES + CWS & Year FE

mod_ni2 <-
  felm(mean_n ~ d + d:percent_ag + d:b_majority_latino + d:log_hh_income
       | SYSTEM_NO + factor(year) | 0 | 0, data = ni_drought)

summary(mod_ni2)


# Overall + linear PWS trends

mod_ni_l1 <-
  felm(
    mean_n ~ d + SYSTEM_NO:year | 0 | 0 | 0,
    data = ni_drought
  )

summary(mod_ni_l1)

# SES + linear trends
mod_ni_l2 <-
  felm(mean_n ~ d + d:percent_ag + d:b_majority_latino + d:log_hh_income + SYSTEM_NO:year 
       | 0 | 0 | 0, data = ni_drought)

summary(mod_ni_l2)

stargazer(mod_ni1, mod_ni2, mod_ni_l1, mod_ni_l2, omit = ':year|Constant', 
          title = 'Impacts of a unit increase in drought measure',
          dep.var.labels = 'Mean concentration of N (mg/l)', type = 'html',
          style = 'qje', digits = 3,
          add.lines = list(c("Fixed effects?", "CWS and year",  "CWS and year", "CWS linear trends", "CWS linear trends")),
          single.row = TRUE)

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
  
# SES + CWS & Year FE

mod_ni2 <-
  felm(mean_n ~ d + d:percent_ag + d:b_majority_latino + d:log_hh_income +
         dlag1 + dlag1:percent_ag + dlag1:b_majority_latino + dlag1:log_hh_income 
       | SYSTEM_NO + factor(year) | 0 | 0, data = ni_drought)

summary(mod_ni2)
# --- 


# # Baseline specifications for the effect of drought on nitrates -----------
# # fe is cws x year
# fe = paste(c('SYSTEM_NO', 'factor(year)'), collapse = '+')
# x <-  paste(c('d', 'd:b_majority_latino', 'd:log_hh_income'), collapse = '+')
# form = as.formula(paste0('mean_n', " ~ ", x,   " | ", fe))
# 
# # run optimal model on 3 different dataset with different scenarios
# mod <- map(ni, ~(
#   feols(fml = form, data = .)
# )) 
# newdata <- tibble(
#   SYSTEM_NO = '0110001',
#   b_majority_latino = 1,
#   hh_income = seq(20000, 200000, 20000),
#   log_hh_income = log(seq(20000, 200000, 20000)),
#   d = 0,
#   year = 2010
# ) %>% bind_rows(
#   tibble(
#     SYSTEM_NO = '0110001',
#     b_majority_latino = 1,
#     hh_income = seq(20000, 200000, 20000),
#     log_hh_income = log(seq(20000, 200000, 20000)),
#     d = 2,
#     year = 2010
#   )
# ) %>% bind_rows(newdata %>% mutate(b_majority_latino=0))
# 
# 
# equal.pred <- predict(mod[[2]], newdata = newdata) %>% bind_cols(newdata) %>% 
#   spread(key = d, value = `...1`) %>% 
#   mutate(drought_effect = `2`-`0`)
# ggplot(equal.pred, aes(b_majority_latino, hh_income, fill= drought_effect)) + 
#   geom_tile() +
#   scale_fill_distiller(palette = 'YlOrRd', direction = 1) +
#   theme_bw()