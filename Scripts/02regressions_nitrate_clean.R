
# Nitrate regression source ------------------------------------------------------

# 2022/03/15
# sandysum@ucsb.edu

# Load packages -----------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(lfe)
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

ni_drought <- subset_years(2006, pollutant = ni , 2021, 1) %>% 
  prep_reg() %>% 
  mutate(b_majority_latino = factor(b_majority_latino),
         b_low_income = factor(b_low_income))

ni_split <- ni_drought %>% split(ni_drought$gw)

# need to find a way to visualize this 
sys <- ni_drought %>% distinct(b_majority_latino, raw, gw, SYSTEM_NO, samplePointID)
table(sys$raw, sys$gw, sys$b_majority_latino)

sys %>% 
  drop_na() %>% 
  group_by(b_majority_latino, gw, raw) %>% 
  summarize(n = n())

ni_drought %>% group_by(year) %>% summarise(mean_d = mean(d, na.rm = TRUE))

# create function

source_reg <- function(df, by = '+ d:b_majority_latino') {

mod_ni <-
  felm(as.formula(paste0('mean_n ~ d ', by, '| factor(year) | 0 | SYSTEM_NO')), 
       data = df, weights = df$n_spid)

# summary(mod_ni)

# 2 Year + SPID FEs

mod_ni_year_res <-
  felm(as.formula(paste0('mean_n ~ d ', by, '| samplePointID + factor(year) | 0 | SYSTEM_NO')), 
       data = df, weights = df$n_spid)

# summary(mod_ni_year_res)

# 3. Linear trends
mod_ni_linear_year <-
  felm(as.formula(paste0('mean_n ~ d + ', by, '+ SYSTEM_NO:year | samplePointID | 0 | SYSTEM_NO')), 
       data = df, weights = df$n_spid)

# summary(mod_ni_linear_year)
x <- list(mod_ni, mod_ni_year_res, mod_ni_linear_year) 
# stargazer::stargazer(mod_ni, mod_ni_year_res, 
#                      mod_ni_linear_year,
#                      omit = c(':year'), single.row = TRUE,
#                      add.lines = list(c("Fixed effects?", "Yr", "Yr, Site", 
#                                         "CWS linear yr, Site")),
#                      # column.labels = c("All California", "Majority Latino", "Low income"),
#                      dep.var.labels   = "Mean Nitrate conc. (mg/L)", 
#                      dep.var.caption = "Outcome:",
#                      omit.stat = c("adj.rsq", "ser"),
#                      type = 'html', style = 'aer')

}
# Main regression GWxML

gwml <- source_reg(ni_split[[1]], by = '+d:b_majority_latino')

# Main regressions GWxLI group --------------------------------------------

gwli <- source_reg(ni_split[[1]], by = "+d:b_low_income")

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

# stargazer::stargazer(mod_ni, mod_ni_ml, 
#                      mod_ni_li,
#                      omit = c(':year'), single.row = TRUE,
#                      add.lines = list(c("Fixed effects?", "Yr", "Yr", 
#                                         "Yr")),
#                      column.labels = c("All California", "Majority Latino", "Low income"),
#                      dep.var.labels   = "Mean Nitrate level (ug/L)", 
#                      dep.var.caption = "Outcome:", omit.stat = c("adj.rsq", "ser"),
#                      type = 'html', style = 'qje')
# 
# stargazer::stargazer(mod_ni_year_res, mod_ni_ml_year_res, 
#                      mod_ni_li_year_res,
#                      omit = c(':year'), single.row = TRUE,
#                      add.lines = list(c("Fixed effects?", "Site, Yr", "Site, Yr", "Site, Yr")),
#                      column.labels = c("All California", "Majority Latino", "Low income"),
#                      dep.var.labels   = "Mean Nitrate level (ug/L)", 
#                      dep.var.caption = "Outcome:", omit.stat = c("adj.rsq", "ser"),
#                      type = 'html', style = 'qje')
# 
# stargazer::stargazer(mod_ni_linear_year, mod_ni_ml_linear_year, 
#                      mod_ni_li_linear_year,
#                      omit = c(':year'), single.row = TRUE,
#                      add.lines = list(c("Fixed effects?", "Site, CWS linear yr", "Site, CWS linear yr", 
#                                         "Site, CWS linear yr")),
#                      column.labels = c("All California", "Majority Latino", "Low income"),
#                      dep.var.labels   = "Mean Nitrate level (ug/L)", 
#                      dep.var.caption = "Outcome:", omit.stat = c("adj.rsq", "ser"),
#                      type = 'html', style = 'qje')
# mod_ni_year_res <-
#   felm(as.formula(paste0('mean_n ~ d ', by, '| CITY + factor(year) | 0 | SYSTEM_NO')), 
#        data = df, weights = df$n_spid)
# 
# mod_ni_year_res <-
#   felm(as.formula(paste0('mean_n ~ d ', by, ' + log_hh_income + percent_ag | CITY + factor(year) | 0 | SYSTEM_NO')), 
#        data = df, weights = df$n_spid)
# 
# summary(mod_ni_year_res)
# summary(mod_ni_year_res)
