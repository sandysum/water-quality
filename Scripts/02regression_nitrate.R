
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
library(Hmisc)
library(cowplot)
source("G:/My Drive/0Projects/1Water/2Quality/water-quality/Scripts/helper_functions_es.R")
source("G:/My Drive/0Projects/1Water/2Quality/water-quality/Scripts/helper_functions_models.R")
source("/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/water-quality/Scripts/helper_functions_models.R")
source("/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/water-quality/Scripts/helper_functions_es.R")
options(digits=3)
# Read in data ------------------------------------------------------------
home <- "G:/My Drive/0Projects/1Water/2Quality/Data/"
home <- "/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/Data/"
pdsi <- readRDS(file.path(home, "../Data/drought/pdsi_pws_year.rds"))
  
ind <- readRDS(file.path(home, "1int/pws_ind.rds"))
# wells is cumulative number of wells and depth sum and cumulative sum of wells
wells <- readRDS(file.path(home,"1int/pws_wells_panel.rds")) %>% 
  mutate(SYSTEM_NO = str_extract(SYSTEM_NO, '\\d+'), 
         wells = wells + 1) 

ni <-read_rds(file.path(home, "1int/caswrb_n_reg.rds")) %>% left_join(ind) %>% 
  left_join(pdsi) %>% 
  left_join(wells)
  
facilities <- read_csv(file.path(home, "SDWA-DL/SDWA_FACILITIES.csv")) %>% 
  filter(PWSID %in% ni$SABL_PWSID, FACILITY_ACTIVITY_CODE == 'A') %>% 
  # to match facilities dataset from SWDA, we have to paste the numerical value of PWSID to the state facility id 
  # and this correspond to the samplePointID in the CA SWRB
  mutate(samplePointID = paste0(str_extract(PWSID, '\\d+'), '-', STATE_FACILITY_ID),
         type = if_else(str_detect(WATER_TYPE_CODE, 'G'), 'GW', "SW")) %>% 
  select(samplePointID, FACILITY_TYPE_CODE, type)

ni_drought <- subset_years(2007, pollutant = ni , 2021, 1) %>% 
  prep_reg() %>% 
  mutate(b_majority_latino = factor(b_majority_latino),
         b_low_income = factor(b_low_income)) %>% 
  filter(STATUS %in% c('AT', 'AR', 'AU', 'CM', 'CR', 'CT', 'DT', 'DR', 'SR',
                       'SU', 'ST', 'CU')) %>% 
  left_join(facilities) %>% 
  mutate(ag_wells_depth_total = ag_wells_depth_total/100,
         depth_sum = scale(depth_sum, center = TRUE))

ni_split <- ni_drought %>% 
  split(ni_drought$type) 

# Final table before leaving for alaska: 2022-07-15 ------------------

df <- ni_split[[1]] %>% filter(FACILITY_TYPE_CODE == 'WL')
pollutant = 'n'
m1 <-
  feols(
    fml = as.formula(paste0('mean_', pollutant, " ~ d | factor(year)")),
    data = df,
    weights = df$n_spid,
    vcov = ~ SYSTEM_NO
  )

summary(m1)

m2 <-
  feols(
    fml = as.formula(
      paste0('mean_', pollutant, " ~ d + d:b_majority_latino | factor(year)")
    ),
    data = df,
    weights = df$n_spid,
    vcov = ~ SYSTEM_NO
  )

summary(m2)

# controlling for unobserved trends

m3 <-
  feols(
    fml = as.formula(
      paste0(
        'mean_',
        pollutant,
        " ~ d + d:b_majority_latino + wells + log_hh_income + avg_percent_ph + log_pop_caswrb + b_ag_area + avg_percent_clay | RegulatingAgency + factor(year)"
      )
    ),
    data = df,
    weights = df$n_spid,
    vcov = ~ SYSTEM_NO
  )

summary(m3)

m32 <-
  feols(
    fml = as.formula(
      paste0(
        'mean_',
        pollutant,
        " ~ d + d:b_majority_latino + d:wells + d:log_hh_income + d:avg_percent_ph + d:log_pop_caswrb + d:b_ag_area | RegulatingAgency + factor(year)"
      )
    ),
    data = df,
    weights = df$n_spid,
    vcov = ~ SYSTEM_NO
  )

summary(m32)

m31 <-
  feols(
    fml = as.formula(
      paste0(
        'mean_',
        pollutant,
        " ~ d + d:b_majority_latino + d:wells + d:log_hh_income + d:avg_percent_ph + d:log_pop_caswrb + d:b_ag_area +
        wells + log_hh_income + avg_percent_ph + log_pop_caswrb + b_ag_area | RegulatingAgency + factor(year)"
      )
    ),
    data = df,
    weights = df$n_spid,
    vcov = ~ SYSTEM_NO
  )

summary(m31)

m4 <-
  feols(
    fml = as.formula(
      paste0(
        'mean_',
        pollutant,
        " ~ d + d:b_majority_latino + d:percent_ag + d:I(percent_ag^2)+ d:log_pop_caswrb + d:b_low_income + wells | SYSTEM_NO[year] + samplePointID"
      )
    ),
    data = df,
    weights = df$n_spid,
    vcov = ~ SYSTEM_NO
  )

summary(m4)

etable(m1, m2, m3, m32, m31, m4, tex = TRUE,
       digits = 3, order = c('d$', 'd:'), drop = 'Intercept')

m2 <-
  feols(
    fml = as.formula(
      paste0('mean_', pollutant, " ~ d + d:b_majority_latino | RegulatingAgency + factor(year)")
    ),
    data = df,
    weights = df$n_spid,
    vcov = ~ SYSTEM_NO
  )

summary(m2)

# controlling for all other variables that could be related to
m3 <-
  feols(
    fml = as.formula(
      paste0(
        'mean_',
        pollutant,
        " ~ d + d:b_majority_latino + wells + log_hh_income + avg_percent_ph + log_pop_caswrb + b_ag_area + avg_percent_clay | factor(year)"
      )
    ),
    data = df,
    weights = df$n_spid,
    vcov = ~ SYSTEM_NO
  )

summary(m3)

m32 <-
  feols(
    fml = as.formula(
      paste0(
        'mean_',
        pollutant,
        " ~ d + d:b_majority_latino + d:wells + d:log_hh_income + d:avg_percent_ph + d:log_pop_caswrb + d:percent_ag | factor(year)"
      )
    ),
    data = df,
    weights = df$n_spid,
    vcov = ~ SYSTEM_NO
  )

summary(m32)

m31 <-
  feols(
    fml = as.formula(
      paste0(
        'mean_',
        pollutant,
        " ~ d + d:b_majority_latino + d:wells + d:log_hh_income + d:avg_percent_ph + d:log_pop_caswrb + d:percent_ag +
        wells + log_hh_income + avg_percent_ph + log_pop_caswrb + percent_ag | factor(year)"
      )
    ),
    data = df,
    weights = df$n_spid,
    vcov = ~ SYSTEM_NO
  )

summary(m31)
m5 <-
  feols(
    fml = as.formula(
      paste0(
        'mean_',
        pollutant,
        " ~ d + d:b_majority_latino + d:b_ag_area + d:avg_percent_ph + d:avg_percent_clay + d:wells + d:log_pop_caswrb + d:b_low_income | SYSTEM_NO[year] + samplePointID"
      )
    ),
    data = df,
    weights = df$n_spid,
    vcov = ~ SYSTEM_NO
  )

summary(m5)

m6 <- feols(fml = as.formula(paste0('mean_', pollutant, " ~ d + d:b_majority_latino + d:b_low_income | samplePointID + SYSTEM_NO[year]")), 
            data = df, weights = df$n_spid, vcov = ~SYSTEM_NO)
summary(m6)

etable(m1, m2, m3, m41, m4, m5, tex = TRUE,
       digits = 3, order = c('d$', 'd:'), drop = 'Intercept')

## Trying to explain WHY majority latino gets hit by drought so bad.


ni_drought %>% group_by(year) %>% summarise(mean_d = mean(d, na.rm = TRUE))

###################### 2022 SPRING

#### Final regressions 2022 spring:
  
# N GW

source_reg(ni_split[[1]] %>% filter(raw==1), pollutant = 'n')
# N SW

source_reg(ni_split[[2]], pollutant = 'n')

# For plotting results ----------------------------------------------------

# ni_split[[1]] <- ni_split[[1]] %>% filter(FACILITY_TYPE_CODE=='WL')

gw_main <-
  feols(
    as.formula(
      paste0(
        'mean_n ~ d ',
        '+ d:b_majority_latino + d:b_low_income + d:wells + d:percent_ag',
        '| samplePointID + SYSTEM_NO[year]'
      )
    ),
    data = ni_split[[1]],
    weights = ni_split[[1]]$n_spid,
    vcov = ~ SYSTEM_NO
  )

summary(gw_main)

sum_marginal(gw_main, nlags=0, int_terms = c('b_majority_latino', 'b_low_income'), pollutant = 'n')

sw_main <- feols(as.formula(paste0('mean_n ~ d ', ' + d:b_majority_latino+ d:b_low_income + d:percent_ag', '| samplePointID + SYSTEM_NO[year]')), 
                 data = ni_split[[2]], weights = ni_split[[2]]$n_spid, vcov = ~SYSTEM_NO)

sum_marginal(sw_main, nlags=0, int_terms = c('b_majority_latino', 'b_low_income'), pollutant = 'n')
# 
# ni.tr <- ni_drought %>% filter(raw == 0)
# tw_main <- feols(as.formula(paste0('mean_n ~ d ', ' + d:b_majority_latino + d:b_low_income', '| samplePointID + SYSTEM_NO[year]')), 
#                  data = ni.tr, weights = ni.tr$n_spid, vcov = ~SYSTEM_NO)
# 
# sum_marginal(tw_main, nlags=0, int_terms = c('b_majority_latino', 'b_low_income'), pollutant = 'n')

ls <- map(list(gw_main, sw_main), sum_marginal, 
          nlags=0, int_terms = c('b_majority_latino', 'b_low_income'), pollutant = 'n') %>% 
  bind_rows(.id = 'model') %>%
  rename(estimate = mean_n, 
         std.error = se, 
         p.value = pval, 
         statistic = t_val) %>% 
  mutate(term = c(rep('Groundwater', 3), rep('Surface water', 3)),
         model = int_terms,
         estimate = estimate*3, 
         std.error = std.error*3)

outn <- dwplot(ls,
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
  scale_x_continuous(n.breaks = 8) +
  theme(axis.text.y = element_text(size = 14),
        legend.position = 'none') +
  xlab("\nNitrate drought response (mg/l)")

save_plot("Plots.spring2022/final_reg_n.png", out, base_asp = 1.4, scale = 1)

# Run all other regression specifications
source_reg(ni_split[[1]], pollutant = 'n')
source_reg(ni_split[[2]], pollutant = 'n')

# Simulations -------------------------------------------------------------


# Baseline specifications for the effect of drought on nitrates -----------
# fe is cws x year

# fe = paste(c('samplePointID', 'factor(year)'), collapse = '+')
fe = 'samplePointID + SYSTEM_NO[year]'
x <-  paste(c('d', 'd:percent_hispanic', 'd:log_hh_income'), collapse = '+')
form = as.formula(paste0('mean_n', " ~ ", x,   " | ", fe))

# run optimal model on 3 different dataset with different scenarios
mod <- feols(fml = form, data = ni_split[[1]], weights = ni_split[[1]]$n_spid)

pdsi %>% group_by(year) %>% summarise(meand = mean(d, na.rm=TRUE)) %>% print(n=50)

mean(pdsi$d, na.rm=TRUE)

newdata.nodrought <- map(2011:2016, function(y) {ni_split[[1]] %>% 
  ungroup() %>% 
  select(SYSTEM_NO, samplePointID, percent_hispanic, log_hh_income) %>% 
  distinct() %>% 
  mutate(year=y,
         d=-1.2 )}) %>% bind_rows() %>% 
  mutate(d = if_else(year==2016, 0.2, d))

newdata.drought <- map(2011:2016, function(y) {
  x <- pdsi %>% group_by(year) %>% summarise(meand = mean(d, na.rm=TRUE)) %>%
    filter(year==y) %>% pull(meand)
  # ceiling(x)
  ni_split[[1]] %>% 
    ungroup() %>% 
    select(SYSTEM_NO, samplePointID, percent_hispanic, log_hh_income) %>% 
    distinct() %>% 
    mutate(year=y,
           d=x+1 )}) %>% bind_rows()

pred0 <- predict(mod, newdata = newdata.nodrought) %>% bind_cols(newdata.nodrought) 
pred1 <- predict(mod, newdata = newdata.drought) %>% bind_cols(newdata.drought) 

# pred1 <- predict(mod, newdata = newdata.drought) %>% bind_cols(newdata.drought) 

pred <- bind_rows('No drought counterfactual' = pred0, "Actual drought" = pred1, .id = 'Scenario') %>% 
  rename(predicted.n = `...1`) %>%
  # select(-year) %>% 
  # spread(key = d, value = predicted.n) %>%
  # rename(Predrought = `-1.5`,
  #        Drought = `1.5`) %>% 
  mutate(
    # drought_effect = Drought - Predrought,
         over10 = as.numeric(predicted.n > 10), 
         over5 = predicted.n > 5) %>% 
  select(-percent_hispanic, -log_hh_income) %>% 
  left_join(ind)

affected <-pred %>% filter(Scenario == 'Actual drought', over10==1) %>% 
  select(names(ind)) %>% distinct()

affected %>% group_by(b_majority_latino) %>% 
  summarise(sum(POP_SERV))

wells_failed <- pred %>% drop_na(b_majority_latino) %>% 
  # filter(over10) %>% 
  group_by(b_majority_latino, Scenario) %>% 
  summarise(count = sum(over10)) 

wells_failed <- pred %>% 
  drop_na(b_majority_latino) %>%
  group_by(year, b_majority_latino, Scenario) %>% 
  summarise(count = sum(over10)) %>% 
  # filter(over10) %>% 
  ggplot(aes(x = year, y = count, color = factor(Scenario))) +
  geom_line(size = 1.2, lineend = 'round', alpha = .7) +
  theme_minimal_hgrid() +
  facet_wrap(vars(b_majority_latino)) +
  lims(y = c(70, 90))+
  labs(y = 'Number of wells > MCL of 10 mg/l \n', x = '\n Year') +
  scale_colour_manual(" ", values=c("firebrick","blue3"),
                      breaks=c("Actual drought", "No drought counterfactual"), 
                      labels=c("Actual drought", "No drought counterfactual"))+
  theme(axis.text.x = element_text(angle = 45, size = 10, hjust = .9, vjust = .9),
        plot.background = element_rect(fill = "white", color = NA)) 

save_plot("Plots.spring2022/simulations.png", wells_failed, base_asp = 2, scale = 1)
  
ggplot(pred, aes(percent_hispanic, median_hh_income)) +
  geom_point(aes(color= drought_effect), shape = 15, size = 4) +
  geom_point(data = pred %>% filter(over10_pre), aes(percent_hispanic, median_hh_income), shape = 17, size = 2) +
  scale_color_distiller(palette = 'YlOrRd', direction = 1, name = 'Severe drought effect') +
  scale_shape_manual(values = c("Well over regulatory threshold of 10mg/l"=17)) +
  theme_minimal() +
  lims(y = c(0, 100))
  labs(x = '% Latino PWS Area Served', y = 'Median Household Income ($)\n')
  

pred_long <- pred %>% gather("When", "meanN", Predrought, Drought)

ggplot(pred_long, aes(meanN, color = When)) +
  geom_density() +
  theme_minimal() +
  facet_wrap(facets = vars(b_majority_latino))


# Do the same for SW ------------------------------------------------------

# run optimal model on 3 different dataset with different scenarios
mod <- feols(fml = form, data = ni_split[[2]], weights = ni_split[[2]]$n_spid)

pdsi %>% group_by(year) %>% summarise(meand = mean(d, na.rm=TRUE)) %>% print(n=50)

mean(pdsi$d, na.rm=TRUE)

newdata.nodrought <- map(2011:2016, function(y) {ni_split[[2]] %>% 
    ungroup() %>% 
    select(SYSTEM_NO, samplePointID, percent_hispanic, log_hh_income) %>% 
    distinct() %>% 
    mutate(year=y,
           d=-1.2 )}) %>% bind_rows() %>% 
  mutate(d = if_else(year==2016, 0.2, d))

newdata.drought <- map(2011:2016, function(y) {
  x <- pdsi %>% group_by(year) %>% summarise(meand = mean(d, na.rm=TRUE)) %>% print(n=50) %>% 
    filter(year==y) %>% pull(meand)
  # ceiling(x)
  ni_split[[2]] %>% 
    ungroup() %>% 
    select(SYSTEM_NO, samplePointID, percent_hispanic, log_hh_income) %>% 
    distinct() %>% 
    mutate(year=y,
           d=x )}) %>% bind_rows()

pred0s <- predict(mod, newdata = newdata.nodrought) %>% bind_cols(newdata.nodrought) 
pred1s <- predict(mod, newdata = newdata.drought) %>% bind_cols(newdata.drought) 

preds <- bind_rows('No drought counterfactual' = pred0s, "Actual drought" = pred1s, .id = 'Scenario') %>% 
  rename(predicted.n = `...1`) %>%
  # select(-year) %>% 
  # spread(key = d, value = predicted.n) %>%
  # rename(Predrought = `-1.5`,
  #        Drought = `1.5`) %>% 
  mutate(
    # drought_effect = Drought - Predrought,
    over10 = as.numeric(predicted.n > 10), 
    over5 = predicted.n > 5) %>% 
  select(-percent_hispanic, -log_hh_income) %>% 
  left_join(ind)

preds %>% filter(b_majority_latino==1, year==2011)

wells_failed <- preds %>% group_by(year, b_majority_latino, Scenario) %>% 
  summarise(count = sum(over10)) %>% 
 drop_na() %>% 
  ggplot(aes(x = year, y = count, color = factor(Scenario))) +
  geom_line() +
  theme_minimal_hgrid() +
  facet_wrap(vars(b_majority_latino)) +
  # lims(y = c(70, 90))+
  labs(y = 'Number of wells > MCL of 10 mg/l \n', x = '\n Year') +
  scale_colour_manual(" ", values=c("blue3", "firebrick"),
                      breaks=c("Actual drought", "No drought counterfactual"), 
                      labels=c("Actual drought", "No drought counterfactual"))+
  theme(axis.text.x = element_text(angle = 45, size = 10, hjust = .9, vjust = .9),
        plot.background = element_rect(fill = "white", color = NA)) 

save_plot("Plots.spring2022/simulations.png", wells_failed, base_asp = 2, scale = 1.2)