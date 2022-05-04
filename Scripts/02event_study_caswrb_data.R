# Heading -----------------------------------------------------------------
# A script to generate event study plots for both Arsenic and Nitrate.
# sandysum@ucsb.edu
# 2022/01/06
rm(list = ls())
library(tidyverse)
library(readr)
library(readxl)
library(readr)
library(lubridate)
library(ggplot2)
library(DescTools)
library(cowplot)
library(lfe)
source("/Users/sandysum/Google Drive/My Drive/0Projects/1Water/2Quality/water-quality/Scripts/helper_functions_models.R")
home <- "/Users/sandysum/Google Drive/My Drive/0Projects/1Water/2Quality/Data/"
source("/Users/sandysum/Google Drive/My Drive/0Projects/1Water/2Quality/water-quality/Scripts/helper_functions_es.R")
# Read data in ------------------------------------------------------------

# read arsenic and nitrate data from the CA SWRB portal 
ind <- readRDS(file.path(home, "1int/pws_ind.rds"))
ar <- read_rds(file.path(home, "1int/caswrb_ar_1974-2022.rds")) %>% 
  left_join(ind)

ni <-read_rds(file.path(home, "1int/caswrb_n_1974-2022.rds")) %>% 
  left_join(ind)

ag <- plot_es(ar, w = 'G', by = 'b_low_income', ylm = c(-1, 9), years = 2005:2021,
               ylab = 'Mean As conc. (ug/l)\n') 

# Generate event study plots: Arsenic ------------------------------
############# ARSENIC ALL #####################

ag <- plot_es(ar, w = 'G', pollutant = 'as_ugl' , ylm = c(2, 6), years = 2005:2021,
               ylab = 'Mean As conc. (ug/l)\n', r = c(1,0))  
as <- plot_es(ar, w = 'S',  ylm = c(-1, 8), pollutant = 'as_ugl', years = 2005:2021,
               ylab = 'Mean As conc. (ug/l)\n', r = c(1,0)) 
# at <- plot_es(ar, w = c('G', 'S'), r = 0,  pollutant = 'as_ugl', ylm = c(-1, 8), years = 2005:2021,
#                ylab = 'Mean As conc. (ug/l)\n')

a <- plot_grid(plotlist = list(ag, as), nrow = 1, label_size = 18,
               labels = c('Groundwater', 'Surface water'))

save_plot('Plots.spring2022/as_es.png', a, base_asp = 2, scale = 1.3)
save_plot('Plots.spring2022/a_gw.png', ag, base_asp = 1, scale = 1.3)
############# ARSENIC LOW INCOME #####################

ag <- plot_es2(ar, w = 'G', by = 'b_low_income', ylm = c(-1, 9), years = 2005:2021,
               ylab = 'Mean As conc. (ug/l)\n', r = c(1,0)) 
as <- plot_es2(ar, w = 'S', by = 'b_low_income', ylm = c(-1, 9), years = 2005:2021,
               ylab = 'Mean As conc. (ug/l)\n', r = c(1,0)) 
# at <- plot_es2(ar, w = c('G', 'S'), r = 0, by = 'b_low_income', ylm = c(-1, 9), years = 2005:2021,
#                ylab = 'Mean As conc. (ug/l)\n')

ali <- plot_grid(plotlist = list(ag, as), nrow = 1)

save_plot('Plots.spring2022/as_es_li.png', ali, base_asp = 2, scale = 1.3)

ali.drought <- plot_grid(plotlist = list(ag, as, at) %>% map(add_drought), nrow = 1)

save_plot('Plots.spring2022/as_es_li_drought.png', ali.drought, base_asp = 2.4, scale = 2)

############# ARSENIC MAJORITY LATINO #####################
ag <- plot_es2(ar, w = 'G', by = 'b_majority_latino', ylm = c(2, 6), years = 2005:2021,
               ylab = 'Mean As conc. (ug/l)\n', r = c(1,0)) 
as <- plot_es2(ar, w = 'S', by = 'b_majority_latino', ylm = c(-1, 9), years = 2005:2021,
               ylab = 'Mean As conc. (ug/l)\n', r = c(1,0)) 
# at <- plot_es2(ar, w = c('G', 'S'), r = 0, by = 'b_majority_latino', ylm = c(-1, 9), years = 2005:2021,
#                ylab = 'Mean As conc. (ug/l)\n')

aml <- plot_grid(plotlist = list(ag, as), nrow = 1)
save_plot('Plots.spring2022/a_gw_ml.png', ag, base_asp = 1, scale = 1.3)
aml.drought <- plot_grid(plotlist = list(ag, as, at) %>% map(add_drought), nrow = 1)

save_plot('Plots.spring2022/as_es_ml_drought.png', aml.drought, base_asp = 2, scale = 2)

save_plot('Plots.spring2022/as_es_ml.png', aml, base_asp = 2, scale = 1.3)

# Nitrates ----------------------------------------------------------------

############# NITRATE ALL #####################

ng <- plot_es(ni, w = c('G'), pollutant = 'n_mgl' , ylm = c(1, 5), years = 1991:2021,
              ylab = 'Mean N conc. (mg/l)\n', r = c(1,0)) 
ng <- ng + scale_y_continuous(breaks = 1:5)
ns <- plot_es(ni, w = 'S',  ylm = c(0, 5.5), pollutant = 'n_mgl', years = 1991:2021,
              ylab = 'Mean N conc. (mg/l)\n', r = c(1,0))
# nt <- plot_es(ni, w = c('G', 'S'), r = 0,  pollutant = 'n_mgl', ylm = c(0, 5.5), years = 1991:2021,
#               ylab = 'Mean N conc. (mg/l)\n')
save_plot('Plots.spring2022/n_gw.png', ng, base_asp = 1, scale = 1.3)
na <- plot_grid(plotlist = list(ng, ns), nrow = 1, label_size = 18,
               labels = c('Groundwater', 'Surface water'))

save_plot('Plots.spring2022/n_es.png', na, base_asp = 2, scale = 1.3)
############# NITRATE LOW INCOME #####################

ng <- plot_es2(ni, w = 'G', pollutant = 'n_mgl', by = 'b_low_income', ylm = c(-.5, 5.5), years = 1991:2021, r = c(1,0)) 
ns <- plot_es2(ni, w = 'S', pollutant = 'n_mgl', by = 'b_low_income', ylm = c(-.5, 5.5), years = 1991:2021, r = c(1,0)) 
# nt <- plot_es2(ni, w = c('G', 'S'), r = 0, pollutant = 'n_mgl', by = 'b_low_income', ylm = c(-.5, 5.5), 
#                years = 1991:2021)

nli <- plot_grid(plotlist = list(ng, ns), nrow = 1, label_size = 18,
                 labels = c('Groundwater', 'Surface water'))

save_plot('Plots.spring2022/n_es_li.png', nli, base_asp = 2, scale = 1.3)

nli.drought <- plot_grid(plotlist = list(ag, as, at) %>% map(add_drought), nrow = 1)

save_plot('Plots.spring2022/as_es_li_drought.png', ali.drought, base_asp = 2.4, scale = 2)

############# NITRATE MAJORITY LATINO #####################
ng <- plot_es2(ni, w = 'G', pollutant = 'n_mgl', by = 'b_majority_latino', ylm = c(1, 5), years = 1991:2021, r = c(1,0)) 
ns <- plot_es2(ni, w = 'S', pollutant = 'n_mgl', by = 'b_majority_latino', ylm = c(.2, 5), years = 1991:2021, r = c(1,0)) 
nt <- plot_es2(ni, w = c('G', 'S'), r = 0, pollutant = 'n_mgl', by = 'b_majority_latino', ylm = c(-.5, 5.5), 
               years = 1991:2021)

nml <- plot_grid(plotlist = list(ng, ns), nrow = 1, label_size = 18,
                 labels = c('Groundwater', 'Surface water'))
save_plot('Plots.spring2022/n_gw_ml.png', ng, base_asp = 1, scale = 1.3)
save_plot('Plots.spring2022/n_es_ml.png', nml, base_asp = 2, scale = 1.3)

