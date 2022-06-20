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

# Generate event study plots: Arsenic ------------------------------
############# ARSENIC ALL #####################
# r determines if we want raw or not, and adding in c(1,0) means both

ag <- plot_es(ar, w = 'G', pollutant = 'as_ugl' , ylm = c(0, 8) , years = 2005:2021,
               ylab = 'Mean As conc. (ug/l)\n', r = c(1,0))  
as <- plot_es(ar, w = 'S',  ylm = c(0, 8) , pollutant = 'as_ugl', years = 2005:2021,
               ylab = 'Mean As conc. (ug/l)\n', r = c(1,0)) 
# at <- plot_es(ar, w = c('G', 'S'), r = 0,  pollutant = 'as_ugl', ylm = c(-1, 8), years = 2005:2021,
#                ylab = 'Mean As conc. (ug/l)\n')

############# ARSENIC LOW INCOME #####################

agli <- plot_es2(ar, w = 'G', by = 'b_low_income', ylm = c(0, 8) , years = 2005:2021,
               ylab = 'Mean As conc. (ug/l)\n', r = c(1,0)) 
asli <- plot_es2(ar, w = 'S', by = 'b_low_income', ylm = c(0, 8) , years = 2005:2021,
               ylab = 'Mean As conc. (ug/l)\n', r = c(1,0)) 
# at <- plot_es2(ar, w = c('G', 'S'), r = 0, by = 'b_low_income', ylm = c(0, 8) , years = 2005:2021,
#                ylab = 'Mean As conc. (ug/l)\n')
  

############# ARSENIC MAJORITY LATINO #####################
agml <- plot_es2(ar, w = 'G', by = 'b_majority_latino', ylm = c(0, 8) , years = 2005:2021,
               ylab = 'Mean As conc. (ug/l)\n', r = c(1,0)) 
asml <- plot_es2(ar, w = 'S', by = 'b_majority_latino', ylm = c(0, 8) , years = 2005:2021,
               ylab = 'Mean As conc. (ug/l)\n', r = c(1,0)) 
# at <- plot_es2(ar, w = c('G', 'S'), r = 0, by = 'b_majority_latino', ylm = c(0, 8) , years = 2005:2021,
#                ylab = 'Mean As conc. (ug/l)\n')

a <-
  plot_grid(
    plotlist = list(
      ag %>% add_drought2(a=0.01),
      as %>% add_drought2(a=0.01),
      agli %>% add_drought2(a=0.005),
      asli%>% add_drought2(a=0.005),
      agml%>% add_drought2(a=0.005),
      asml%>% add_drought2(a=0.005)
    ),
    nrow = 3
  )

# Nitrates ----------------------------------------------------------------

############# NITRATE ALL #####################

ng <- plot_es(ni, w = c('G'), pollutant = 'n_mgl' , ylm = c(-.5, 5.5) , years = 1991:2021,
              ylab = 'Mean N conc. (mg/l)\n', r = c(1,0))
# ng <- ng + scale_y_continuous(breaks = 1:5)
ns <- plot_es(ni, w = 'S',  ylm = c(-.5, 5.5) , pollutant = 'n_mgl', years = 1991:2021,
              ylab = 'Mean N conc. (mg/l)\n', r = c(1,0))


############# NITRATE LOW INCOME #####################

ngli <- plot_es2(ni, w = 'G', pollutant = 'n_mgl', by = 'b_low_income', ylm = c(-.5, 5.5) , 
                 years = 1991:2021, r = c(1,0))
nsli <- plot_es2(ni, w = 'S', pollutant = 'n_mgl', by = 'b_low_income', ylm = c(-.5, 5.5) , 
                 years = 1991:2021, r = c(1,0))
# nt <- plot_es2(ni, w = c('G', 'S'), r = 0, pollutant = 'n_mgl', by = 'b_low_income', ylm = c(-.5, 5.5), 
#                years = 1991:2021)

############# NITRATE MAJORITY LATINO #####################
ngml <- plot_es2(ni, w = 'G', pollutant = 'n_mgl', by = 'b_majority_latino', ylm = c(-.5, 5.5),
                 years = 1991:2021, r = c(1,0))
nsml <- plot_es2(ni, w = 'S', pollutant = 'n_mgl', by = 'b_majority_latino', ylm = c(-.5, 5.5),
                 years = 1991:2021, r = c(1,0))
nt <- plot_es2(ni, w = c('G', 'S'), r = 0, pollutant = 'n_mgl', by = 'b_majority_latino', ylm = c(-.5, 5.5), 
               years = 1991:2021)

n <-
  plot_grid(
    plotlist = list(
      ng %>% add_drought(a=0.01),
      ns %>% add_drought(a=0.01),
      ngli %>% add_drought(a=0.005),
      nsli%>% add_drought(a=0.005),
      ngml%>% add_drought(a=0.005),
      nsml%>% add_drought(a=0.005)
    ),
    nrow = 3
  )
trends <- plot_grid(n, a, labels = c("Nitrate", "Arsenic"), rel_widths = c(3,2))
save_plot('Plots-DWQ/n_as_temp_trends.png', trends, base_asp = 1.4, scale = 2.5)

