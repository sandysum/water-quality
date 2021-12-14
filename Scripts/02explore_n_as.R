
# Investigate iron, nitrates, and arsenic ---------------------------------

# this is an auxilliary investigation that looks at whether increased presence of nitrates in groundwater is causing this thing to happen

# Scientists determined that the addition of nitrate to anoxic groundwater enhanced microbially 
# mediated processes in the aquifer that ultimately resulted in arsenic immobilization. More specifically, 
# the injected nitrate oxidized reduced iron in the absence of oxygen, producing iron oxyhydroxides, 
# which then immobilized the contaminant compounds, such as arsenic (V), arsenic (III), and phosphate through sorption.
# https://www.usgs.gov/ecosystems/environmental-health-program/science/nitrate-addition-enhances-arsenic-immobilization?qt-science_center_objects=0#qt-science_center_objects

# This study is the first field demonstration of the effectiveness and rapid rate 
# at which nitrate reduction can stimulate iron oxidation and subsequent arsenic removal. 

# Maybe that's why nitrate concentrations in GW only increased N by a small amount

rm(list = ls())

library(tidyverse)
library(lfe)
library(DescTools)
library(future.apply)
library(cowplot)
source("Scripts/helper_functions_models.R")

# Read in data ------------------------------------------------------------
pdsi <- readRDS("../Data/drought/pdsi_pws_year.rds") 
n_reg <- read_rds("../Data/1int/caswrb_n_reg.rds")
ar_reg <- read_rds("../Data/1int/caswrb_ar_reg.rds")
# leave out mean_fe for now
all <- n_reg %>% left_join(ar_reg %>% dplyr::select(samplePointID, mean_ar, median_ar, year)) %>% 
  # left_join(fe_reg_balanced %>% dplyr::select(samplePointID, mean_fe, median_fe, year)) %>% 
  group_by(samplePointID) %>% 
  # Drop anything that has NA so only retaining samplepointIDs with yearly sample results for as, n, and fe from 2010-2020
  drop_na(mean_n, mean_ar) %>% 
  # look at only in GW and raw!!
  filter(gw==1, raw==1) 

# Plot some random sample
# hard to tell variation because n and as in different scales and hard to compare...
# try 1) convert n to mg?
# try 2) compare with x and y axis being each of the elements?
set.seed(54)

p <- ggplot(data = all %>% 
         filter(samplePointID %in% sample(unique(all$samplePointID), 25, replace = FALSE)) %>% 
                  gather("element", "conc", mean_n, mean_ar), 
       aes(x=year, y = conc, color = element)) + 
  geom_point() +
  geom_line() +
  theme_minimal() +
  facet_wrap(vars(samplePointID), scales = 'free')

save_plot("Plots/n_as5.png", p, base_asp = 1.3, scale = 1.6)
  
