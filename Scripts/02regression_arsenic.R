
# Arsenic regression ------------------------------------------------------

# 2021/11/4
# sandysum@ucsb.edu


# Soil exploration --------------------------------------------------------

ggplot(soil, aes(ph, clay)) +
  geom_point(size = 1, alpha = .7) +
  theme_minimal_hgrid() +
  geom_hline(aes(yintercept = 33), color = 'red')

# Load packages -----------------------------------------------------------

library(tidyverse)
library(lfe)
library(DescTools)
library(future.apply)
library(cowplot)
source("Scripts/helper_functions_models.R")

# Read in data ------------------------------------------------------------
pdsi <- readRDS("../Data/drought/pdsi_pws_year.rds") 

ar_reg <-read_rds(file.path(home, "1int/caswrb_ar_reg.rds"))

pdsi <- readRDS("../Data/drought/pdsi_pws_year.rds") 
ind <- readRDS(file.path(home, "1int/pws_ind.rds"))
loc <- read_xlsx(file.path(home, "ca_water_qual/siteloc.xlsx")) %>% 
  mutate(STATUS = str_to_upper(STATUS))
as <- read_rds(file.path(home, "1int/caswrb_ar_reg.rds")) %>% left_join(ind) %>% 
  left_join(pdsi) %>% 
  left_join(loc, by =c('samplePointID' = 'PRI_STA_C', "SYSTEM_NO"))

as_drought <- subset_years(2008, pollutant = as, 2021, 1) %>% 
  prep_reg() %>% 
  mutate(b_majority_latino = factor(b_majority_latino),
         b_low_income = factor(b_low_income)) %>% 
  filter(STATUS %in% c('AT', 'AR', 'AU', 'CM', 'CR', 'CT', 'DT', 'DR', 'SR',
                       'SU', 'ST', 'CU'))

as_split <- as_drought %>% 
  split(as_drought$gw)

source_reg(as_split[[1]], pollutant = 'as')
source_reg(as_split[[2]], pollutant = 'as')

# Visualize annual trends within PWS --------------------------------------
set.seed(8997)
q <- sample(unique(ar_drought$SYSTEM_NO), 100)
quartz()
p <- ar_drought %>% 
  filter(SYSTEM_NO %in% q) %>% 
  ggplot(aes(x = year, y = mean_ar, group = samplePointID, color = raw)) +
  geom_line() +
  theme_light() +
  geom_smooth(aes(group = SYSTEM_NO), method = 'lm') +
  scale_x_continuous(breaks = seq(2010, 2022, 2)) +
  # geom_hline(yintercept = 10, color = 'red') +
  theme(axis.text.x = element_text(angle = 45)) +
  facet_wrap(vars(SYSTEM_NO), scales = "free")

save_plot("Google Drive/My Drive/0Projects/1Water/2Quality/water-quality/Plots/pws_linear_ar.png", p, base_asp = 1.2, scale = 5)

# For plotting results ----------------------------------------------------

gw_main <- feols(as.formula(paste0('mean_as ~ d ', ' + d:b_majority_latino + d:b_low_income', '| samplePointID + SYSTEM_NO[year]')), 
                 data = as_split[[1]], weights = as_split[[1]]$n_spid, vcov = ~SYSTEM_NO)

# sum_marginal(gw_main, nlags=0, int_terms = c('b_majority_latino', 'b_low_income'), pollutant = 'n')

sw_main <- feols(as.formula(paste0('mean_as ~ d ', ' + d:b_majority_latino + d:b_low_income', '| samplePointID + SYSTEM_NO[year]')), 
                 data = as_split[[2]], weights = as_split[[2]]$n_spid, vcov = ~SYSTEM_NO)


ls <- map(list(gw_main, sw_main), sum_marginal, 
          nlags=0, int_terms = c('b_majority_latino', 'b_low_income'), pollutant = 'as') %>% 
  bind_rows(.id = 'model') %>%
  rename(estimate = mean_as, 
         std.error = se, 
         p.value = pval, 
         statistic = t_val) %>% 
  mutate(term = c(rep('Groundwater', 3), rep('Surface water', 3)),
         model = int_terms,
         estimate = estimate*3, 
         std.error = std.error*3)

outas <- dwplot(ls,
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
  scale_x_continuous(n.breaks = 6)+
  theme(axis.text.y = element_text(size = 14)) +
  xlab('\nArsenic drought reponse (ug/l)')

reg <- plot_grid(outn, outas, nrow = 1, rel_widths = c(2.9, 4))
save_plot("../water-quality/Plots-DWQ/final_reg.png", reg, base_asp = 2.5, scale = 1)
ni_drought$mean_n %>% mean()

ni_split[[1]]$mean_n %>% mean()
ni_split[[2]]$mean_n %>% mean()
0.25/3.4
0.25/1.9

