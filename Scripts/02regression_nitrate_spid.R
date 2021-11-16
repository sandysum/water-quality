
# Nitrate regression ------------------------------------------------------

# 2021/11/4
# sandysum@ucsb.edu


# Load packages -----------------------------------------------------------

library(tidyverse)
library(lfe)
library(DescTools)
library(future.apply)
library(cowplot)
source("Scripts/helper_functions_models.R")

# Read in data ------------------------------------------------------------

pdsi <- readRDS("../Data/drought/pdsi_pws_year.rds") 

ni_reg <-read_rds(file.path(home, "1int/caswrb_ni_reg.rds"))

# 1. CLEAN DATA FOR: Regression at the monitor month year level ------------------------------

# 2. Filter to balanced panel for year 1996 to 2021

# this function subsets to only balanced panels that has the

ni_reg_balanced <- subset_years(2001, pollutant = ni_reg, 2020, 1)

ni_drought <- ni_reg_balanced %>% 
  ungroup() %>% 
  left_join(pdsi, c("year", "SYSTEM_NO")) %>% 
  group_by(samplePointID) %>% 
  mutate(
    d = mean_pdsi, 
    dlead = lead(d),
    dlead2 = lead(dlead),
    dlag1 = lag(d),
    dlag2 = lag(dlag1),
    dlag3 = lag(dlag2),
    dlag4 = lag(dlag3),
    dlag5 = lag(dlag4),
    dlag6 = lag(dlag5),
    gXr = gw*raw,
    rXcv = raw*cv) %>% 
  mutate(
    gw = factor(gw, levels = c("1", "0")),
    raw = factor(raw),
    SYSTEM_NO = factor(SYSTEM_NO)
  ) %>%
  group_by(SYSTEM_NO, year) %>%
  mutate(n_spid = 1 / (unique(samplePointID) %>% length())) %>%
  ungroup()

ni_drought %>% drop_na()
# A tibble: 17,232 × 29
# A tibble: 18,276 × 29
# Visualize annual trends within PWS --------------------------------------
set.seed(1297)
q <- sample(unique(ni_drought$SYSTEM_NO), 100)
quartz()
p <- ni_drought %>% 
  filter(SYSTEM_NO %in% q) %>% 
  ggplot(aes(x = year, y = mean_n, color = raw, group = samplePointID)) +
  geom_line() +
  geom_smooth(aes(group = SYSTEM_NO), method = 'lm') +
  theme_light() +
  scale_x_continuous(breaks = seq(2000, 2022, 2)) +
  # geom_hline(yintercept = 10, color = 'red') +
  theme(axis.text.x = element_text(angle = 45)) +
  facet_wrap(vars(SYSTEM_NO), scales = "free")

save_plot("Google Drive/My Drive/0Projects/1Water/2Quality/water-quality/Plots/pws_linear_n.png", p, base_asp = 1.2, scale = 5)

# NO LAGGED EFFECTS  -------------------------------------------------

# instantaneous effect is on the surface
# mean contemporaneous effect of precip on raw GW, mean contemporaneous effect of precip on raw S, mean contemporaneous effect of precip on treated water
mod_ni_1 <- 
  felm(mean_n ~ d + d:gw + d:raw | samplePointID + factor(year) | 0 | SYSTEM_NO, data = ni_drought, weights = ni_drought$n_spid)

summary(mod_ni_1)

mod_ni_2 <- 
  felm(mean_n ~ d + d:gw + d:raw + year | samplePointID | 0 | SYSTEM_NO, data = ni_drought, weights = ni_drought$n_spid)

summary(mod_ni_2)

mod_ni_3 <- 
  felm(mean_n ~ d + d:gw + d:raw + SYSTEM_NO:year | samplePointID | 0 | SYSTEM_NO, data = ni_drought, weights = ni_drought$n_spid)

summary(mod_ni_3)

mod_ni_4 <- 
  felm(mean_n ~ d + d:gw + d:raw + d:gXr + SYSTEM_NO:year | samplePointID | 0 | SYSTEM_NO, data = ni_drought, weights = ni_drought$n_spid)

summary(mod_ni_4)

stargazer::stargazer(mod_ni_1, mod_ni_2, mod_ni_3, mod_ni_4, omit = c(':year'), single.row = TRUE,
                     dep.var.labels   = "Mean Nitrate level (ug/L)", dep.var.caption = "Outcome:", omit.stat = c("adj.rsq", "ser"))


# ADDING LAGGED EFFECTS ---------------------------------------------------

mod_ni_lag1 <- 
  felm(mean_n ~ d
       + dlag1
       + dlag2
       + dlag3
       + d:gw
       + dlag1:gw
       + dlag2:gw
       + dlag3:gw
       + d:raw
       + dlag1:raw
       + dlag2:raw
       + dlag3:raw
       # + d:gXr
       # + dlag1:gXr
       # + dlag2:gXr
       # + dlag3:gXr
       | samplePointID + factor(year) | 0 | SYSTEM_NO, data = ni_drought, weights = ni_drought$n_spid)

summary(mod_ni_lag1)

mod_ni_lag2 <- 
  felm(mean_n ~ year + d + dlag1 + dlag2 + dlag3
       + d:gw
       + dlag1:gw
       + dlag2:gw
       + dlag3:gw
       + d:raw
       + dlag1:raw
       + dlag2:raw
       + dlag3:raw + year
       | samplePointID | 0 | SYSTEM_NO, data = ni_drought, weights = ni_drought$n_spid)

summary(mod_ni_lag2)

mod_ni_lag3 <- 
  felm(mean_n ~ d + dlag1 + dlag2 + dlag3 
       + d:gw
       + dlag1:gw
       + dlag2:gw
       + dlag3:gw
       + d:raw
       + dlag1:raw
       + dlag2:raw
       + dlag3:raw + year:SYSTEM_NO
       | samplePointID | 0 | SYSTEM_NO, data = ni_drought, weights = ni_drought$n_spid)

summary(mod_ni_lag3)

df <- sum_lags(mod_ni_lag3)
save_plot("Plots/cumulative_lagged_effects_ni.png", plot_coeff(df, contaminant = 'ni'), scale = 1, base_asp = 2)

stargazer::stargazer(mod_ni_lag1, mod_ni_lag2, mod_ni_lag3, omit = c('year'), single.row = TRUE,
                     dep.var.labels   = "Mean Nitrate level (ug/L)", dep.var.caption = "Outcome:", omit.stat = c("adj.rsq", "ser"))

# GW X RAW X HIGHCLAY -----------------------------------------------------

# in central valley

mod_ni_lag_cv1 <- 
  felm(mean_n ~ d
       + dlag1
       + dlag2
       + dlag3
       + d:gw
       + dlag1:gw
       + dlag2:gw
       + dlag3:gw
       + d:raw
       + dlag1:raw
       + dlag2:raw
       + dlag3:raw + SYSTEM_NO:year
       | samplePointID | 0 | SYSTEM_NO, data = ni_drought %>% filter(cv==1), weights = ni_drought[ni_drought$cv==1,]$n_spid)

summary(mod_ni_lag_cv1)

# non-central valley 

mod_ni_lag_cv0 <- 
  felm(mean_n ~ d
       + dlag1
       + dlag2
       + dlag3
       + d:gw
       + dlag1:gw
       + dlag2:gw
       + dlag3:gw
       + d:raw
       + dlag1:raw
       + dlag2:raw
       + dlag3:raw + SYSTEM_NO:year
       | samplePointID | 0 | SYSTEM_NO, data = ni_drought %>% filter(cv==0), weights = ni_drought[ni_drought$cv==0,]$n_spid)


summary(mod_ni_lag_cv0)

stargazer::stargazer(mod_ni_lag_cv1, mod_ni_lag_cv0, omit = c('year'), single.row = TRUE,
                     dep.var.labels   = "Mean Nitrate level (ug/L)", dep.var.caption = "Outcome:", omit.stat = c("adj.rsq", "ser"),
                     column.labels = c('In Central Valley', 'Outside of Central Valley'))


# 2. RUN stacked regressions ---------------------------------------------------------

mod_ni_stacked <- 
  felm(mean_n ~ d + d:groundwater + d:raw | samplePointID | 0 | SYSTEM_NO, data = ni_drought)

summary(mod_ni_stacked)

#stargazer::stargazer(mod_ni_stacked)

mod_ni_stacked2 <- 
  felm(mean_n ~ d + dlag1 + dlag2 + dlag3 + dlag4 + dlag5 +
         d:groundwater + d:raw
       + dlag1:groundwater
       + dlag2:groundwater
       + dlag3:groundwater
       + dlag4:groundwater
       + dlag5:groundwater
       + dlag1:raw
       + dlag2:raw
       + dlag3:raw 
       + dlag4:raw 
       + dlag5:raw| samplePointID + SYSTEM_NO*year | 0 | SYSTEM_NO, data = ni_drought)

summary(mod_ni_stacked2)

stargazer::stargazer(mod_ni_stacked, mod_ni_stacked2)

# 3. RUN piecewise regression ---------------------------

ni_gw_raw <- ni_drought %>% filter(groundwater ==1, raw == 1, year > 1995)  

mod_gw <- felm(mean_n ~ d + dlag1 + dlag2 + dlag3 + dlag4 + dlag5 | samplePointID | 0 | SYSTEM_NO, data = ni_gw_raw, weights = ni_gw_raw$n_spid)

ni_s_raw <- ni_drought %>% filter(groundwater == 0, raw == 1, year > 1995)  

mod_s <- felm(mean_n ~ d + dlag1 + dlag2 + dlag3 + dlag4 + dlag5 | samplePointID | 0 | SYSTEM_NO, data = ni_s_raw, weights = ni_s_raw$n_spid)

ni_tr <- ni_drought %>% filter(raw == 0, year > 1995)  

mod_tr <- felm(mean_n ~ d + dlag1 + dlag2 + dlag3 + dlag4 + dlag5 | samplePointID | 0 | SYSTEM_NO, data = ni_tr, weights = ni_tr$n_spid)

plot_reg(mod_tr, contaminant = "n", 
         main = "Nitrate (mg/L) response to +1 in PDSI, \nRegression at the sample point level", nleads = 0, nlags = 5, ylm = c(-.06, .08))

# Plot and save -----------------------------------------------------------

plist <- pmap(list(list(mod_gw, mod_s, mod_tr), c("Raw groundwater", "Raw surface water", "Treated water"), list(c(-.06, .02), c(-.06, .02), c(-.06, .15))), plot_reg, contaminant = "n", nleads = 0, nlags = 5)

save_plot("Plots/n_pdsi_coefs_spid.png", plot_grid(plotlist = plist, ncol = 1), base_asp = .5, scale = 4)

# Regression at the PWS year level --------------------------------------
######################################################################################

# Regression PDSI on raw groundwater ----------------------------------------

ni_py <- ni %>%
  # filter only to groundwater and raw sources
  # star from year 1984, when there are more data points..
  filter(WATER_TYPE == "G", raw == 1, year > 1985) %>%
  mutate(month = month(sampleDate),
         cv = if_else(countyName %in% cv_counties, 1, 0),
         gold = if_else(countyName %in% gold, 1, 0)) %>%
  group_by(
    SYSTEM_NO,
    year,
    countyName,
    gold,
    cv,
    DISTRICT,
    CITY,
    POP_SERV,
    ZIP,
    ZIP_EXT,
    CONNECTION,
    AREA_SERVE
  ) %>%
  # Winsorize as per Shapiro (2021 paper)
  dplyr::summarise(
    median_n = median(n_mgl, na.rm = TRUE),
    mean_n = mean(n_mgl, na.rm = TRUE)
  ) %>%
  # keep PWS with at least 10 observations
  group_by(SYSTEM_NO) %>%
  filter(n()>5) %>%
  ungroup() %>%
  mutate(mean_n = Winsorize(mean_n, probs = c(0, .99))) %>% 
  filter(cv==1)

# prepping data for interpolation
comb <- expand_grid(unique(ni_py$SYSTEM_NO), 1986:2021) 
names(comb) <- c('SYSTEM_NO', 'year')

ni_py <- left_join(comb, ni_py) %>% 
  group_by(SYSTEM_NO) %>% 
  fill_(c("countyName", "cv", "gold", "DISTRICT" , "CITY", "POP_SERV", "ZIP", "ZIP_EXT", "CONNECTION" ,"AREA_SERVE"), "downup")

# now we interpolate the data

ni_py_int <- ni_py %>% 
  arrange(SYSTEM_NO, year) %>% 
  group_by(SYSTEM_NO) %>%
  mutate(true = if_else(!is.na(mean_n), "true value", "interpolated"),
         mean_n = na.spline(mean_n, 
                            maxgap = 2,
                            na.rm = FALSE),
         n_obs = sum(is.na(mean_n)))

# 137532-118138 = 19394 is the number interpolated....

ni_drought <- ni_py_int %>% 
  ungroup() %>% 
  left_join(pdsi %>% mutate(SYSTEM_NO = str_extract(SABL_PWSID, "\\d+")), c("year", "SYSTEM_NO")) %>% 
  group_by(SYSTEM_NO) %>% 
  mutate(
    d = mean_pdsi, 
    dlead = lead(d),
    dlead2 = lead(dlead),
    dlag1 = lag(d),
    dlag2 = lag(dlag1),
    dlag3 = lag(dlag2),
    dlag4 = lag(dlag3),
    dlag5 = lag(dlag4),
    dlag6 = lag(dlag5))

# drop data / pws 

# run regressions

# can I believe this model???

mod_gw_noCV <-
  felm(mean_n ~ dlead + d + dlag1 + dlag2 + dlag3 + dlag4 + dlag5 + dlag6 | SYSTEM_NO + year | 0 | SYSTEM_NO,
       data = ni_drought)
mod_gw_CV <-
  felm(mean_n ~ dlead + d + dlag1 + dlag2 + dlag3 + dlag4 + dlag5 + dlag6 | SYSTEM_NO + year | 0 | SYSTEM_NO,
       data = ni_drought)

cv <- plot_reg(mod_gw_CV, contaminant = "n", 
               main = "Groundwater response to unit increase in PDSI, \nCentral Valley CA only", nleads = 1, nlags = 6)
noCV <- plot_reg(mod_gw_noCV, contaminant = "n", 
                 main = "Groundwater response to unit increase in PDSI, \nall other CA", nleads = 1, nlags = 6)

save_plot(filename = "Plots/n_pdsi_coefs_CV.png", plot_grid(cv, noCV, nrow = 1), base_asp = 2, nrow = 1, scale = 2.5)

# mean(ar[(ar$raw==1&ar$WATER_TYPE=="G"),]$ar_ugl, na.rm = TRUE)

.1/8.9

# Regression PDSI on treated water ----------------------------------------

ni_py <- ni %>%
  # filter only to groundwater and raw sources
  # star from year 1984, when there are more data points..
  filter(raw == 0, year > 1985) %>%
  mutate(month = month(sampleDate),
         cv = if_else(countyName %in% cv_counties, 1, 0),
         gold = if_else(countyName %in% gold, 1, 0)) %>%
  group_by(
    SYSTEM_NO,
    year,
    countyName,
    gold,
    cv,
    DISTRICT,
    CITY,
    POP_SERV,
    ZIP,
    ZIP_EXT,
    CONNECTION,
    AREA_SERVE
  ) %>%
  # Winsorize as per Shapiro (2021 paper)
  dplyr::summarise(
    median_n = median(n_mgl, na.rm = TRUE),
    mean_n = mean(n_mgl, na.rm = TRUE)
  ) %>%
  # keep PWS with at least 10 observations
  group_by(SYSTEM_NO) %>%
  filter(n()>5) %>%
  ungroup() %>%
  mutate(mean_ar = Winsorize(mean_n, probs = c(0, .99)))

# 38 years
2021-1984 +1

# prepping data for interpolation
comb <- expand_grid(unique(ni_py$SYSTEM_NO), 1986:2021) 
names(comb) <- c('SYSTEM_NO', 'year')

ar_py <- left_join(comb, ni_py) %>% 
  group_by(SYSTEM_NO) %>% 
  fill_(c("countyName", "cv", "gold", "DISTRICT" , "CITY", "POP_SERV", "ZIP", "ZIP_EXT", "CONNECTION" ,"AREA_SERVE"), "downup")

# now we interpolate the data

ni_py_int <- ni_py %>% 
  arrange(SYSTEM_NO, year) %>% 
  group_by(SYSTEM_NO) %>%
  mutate(true = if_else(!is.na(mean_ar), "true value", "interpolated"),
         mean_n = na.spline(mean_n, 
                            maxgap = 2,
                            na.rm = FALSE),
         n_obs = sum(is.na(mean_n)))

# what if we drop those with less than 10 observations: 42615

ni_drought <- ni_py_int %>% 
  ungroup() %>% 
  left_join(pdsi %>% mutate(SYSTEM_NO = str_extract(SABL_PWSID, "\\d+")), c("year", "SYSTEM_NO")) %>% 
  group_by(SYSTEM_NO) %>% 
  mutate(
    d = mean_pdsi, 
    dlead = lead(d),
    dlead2 = lead(dlead),
    dlag1 = lag(d),
    dlag2 = lag(dlag1),
    dlag3 = lag(dlag2),
    dlag4 = lag(dlag3),
    dlag5 = lag(dlag4),
    dlag6 = lag(dlag5))

# if the previous model is to be believed, then there should be no relationship here

mod_tr <-
  felm(mean_n ~ dlead + d + dlag1 + dlag2 + dlag3 + dlag4 + dlag5 + dlag6 | SYSTEM_NO + year | 0 | SYSTEM_NO,
       data = ni_drought)

summary(mod_tr)

# Regression PDSI on surface water ----------------------------------------

ni_py <- ni %>%
  # filter only to groundwater and raw sources
  # star from year 1984, when there are more data points..
  filter(raw == 1, WATER_TYPE == "S", year > 1985) %>%
  mutate(month = month(sampleDate),
         cv = if_else(countyName %in% cv_counties, 1, 0),
         gold = if_else(countyName %in% gold, 1, 0)) %>%
  group_by(
    SYSTEM_NO,
    year,
    countyName,
    gold,
    cv,
    DISTRICT,
    CITY,
    POP_SERV,
    ZIP,
    ZIP_EXT,
    CONNECTION,
    AREA_SERVE
  ) %>%
  # Winsorize as per Shapiro (2021 paper)
  dplyr::summarise(
    median_n = median(n_mgl, na.rm = TRUE),
    mean_n = mean(n_mgl, na.rm = TRUE)
  ) %>%
  # keep PWS with at least 10 observations
  group_by(SYSTEM_NO) %>%
  filter(n()>5) %>%
  ungroup() %>%
  mutate(mean_ar = Winsorize(mean_n, probs = c(0, .99)))

# 38 years
2021-1984 +1

# prepping data for interpolation
comb <- expand_grid(unique(ni_py$SYSTEM_NO), 1986:2021) 
names(comb) <- c('SYSTEM_NO', 'year')

ar_py <- left_join(comb, ni_py) %>% 
  group_by(SYSTEM_NO) %>% 
  fill_(c("countyName", "cv", "gold", "DISTRICT" , "CITY", "POP_SERV", "ZIP", "ZIP_EXT", "CONNECTION" ,"AREA_SERVE"), "downup")

# now we interpolate the data

ni_py_int <- ni_py %>% 
  arrange(SYSTEM_NO, year) %>% 
  group_by(SYSTEM_NO) %>%
  mutate(true = if_else(!is.na(mean_n), "true value", "interpolated"),
         mean_ar = na.spline(mean_n, 
                             maxgap = 2,
                             na.rm = FALSE),
         n_obs = sum(is.na(mean_n)))

# what if we drop those with less than 10 observations: 42615

ni_drought <- ni_py_int %>% 
  ungroup() %>% 
  left_join(pdsi %>% mutate(SYSTEM_NO = str_extract(SABL_PWSID, "\\d+")), c("year", "SYSTEM_NO")) %>% 
  group_by(SYSTEM_NO) %>% 
  mutate(
    d = mean_pdsi, 
    dlead = lead(d),
    dlead2 = lead(dlead),
    dlag1 = lag(d),
    dlag2 = lag(dlag1),
    dlag3 = lag(dlag2),
    dlag4 = lag(dlag3),
    dlag5 = lag(dlag4),
    dlag6 = lag(dlag5))

# there is not a lot of arsenic in groundwater

mod_s <-
  felm(mean_n ~ dlead + d + dlag1 + dlag2 + dlag3 + dlag4 + dlag5 + dlag6 | SYSTEM_NO + year | 0 | SYSTEM_NO,
       data = ni_drought)

summary(mod_s)

# Plot and save -----------------------------------------------------------


plist <- map2(list(mod_gw, mod_s, mod_tr), c("Raw groundwater", "Raw surface water", "Treated water"), plot_reg, contaminant = "n", nleads = 1, nlags = 6)

save_plot("Plots/n_pdsi_coefs.png", plot_grid(plotlist = plist, ncol = 1), base_asp = .5, scale = 3.9)

# yay I think it works because I am having year FE and system SE and clustering at the city level

# ni_my <- ni %>% 
#   filter(!is.na(countyName), WATER_TYPE == "G") %>% 
#   mutate(month = month(sampleDate)) %>% 
#   group_by(countyName, year, month, raw) %>%
#   dplyr::summarise(median_ni = median(n_mgl, na.rm = TRUE),
#             mean_ni = mean(n_mgl, na.rm = TRUE)) 
# 
# ni_my_drought <- ni_my %>% 
#   mutate(countyName = str_to_lower(countyName)) %>% 
#   left_join(climdiv_cw, by = c('countyName' = 'NAME')) %>% 
#   mutate(climdiv = as.integer(climdiv_assigned),
#          in_cv = factor(countyName%in%cv_counties)) %>% 
#   left_join(pdsi, by = c("year", "month", "climdiv")) %>% 
#   mutate(pdsi2 = (pdsi^2)*sign(pdsi))
# 
# # visualize
# 
# ni_my_drought %>% 
#   filter(countyName == "tulare", raw == 1) %>% 
#   ggplot(aes(date, median_ni)) +
#   geom_line() +
#   geom_line(aes(date, pdsi), color = 'blue') +
#   theme_minimal_hgrid() +
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 0.9))
# 
# mod <-
#   felm(
#     mean_ni ~ pdsi |
#       month + countyName |
#       0 | countyName + year,
#     data = ni_my_drought %>% filter(raw == 1)
#   )
# 
# summary(mod)  

