#################################
# Read and clean infant death data
# 
# 5/8/2021
# sandysum@ucsb.edu
#################################
#rm(list=ls())
home <- "/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/Data"

# load package

library(tidyverse)
library(readr)
library(data.table)
# Read in weather

# Read in all facilities data from USE
as.counties <- read_csv("../Data/1int/usgs_counties_as.csv")
w.year <- readRDS("../Data/1int/county_year_weather.rds")
fac <- read_csv("../Data/SDWA-DL/SDWA_FACILITIES.csv")
geog <- read_csv("../Data/SDWA-DL/SDWA_GEOGRAPHIC_AREAS.csv") %>% 
  distinct()
df2 <- readRDS('../Data/1int/usa_inf_deaths.rds')
# This contains all US PWS areas/countys served!!!
# Doing everything at county level now
pws <- read_csv("../Data/SDWA-DL/SDWA_PUB_WATER_SYSTEMS.csv") %>% 
  # select(-FISCAL_YEAR) %>% 
  distinct() %>% # filter out inactive PWS
  filter(PWS_ACTIVITY_CODE != "I") %>% 
  left_join(geog %>% select(-SUBMISSIONYEARQUARTER, -LAST_REPORTED_DATE))

# remove the transient one / so only serve people who drink it regularly 
pws.hh <- pws %>% filter(PWS_TYPE_CODE != 'TNCWS')

pws.hh$POPULATION_SERVED_COUNT %>% sum(na.rm = TRUE)

pws.hh %>% ggplot(aes(x = STATE_CODE, fill = PRIMARY_SOURCE_CODE)) +
  geom_bar() +
  theme_minimal_hgrid() +
  labs(x = '\nState', y = 'Count of PWSs') +
  theme(axis.text.x = element_text(size =8)) +
  scale_fill_brewer(palette = 'Dark2')

# Read in the violations data

violations <- read_csv("../Data/SDWA-data/SDWA_VIOLATIONS.csv")

# Clean violations data for arsenic ----------------

as_vio_new <- violations %>%
  filter(RULE_NAME == "Arsenic" & HEALTH_BASED == "Y") %>%
  # mutate(SYSTEM_NO = str_extract(PWSID, "(?<=[A-Z]{2})\\d+")) %>% 
  dplyr::select(PWSID, year = BEGIN_YEAR, VIOLATION_ID, VIOLATION_NAME, RULE_NAME) %>% 
  group_by(PWSID, year) %>% 
  # summarise(n_new_violations = unique(VIOLATION_ID) %>% length())
  summarise(as_violations = n()) %>% left_join(pws.hh)

# Explore violations data -------------------------------------------------

as_vio_new %>%
  filter(!is.na(STATE_CODE)) %>% 
  group_by(year, STATE_CODE) %>% 
  summarise(sum.as = sum(as_violations, na.rm = TRUE)) %>% 
  arrange(desc(sum.as)) %>% 
  filter(year == 2010)

as_vio_new %>%
  filter(!is.na(STATE_CODE)) %>% 
  ggplot(aes(year, as_violations, group = STATE_CODE)) +
  stat_summary(fun.y = sum,
               geom = "line", color = 'grey70', alpha = .6) +
  # stat_summary(fun.y = sum,
  #              geom = "point") +
  scale_color_brewer(palette = 'Dark2') +
  labs(x='Year', y='Number of violations, As', color = 'Source water') +
  geom_vline(xintercept = c(2001, 2006), color = 'red') +
  theme_minimal_hgrid()

county_vio <- as_vio_new %>% 
  group_by(PrincipalCountyServed) %>% 
  summarise(as.violations = sum(as_violations)) %>% 
  mutate(county.nm = str_to_lower(PrincipalCountyServed))

county_vio %>% ggplot(aes(x=as.violations)) +
  geom_density() +
  theme_cowplot()

county_stats <- county_stats %>% right_join(w.year)

df2 %>%
  filter(!is.na(state)) %>% 
  group_by(year, state) %>% 
  summarise(sum.dr = mean(death.rates, na.rm = TRUE)) %>% 
  arrange(desc(sum.dr)) %>% 
  filter(year == 2005) %>% print(n=20)

df2 %>%
  filter(GEOID %in% ) %>%
  ggplot(aes(year, death.rates)) +
  stat_summary(aes(group = state), fun = mean,
               geom = "line", color = 'grey70', alpha = .6) +
  stat_summary(fun = mean,
               geom = "line") +
  # scale_color_brewer(palette = 'Dark2') +
  labs(x='Year', y='Infant mortality rate') +
  geom_vline(xintercept = c(2001, 2006), color = 'red') +
  theme_minimal_hgrid()

w.year %>% ggplot(aes(PM25_conc, death.rates)) +
  # stat_summary(fun.y = mean,
  #              geom = "line") +
  stat_summary(fun.y = mean,
               geom = "point") +
  # geom_vline(xintercept = c(2001, 2006), color = 'red') +
  theme_bw()

county_stats %>% 
  left_join(fips) %>% 
  ggplot(aes(year, PM25_conc)) +
  stat_summary(aes(group = state), fun = mean,
               geom = "line", color = 'grey70', alpha = .6) +
  stat_summary(fun = mean,
               geom = "line") +
  geom_vline(xintercept = c(2001, 2006), color = 'red') +
  theme_bw()
# join df2 to air_inf

df2 <- df2 %>% left_join(county_stats)

air_inf <- feols(death.rates ~ PM25_conc + prcp + O3_conc + tmax | GEOID, df2)

df.reg <- df2 %>% drop_na(death.rates, PM25_conc, GEOID)
df.reg$death.rates.res <- air_inf$residuals

df.reg %>% ggplot(aes(year, death.rates.res)) +
  stat_summary(aes(group = state), fun = mean,
               geom = "line", alpha = .6, color = 'grey70') +
  stat_summary(fun = mean,
               geom = "line") +
  stat_summary(fun = mean,
               geom = "point") +
  geom_vline(xintercept = c(2001, 2006), color = 'red') +
  theme_bw()

# read in As level

df3 <- df2 %>% 
  filter(year>2001) %>% 
  left_join(as.counties %>% mutate(
  as_bins = if_else(usgs_as >= 9, 'high', 'low')
) %>% select(matches('as'), GEOID)) %>% 
  mutate(treat = if_else(as_bins == 'high', 2010, 0),
         post = if_else(year > 2010, 1, 0))

femod1 <- feols(death.rates ~ post:as_bins + PM25_conc + tmax + O3_conc | state + county_fips + year, df3)
summary(femod1)

femod2 <- feols(death.rates ~ post:as_bins + PM25_conc + tmax + O3_conc | state + county_fips[year], df3)
summary(femod2)

femod3 <- feols(death.rates ~ post:as_bins + PM25_conc + tmax + O3_conc | state[year] + county_fips, df3)
summary(femod3)

fixest::coefplot(femod2)
fixest::etable(femod1, femod2, femod3, tex = TRUE)
  
  
df3 <- df2 %>% 
  filter(year>2001) %>% 
  left_join(as.counties %>% mutate(
    as_bins = if_else(usgs_as >= 9, 'high', 'low')
  ) %>% select(matches('as'), GEOID)) %>% 
  mutate(treat = if_else(as_bins == 'high', 2010, 0),
         post = if_else(year > 2010, 1, 0))

example_attgt <- att_gt(yname = "death.rates",
                        tname = "year",
                        idname = "county_fips",
                        gname = "treat",
                        xformla = ~PM25_conc + county_fips + tmax,
                        data = df3
)

ggdid(example_attgt)

df.reg <- df3 %>% drop_na(death.rates, PM25_conc, county_fips, as_bins, tmax, O3_conc, year, state)

## ---------------

county_stats <- get_acs(
  geography = "county",
  # Median household income in the past 12 months (in 2019 inflation-adjusted dollars)
  # https://api.census.gov/data/2019/acs/acs5/groups/B19013.html
  # median income, total population, total hispanic or latino, total white
  # percent speak spanish, speak other language and english not well, less than HS graduates
  variables = c("B19013_001", "B01003_001", "B03001_003E", "B02001_002", "B06007_003", "B06007_008",
                "B16010_002"),
  # state = "CA", 
  geometry = TRUE
) %>% st_simplify()

cs <- county_stats %>% filter(GEOID %in% df.reg$GEOID)

plot(st_geometry(cs))
