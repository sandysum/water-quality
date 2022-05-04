#################################
# Read and clean infant death data
# 
# 2022/05/01
# sandysum@ucsb.edu
#################################
#rm(list=ls())
library(tidycensus)
county_stats <- get_acs(
  geography = "county",
  # Median household income in the past 12 months (in 2019 inflation-adjusted dollars)
  # https://api.census.gov/data/2019/acs/acs5/groups/B19013.html
  # median income, total population, total hispanic or latino, total white
  # percent speak spanish, speak other language and english not well, less than HS graduates
  variables = c("B19013_001", "B01003_001", "B03001_003E", "B02001_002", "B06007_003", "B06007_008",
                "B16010_002"),
  # state = "CA",
  geometry = FALSE
)

# View(load_variables(2019, 'acs5'))

# I am not sure if the variable name is correct. Seems like total pop latino and total pop is giving me the same thing 2
county_stats <- county_stats %>% mutate(variable_nm = case_when(
  variable == "B19013_001" ~ "median_hh_income",
  variable == "B01003_001" ~ "total_pop",
  variable == "B03001_003" ~ "total_pop_latino",
  variable == "B02001_002" ~ "total_pop_white",
  variable == 'B06007_003' ~ "total_pop_speak_spanish",
  variable == 'B06007_008' ~ "total_pop_english_not_well",
  variable == 'B16010_002' ~ "total_pop_low_edu",
  TRUE ~ NA_character_
)) %>% dplyr::select(-moe, -variable)


# generating some social equality metrics
county_stats <-
  county_stats %>% spread(key = variable_nm, value = estimate) %>%
  mutate(
    percent_non_white = ((total_pop - total_pop_white) / total_pop) *100,
    percent_hispanic = (total_pop_latino / total_pop)*100,
    percent_low_edu = (total_pop_low_edu / total_pop)*100,
    percent_spanish_speaker = (total_pop_speak_spanish / total_pop)*100,
    percent_not_fluent_english = (total_pop_english_not_well / total_pop)*100
  )

fips <- fips_codes %>% mutate(GEOID = paste0(state_code, county_code))

# Read in infant death data for all counties in US

infpre0 <- read.table('/Users/sandysum/Downloads/infant_death_ca_1995-1998.txt', header=TRUE, fill = TRUE)

names(infpre0) <- c("county", 'county.code', 'year', 'year.code', 'deaths', 'births', 'death.rates', 'notes', 'notes2')

infpre0 <- infpre0 %>% filter(county != 'Total', !is.na(death.rates), !str_detect(county, 'Unidentified'), 
                              death.rates != "Missing", death.rates != "", death.rates != "Suppressed") %>% 
  select(-notes, -notes2) %>% 
  mutate_at(vars(year, year.code, death.rates, births, deaths), as.double)

infpre <- read.table('/Users/sandysum/Downloads/infant_death_ca_1999-2002.txt', header=TRUE, fill = TRUE)

names(infpre) <- c("county", 'county.code', 'year', 'year.code', 'deaths', 'births', 'death.rates', 'notes', 'notes2')

infpre <- infpre %>% filter(county != 'Total', !is.na(death.rates), !str_detect(county, 'Unidentified'), 
                            death.rates != "Missing", death.rates != "", death.rates != "Suppressed") %>% 
  select(-notes, -notes2) %>% 
  mutate_at(vars(year, year.code, death.rates, births, deaths), as.double)

infpost0 <- read.table('/Users/sandysum/Downloads/infant_death_ca_2003-2006.txt', header=TRUE, fill = TRUE)
names(infpost0) <- c("county", 'county.code', 'year', 'year.code', 'deaths', 'births', 'death.rates', 
                     'notes', 'notes2')

infpost0 <- infpost0 %>% filter(county != 'Total', !is.na(death.rates), !str_detect(county, 'Unidentified'), 
                                death.rates != "Missing", death.rates != "", death.rates != "Suppressed") %>% 
  select(-notes, -notes2) %>% 
  mutate_at(vars(year, year.code, death.rates, births, deaths), as.double)

infpost <- read.table('/Users/sandysum/Downloads/infant_death_ca_2007-2019.txt', header=TRUE, fill = TRUE)
names(infpost) <- c("county", 'county.code', 'year', 'year.code', 'deaths', 'births', 'death.rates', 
                    'notes', 'notes2')

infpost <- infpost %>% filter(county != 'Total', !is.na(death.rates), !str_detect(county, 'Unidentified'), 
                              death.rates != "Missing", death.rates != "", death.rates != "Suppressed") %>% 
  select(-notes, -notes2) %>% 
  mutate_at(vars(year, year.code, death.rates, births, deaths), as.double)

fips <- fips_codes %>% mutate(GEOID = paste0(state_code, county_code))

df <- bind_rows(infpre, infpre0, infpost, infpost0) %>% 
  mutate(county.nm = str_remove_all(county, ' County, CA') %>% str_to_lower()) 

df2 <- df %>%
  select(-county) %>% 
  left_join(fips, by = c('county.code' = 'GEOID')) %>%
  left_join(county_stats, by = c('county.code' = 'GEOID')) %>%
  mutate(
    GEOID = county.code, 
    b.income = if_else(median_hh_income < 54000, 1, 0),
    b.non.white = if_else(percent_non_white > 50, 1, 0)
  ) %>% select(-matches(".code"))

saveRDS(df2, '../Data/1int/usa_inf_deaths.rds')


# Read,  clean,  and save weather  ----------------------------------------

w <- fread("../../../../0UCSB_EES/3W2022/PSTAT_231_W22/W22_PSTAT231_Project/ML Project Data.csv")

w.year <- w %>% group_by(county_fips, year) %>% 
  summarise_at(vars(PM25_conc:wind_speed), mean, na.rm = TRUE) %>% 
  left_join(w %>% group_by(county_fips, year) %>% 
              select(tot_pop:hnac_female) %>% slice(1)) %>% 
  mutate(GEOID = paste0('0' , county_fips))

saveRDS(w.year, "../Data/1int/county_year_weather.rds")