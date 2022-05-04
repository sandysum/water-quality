
# Exploring Consumer Expenditure Data ----------------------------


library(tidyverse)
library(pdsi)
library(lubridate)
library(sf)
library(tigris)
library(rgeos)
library(cowplot)
library(tidycensus)
# Read in datasets --------------------------------------------------------

home <- "/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/Data/"
# this was old and only have at the climdiv level

# pdsi <- readRDS("../Data/drought/pdsi_ca_1970_2021.rds")
ce <- read_csv("/Users/sandysum/Downloads/intrvw20/fmli202.csv") 
ind <- read_rds("../Data/1int/pws_ej_ind.rds")
ses <- read_sf("../Data/shp_ej.shp")
names(ce)
# Down census data at the county level ------------------------------------

# Download and clean census tract data for socio-economic 

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
ca_stats <- ca_stats %>% mutate(variable_nm = case_when(
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
ca_stats <-
  ca_stats %>% spread(key = variable_nm, value = estimate) %>%
  mutate(
    percent_non_white = ((total_pop - total_pop_white) / total_pop) *100,
    percent_hispanic = (total_pop_latino / total_pop)*100,
    percent_low_edu = (total_pop_low_edu / total_pop)*100,
    percent_spanish_speaker = (total_pop_speak_spanish / total_pop)*100,
    percent_not_fluent_english = (total_pop_english_not_well / total_pop)*100
  ) %>% st_transform(3488)

write_sf(ca_stats, "../Data/shp_ej_counties.shp")

fips <- fips_codes %>% mutate(GEOID = paste0(state_code, county_code))

sales_ses <- ca_stats %>% mutate(fips = as.numeric(str_remove(GEOID, "^6"))) %>% 
  left_join(sales %>% group_by(fips, year) %>% summarise(mean_ln_water_sales = mean(ln_water_sales, na.rm = TRUE))) %>% 
  group_by(fips, year)

sales_ses %>% ggplot(aes(year, mean_ln_water_sales, group = fips)) +
  geom_line()
