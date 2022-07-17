#################################
# Read and clean NWIS wells monitoring data
# 
# 7/12/2021
# sandysum@ucsb.edu
#################################

rm(list = ls())
home <- "/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/Data"
setwd(home)

# load package

library(sp)
library(tidyverse)
library(readxl)
library(readr)
library(lubridate)
library(ggplot2)
library(maps)
library(tigris)
library(DescTools)
# read in data files
# the guide and explanation to the format and nomenclature are here: https://www.waterqualitydata.us/portal_userguide/
nwis_n <- read_csv(file.path(home, "/nwis/nwis_ca_N_2005-2021.csv")) 
nwis_a <- read_csv(file.path(home, "/nwis/nwis_ca_Ar_1995-2021.csv"))
station <- read_csv(file.path(home, "/nwis/nwis_ca_stations_2005-2021.csv"))
ca <- counties("California", cb = TRUE)

########################################################
# Clean station data
########################################################

names(station)

station2 <- station %>% select(locationId = MonitoringLocationIdentifier, locationNm = MonitoringLocationName, locationType = MonitoringLocationTypeName, huc = HUCEightDigitCode, areaVal = `DrainageAreaMeasure/MeasureValue`, areaUnit = `DrainageAreaMeasure/MeasureUnitCode`, lat = LatitudeMeasure, lon = LongitudeMeasure, crs = HorizontalCoordinateReferenceSystemDatumName, StateCode, CountyCode, AquiferName, AquiferTypeName, wellDepth = `WellDepthMeasure/MeasureValue`, wellDepthUnit = `WellDepthMeasure/MeasureUnitCode`, constructionDate = ConstructionDateText) %>% 
  mutate(wellDepth_m = wellDepth*0.3048,
         constructionDateYr = str_extract(constructionDate,"\\d{4}") %>% as.integer()) %>% 
  select(-wellDepth, -wellDepthUnit)

station2$locationType %>% table()

########################################################
# Clean NWIS monitoring data for Arsenic
########################################################

glimpse(nwis_a)

# how many unique values does each column has
nwis_a %>% purrr::map(~(length(unique(.)))) 

nwis_a$ResultDetectionConditionText %>% table()
nwis_a$`ResultMeasure/MeasureUnitCode` %>% table()
nwis_a$MethodDescriptionText %>% unique() # not useful
nwis_a$SubjectTaxonomicName %>% unique() # they all look and sound v cool but unfortunately this is white noise to me
nwis_a$ResultIdentifier %>% unique() %>% length()
nwis_a$ActivityIdentifier %>% unique() %>% length()

# let's keep only wells data

nwis_a <- nwis_a %>% select(
    orgId = OrganizationIdentifier,
    activityId = ActivityIdentifier,
    resultId = ResultIdentifier,
    date = ActivityStartDate,
    time = `ActivityStartTime/Time`,
    timeZone = `ActivityStartTime/TimeZoneCode`,
    locationId = MonitoringLocationIdentifier,
    detectedOrNot = ResultDetectionConditionText,
    value = ResultMeasureValue,
    unit = `ResultMeasure/MeasureUnitCode`,
    actualOrEstimated = ResultValueTypeName,
    methodName = `ResultAnalyticalMethod/MethodName`
  ) %>% 
  left_join(station2, by = 'locationId') %>% 
  filter(locationType == "Well"|locationType == "Well: Test hole not completed as a well ") 

nwis_a$unit %>% table()
nwis_a$detectedOrNot %>% table()

nwis_a2 <- nwis_a %>%
  mutate(
    value_ugL = case_when(
      is.na(detectedOrNot) ~ value,
      detectedOrNot == "Not Detected" ~ 0,
      detectedOrNot == "Detected Not Quantified" ~ NA_real_,
      TRUE ~ NA_real_
    )
  ) %>% 
select(orgId, activityId, date, locationId, value, value_ugL, unit, detectedOrNot, everything()) 

# Basic cleaning is done and we can check if there are repeats

many <- nwis_a2 %>% 
  # but there are truly not repeats because they have different result ids....
  group_by(locationId, date) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  filter(n>1)

wells_a <- nwis_a2 %>%
  mutate(year = year(date)) %>% 
  group_by(locationId) %>% 
  mutate(freq = n())

wells_a %>% 
  group_by(AquiferName, year) %>%
  filter(!is.na(AquiferName)) %>% 
  summarise(prop_over_10 = sum((value >= 10))/n(),
            med_Ar = median(value_ugL, na.rm = TRUE)) %>% 
  mutate(source = "nwis") %>% 
  ggplot(aes(year, med_Ar, color = AquiferName)) +
  geom_point() + 
  geom_line() +
  geom_vline(xintercept = c(2012, 2016), color = 'red') +
  facet_wrap(vars(AquiferName)) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2004, 2020, 2)) +
  scale_color_brewer(palette = "Set2")

station_med <- wells_a %>% 
  group_by(locationId, lat, lon) %>% 
  summarise(well_depth = mean(wellDepth_m, na.rm = TRUE), 
            median_Ar_ugl = median(value, na.rm = TRUE), 
            mean_Ar_ugl = mean(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate_at(vars(contains('ugl')), ~Winsorize(., maxval = 50, na.rm = TRUE))

ggplot() + 
  geom_sf(data = ca, color="black",
          fill="white", size=0.25) +
  geom_point(
    data = station_med,
    aes(x = lon, y = lat, color = median_Ar_ugl),
    size = 2,
    alpha = .6,
    shape = 4
  ) +
  scico::scale_color_scico(palette = "lajolla") + theme_minimal()

########################################################
# Clean NWIS monitoring data for Nitrate
########################################################

# 8/10/2021 Also drop non-well data for Nitrates before cleaning first. Makes the data cleaning easier

summary(nwis_n)
nwis_n %>% purrr::map(~(length(unique(.)))) 

nwis_n2 <-
  nwis_n %>% select(
    orgId = OrganizationIdentifier,
    activityId = ActivityIdentifier,
    date = ActivityStartDate,
    time = `ActivityStartTime/Time`,
    timeZone = `ActivityStartTime/TimeZoneCode`,
    locationId = MonitoringLocationIdentifier,
    detectedOrNot = ResultDetectionConditionText,
    value = ResultMeasureValue,
    unit = `ResultMeasure/MeasureUnitCode`,
    actualOrEstimated = ResultValueTypeName
  ) %>%
  mutate(
    value = case_when(
      is.na(detectedOrNot) ~ value,
      detectedOrNot == "Systematic Contamination" ~ NA_real_,
      detectedOrNot == "Not Detected" ~ 0,
    )
  ) %>%
  filter(detectedOrNot != "Systematic Contamination" |
           is.na(detectedOrNot))

# why are some collected by NV and AZ??
nwis_n$OrganizationIdentifier %>% table()
nwis_n$ResultSampleFractionText %>% table()
nwis_n$`ResultMeasure/MeasureUnitCode` %>% table()
nwis_n2$unit %>% table()

one_measurement <- nwis_n2 %>% 
  group_by(locationId, date) %>% 
  summarise(n = n()) %>% 
  filter(n==1)

# check out each sampling activity that only has one single observation (likely to be mg/L as N)
one_measurement_df <- nwis_n2 %>% 
  right_join(one_measurement, by = c('locationId', 'date'))

# units in c(na, n, and no3) are partition of the dataset!
# I think that I want to work with N so will convert and clean everything to be in the mg/L as N dataset

nwis_n_asN <- nwis_n2 %>% 
  filter(unit == "mg/l as N")

nwis_n_na <- nwis_n2 %>% filter(is.na(unit)) %>% 
  mutate(unit = "mg/L as N")

nwis_n_asNO3 <- nwis_n2 %>% filter(unit == "mg/l asNO3") %>% 
  # filter(!is.na(value)) %>% 
  mutate(valueConvertedFromNO3 =  round(0.2259*value, digits = 3)) %>% 
  select(-value, -unit)


### COOL A LOT OF THEM ARE JUST REPEATED VALUES: 
### NEED TO MERGE THE NA VALUES IN
nwis_n_merged <- full_join(nwis_n_asN, nwis_n_asNO3) %>% full_join(nwis_n_na)

nwis_n_merged2 <- nwis_n_merged %>% 
  # verified that the value converted from NO3 is pretty similar! So we can just use the n measurement
  mutate(valDiff = value-valueConvertedFromNO3,
         value = if_else(is.na(value), valueConvertedFromNO3, value))

nwis_n_merged2 %>% 
  group_by(locationId) %>% 
  summarise(number_of_obs = n())

# %>% ggplot(aes(n_obs)) 
# + geom_histogram(binwidth = 2) + theme_minimal() +
#   lims(x = c(0, 50))

########################################################
# Save and output cleaned station and measurement data
########################################################

saveRDS(station2, "../Data/1int/nwis_n_station.rds")
saveRDS(nwis_n_merged2, "../Data/1int/nwis_n_clean_n.rds")
saveRDS(wells_a, "1int/nwis_ar_clean.rds")

########################################################
# Explore NWIS well monitoring data
########################################################
rm(nwis_n, nwis_n_asN, nwis_n_na, nwis_n_asNO3, one_measurement, one_measurement_df, nwis_merged)

wells <- nwis_n_merged2 %>% left_join(station2, by = 'locationId') %>% 
  filter(locationType == "Well") %>% 
  mutate(year = year(date)) %>% 
  group_by(locationId) %>% 
  mutate(freq = n())

wells %>% 
  group_by(AquiferName, year) %>%
  filter(!is.na(AquiferName)) %>% 
  summarise(prop_over_10 = sum((value >= 10))/n(),
            med_N = median(value, na.rm = TRUE)) %>% 
  mutate(source = "nwis") %>% 
  ggplot(aes(year, med_N, color = AquiferName)) +
  geom_point() + 
  geom_line() +
  geom_vline(xintercept = c(2012, 2016), color = 'red') +
  facet_wrap(vars(AquiferName)) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2004, 2020, 2)) +
  scale_color_brewer(palette = "Set2")

# Since 2000, the longest duration of drought (D1–D4) in California lasted 376 weeks beginning on December 27, 2011, and ending on March 5th, 2019. The most intense period of drought occurred the week of July 29, 2014, where D4 affected 58.41% of California land.

well_d %>% 
  ggplot(aes(x=n_obs)) +
  geom_histogram(bins = 100, fill = "white", color = "grey30") +
  theme_minimal()

# how many wells have more than 5 observations? 402
# 10? 173

well_d %>% filter(n_obs>5) %>% nrow()

# look at locations with at least 8 observations

wells8 <- wells %>% filter(freq>8)

set.seed(0807)
wells8 %>% 
  filter(locationId %in% sample(wells8$locationId %>% unique(), 6)) %>% 
  ggplot(aes(x=date, y=value, color = factor(locationId))) +
  geom_point() + 
  geom_line() +
  theme_minimal() +
  scale_color_brewer(palette = "Set2")

station_med <- wells %>% 
  group_by(locationId, lat, lon) %>% 
  summarise(well_depth = mean(wellDepth_m, na.rm = TRUE), median_n_mgl = median(value, na.rm = TRUE)) %>% 
  filter(median_n_mgl > 0, median_n_mgl < 50)

ggplot() + 
  geom_sf(data = ca, color="black",
          fill="white", size=0.25) +
  geom_point(
    data = station_med,
    aes(x = lon, y = lat, color = median_n_mgl),
    size = 1,
    alpha = .3
  ) +
  scico::scale_color_scico(palette = "lajolla") + theme_minimal()

########################################################
# Explore station data
########################################################

ggplot() + 
  geom_sf(data = ca, color="black",
          fill="white", size=0.25) +
  geom_point(
    data = station2,
    aes(x = lon, y = lat, color = wellDepth_m),
    size = 1,
    alpha = .5
  ) +
  scico::scale_color_scico(palette = "berlin") + theme_minimal()

# age of wells and well depth, not a super super strong pattern, it's ok

station2 %>% ggplot(aes(x = constructionDateYr, y = wellDepth_m)) +
  geom_point() +
  stat_summary(
    geom = "point",
    fun.y = "median",
    # col = "black",
    size = 2,
    shape = 24,
    fill = "red"
  ) +
  theme_minimal()
