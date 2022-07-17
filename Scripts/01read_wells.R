###########################
# This script explores the well drilling data 
# (1) are there disparities in terms of the age of wells and depth
# for pulic access wells?
# (2) could the disparities that I see in raw N drought response be attributable to 
# old and failing wells serving majority-Latino?
# (3) or could it be agricultural environment that I am failing to find?
# I feel like I am on to something.


home <- "/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/Data"

home <- "G:/My Drive/0Projects/1Water/2Quality/Data/"

# load package

library(tidyverse)
library(readr)
library(lubridate)
library(readxl)
library(janitor)
library(cowplot)
library(tigris)
library(sf)
library(fedmatch)
cal <- c(-124.409591,	32.534156	,-114.131211,	42.009518)
wells <- read_csv("../Data/wellcompletionreports.csv") %>% clean_names() %>% 
  mutate(proposed_use = clean_strings(planneduseformeruse) %>% str_replace('agriculture', 'agricultural') %>% 
  str_replace('^irrigation agricultural', "water supply irrigation agricultural")) 

irr.wells <- wells %>% filter(str_detect(proposed_use, 'water supply irrigation agricultural')) %>% 
  mutate(date_ended = mdy(dateworkended),
         month_drilled = month(date_ended),
         year_drilled = year(date_ended),
         age = 2022-year_drilled) %>% 
  filter(year_drilled > 1949, year_drilled < 2023, ) %>% 
  rename(lat = decimallatitude, lon = decimallongitude) %>% 
  #filter to within CA
  filter(lon > cal[1], lon < cal[3], lat >  cal[2], lat < cal[4]) %>% 
  select(wcrnumber, countyname, lat, lon, totalcompleteddepth, date_ended, age, matches('drilled'), )

quartz()
ggplot(irr.wells, aes(lon, lat, color = totalcompleteddepth)) + 
  geom_point(size = 1, alpha = .6) +
  scale_color_gradient(low = 'linen', high = 'deeppink4') +
  theme_cowplot() +
  coord_quickmap()


# Compute ag well metrics for each PWS ------------------------------------

# 1500 is around 1+ mile 3000 is around 5 miles 750

# change everything to a circle with a fixed radius for computing ag land
pws <- read_sf("../Data/shp_PWS_SABL_Public_080221/SABL_Public_080221.shp") %>% 
  distinct(SABL_PWSID, .keep_all = TRUE) %>% 
  st_centroid() %>% 
  st_buffer(dist = 1500)

# pws_sf_zip is mutually exclusive of pws_sf
pws_sf_zip <- read_sf("../Data/shp_PWS_SABL_Public_080221/PWS_by_zip.shp") %>%
  st_transform(crs = st_crs(pws)) %>% 
  # filter(SHAPE_AREA > 0, SHAPE_LEN > 0) %>%
  distinct(PWSID, .keep_all = TRUE) %>% 
  st_centroid() %>% 
  st_buffer(dist = 1500)

# convert wells data to sf object
irr.wells.sf <- st_as_sf(irr.wells, coords=c("lon", "lat"), crs=4326) %>% 
  st_transform(crs = st_crs(pws))

int <- sf::st_intersects(pws, irr.wells.sf) 
int.zip <- sf::st_intersects(pws_sf_zip, irr.wells.sf) 

# Gut check to see if its correct
# looks correct
plot(st_geometry(pws[c(4, 12), ]))
plot(st_geometry(irr.wells.sf[int[[4]],]), add = TRUE)
plot(st_geometry(irr.wells.sf[int[[12]],]), add = TRUE)

# create and save two datasets: one cross sectional and one panel

# 1. create the cross sectional one: which I guess... doesn't make sense

int.cs <-  map(1:length(int), function(x) {
  y <- irr.wells[int[[x]],]
  tibble(
    ag_wells_n = nrow(y),
    ag_wells_depth = mean(y$totalcompleteddepth, na.rm = TRUE),
    ag_wells_depth_total = sum(y$totalcompleteddepth, na.rm = TRUE),
    ag_wells_age = mean(y$age, na.rm = TRUE)
  )
}) %>% bind_rows() %>%
  mutate_if(is.numeric, ~ (ifelse(is.nan(.), NA, .)))

int.zip.cs <- map(1:length(int.zip), function(x) {
  y <- irr.wells[int[[x]], ]
  tibble(
    ag_wells_n = nrow(y),
    ag_wells_depth = mean(y$totalcompleteddepth, na.rm = TRUE),
    ag_wells_depth_total = sum(y$totalcompleteddepth, na.rm = TRUE),
    ag_wells_age = mean(y$age, na.rm = TRUE)
  )
}) %>% bind_rows() %>% 
  mutate_if(is.numeric, ~(ifelse(is.nan(.), NA, .)))

pws_wells <- pws %>%
  as_tibble() %>%
  select(SYSTEM_NO = SABL_PWSID) %>%
  bind_cols(int.cs)

pws_wells_zip <- pws_sf_zip %>%
  as_tibble() %>%
  select(SYSTEM_NO = PWSID) %>%
  bind_cols(int.zip.cs) %>% 
  filter(!(SYSTEM_NO %in% pws_wells$SYSTEM_NO))

pws_wells_all <- bind_rows(pws_wells, pws_wells_zip)

ind <- readRDS(file.path(home, "1int/1_pws_ind.rds")) %>% 
  # select(-names(pws_wells_all)[-1]) %>% 
  left_join(pws_wells_all %>% mutate(SYSTEM_NO = str_extract(SYSTEM_NO, "\\d+")))

saveRDS(ind, file.path(home,"1int/pws_ind.rds"))

ind %>% ggplot(aes(percent_hispanic, ag_wells_n)) +
  geom_point()

ind %>% ggplot(aes(percent_hispanic, ag_wells_depth_total)) +
  geom_point()

ind %>% ggplot(aes(avg_percent_ph, ag_wells_n)) +
  geom_point()

# 2. create the panel data

int.p <-  map(1:length(int), function(x) {
  y <- irr.wells[int[[x]], ] %>% 
    dplyr::select(year = year_drilled, totalcompleteddepth, age) %>% 
    group_by(year) %>% 
    summarise(wells_drilled = n(), 
           mean_depth = mean(totalcompleteddepth, na.rm = TRUE))
  # print(x)
  if (nrow(y) == 0) {
    out <- tibble(SYSTEM_NO = pws[x,]$SABL_PWSID,
                  year = 1990:2022,
                  wells_drilled = 0, 
                  mean_depth =0, 
                  wells = 0, 
                  depth_sum = 0)
  } else {
  out <- tibble(SYSTEM_NO = pws[x,]$SABL_PWSID,
         year = min(y$year):2022) %>% 
    left_join(y) %>% 
    mutate(wells_drilled = replace_na(wells_drilled, 0),
           mean_depth = replace_na(mean_depth, 0),
           wells = cumsum(wells_drilled),
           depth_sum = cumsum(mean_depth)
    ) %>% filter(year > 1989)}
  return(out)
}) %>% bind_rows() 

int.p.zip <-  map(1:length(int.zip), function(x) {
  y <- irr.wells[int.zip[[x]], ] %>% 
    dplyr::select(year = year_drilled, totalcompleteddepth, age) %>% 
    group_by(year) %>% 
    summarise(wells_drilled = n(), 
              mean_depth = mean(totalcompleteddepth, na.rm = TRUE))
  # print(x)
  if (nrow(y) == 0) {
    out <- tibble(SYSTEM_NO = pws_sf_zip[x,]$PWSID,
                  year = 1990:2022,
                  wells_drilled = 0, 
                  mean_depth =0, 
                  wells = 0, 
                  depth_sum = 0)
  } else {
    out <- tibble(SYSTEM_NO = pws_sf_zip[x,]$SABL_PWSID,
                  year = min(y$year):2022) %>% 
      left_join(y) %>% 
      mutate(wells_drilled = replace_na(wells_drilled, 0),
             mean_depth = replace_na(mean_depth, 0),
             wells = cumsum(wells_drilled),
             depth_sum = cumsum(mean_depth)
      ) %>% filter(year > 1989)}
  return(out)
}) %>% bind_rows()  

pws_wells_p_all <- bind_rows(int.p, int.p.zip) %>% 
  distinct(SYSTEM_NO, year, .keep_all = TRUE)

pws_wells_p_all %>% filter(SYSTEM_NO %in% sample(unique(pws_wells_p_all$SYSTEM_NO), 12)) %>% 
  ggplot(aes(year, wells, color = SYSTEM_NO)) +
  geom_line() +
  theme_cowplot()

saveRDS(pws_wells_p_all, file.path(home,"1int/pws_wells_panel.rds"))
print('done')
ni_split[[1]] %>%
  ungroup() %>% 
  # slice(1:1000) %>% 
  filter(SYSTEM_NO %in% sample(unique(ni_split[[1]]$SYSTEM_NO))) %>% 
  ggplot(aes(d, wells_drilled)) +
  geom_point() +
  theme_cowplot()

# Clean up the planned use categories -------------------------------------

use <- wells$proposed_use %>% table() %>% as_tibble() 

names(use) <- c('type', 'freq')

use %>% arrange(-freq) %>% print(n=50)

# Pulic wells -------------------------------------------------------------

public.wells <- wells %>% filter(str_detect(planneduseformeruse, '^Public$|Supply Public$')) %>% 
  mutate(date_ended = mdy(dateworkended),
         month_drilled = month(date_ended),
         year_drilled = year(date_ended),
         age = 2022-year_drilled) %>% 
  filter(year_drilled > 1949, year_drilled < 2023) %>% 
  rename(lat = decimallatitude, lon = decimallongitude)

# Explore before matching

# mean depth has remained kinda constant
public.wells %>% 
  group_by(year_drilled) %>% 
  summarise(wells_drilled = n(),
            mean_depth = mean(totalcompleteddepth, na.rm = TRUE)) %>% 
  ggplot(aes(x = year_drilled, y = mean_depth)) + 
  geom_col() +
  scale_x_continuous(breaks = seq(1950, 2022, by=5)) + 
  theme_cowplot()


# number of wells drilled by year
public.wells %>% 
  group_by(year_drilled) %>% 
  summarise(wells_drilled = n(),
            mean_depth = mean(totalcompleteddepth, na.rm = TRUE)) %>% 
  ggplot(aes(x = year_drilled, y = wells_drilled)) + 
  scale_x_continuous(breaks = seq(1950, 2022, by=5)) + 
  geom_col() +
  theme_cowplot()

recent <- public.wells %>% filter(year_drilled < 2023) %>% 
  rename(lat = decimallatitude, lon = decimallongitude)

quartz()
ggplot(recent, aes(lon, lat, color = totalcompleteddepth)) + 
  geom_point(size = 1, alpha = .6) +
  scale_color_gradient(low = 'linen', high = 'deeppink4') +
  theme_cowplot() +
  coord_quickmap()

quartz()
ggplot(recent, aes(lon, lat, color = age)) + 
  geom_point(size = 1) +
  scale_color_gradient(low = 'linen', high = 'slateblue4') +
  theme_cowplot() +
  coord_quickmap()

# Irrigation wells --------------------------------------------------------

# First need to identify all irrigation wells
use <- use %>% as_tibble()
names(use) <- c('use', 'freq')

use %>% arrange(-freq) %>% print(n=50)
