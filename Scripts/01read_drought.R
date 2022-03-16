# Heading -----------------------------------------------------------------
# A script to expore CA SWRB contaminant monitoring data
# sandysum@ucsb.edu
# 2021/09/17

library(tidyverse)
library(pdsi)
library(ncdf4)
library(exactextractr)
library(lubridate)
library(sf)
library(tigris)
library(rgeos)
library(cowplot)
library(thredds)

# Read in datasets --------------------------------------------------------

home <- "/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/Data/"
# this was old and only have at the climdiv level

# pdsi <- readRDS("../Data/drought/pdsi_ca_1970_2021.rds")
pwss <-
  read_sf(file.path(home, "/shp_PWS_SABL_Public_080221/SABL_Public_080221.shp")) %>%
  filter(SHAPE_LEN > 0) %>%
  mutate(SYSTEM_NO = str_extract(SABL_PWSID, "\\d+"))

# Downlaoding from thredds server gridded PDSI data from the thredds server --------------------

nw_url <- "http://thredds.northwestknowledge.net/thredds/reacch_climate_MET_aggregated_catalog.html"

ds <- tds_list_datasets(thredds_url = nw_url)

pdsi_url <- ds[14,]$path
pdsi_services <- tds_list_services(pdsi_url)
pdsi_ncss <- pdsi_services[pdsi_services$service == "NetcdfSubset",]$path
pdsi_vars <- tds_ncss_list_vars(ncss_url = pdsi_ncss)

pdsi_loc <- tds_ncss_download(ncss_url = pdsi_ncss,
                           out_file = "../Data/drought/pdsi.nc",
                          # -124.409591	32.534156	-114.131211	42.009518 for CA
                          # xmin ymin xmax ymax
                           bbox = sf::st_bbox(c(xmin = -124.4, xmax = -114.1, ymin = 32.5, ymax = 42)),
                           vars = c("daily_mean_palmer_drought_severity_index"),
                           ncss_args = list(temporal = "all"))

# Reading in PDSI nc file -------------------------------------------------

# read in CA shapefile

ca <- counties(state = "ca") %>% st_simplify(dTolerance = 10)
ca_outline <- as(st_geometry(ca), Class="Spatial")
plot(ca_outline)

pdsi_ca <-
  raster::brick(paste0(home, "drought/pdsi.nc"), varname = "daily_mean_palmer_drought_severity_index")

# convert PDSI to month-year data

# extract the date from the name of the raster

pdsi_date <- ymd_hms('1900-01-01 00:00:00') + days(as.numeric(str_extract(names(pdsi_ca), "\\d+"))) 

# pdsi is available every 5 days

pdsi_date <- pdsi_date %>% as_date()
names(pdsi_ca) <- as.character(pdsi_date)

raster::values(pdsi_ca[[3000]]) %>% range(na.rm = TRUE)

#get the date from the names of the layers and extract the month
indices <- format(pdsi_date, format = "%m%Y")
indices <- as.numeric(indices)

# # plotting and exploring for dates
# 
# mapTheme <- rasterTheme(region = brewer.pal(15, "RdBu"))
# cutpts <- seq(-16, 10, 2)
# # names(pdsi_ca) <- pdsi_date
# plt <-
#   levelplot(
#     subset(pdsi_ca, seq(2045, 3045, 100)),
#     margin = F,
#     at = cutpts,
#     cuts = 15,
#     pretty = TRUE,
#     par.settings = mapTheme,
#     main = "PDSI 2008-2022"
#   )

# sum layers so that we have monthly means

pdsi_ca_month <- stackApply(pdsi_ca, indices, fun = mean, na.rm = TRUE)
names(pdsi_ca_month) <- names(pdsi_ca_month) %>% str_extract("\\d+") %>% str_pad(width=6, side="left", pad="0") 

# save pdsi rasterbrick - month year

saveRDS(pdsi_ca_month, "../Data/drought/pdsi_grid_monthyear.rds")

plot(pdsi_ca_month, )

# Spatial aggregation -1)PWS shapefile 2)PWS with no shapefile ------------

# 1. aggregate to the pws-month-year level for those without shapefiles
# in this case I am using either zipcode or counties shapefiles

pws_meta <- read_csv('/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/Data/SDWA-DL/SDWA_PUB_WATER_SYSTEMS.csv')

ni <-read_rds(file.path(home, "1int/caswrb_n_1974-2021.rds"))
ar <-read_rds(file.path(home, "1int/caswrb_ar_1974-2021.rds"))

# identify pws with no shapefiles
x <- unique(c(ni$SYSTEM_NO, ar$SYSTEM_NO))

no_shp <- paste0("CA", unique(c(ni$SYSTEM_NO, ar$SYSTEM_NO))[!(unique(c(ni$SYSTEM_NO, ar$SYSTEM_NO)) %in% pwss$SYSTEM_NO)]) %>% as_tibble() %>% 
  rename(PWSID = value) %>% 
  left_join(pws_meta %>% select(PWSID, PWS_NAME, ADDRESS_LINE1, ADDRESS_LINE2, CITY_NAME, ZIP_CODE, COUNTRY_CODE)) %>%
  left_join(dww %>% select(WaterSystemNo, PrincipalCountyServed), by = c('PWSID' = 'WaterSystemNo')) %>% 
  mutate(zip = ZIP_CODE %>% str_extract("\\d{5}"))

# download zip shapefile
zip_shp <- tigris::zctas(cb = TRUE, starts_with = no_shp$ZIP_CODE %>% str_extract("\\d{2}") %>% unique())

zip_shp <- zip_shp %>% filter(ZCTA5CE10%in%no_shp$zip) %>% 
  left_join(no_shp, by = (c('ZCTA5CE10'='zip')))

# extract for these shapes
zip_shp <- read_sf(file.path(home, "/shp_PWS_SABL_Public_080221/PWS_by_zip.shp")) %>%
  # filter(SHAPE_LEN > 0) %>%
  mutate(SYSTEM_NO = str_extract(PWSID, "\\d+"))

r.vals <- raster::extract(pdsi_ca_month, zip_shp)

pws_pdsi_my <- map2(r.vals, zip_shp$PWSID, function(p, id) {
  if (is.null(p)) {
    return(NULL)
  } else {  p %>% as.tibble() %>% gather(my, pdsi) %>% 
    group_by(my) %>% 
    summarise(mean_pdsi = mean(pdsi, na.rm = TRUE)) %>% 
    mutate(SABL_PWSID = id)
  }
})

pws_pdsi_my <- pws_pdsi_my %>% 
  bind_rows() 

saveRDS(pws_pdsi_my, "../Data/drought/pdsi_pws_monthyear_zip.rds")

zip_shp <- zip_shp %>% rename(zip = ZCTA5CE10, address1 = ADDRESS_LINE1, address2 = ADDRESS_LINE2, county = PrincipalCountyServed)

# pdsi_pws_year %>% group_by(SABL_PWSID, year) %>% distinct()

# 2. aggregate to the pws-month-year level for those with shapefiles -----------------------------------

# need to check if this is actually working!

r.vals <- raster::extract(pdsi_ca_month, pwss)

# compute mean per month-year

p <- r.vals[[15]]

pws_pdsi_my <- map2(r.vals, pwss$SABL_PWSID, function(p, id) {
p %>% as.tibble() %>% tidyr::gather(my, pdsi) %>% 
  group_by(my) %>% 
  summarise(mean_pdsi = mean(pdsi, na.rm = TRUE)) %>% 
  mutate(SABL_PWSID = id)
})

pws_pdsi_my <- pws_pdsi_my %>% 
  bind_rows() 

saveRDS(pws_pdsi_my, "../Data/drought/pdsi_pws_monthyear.rds")

pdsi2 <- readRDS("../Data/drought/pdsi_pws_monthyear_zip.rds")
pdsi <- readRDS("../Data/drought/pdsi_pws_monthyear.rds") 
pdsi_pws_year <- bind_rows(pdsi, pdsi2) %>% 
  mutate(month = as.numeric(str_extract(my, "\\d{2}")),
         year = as.numeric(str_extract(my, "\\d{4}$"))) %>% 
  group_by(SABL_PWSID, year) %>% 
  dplyr::summarise(mean_pdsi = mean(mean_pdsi, na.rm = TRUE)) %>% 
  mutate(SYSTEM_NO = str_extract(SABL_PWSID, "\\d+"))

saveRDS(pdsi_pws_year, file = "../Data/drought/pdsi_pws_year.rds")

# randomly plot some shapefiles from the new zip code level dataset to check 

set.seed(8067986)
q <- sample(unique(pdsi_pws_year$SABL_PWSID), 400)
quartz()

pdsi_pws_year %>% 
  # mutate(month = as.numeric(str_extract(my, "\\d{2}")),
  #        year = as.numeric(str_extract(my, "\\d{4}$"))) %>%
  # group_by(SABL_PWSID, year) %>%
  # dplyr::summarise(mean_pdsi = mean(mean_pdsi, na.rm = TRUE)) %>%
  # mutate(SYSTEM_NO = str_extract(SABL_PWSID, "\\d+")) %>%
  filter(SABL_PWSID %in% q) %>%
  ggplot(aes(x = year, y = mean_pdsi, group = SABL_PWSID)) +
  geom_line(alpha = .4, color = 'grey70') +
  theme_minimal_hgrid() +
  # stat_summary(fun = mean,
  #              geom = "point", color = 'darkblue', aes(group = year)) +
  scale_x_continuous(breaks = seq(1974, 2022, 2)) +
  # geom_hline(yintercept = 10, color = 'red') +
  theme(axis.text.x = element_text(angle = 45, size = 10)) +
  # ylim(c(-8, 8)) +
  scale_y_continuous(breaks = seq(-8, 8, 1)) +
  geom_hline(yintercept = -2)
  
# WHY DOES MY PDSI DATA GOES UP TO -12??? and 14??? NEED TO CHECK

# This is old stuff from the county level PDSI data -----------------------
# Clean and save PDSI

# pdsi <- pdsi_get(url = "climdiv-pdsidv-v1.0.0-20210907")
# # keep only California climate divisions
# 
# pdsi <- pdsi %>% filter(climdiv %in% c(paste0("040", 1:7)))
# 
# pdsi <- pdsi %>% mutate(date = dmy(paste0("01-", month, "-", year))) %>%
#   filter(year > 1969)
# 
# saveRDS(pdsi, "../Data/drought/pdsi_ca_1970_2021.rds")

# Read in shapefiles and keep only California -----------------------------

climdiv_shp <- read_sf("../Data/CONUS_CLIMATE_DIVISIONS.shp") %>% 
  filter(STATE_FIPS == "06")

climdiv_shp2 <- sf::st_simplify(climdiv_shp, dTolerance = 10) %>% 
  st_transform(st_crs(ca)) %>% 
  rename(CLIMDIV_NM = NAME)

ca <- counties(state = "ca") %>% st_simplify(dTolerance = 10)

# some counties are definitely overlapping with more than one climdiv... gotta calculate areas overlap

# Visualize CA counties and Climdivs --------------------------------------

ggplot() + geom_sf(
  data = ca,
  color = "black",
  fill = "white",
  size = 0.25
) +
  geom_sf(
    data = climdiv_shp2,
    color = 'red',
    alpha = .5,
    size = .20,
    aes(fill = factor(CLIMDIV))
  ) +
  theme_light()

# Ignore this
# Identify the climate division with the largest spatial overlap --------

int <- ca$NAME %>% purrr::map(function(x) {
  # for each climate division find the intersecting counties
  county <- ca[ca$NAME == x, ]
  i <- st_intersection(county, climdiv_shp2) %>% as_tibble()
  # calculate how much each counties overlap with intersecting climdivs
  i["mAcres"] <- st_area(i$geometry) / (4047 * 1000000)
  return(i)
}) %>% bind_rows() 

# get the climdiv with the biggest area overlap
int <- int %>% 
  group_by(NAME) %>% 
  # only retain climdiv with the largest area overlap
  summarise(climdiv_assigned = CLIMDIV[which(mAcres == max(max(mAcres)))])

write_csv(int, "../Data/drought/ca_climdiv_crosswalk.csv")

ca <- ca %>% left_join(int, by = 'NAME')

# check
gg <- ggplot() + geom_sf(data = ca,
                   color = "black",
                   aes(fill = factor(climdiv_assigned)),
                   size = 0.25) +
  geom_sf(data = climdiv_shp2,
          aes(color = factor(CLIMDIV)),
          alpha = .5,
          size = .8) +
  theme_light()

sf::write_sf(ca, "../Data/drought/ca_climdiv.shp")

pdsi %>% ggplot(aes(date, pdsi)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_line() +
  geom_vline(xintercept = dmy(c('1-1-2012', '1-1-2017')), col = "orange") +
  facet_grid(rows = vars(climdiv)) +
  theme_minimal() +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9))
