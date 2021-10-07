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

home<- "/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/Data/"
# this was old and only have at the climdiv level

# pdsi <- readRDS("../Data/drought/pdsi_ca_1970_2021.rds")
pwss <- read_sf("../Data/SABL_Public_080221/SABL_Public_080221.shp") %>% 
  filter(SHAPE_LEN > 0)

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
  raster::brick("../Data/drought/pdsi.nc", varname = "daily_mean_palmer_drought_severity_index")

# convert PDSI to month-year data

# extract the date from the name of the raster

pdsi_date <- ymd_hms('1900-01-01 00:00:00') + days(as.numeric(str_extract(names(pdsi_ca), "\\d+"))) 

# pdsi is available every 5 days

pdsi_date <- pdsi_date %>% as_date()
names(pdsi_ca) <- as.character(pdsi_date)

values(pdsi_ca[[3000]]) %>% range(na.rm = TRUE)

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

plot(pdsi_ca_month, 500)

# aggregate to the pws-month-year level -----------------------------------

# need to check if this is actually working!

r.vals <- raster::extract(pdsi_ca_month, pwss)

# compute mean per month-year

p <- r.vals[[15]]

pws_pdsi_my <- map2(r.vals, pwss$SABL_PWSID, function(p, id) {
p %>% as.tibble() %>% gather(my, pdsi) %>% 
  group_by(my) %>% 
  summarise(mean_pdsi = mean(pdsi, na.rm = TRUE)) %>% 
  mutate(SABL_PWSID = id)
})

pws_pdsi_my <- pws_pdsi_my %>% 
  bind_rows() 

saveRDS(pws_pdsi_my, "../Data/drought/pdsi_pws_monthyear.rds")

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
