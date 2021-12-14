# Heading -----------------------------------------------------------------
# These are helper functions for aggregating spatial data to the PWS level
# sandysum@ucsb.edu
# 2021/09/04
library(raster)
library(rgdal)
library(sp)
library(future.apply)
library(tidyverse)
library(readr)
library(readxl)
library(readr)
library(lubridate)
library(ggplot2)
library(DescTools)
library(sf)
library(lfe)

clay <- raster("../Data/shp_soil/map_ca_clay.tif")
ph <- raster("../Data/shp_soil/map_ca_ph.tif")
pws_sf <- read_sf("../Data/shp_PWS_SABL_Public_080221/SABL_Public_080221.shp") %>% filter(SHAPE_AREA > 0, SHAPE_LEN > 0) 
pws <- read_sf("../Data/SABL_Public_080221/SABL_Public_080221.shp") %>% filter(SHAPE_AREA > 0, SHAPE_LEN > 0) %>% st_geometry()
# pws_sf_zip is mutually exclusive of pws_sf
pws_sf_zip <- read_sf("../Data/shp_PWS_SABL_Public_080221/PWS_by_zip.shp") %>% st_geometry()
# pws_sub <- st_geometry(pws[1:3,])
# 
# plot(pws_sub)
# 
# pws_sub_spatial <- as(pws_sub, 'Spatial')

pws_sp <- as(pws, 'Spatial')
pws_sp_zip <- as(pws_sf_zip, 'Spatial')

extract_one <-
  function(i, ras, pws) {
    raster::extract(ras, pws[i,], fun = mean, na.rm = TRUE)
  }


# Gen cPWS clay -----------------------------------------------------------

# now repeating the procedure for the zip shapefile
clay_by_pws <- future_lapply(1:4490  , FUN = extract_one, ras = clay, pws = pws_sp_zip)

clay_by_pws_fl <- clay_by_pws %>% flatten()

# there are zero elements and also NaNs in the nested list
# the boolean test for NaN do not return a T or F if there are zero element in the list. It will throw an error. Hence, we have to first fix the \
# list elements with zero element in it with an NA, and then that will return a FALSE for the NaN test, and we can fix the NaNs.
# not sure how zero elements or NaNs got into this DF...
out <- lapply(clay_by_pws_fl, (function(x) {
  if (is.null(x) | length(x) == 0) {
    NA
  } else {
    x
  }
}))
out <- lapply(out, (function(x) {
  if (is.nan(x)) {
    NA
  } else {
    x
  }
}))

out %>% unlist() %>% length()

pws_sf_zip$avg_percent_clay <- clay_by_pws_ls

# st_write(pws_sf, dsn = "../Data/1int/pws_sf_clay.shp")
pws_sf_zip <- read_sf("../Data/shp_PWS_SABL_Public_080221/PWS_by_zip.shp")
pws_sf_zip_df <- pws_sf_zip %>% as_data_frame() %>% mutate(avg_percent_clay = out %>% unlist()) %>% 
  dplyr::select(avg_percent_clay, PWSID) %>% 
  mutate(SYSTEM_NO = str_extract(PWSID, "\\d+")) %>% 
  distinct()

# combine the zipcode-retrieved clay pct with pws shapefile-retrieved one
soil <- sf::read_sf("../Data/1int/pws_sf_clay_ph.shp") %>% as_data_frame() %>% 
  dplyr::select(SYSTEM_NO = SABL_PWSID, avg_percent_clay = clay) %>% 
  mutate(SYSTEM_NO = str_extract(SYSTEM_NO, "\\d+")) %>% 
  bind_rows(pws_sf_zip_df %>% dplyr::select(-PWSID)) %>% distinct()

# levels(soil$ph_grp) <- c('low', 'high')
levels(soil$clay_grp) <- c('low', 'high')

write_rds(soil, file = "../Data/1int/pws_clay_merged.rds")

# Check for duplicates ----------------------------------------------------

pws_sf_zip_df %>% group_by(SYSTEM_NO) %>% filter(n()>1)
# 2 PWS has slightly different shapefiles
pws_sf %>% group_by(SABL_PWSID) %>% filter(n()>1)

# Gen PWS level PH --------------------------------------------------------

ph_by_pws <- future_lapply(1:4827, FUN = extract_one, ras = ph, pws = pws_sp)

ph_by_pws_ls <- flatten(ph_by_pws) %>% unlist()

pws_sf$avg_ph <- ph_by_pws_ls

pws_sf <- pws_sf %>% rename(clay = avg_percent_clay, ph = avg_ph)

st_write(pws_sf, dsn = "../Data/1int/pws_sf_clay_ph.shp")


# Plot the clay layer to see if it fits -----------------------------------

pws <- read_sf("../Data/1int/pws_sf_clay.shp")

clay_df <- as.data.frame(clay, xy = TRUE)

ggplot() + 
  geom_sf(data = pws, fill = NA, colour = "grey", size = 0.25) +
  geom_tile(data=clay_df, aes(x=x, y=y, fill= map_ca_clay), alpha=0.8) + 
  # geom_tile(data = clay_df %>% filter(is.na(value)), mapping = aes(x = x, y = y), size = 0.25, fill = NA, color = alpha("gray", 0.25)) +
  # geom_tile(data = clay_df %>% filter(!is.na(value)), mapping = aes(x = x, y = y), size = 0.25, fill = NA, color = alpha("red", 0.5)) +
  theme_minimal()

clay <- ggplot(pws_sf) +
  geom_sf(aes(fill = avg_percent_clay), size = .2, color = 'black') +
  theme_minimal() +
  scale_fill_distiller(palette = "YlGnBu")

save_plot('Plots/clay_pws.png', clay)

ph <- ggplot(pws_sf) +
  geom_sf(aes(fill = avg_ph), size = .2, color = 'black') +
  theme_minimal() +
  scale_fill_distiller(palette = "Purples")

save_plot('Plots/ph_pws.png', ph)


