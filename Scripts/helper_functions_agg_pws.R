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

clay <- raster("../Data/map_ca_clay.tif")
ph <- raster("../Data/map_ca_ph.tif")
pws_sf <- read_sf("../Data/SABL_Public_080221/SABL_Public_080221.shp") %>% filter(SHAPE_AREA > 0, SHAPE_LEN > 0) 
pws <- read_sf("../Data/SABL_Public_080221/SABL_Public_080221.shp") %>% filter(SHAPE_AREA > 0, SHAPE_LEN > 0) %>% st_geometry()

# pws_sub <- st_geometry(pws[1:3,])
# 
# plot(pws_sub)
# 
# pws_sub_spatial <- as(pws_sub, 'Spatial')

pws_sp <- as(pws, 'Spatial')

extract_one <-
  function(i, ras, pws) {
    raster::extract(ras, pws[i,], fun = mean, na.rm = TRUE)
  }


# Gen cPWS clay -----------------------------------------------------------

clay_by_pws <- future_lapply(1:4827, FUN = extract_one, ras = clay, pws = pws_sp)

clay_by_pws_ls <- flatten(clay_by_pws) %>% unlist()

pws_sf$avg_percent_clay <- clay_by_pws_ls

# st_write(pws_sf, dsn = "../Data/1int/pws_sf_clay.shp")


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


