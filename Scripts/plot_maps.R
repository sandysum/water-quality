########################## Make maps ############################

library(tidyverse)
library(tigris)
library(sf)
# Read in data ------------------------------------------------------------

# read in counties shape file
ca <- tracts(state = 'ca')
ca <- ca %>% st_simplify(dTolerance = 10) %>% 
  st_transform(crs = st_crs(pws)) 

# read in PWS shape files
pws <- read_sf('../Data/shp_PWS_SABL_Public_080221/SABL_Public_080221.shp') %>% 
  st_centroid()
pws_zip <- read_sf('../Data/shp_PWS_SABL_Public_080221/PWS_by_zip.shp') %>% 
  st_centroid() %>% 
  st_transform(crs = st_crs(pws)) 

pws_zip <- st_intersection(pws_zip, ca) 

# create point data for plotting N and As levels?
pws_all <- pws %>% bind_rows(pws_zip) %>% 
  mutate(SYSTEM_NO = str_extract(SABL_PWSID, "\\d+")) %>% 
  left_join(as_pws_10yr, by = 'SYSTEM_NO') %>% 
  left_join(n_pws_10yr,by = 'SYSTEM_NO') %>% 
  mutate(n_bins = pws_n_mean > 5,
         as_bins = pws_as_mean > 5)

shp_ej <- read_sf("../Data/shp_ej.shp") %>% 
  st_simplify(dTolerance = 20) %>% 
  mutate(percent_latino_bins = cut_interval(prcnt_h,n=8)) %>% 
  filter(!is.na(percent_latino_bins)) %>% 
  bind_rows()

# read in crop map
crop <- read_sf("../Data/shp_cropping_2018/i15_Crop_Mapping_2018.shp")

crop_codes = c('G', 'R', 'F', 'P', 'T', 'D', 'C','V', 'S')

# Drop all polygons that are not ag land code
crop_only <- crop %>% filter(CLASS2 %in% crop_codes)

# Clean up crop only 
crop_only_buff <- sf::st_buffer(crop_only, dist = 0) 
crop_only_buff <- crop_only_buff %>% 
  st_transform(crs = st_crs(shp_ej)) 

# this returns the area intersections between all crop polygons and ca county
# polygons. all I need to do is to 
# compute the total areas by summing over all pws similar shapefiles
int <- sf::st_intersection(crop_only_buff, ca) %>% 
  mutate(intersect_area = st_area(.) %>% as.vector()) %>%  
  as_tibble() %>%
  dplyr::select(names(ca)[1:4], intersect_area) %>% 
  ungroup() %>% 
  dplyr::group_by(GEOID) %>% 
  summarise(agArea = sum(intersect_area))

shp_ej <- shp_ej %>%
  left_join(int) %>%
  mutate(shp_area = st_area(shp_ej),
    agArea = if_else(is.na(agArea), 0, agArea),
    percAgArea = (agArea / shp_area)*100,
    percent_agarea_bins = cut_interval(as.double(percAgArea), n = 8),
    income_bins = cut_number(as.double(mdn_hh_), n = 8)) %>%
  filter(!is.na(percent_agarea_bins), !is.na(income_bins))

# 1. Draw map for percent latino
lab <- shp_ej$percent_latino_bins %>% levels() %>% str_extract_all('\\d+\\.?(\\d+)?') %>% 
  map(~(paste0(.[1], ' to ', .[2])))

l <- ggplot(shp_ej) +
  geom_sf(aes(fill = percent_latino_bins), color = 'black', size = .2) +
  # geom_sf(data = pws_c, alpha = .6) +
  theme_void() +
  scale_fill_brewer(palette = 'Purples', name = 'Percent Latino (%)', 
                    labels = lab) +
  theme(legend.key.size = unit(.25, 'cm'),
        legend.text = element_text(size = 8))

save_plot("Plots.spring2022/map_latino.png", l, base_asp = .8, scale = 1.5)
  # scale_fill_distiller(palette = "RdPu", trans = 'reverse', name = 'Percent Latino')

# 1. Draw map for cropped area
lab <- ca2$percent_agarea_bins %>% levels() %>% str_extract_all('\\d+\\.?(\\d+)?') %>% 
  map(~(paste0(round(as.numeric(.[1]), 0), ' to ', round(as.numeric(.[2]), 0))))

c <- ggplot(shp_ej) +
  geom_sf(aes(fill = percent_agarea_bins), color = 'black', size = .2) +
  # geom_sf(data = pws_c, alpha = .6) +
  theme_void() +
  scale_fill_brewer(palette = 'Greens', name = 'Percent crops (%)', 
                    labels = lab) +
  theme(legend.key.size = unit(.25, 'cm'),
        legend.text = element_text(size = 8))
save_plot("Plots.spring2022/map_crop.png", c, base_asp = .8, scale = 1.5)

lab <- c('7,500 - 42,000', '42,000 - 52,000', '52,000 - 62,000',
         '62,000 - 74,000', '74,000 - 86,000', '86,000 - 101,000',
         '101,000 - 127,000', '127,000 - 250,000')

i <- ggplot(shp_ej) +
  geom_sf(aes(fill = income_bins), color = 'black', size = .2) +
  # geom_sf(data = pws_c, alpha = .6) +
  theme_void() +
  scale_fill_brewer(palette = 'Blues', name = 'Median household income', 
                    labels = lab) +
  theme(legend.key.size = unit(.25, 'cm'),
        legend.text = element_text(size = 8))
save_plot("Plots.spring2022/map_income.png", i, base_asp = .8, scale = 1.5)

# plot As and N

n <- ggplot(shp_ej) +
  geom_sf(color = 'black', size = .2, fill = 'white') +
  geom_sf(data = pws_all %>% filter(!is.na(pws_n_mean)), aes(color = pws_n_mean), 
          alpha = .7) +
  theme_void() +
  scale_colour_gradient(
    name = 'Mean N conc (mg/l)',
    low = "yellow1",
    high = "midnightblue",
    na.value = 'grey50',
    guide = "colourbar",
    aesthetics = "colour"
  )
save_plot("Plots.spring2022/map_n.png", n, base_asp = .8, scale = 1.5)


n <- ggplot(shp_ej) +
  geom_sf(color = 'black', size = .2, fill = 'white') +
  geom_sf(data = pws_all %>% filter(!is.na(pws_n_mean)), aes(color = n_bins), 
          alpha = .7) +
  theme_void() +
 scale_color_brewer(palette = 'Dark2', 
                    name = 'N > 5 mg/l')
save_plot("Plots.spring2022/map_n_bin.png", n, base_asp = .8, scale = 1.5)


as <- ggplot(shp_ej) +
  geom_sf(color = 'black', size = .2, fill = 'white') +
  geom_sf(data = pws_all %>% filter(!is.na(pws_as_mean)), aes(color = pws_as_mean), 
          alpha = .7) +
  theme_void() +
  scale_colour_gradient(
    name = 'Mean As conc ug/l)',
    low = "peachpuff2",
    high = "darkcyan",
    na.value = 'grey50',
    guide = "colourbar",
    aesthetics = "colour"
  )
save_plot("Plots.spring2022/map_as.png", as, base_asp = .8, scale = 1.5)


as <- ggplot(shp_ej) +
  geom_sf(color = 'black', size = .2, fill = 'white') +
  geom_sf(data = pws_all %>% filter(!is.na(pws_as_mean)), aes(color = as_bins), 
          alpha = .7) +
  theme_void() +
  scale_color_brewer(palette = 'Set1', 
                     name = 'As > 5 mg/l')
save_plot("Plots.spring2022/map_as_bin.png", as, base_asp = .8, scale = 1.5)

