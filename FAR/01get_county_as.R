#################################
# Read and clean arsenic USGS county maps
# 
# 2022/05/01
# sandysum@ucsb.edu
#################################

library(sf)
library(tidyverse)
library(tigris)
library(tidycensus)

geo <- read_sf("/Users/sandysum/Downloads/ngs/ngs.shp") %>% 
  st_transform(st_crs(counties)) 

counties <- counties()
counties.small <- st_simplify(counties, dTolerance = 20)
plot(geo['AS_AA'])

as.counties <- aggregate(geo['AS_AA'], by = counties.small, mean, na.rm = TRUE)
plot(as.counties[1:50,])

as.counties <- as.counties %>% bind_cols(counties %>% as.data.frame()) %>% as.data.frame() %>% 
  select(-matches('geometry'))

fips_codes <- fips_codes %>% mutate(GEOID = paste0(state_code, county_code))

# merge with as levels

as.counties <- rename(as.counties, usgs_as = AS_AA) %>% left_join(fips_codes) 

write_csv(as.counties,"../Data/1int/usgs_counties_as.csv")

# plot counties
