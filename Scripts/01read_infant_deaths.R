#################################
# Read and clean infant death data
# 
# 5/8/2021
# sandysum@ucsb.edu
#################################
#rm(list=ls())
home <- "/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/Data"

# load package

library(tidyverse)
library(readr)

inf <- read.table(file.path(home, "ca_infant_deaths_2007-2018.txt"), header=TRUE, fill = TRUE)
inf2003 <- read.table(file.path(home, "ca_infant_deaths_2003-2006.txt"), header=TRUE, fill = TRUE)

inf2 <- inf[1:7040,c(1:5,8)]
inf2003 <- inf2003[1:368,c(1:3,5,6,8)]

names(inf2) <- c('county', 'county_code', 'cause', 'cause_code', 'year', 'death_counts')
names(inf2003) <- c('county', 'county_code', 'year', 'cause', 'cause_code', 'death_counts')

inf2 <- inf2 %>% mutate(death_counts = as.numeric(death_counts))

# check if complete

# inf2 %>% group_by(county, year) %>% summarise(n = n())
# inf2 %>% group_by(county) %>% summarise(n = n()) %>% print(n=100)

blue <- inf2 %>%
  bind_rows(inf2003) %>% 
  filter(cause == "Hemorrhagic conditions and other diseases of blood and blood-forming organs (D65-D76)") %>% 
  filter(county_code != "06999") 

COUNTY <- str_extract_all(blue$county, regex(".*(?= County, CA)")) %>% unlist() 
COUNTY <- COUNTY[!COUNTY==""]
rm(inf2); rm(inf); rm(inf2003)

blue <- blue %>% mutate(COUNTY=str_to_lower(COUNTY),
                        death_counts = as.numeric(death_counts))

blue %>% 
  ggplot(aes(year, death_counts)) +
  # geom_point()
  geom_line() +
  facet_wrap(vars(county), scales = "free") 
