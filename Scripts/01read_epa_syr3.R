#################################
# Read and clean EPA sixth year review data
# 
# 8/13/2021
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

# read in data

# This data is the non-comprehensive version of the CA water quality analyses one
# check
ar2006 <- read_delim("../Data/epa_dwdata_2006-2011/arsenic.txt", delim = '\t', 
                 col_types = list(`Analyte ID` = col_double(),
                 `Analyte Name` = col_character(),
                 `State Code` = col_character(),
                 PWSID = col_character(),
                 `System Name` = col_character(),
                 `System Type` = col_character(),
                 `Retail Population Served` = col_double(),
                 `Adjusted Total Population Served` = col_double(),
                 `Source Water Type` = col_character(),
                 `Water Facility ID` = col_double(),
                 `Water Facility Type` = col_character(),
                 `Sampling Point ID` = col_double(),
                 `Sampling Point Type` = col_character(),
                 `Source Type Code` = col_character(),
                 `Sample Type Code` = col_character(),
                 `Laboratory Assigned ID` = col_character(),
                 `Six Year ID` = col_double(),
                 `Sample ID` = col_double(),
                 `Sample Collection Date` = col_datetime(format = ""),
                 `Detection Limit Value` = col_double(),
                 `Detection Limit Unit` = col_character(),
                 `Detection Limit Code` = col_character(),
                 Detect = col_double(),
                 Value = col_double(),
                 Unit = col_character(),
                 `Presence Indicator Code` = col_logical(),
                 `Residual Field Free Chlorine mg/L` = col_double(),
                 `Residual Field Total Chlorine mg/L` = col_double()))

ar1998 <- read_delim("../Data/epa_dwdata_1998-2005/Arsenic_Chem1005.txt", delim = ",")

names(ar2006) <- names(ar2006) %>% str_remove_all('\\s')

# data from the EPA SYR 3 has more useful information the most useful information I would need would be the sampling point ID and sampling point type which would give me information on the location point of a sampling point.. and if it is "finished" or not, which would tell me if that water goes to consumer or is prior to being treated

#################
# Investigate if I can match sampling point ID from the 2006 file to the 1998 file

# Filter to CA only to speed things up but I do think that I want to look at the whole US 

ar1998 <- ar1998 %>% filter(STATE == 'CA')
ar2006 <- ar2006 %>% filter(StateCode == 'CA')

# first see at what unit of granularity; how many sampling point ID are there for each PWSID
# the water facility ID in 2006 NEQ ID in 1998 
# each PWSID has more than 1 water facility ID and ID 

ar2006 %>% group_by(PWSID) %>% 
  summarise(n_spid = unique(SamplingPointID) %>% length()) %>% 
  # holy guacamole-- city of fresno has so many SPID...
  arrange(desc(n_spid))

ar_finished <- ar %>% filter(SourceTypeCode=="FN", StateCode == "CA")

# is there is reason to filter to california only for this analysis?


