
# Explore Arsenic after FAR -----------------------------------------------
# 2022/05/02
# this scripts explores arsenic levels in CA after FAR
# test for heterogeneous effects by violations and also SW / GW

library(sf)
library(tidyverse)
library(readxl)
library(readr)
library(lubridate)
library(tigris)
library(DescTools)
library(Hmisc)
library(fixest)
library(zoo)
options(scipen=999)
rm(list = ls())

home <- "/Volumes/GoogleDrive/My Drive/0Projects/1Water/2Quality/Data/"
ind <- readRDS("../Data/1int/pws_ind.rds")
# home <- "G:/My Drive/0Projects/1Water/2Quality/Data"
# Read data in ------------------------------------------------------------

# read arsenic and nitrate data from the CA SWRB portal 
ar <- read_rds(file.path(home, "1int/caswrb_ar_1974-2022.rds")) %>% 
  left_join(ind)

as.usgs <- read_csv("../D")