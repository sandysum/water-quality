
# Explore thoroughly SWDIS data -------------------------------------------
# I need to look for s spatial varying fixed effect larger than CWS to control for space hydrologic varying 
# features. First step is to look into these dataset that seems comprehensive.

ind <- readRDS("1int/pws_ind.rds") %>% 
  mutate(PWSID = paste0('CA', SYSTEM_NO))
facilities <- read_csv("SDWA-DL/SDWA_FACILITIES.csv")
ca <- facilities %>% filter(PWSID %in% ind$PWSID)
ca$PWSID %>% unique() %>% length()
