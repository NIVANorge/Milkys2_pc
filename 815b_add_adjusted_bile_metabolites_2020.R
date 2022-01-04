
#
# get adjusted bile metabolites from Anders
#   (only unadjusted ones inserted intto NIVAdatabase)
#

library(dplyr)
library(tidyr)

dat <- readxl::read_excel("Input_files_2020/BEM-JAMP_2020_ARU.xls", na = "n.d.")

str(dat)

xtabs(~nummer + stasjon, dat)

dat_tidy <- dat %>%
  mutate(STATION_CODE = paste0(stasjon, "B")) %>%
  select(STATION_CODE, nummer, BLOPR:EROD) %>%
  pivot_longer(cols = c(-STATION_CODE, -nummer), names_to = "PARAM", values_to = "VALUE")

# Save temporary file to just directly in script 101 in Milkys2 (Jupyterhub)
write.csv(dat_tidy, "Files_to_Jupyterhub_2020/902_TEMPORARY_adjusted_bile_metabolites.csv")

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# SHOULD later be added to Nivadatabase  
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
