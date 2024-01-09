library(shiny)
library(dplyr)
library(ggplot2)
library(readxl)

library(maps)
library(mapdata)
simple_map <- map_data("worldHires", c("Norway"))

dat_med <- readRDS("Files_from_Jupyterhub_2020/Raw_data/110_mediandata_updated_2021-10-08.rds")
dat_coord <- read_excel("Files_for_other_use/Milkys_stasjoner_for_kart.xlsx") %>%
  select(STATION_CODE, Lat, Long, Station_name)

df_stations <- dat_med %>%
  count(STATION_CODE) %>%
  left_join(dat_coord) %>%
  filter(!is.na(Lat))

# For now we just delete Svalbard
df_stations <- df_stations %>%
  filter(Lat < 72)

# Map from library(maps)
ggplot(df_stations, aes(Long, Lat)) +
  annotation_map(simple_map, fill = "lightgreen") +
  geom_point() +
  coord_map("lambert", parameters = c(10.4, 59.3))
  
  