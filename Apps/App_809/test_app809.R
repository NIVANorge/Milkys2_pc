
#
# Getting data ----
#

library(shiny)
library(dplyr)
library(ggplot2)
library(readxl)

library(maps)
library(mapdata)
simple_map <- map_data("worldHires", c("Norway"))

# Station coordinates
# IN TEST SCRIPT: path changed (../.. removed) 
dat_coord <- read_excel("Files_for_other_use/Milkys_stasjoner_for_kart.xlsx") %>%
  select(STATION_CODE, Lat, Long, Station_name)

# Some of the stations are very close together and it's impossible to click a single station
# Therefore, we move some points, and connect them to the original points using a line
# (this is just an example, this should be done also for other clusters of points)
dat_coord_moved <- tibble::tribble(
  ~STATION_CODE, ~Lat, ~Long,
  "I301", 62, 15,
  "30A",  61.5, 15,
  "I304", 61, 15,
  "30B",  60.5, 15,
  "31A",  60, 15,
) 

# Modify the data so we end up with two columns for the original coordinates (_orig),
#   and Long and Lat are modified for staations given by 'dat_coord_moved'  
# Both sets of coordinates will be used when plotting the map 
#   (black dot for the orginal coordinaytes, coloured dot for new coordinates, and a line between)
# Clicking on the *moved* points in the map affects 
dat_coord <- dat_coord %>%
  rename(
    Lat_orig = Lat, 
    Long_orig = Long) %>%
  # New ("moved") coordinates are added as new columns  
  left_join(dat_coord_moved, by = "STATION_CODE") %>%
  # Where there are no new coordinates given by 'dat_coord_moved', the original coordinates will be used
  mutate(
    Lat = ifelse(is.na(Lat), Lat_orig, Lat),
    Long = ifelse(is.na(Long), Long_orig, Long)
  ) 

# Median concentrations (median per year per time series)
# IN TEST SCRIPT: path changed (../.. removed) 
dat_med <- readRDS("Files_from_Jupyterhub_2020/Raw_data/110_mediandata_updated_2021-10-08.rds") %>%
  # 5 columns (old Coordinates, moved coordinates and Station_name) are added  
  left_join(dat_coord, by = "STATION_CODE") %>%
  # Keep only data for which we have coordinates 
  filter(!is.na(Lat))

# Stations (with coordinates) that are found in the data since 2020  
dat_stations <- dat_med %>%
  filter(MYEAR >= 2020) %>%
  distinct(STATION_CODE, Lat_orig, Long_orig, Lat, Long, Station_name, LATIN_NAME) %>%
  # For now we just delete Svalbard (to make the map fit better)
  filter(Lat_orig < 72)
# filter(Lat > 65 & Lat < 68)

#
# Map ----
#

# IN TEST SCRIPT: let all stations are available 
# In the app, stations_available() is a reactive function that returns
#   which stations that are available with the give selection of PARAM
dat_stations_available <- dat_stations 
#  filter(STATION_CODE %in% stations_avaliable())

gg <- ggplot(dat_stations_available, aes(Long, Lat)) +
  annotation_map(simple_map, fill = "lightgreen") +
  geom_point(aes(x = Long_orig, y = Lat_orig)) +
  geom_segment(aes(x = Long_orig, y = Lat_orig, xend = Long, yend = Lat)) +
  geom_point(aes(color = LATIN_NAME)) +
  geom_text(aes(label = STATION_CODE), hjust = 0, vjust = 0.25, nudge_x = 0.35) 
gg

#
# Stations to put in 'dat_coord_moved'
#

# These stations have been added to dat_coord_moved:
gg + coord_cartesian(xlim = c(10,15), ylim = c(59.3, 60))
# These stations should be added to dat_coord_moved:
gg + coord_cartesian(xlim = c(10,15), ylim = c(59.3, 60) - 0.7)     # outer Oslo fjord
gg + coord_cartesian(xlim = c(6.5,8.5), ylim = c(57.5, 58.5))       # South coast
gg + coord_cartesian(xlim = c(7.75,8.25), ylim = c(58, 58.25))      # South coast, detail
gg + coord_cartesian(xlim = c(5, 9), ylim = c(59, 61))              # West coast
gg + coord_cartesian(xlim = c(5, 6), ylim = c(60.25, 60.5))         # West coast, detail
gg + coord_cartesian(xlim = c(11, 17), ylim = c(66, 69))            # Nordland  
gg + coord_cartesian(xlim = c(14.6, 15.1), ylim = c(68.15, 68.27))  # Nordland, detail  
