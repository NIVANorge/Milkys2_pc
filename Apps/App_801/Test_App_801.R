
#
# Code for testing App_801
#

# library(shiny)
library(dplyr)
library(ggplot2)

RColorBrewer::display.brewer.all()

# Set working directory before reunning code
setwd(here::here("App_801"))

dat_med <- readRDS("../Files_from_Jupyterhub_2020/Raw_data/110_mediandata_updated_2021-10-08.rds")

params <- dat_med %>%
  filter(MYEAR == 2020) %>%
  distinct(PARAM) %>%
  pull(PARAM)

params_selected <- "CB180"
stations_selected <- "30A"

#
# Single parameter / station
#
dat_plot <- dat_med %>%
  filter(PARAM %in% params_selected,
         STATION_CODE %in% stations_selected,
         Basis == "WW") %>%
  mutate(
    Over_LOQ_perc = round(Over_LOQ/N_median*100,0),
    Over_LOQ_med = ifelse(Over_LOQ_perc >= 50, ">=50% over LOQ", "<50% over LOQ")
  )

ggplot(dat_plot, aes(MYEAR, Value)) +
  geom_line() +
  geom_point(aes(shape = Over_LOQ_med, fill = Over_LOQ_perc), size = 2) +
  scale_shape_manual(values = c(25,21)) +
  scale_fill_viridis_b() 

#
# Several parameters, stations
#
params_selected <- c("CB52", "CB180")

dat_select1 <- dat_med %>%
  filter(PARAM %in% params_selected,
         Basis == "WW")
  
min_no_years <- 3
min_no_years_start <- 2015
min_no_years_end <- 2020
stations_avaliable <- dat_select1 %>%
  filter(MYEAR %in% min_no_years_start:min_no_years_end) %>%
  distinct(STATION_CODE, MYEAR) %>%
  count(STATION_CODE) %>%
  filter(n >= min_no_years) %>%
  pull(STATION_CODE)

stations_avaliable

stations_selected <- c("30A", "97A2")

dat_plot <- dat_select1 %>%
  filter(STATION_CODE %in% stations_selected) %>%
  mutate(
    Over_LOQ_perc = round(Over_LOQ/N_median*100,0),
    Over_LOQ_med = ifelse(Over_LOQ_perc >= 50, ">=50% over LOQ", "<50% over LOQ")
  )

#
# Plot 1 - panel grid of STATION_CODE x PARAM, colours show percentage over LOQ
#
ggplot(dat_plot, aes(MYEAR, Value)) +
  geom_line() +
  geom_point(aes(shape = Over_LOQ_med, fill = Over_LOQ_perc), size = 2) +
  scale_shape_manual(values = c(25,21)) +
  scale_fill_viridis_b() +
  facet_grid(vars(STATION_CODE), vars(PARAM))

#
# Plot 2 - panel wrap of STATION_CODE, colours show PARAM
#
ggplot(dat_plot, aes(MYEAR, Value, group = PARAM)) +
  geom_line() +
  geom_point(aes(shape = Over_LOQ_med, fill = PARAM), size = 2) +
  scale_shape_manual(values = c(25,21)) +
  scale_fill_brewer(palette = "Set3") +
  facet_wrap(vars(STATION_CODE))

#
# Plot 3 - panel wrap of PARAM, colours show STATION_CODE
#
ggplot(dat_plot, aes(MYEAR, Value, group = STATION_CODE)) +
  geom_line() +
  geom_point(aes(shape = Over_LOQ_med, fill = STATION_CODE), size = 2) +
  scale_shape_manual(values = c(25,21)) +
  scale_fill_brewer(palette = "Set3") +
  facet_wrap(vars(PARAM))


# Set working directory back
setwd(here::here())
