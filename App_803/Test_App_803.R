
#
# Code for testing App_803
#

# Set working directory before reunning code
setwd(here::here("App_803"))

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 1. Startup A ----
# Code that will run one time (when app is starting up)
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(forcats)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 2. Pre-reading data ----
#    Pre-reading data and writing to two text files in app directory
#    The app will use these files  
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#
# . 2a. Raw data ----
#

dat_loc <- read_csv2("../../../CEMP/2016_Milkys/dbo_locality_ny.txt") %>%
  mutate(
    Station_name = paste(jmpst, stnam)
  )

# The following file cotains a few duplicates, which are fixed below  
dat_mas_orig <- read_csv2("../../../CEMP/2016_Milkys/dbo_fish_mas.txt") %>%
  rename(
    Year = myear,
    LATIN_NAME = speci
  ) %>%
  add_count(Year, seqno, rlabo)

# Check a very few dublettes in dat_mas
# dat_mas_orig %>%
#   arrange(desc(n), Year, seqno, rlabo) %>%
#   filter(n >= 2) %>%
#   View("dat_mas")

# Remove dublettes
dat_mas <- bind_rows(
  dat_mas_orig %>% filter(n == 1),                    # these are ok (non-dublettes)
  dat_mas_orig %>% filter(n >= 2) %>%                 # dublettes; removed by using first values (see commented part for check) 
    group_by(Year, seqno, rlabo) %>%
    summarise(across(.fn = first), .groups = "drop"),
)

dat_spe <- read_csv2("../../../CEMP/2016_Milkys/dbo_fish_spe.txt") %>%
  rename(
    Year = myear
  )

dat_tis <- read_csv2("../../../CEMP/2016_Milkys/dbo_fish_tis.txt") %>%
  rename(
    Year = myear,
    TISSUE_NAME = tissu
  )

# All concentrations, including some data with 'NA' in  value field 'valsnf'   
dat_con_orig <- read_csv2("../../../CEMP/2016_Milkys/dbo_fish_con.txt") %>%
  rename(
    Year = myear,
    PARAM = param,
    Basis = basis,
    TISSUE_NAME = tissu
  ) %>%
  mutate(
    value = as.numeric(valsnf)
  )

# Check
# dat_con %>% filter(!is.na(valsnf) & is.na(value)) %>% View()

# Remove NA values  
dat_con <- dat_con_orig %>%
  filter(!is.na(value))

dat <- dat_con %>%
  left_join(dat_tis, by = c("Year", "seqno", "rlabo", "subno", "TISSUE_NAME")) %>%
  left_join(dat_spe, by = c("Year", "seqno", "rlabo", "subno")) %>%
  left_join(dat_mas, by = c("Year", "seqno", "rlabo")) %>%
  left_join(dat_loc %>% select(jmpst, Station_name), by = "jmpst") %>%
  filter(Year >= 1980)


# Write to file (to be used by app)
fn_raw <- "dat.csv"
write_csv(dat, fn_raw)

#
# . 2b. Summary data ----
#   App reads from file made below, if it exists and 'update_summary_table' is set to FALSE
#

fn_summ <- "dat_summ.csv"
update_summary_table <- FALSE
# update_summary_table <- TRUE

if (!file.exists(fn_summ) | update_summary_table){
  
  library(dtplyr)
  # help(package = "dtplyr")
  
  dat_dt <- lazy_dt(dat)
  
  dat_summ <- dat %>%
    group_by(PARAM, Basis, Station_name, Year) %>%
    summarize(
      N = n(),
      Over_LOQ = sum(is.na(qflag)),
      Prop_over_LOQ = Over_LOQ/N,
      Median_value = median(value)
    ) %>%
    mutate(Station_name = factor(Station_name)) %>%
    group_by(PARAM, Basis, Station_name) %>%
    mutate(
      No_years = n(),
      Last_year = max(Year)
    )
  
  # Write to file (to be used by app)
  write_csv(dat_summ, fn_summ)
  
} else {

  dat_summ <- read_csv(fn_summ)
  
}  

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 3. Checking data ----
#    Not included in app
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

dat %>%
  filter(jmpst == "56A" & PARAM == "CB138") %>% View("56A")

dat %>%
  filter(jmpst %in% c("51A", "52A", "56A", "57A") & PARAM == "CB138") %>% View("56A")

# RColorBrewer::display.brewer.all()

#
# Species
#
tab <- table(addNA(dat$LATIN_NAME)) %>% sort(decreasing = TRUE)
tab

dat %>%
  count(Year, LATIN_NAME) %>%
  ggplot(aes(Year, LATIN_NAME, fill = n)) +
  geom_tile() +
  scale_fill_viridis_b()

# Basis
table(addNA(dat$Basis))

# Basis = dry weight
xtabs(~ Year + TISSUE_NAME, dat %>% filter(Basis == "D"))

# Basis = dry weight, each matrix
xtabs(~ Year + addNA(PARAM) + TISSUE_NAME, dat %>% filter(Basis %in% "D" & TISSUE_NAME == "LI"))
xtabs(~ Year + addNA(PARAM) + TISSUE_NAME, dat %>% filter(Basis %in% "D" & TISSUE_NAME == "MU"))
xtabs(~ Year + addNA(PARAM) + TISSUE_NAME, dat %>% filter(Basis %in% "D" & TISSUE_NAME == "SB"))
xtabs(~ Year + addNA(PARAM) + TISSUE_NAME + LATIN_NAME, dat %>% filter(Basis %in% "D" & TISSUE_NAME == "TM"))

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 4. Startup B ----
# Code that will run one time (when app is starting up)
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

species_available <- dat %>%
  count(LATIN_NAME) %>%
  arrange(desc(n)) %>%
  mutate(LATIN_NAME = factor(LATIN_NAME) %>% fct_inorder) %>%
  pull(LATIN_NAME)

params_available <- dat %>%
  distinct(PARAM) %>%
  pull(PARAM)

stations_available <- dat %>%
  distinct(Station_name) %>%
  pull(Station_name)

basis_available <- dat %>%
  distinct(Basis) %>%
  pull(Basis)  

grep("30A", stations_available, value = TRUE)

params_selected <- "CB180"
stations_selected <- "30A Gressholmen"
basis_selected <- "W"

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 5. UI common code ----
# Code that will run one time (when app is starting up)
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

input <- list()
input$species_selected <- "MYTI EDU")   
input$params_selected <- c("CB28", "CB52", "CB101", "CB118", "CB138", "CB153", "CB180")
input$basis_selected <- "W"

#
# One parameter, all stations  
#

dat_plot1 <- dat %>%
  filter(determinand %in% params_selected) %>%
  group_by(station_name, Year) %>%
  summarize(
    N = n(),
    Over_LOQ = sum(is.na(qflag)),
    Prop_over_LOQ = Over_LOQ/N,
    Median_value = median(value)
    # Median_value_over_LOQ = median(value[is.na(qflag)])
  ) %>%
  mutate(station_name = factor(station_name)) %>%
  group_by(station_name) %>%
  mutate(
    No_years = n(),
    Last_year = max(Year))

levels(dat_plot2$station_name)

min_no_years <- 5
series_lasting_until <- 2010

dat_plot1 %>%
  mutate(station_name = fct_rev(station_name)) %>%
  filter(No_years >= min_no_years & Last_year >= series_lasting_until) %>%
  ggplot(aes(Year, station_name)) +
  geom_raster(aes(fill = log10(Median_value))) +
  scale_fill_viridis_c() +
  geom_point(data = dat_plot1 %>% filter(Prop_over_LOQ > 0.5 & No_years >= min_no_years & Last_year >= series_lasting_until))


#
# Single parameter / station
#
dat_plot2 <- dat %>%
  filter(determinand %in% params_selected,
         basis %in% basis_selected,
         station_name %in% stations_selected) %>%
  mutate(LOQ = ifelse(is.na(qflag), "Over LOQ", "Under LOQ")) 

# dat_plot <- dat_med %>%
#   filter(PARAM %in% params_selected,
#          STATION_CODE %in% stations_selected,
#          Basis == "WW") %>%
#   mutate(
#     Over_LOQ_perc = round(Over_LOQ/N_median*100,0),
#     Over_LOQ_med = ifelse(Over_LOQ_perc >= 50, ">=50% over LOQ", "<50% over LOQ")
#   )

ggplot(dat_plot2, aes(Year, value)) +
  geom_smooth(se = FALSE) +
  geom_point(aes(shape = LOQ, color = basis), size = 2) +
  scale_shape_manual(values = c(19,25)) +
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
