
#
# Code for testing App_802
# NOTE: this also has code for creating the files read by he app (part 2 below)
#

# Set working directory before rerunning code
setwd(here::here("App_802"))


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
#  . Raw data ----
#  App reads from file made below
#

#
# NOTE: The files used (and made below) are too big to put on Github
# NIVA people can find them here: 'K:\Avdeling\214-Oseanografi\DHJ\Data\Milkys_ICES_files'
#  
dat_orig <- read_csv("../Files_to_ICES/Data_from_ICES/data_extraction_211008/Norway.csv") %>%
  rename(
    LATIN_NAME = species,
    TISSUE_NAME = matrix,
    PARAM = determinand,
    MYEAR = year
  ) %>%
  mutate(
    Station_name = case_when(
      !is.na(station_name) ~ station_name,
      is.na(station_name) ~ submitted.station),
    Basis = case_when(
      is.na(basis) ~ "None",
      TRUE ~ basis)
  )

# 
# Get rid of duplicates (see section '3B Check duplicates' below)
#
dat_orig_with_count <- dat_orig %>%
  add_count(MYEAR, PARAM, Station_name, TISSUE_NAME, smpno, subno, sub.sample)
table(dat_orig_with_count$n)
#      1      2 
# 545988   1420

# For duplicates: take the mean  the 
dat_orig_with_dupl <- dat_orig_with_count %>%
  filter(n > 1) %>%
  group_by(MYEAR, PARAM, Station_name, TISSUE_NAME, smpno, subno, sub.sample) %>%
  arrange(is.na(uncertainty)) %>%                   # sort so the values WITH uncertainty comes first (only makes a difference for DRYWT 2017)
  mutate(value = mean(value, na.rm = TRUE)) %>%     # mean of the two values for the same sample 
  summarise(across(.fn = first), .groups = "drop")

dat_orig_without_dupl <- dat_orig_with_count %>%
  filter(n == 1)

dat_duplicates_removed <- bind_rows(
  dat_orig_with_dupl %>% select(-n),
  dat_orig_without_dupl %>% select(-n)
)

#
# Make data sets for lipid and dry weight percentage (for adding as extra columns)
#
dat_lipid <- dat_duplicates_removed %>% 
  filter(PARAM %in% c("FATWT%", "EXLIP%")) %>%
  select(MYEAR, PARAM, Station_name, TISSUE_NAME, smpno, subno, sub.sample, value) %>%
  rename(Fat_perc = value) %>%
  select(-PARAM)

dat_dryweight <- dat_duplicates_removed %>% 
  filter(PARAM %in% c("DRYWT%")) %>%
  select(MYEAR, PARAM, Station_name, TISSUE_NAME, smpno, subno, sub.sample, value) %>%
  rename(Dry_perc = value) %>%
  select(-PARAM)

#
# Add columns for lipid and dry weight percentage
# - and add concentration on wet-weight, lipid and dry weight basis
#

dat <- dat_duplicates_removed %>%
  left_join(dat_lipid) %>%
  left_join(dat_dryweight) %>%
  rename(VALUE_ORIG = value) %>%
  mutate(
    VALUE_WW = case_when(
      Basis == "W" | PARAM %in% c("FATWT%", "EXLIP%","DRYWT%") ~ VALUE_ORIG,
      Basis == "D" ~ VALUE_ORIG*(Dry_perc/100),
      Basis == "F" ~ VALUE_ORIG/(Fat_perc/100)
    ),
    VALUE_DW = case_when(
      Basis == "W" ~ VALUE_ORIG/(Dry_perc/100),
      Basis == "D" ~ VALUE_ORIG,
      Basis == "F" ~ VALUE_ORIG*(Fat_perc/100)/(Dry_perc/100)
      ),
    VALUE_FB = case_when(
      Basis == "W" ~ VALUE_ORIG/(Fat_perc/100),
      Basis == "D" ~ VALUE_ORIG*(Dry_perc/100)/(Fat_perc/100),
      Basis == "F" ~ VALUE_ORIG
    )
  )



fn <- "../Files_to_ICES/Data_from_ICES/data_extraction_211008/Norway_added_columns.csv"
# Uncomment to overwrite (takes 30-40 seconds)
# write_csv(dat, fn)


#
#  . Summary data ----
#  App reads from file made below
#

fn_summ <- "../Files_to_ICES/Data_from_ICES/data_extraction_211008/Norway_summ.csv"
update_summary_table <- FALSE
# update_summary_table <- TRUE

if (!file.exists(fn_summ) | update_summary_table){
  
  library(dtplyr)
  # help(package = "dtplyr")
  
  dat_dt <- lazy_dt(dat)
  
  dat_summ <- dat %>%
    group_by(PARAM, Basis, Station_name, TISSUE_NAME, MYEAR) %>%
    summarize(
      N = n(),
      Over_LOQ = sum(is.na(qflag)),
      Prop_over_LOQ = Over_LOQ/N,
      Value_orig_median = median(VALUE_ORIG),
      Value_wet_median = median(VALUE_WW),
      Value_dry_median = median(VALUE_DW),
      Value_lip_median = median(VALUE_FB),
      Value_orig_min = min(VALUE_ORIG),
      Value_wet_min = min(VALUE_WW),
      Value_dry_min = min(VALUE_DW),
      Value_lip_min = min(VALUE_FB),
      Value_orig_max = max(VALUE_ORIG),
      Value_wet_max = max(VALUE_WW),
      Value_dry_max = max(VALUE_DW),
      Value_lip_max = max(VALUE_FB),
      Value_orig_min_overloq = min(VALUE_ORIG[is.na(qflag)], na.rm = TRUE),
      Value_wet_min_overloq = min(VALUE_WW[is.na(qflag)], na.rm = TRUE),
      Value_dry_min_overloq = min(VALUE_DW[is.na(qflag)], na.rm = TRUE),
      Value_lip_min_overloq = min(VALUE_FB[is.na(qflag)], na.rm = TRUE),
      Value_orig_max_underloq = max(VALUE_ORIG[!is.na(qflag)], na.rm = TRUE),
      Value_wet_max_underloq = max(VALUE_WW[!is.na(qflag)], na.rm = TRUE),
      Value_dry_max_underloq = max(VALUE_DW[!is.na(qflag)], na.rm = TRUE),
      Value_lip_max_underloq = max(VALUE_FB[!is.na(qflag)], na.rm = TRUE)
    ) %>%
    mutate(
      Station_name = factor(Station_name),
      Speciesgroup = case_when(
        grepl("[0-9]+B", Station_name) ~ "Cod",
        grepl("[0-9]+F", Station_name) ~ "Flatfish",
        grepl("[0-9]+A", Station_name) ~ "Blue mussel",
        grepl("[0-9]+X", Station_name) ~ "Blue mussel",
        grepl("I[0-9]+", Station_name) ~ "Blue mussel",
        grepl("305", Station_name) ~ "Blue mussel",
        grepl("[0-9]+G", Station_name) ~ "Snail",
        grepl("[0-9]+N", Station_name) ~ "Eider duck",
        grepl("[0-9]+C", Station_name) ~ "Shrimp",
        TRUE ~ "Others")
      ) %>%
    group_by(PARAM, Basis, Station_name) %>%
    mutate(
      No_years = n(),
      Last_year = max(MYEAR)
    )
  
  write_csv(dat_summ, fn_summ)
  
} else {

  dat_summ <- read_csv("../Files_to_ICES/Data_from_ICES/data_extraction_211008/Norway_summ.csv")
  
}  


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 3A. Checking data ----
#    Not included in app
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


if (FALSE){
  
  
  dat %>%
    filter(MYEAR == 2020) %>%
    View()
  
  dat %>%
    filter(MYEAR == 2000 & Station_name == "36B Færder area" & PARAM == "EXLIP%") %>% nrow()
  
  dat_lipid %>%
    filter(MYEAR == 2000 & Station_name == "36B Færder area") %>% #View()
    xtabs(~smpno + subno, .)
  dat_lipid %>%
    filter(MYEAR == 2000 & Station_name == "36B Færder area") %>% #View()
    xtabs(~replicate, .)
  dat_orig %>%
    filter(MYEAR == 2000 & Station_name == "36B Færder area" & PARAM == "HCHA") %>% # View()
    xtabs(~smpno + subno, .)
  dat_orig %>%
    filter(MYEAR == 2000 & Station_name == "36B Færder area" & PARAM == "HCHA") %>% # View()
    xtabs(~replicate, .)
  
  dat_orig %>%
    filter(MYEAR == 2000 & Station_name == "36B Færder area" & PARAM %in% c("HCHA","EXLIP%")) %>% View()
  
  
  # Basis
  table(addNA(dat$Basis))
  
  # Stations with several species (flatfish)
  dat %>% filter(grepl("[0-9]+F", Station_name)) %>% xtabs(~Station_name + LATIN_NAME, .)
  
  dat %>% filter(grepl("fat", Station_name)) %>% xtabs(~Station_name + LATIN_NAME, .)
  
  dat %>% filter(grepl("[0-9]+F", Station_name)) %>% xtabs(~TISSUE_NAME + LATIN_NAME, .)
  
  # Tissues, organics
  tab <- dat %>% filter(PARAM == "CB118") %>% xtabs(~LATIN_NAME + TISSUE_NAME, .)
  tab <- tab[rev(order(apply(tab,1,sum))),]
  tab <- tab[,rev(order(apply(tab,2,sum)))]
  tab2 <- tab
  for (i in 1:ncol(tab2)) tab2[,i] <- round(tab2[,i]/apply(tab,1,sum), 3)
  
  tab <- dat %>% xtabs(~LATIN_NAME + TISSUE_NAME, .)
  tab <- tab[rev(order(apply(tab,1,sum))),]
  tab <- tab[,rev(order(apply(tab,2,sum)))]
  tab
  
  # Metals  
  params <- dat %>% xtabs(~PARAM, .) %>% names()
  params[nchar(params) == 2]
  
  # Fat weight  
  params <- dat %>% xtabs(~PARAM, .) %>% names()
  grep("fat", params, ignore.case = TRUE, value = TRUE)
  grep("lip", params, ignore.case = TRUE, value = TRUE)
  xtabs(~MYEAR + PARAM, dat %>% filter(PARAM %in% c("FATWT%", "EXLIP%")))
  dat %>% 
    filter(MYEAR == 2020 & PARAM %in% c("FATWT%", "EXLIP%")) %>% View()
  
  # Dry weight  
  grep("dry", params, ignore.case = TRUE, value = TRUE)
  
  
  
}


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 3B. Check duplicates ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# wrap the entire chapter in an if, for simple restart and run of the entire thing
if (FALSE){
  
  # replicate cannot be used
  #  - it is unique for samples, but different for different parameters in the same sample  
  # (not demonstrated here)
  
  # smpno and subno isn't enough
  if (FALSE)
    dat_orig %>%
    count(MYEAR, PARAM, Station_name, TISSUE_NAME, smpno, subno) %>%
    filter(n > 1) %>%
    xtabs(~MYEAR, .)
  # MYEAR
  # 1983 1985 1986 1987 2008 2015 2017 
  #   90   12   27   21   25   69  631
  
  # adding sub.sample still isn't enough
  if (FALSE)
    dat_orig %>%
    count(MYEAR, PARAM, Station_name, TISSUE_NAME, smpno, subno, sub.sample) %>%
    filter(n > 1) %>%
    xtabs(~MYEAR, .)
  # 1983 1985 1986 1987 2008 2017 
  #   90   12   27   21   25  535 
  
  #
  # The problem with 2017 data is duplicates of DRYWT%
  #
  dat_orig %>%
    filter(MYEAR %in% 2017) %>%
    count(MYEAR, PARAM, Station_name, TISSUE_NAME, smpno, subno, sub.sample) %>%
    filter(n > 1) %>%
    xtabs(~PARAM + Station_name, .)
  # ... the two values are always the same
  dat_orig %>%
    filter(MYEAR %in% 2017 & PARAM %in% "DRYWT%") %>%
    group_by(MYEAR, PARAM, Station_name, TISSUE_NAME, smpno, subno, sub.sample) %>%
    summarise(Value_diff = diff(range(value))) %>%
    # View()
    filter(Value_diff > 0) %>%
    nrow()
  # But only one has limit_detection and uncertainty given ()
  dat_orig %>%
    filter(MYEAR %in% 2017 & Station_name == "10A2 Skallneset" & PARAM %in% c("CD", "DRYWT%")) %>%
    add_count(MYEAR, PARAM, Station_name, TISSUE_NAME, smpno, subno, sub.sample) %>% 
    arrange(smpno, subno, sub.sample, PARAM) %>% 
    View("2017 example")
  # (same, shown for all values)
  dat_orig %>%
    filter(MYEAR %in% 2017 & PARAM %in% "DRYWT%") %>%
    group_by(MYEAR, PARAM, Station_name, TISSUE_NAME, smpno, subno, sub.sample) %>%
    summarise(N_limit_detection = sum(!is.na(uncertainty))) %>%
    # View()
    xtabs(~N_limit_detection, .)
  
  # SOLUTION: pick the DRYWT% with a value given for 'uncertainty'  
  # - or take the mean 
  
  dat_orig_with_dupl %>% # names()
    filter(MYEAR %in% 2017 & PARAM %in% "DRYWT%") %>%
    select(MYEAR, Station_name, TISSUE_NAME, smpno, subno, sub.sample, PARAM, TISSUE_NAME, Basis, value, uncertainty) %>% View("EROD 2008")
  
  
  
  #
  # The problem with 2008 data is duplicates of EROD in 30B Oslo City area
  #
  dat_orig %>%
    filter(MYEAR %in% 2008) %>%
    count(MYEAR, PARAM, Station_name, TISSUE_NAME, smpno, subno, sub.sample) %>%
    filter(n > 1) %>%
    xtabs(~PARAM + Station_name, .)
  # ... the two values are always the same
  dat_orig %>%
    filter(MYEAR %in% 2008 & PARAM %in% "EROD") %>%
    group_by(MYEAR, PARAM, Station_name, TISSUE_NAME, smpno, subno, sub.sample) %>%
    summarise(Value_diff = diff(range(value))) %>%
    # View()
    filter(Value_diff > 0) %>%
    nrow()
  # Example (limit_detection and uncertainty not given)  
  dat_orig %>%
    filter(MYEAR %in% 2008 & PARAM %in% "EROD") %>%
    add_count(MYEAR, PARAM, Station_name, TISSUE_NAME, smpno, subno, sub.sample) %>% 
    filter(n > 1) %>%
    arrange(smpno, subno, sub.sample, PARAM) %>% 
    View("EROD 2008 example")
  # (same, shown for all values)
  
  # SOLUTION: just pick the first one
  # - or take the mean
  
  dat_orig_with_dupl %>% # names()
    filter(MYEAR %in% 2008 & PARAM %in% "EROD") %>%
    select(MYEAR, Station_name, TISSUE_NAME, smpno, subno, sub.sample, PARAM, TISSUE_NAME, Basis, value) %>% View("EROD 2008")
  
  
  #
  # For the 1980s, varying stations (but 33F is several times)
  #
  for (yr in c(1983, 1985:1987)){
    cat("======", yr, "======\n")
    print(
      dat_orig %>%
        filter(MYEAR %in% yr) %>%
        count(MYEAR, PARAM, Station_name, TISSUE_NAME, smpno, subno, sub.sample) %>%
        filter(n > 1) %>% 
        xtabs(~PARAM + Station_name, .) 
    )
  }
  
  # ... the two values are NOT always the same (example year)
  dat_orig %>%
    filter(MYEAR %in% 1987) %>%
    group_by(MYEAR, PARAM, Station_name, TISSUE_NAME, smpno, subno, sub.sample) %>%
    summarise(N = n(), Value_diff = diff(range(value))) %>%
    # View()
    filter(Value_diff > 0) %>%
    xtabs(~PARAM + Station_name, .) 
  
  # ... the two values are NOT always the same (example)
  dat_orig %>% # names()
    filter(MYEAR %in% 1987 & PARAM %in% "CD") %>%
    group_by(MYEAR, PARAM, Station_name, TISSUE_NAME, smpno, subno, sub.sample) %>%
    mutate(N = n(), Value_diff = diff(range(value))) %>%
    filter(N > 1) %>%
    arrange(MYEAR, Station_name, TISSUE_NAME, smpno, subno, sub.sample, PARAM) %>%
    select(MYEAR, Station_name, TISSUE_NAME, smpno, subno, sub.sample, PARAM, TISSUE_NAME, Basis, value)
  
  # SOLUTION: take the mean
  
  dat_orig_with_dupl %>% # names()
    filter(MYEAR %in% 1987 & PARAM %in% "CD") %>%
    select(MYEAR, Station_name, TISSUE_NAME, smpno, subno, sub.sample, PARAM, TISSUE_NAME, Basis, value)
  
  
  
}


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

speciesgroup_available <- dat_summ %>%
  count(Species) %>%
  arrange(desc(n)) %>%
  mutate(Species = factor(Species) %>% fct_inorder) %>%
  pull(Species)

params_available <- dat %>%
  distinct(PARAM) %>%
  pull(PARAM)

stations_available <- dat %>%
  distinct(Station_name) %>%
  pull(Station_name)

basis_available <- dat %>%
  distinct(Basis) %>%
  pull(Basis)  




#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 5. UI common code ----
# Code that will run one time (when app is starting up)
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

input <- list()
input$params_selected <- c("CB28", "CB52", "CB101", "CB118", "CB138", "CB153", "CB180")
input$basis_selected <- "W"
input$valuebasis_selected <- "Wet weight basis"

input$params_selected <- "HCHA"
input$basis_selected <- "W"
input$valuebasis_selected <- "Wet weight basis"

#
# . Plot 1 (overview) ----
#

# From menus
input$speciesgroup_selected <- "Blue mussel"
input$speciesgroup_selected <- "Cod"
input$min_no_years <- 5
input$p1_show_50perc_overLOQ <- TRUE

# Code in app, unchanged
dat_plot1 <- dat_summ %>%
  filter(PARAM %in% input$params_selected,
         Basis %in% input$basis_selected,
         Speciesgroup %in% input$speciesgroup_selected)

if (input$valuebasis_selected == "Wet weight basis"){
  dat_plot1$Value_plot <- dat_plot1$Value_wet_median
} else  if (input$valuebasis_selected == "Dry weight basis"){
  dat_plot1$Value_plot <- dat_plot1$Value_dry_median
} else if (input$valuebasis_selected == "Lipid weight basis"){
  dat_plot1$Value_plot <- dat_plot1$Value_lip_median
} else if (input$valuebasis_selected == "Original basis"){
  dat_plot1$Value_plot <- dat_plot1$Value_orig_median
}

# browser()

# min_no_years <- 1
series_lasting_until <- 1980

gg <- dat_plot1 %>%
  mutate(Station_name = fct_rev(Station_name)) %>%
  filter(No_years >= input$min_no_years,
         Last_year >= series_lasting_until) %>%
  ggplot(aes(MYEAR, Station_name)) +
  geom_raster(aes(fill = log10(Value_plot))) +
  scale_fill_viridis_c("log10(concentration)") +
  facet_grid(rows = vars(PARAM)) +
  labs(x = "Year", y = "Station")

if (input$p1_show_50perc_overLOQ)
  gg <- gg +   geom_point(data = dat_plot1 %>% filter(Prop_over_LOQ > 0.5, 
                                                      No_years >= input$min_no_years,
                                                      Last_year >= series_lasting_until))

gg   


#
# . Plot 2 (time series) ----
#

# From menus
input$species_selected <- "Gadus morhua"
input$tissues_selected <- "LI"
input$stations_selected <- "36B Færder area"

input$number_of_points <- FALSE
input$max_y <- 0
input$range_x <- c(1980,2020)


input$species_selected <- "Mytilus edulis"
input$tissues_selected <- "SB"
input$stations_selected <- "30A Gressholmen"

input$params_selected <- "HCHA"
input$params_selected <- "EXLIP%"
input$valuebasis_selected <- "Wet weight basis"
input$valuebasis_selected <- "Lipid weight basis"
  
# Code in app, unchanged

dat_plot2 <- dat %>%
  filter(PARAM %in% input$params_selected,
         Basis %in% input$basis_selected,
         LATIN_NAME %in% input$species_selected,
         TISSUE_NAME %in% input$tissues_selected,
         Station_name %in% input$stations_selected) %>%
  mutate(
    LOQ = ifelse(is.na(qflag), "Over LOQ", "Under LOQ"),
    Series = paste0(LATIN_NAME, ", ", 
                    TISSUE_NAME, ", ", 
                    Basis, "-basis")
  )

if (input$valuebasis_selected == "Wet weight basis"){
  dat_plot2$Value_plot <- dat_plot2$VALUE_WW
} else  if (input$valuebasis_selected == "Dry weight basis"){
  dat_plot2$Value_plot <- dat_plot2$VALUE_DW
} else if (input$valuebasis_selected == "Lipid weight basis"){
  dat_plot2$Value_plot <- dat_plot2$VALUE_FB
} else if (input$valuebasis_selected == "Original value"){
  dat_plot2$Value_plot <- dat_plot2$VALUE_ORIG
}


# browser()

gg <- ggplot(dat_plot2, aes(MYEAR, Value_plot)) +
  geom_smooth(se = FALSE, method = 'loess', formula = 'y ~ x') +
  geom_point(aes(shape = LOQ, color = Series), size = 2) +
  scale_shape_manual(values = c(19,25)) +
  scale_fill_viridis_b() +
  facet_grid(vars(Station_name), vars(PARAM)) +
  labs(y = paste0("Concentration (", tolower(input$valuebasis_selected), ")"))
if (input$number_of_points)
  gg <- gg +
  geom_text(
    data = dat_plot2 %>% count(MYEAR, Value_plot) %>% filter(n > 1),  
    aes(x = MYEAR + 0.3, label = n))
if (input$max_y != 0){
  if (input$range_x[1] == 1980 & input$range_x[2] == 2020){
    gg <- gg + 
      coord_cartesian(ylim = c(0, input$max_y))
  } else {
    gg <- gg + 
      coord_cartesian(xlim = input$range_x, ylim = c(0, input$max_y))
  }
} else {
  if (input$range_x[1] != 1980 | input$range_x[2] != 2020){
    gg <- gg + 
      coord_cartesian(xlim = input$range_x)
  }
}
gg  


#
# . Plot 3 (scatter/correlation plot) ----
#

# Same parameter, different basis
input$p3_params_selected_x <- "HCHA"
input$p3_params_selected_y <- "HCHA"
input$p3_valuebasis_selected_x <- "Wet weight basis"
input$p3_valuebasis_selected_y <- "Lipid weight basis"

# Parameter on wet weight vs fat percentage  
input$p3_params_selected_x <- c("EXLIP%", "FATWT%")
input$p3_params_selected_y <- "HCHA"
input$p3_valuebasis_selected_x <- "Wet weight basis"
input$p3_valuebasis_selected_y <- "Wet weight basis"

input$p3_select_years <- seq(1980,2020)

dat_plot3a <- dat %>%
  filter(PARAM %in% c(input$p3_params_selected_x, input$p3_params_selected_y),
         LATIN_NAME %in% input$species_selected,
         TISSUE_NAME %in% input$tissues_selected,
         Station_name %in% input$stations_selected) %>%
  mutate(
    LOQ = ifelse(is.na(qflag), "Over LOQ", "Under LOQ"),
    Series = paste0(LATIN_NAME, ", ", 
                    TISSUE_NAME, ", ", 
                    Basis, "-basis")
  )

# xtabs(~MYEAR + PARAM, dat_plot3b)
# dat_plot3b %>% filter(MYEAR == 2000) %>% View()

if (input$p3_valuebasis_selected_x == "Wet weight basis"){
  dat_plot3a$Value_plot_x <- dat_plot3a$VALUE_WW
} else  if (input$p3_valuebasis_selected_x == "Dry weight basis"){
  dat_plot3a$Value_plot_x <- dat_plot3a$VALUE_DW
} else if (input$p3_valuebasis_selected_x == "Lipid weight basis"){
  dat_plot3a$Value_plot_x <- dat_plot3a$VALUE_FB
} else if (input$p3_valuebasis_selected_x == "Original value"){
  dat_plot3a$Value_plot_x <- dat_plot3a$VALUE_ORIG
}

if (input$p3_valuebasis_selected_y == "Wet weight basis"){
  dat_plot3a$Value_plot_y <- dat_plot3a$VALUE_WW
} else  if (input$p3_valuebasis_selected_y == "Dry weight basis"){
  dat_plot3a$Value_plot_y <- dat_plot3a$VALUE_DW
} else if (input$p3_valuebasis_selected_y == "Lipid weight basis"){
  dat_plot3a$Value_plot_y <- dat_plot3a$VALUE_FB
} else if (input$p3_valuebasis_selected_y == "Original value"){
  dat_plot3a$Value_plot_y <- dat_plot3a$VALUE_ORIG
}


df_x <- dat_plot3a %>%
  filter(PARAM %in% input$p3_params_selected_x) %>%
  select(PARAM, MYEAR, Basis, LATIN_NAME, TISSUE_NAME, Station_name, smpno, subno, sub.sample, Value_plot_x, LOQ) %>%
  mutate(Under_LOQ_x = ifelse(LOQ == "Under LOQ", "x", ""))
df_y <- dat_plot3a %>%
  filter(PARAM %in% input$p3_params_selected_y) %>%
  select(PARAM, MYEAR, Basis, LATIN_NAME, TISSUE_NAME, Station_name, smpno, subno, sub.sample, Value_plot_y, LOQ) %>%
  mutate(Under_LOQ_y = ifelse(LOQ == "Under LOQ", "y", ""))

# df_x %>% filter(MYEAR == 2000) %>% View()
df_x %>% filter(MYEAR == 2000) %>% head()
df_y %>% filter(MYEAR == 2000) %>% head()

dat_plot3b <- inner_join(
  df_x %>% select(-LOQ), 
  df_y %>% select(-LOQ),
  by = c("MYEAR", "LATIN_NAME", "TISSUE_NAME", "Station_name", "smpno", "subno", "sub.sample")) %>%
  mutate(
    Under_LOQ = case_when(
      Under_LOQ_x == "" & Under_LOQ_y == "" ~ "None under",
      TRUE ~ paste0(Under_LOQ_x, Under_LOQ_y, " under")
    )
  )

dat_plot3b <- dat_plot3b %>%
  filter(MYEAR %in% input$p3_select_years)

# xtabs(~MYEAR, dat_plot3b)
# stringi::stri_enc_toutf32("x")
# stringi::stri_enc_toutf32("y")


paramtext_x <- paste(input$p3_params_selected_x, collapse = ", ")
paramtext_y <- paste(input$p3_params_selected_y, collapse = ", ")

gg1 <- ggplot(dat_plot3b, aes(Value_plot_x, Value_plot_y)) +
  geom_smooth(se = FALSE, method = 'lm', formula = 'y ~ x') +
  geom_point(aes(shape = Under_LOQ, color = MYEAR), size = 2) +
  scale_shape_manual(values = c(`None under` = 19, `x under` = 120, `y under` = 121, `xy under` = 6)) +
  scale_color_viridis_c() +
  labs(
    x = paste0(paramtext_x, " (", input$p3_valuebasis_selected_x, ")"),
    y = paste0(paramtext_y, " (", input$p3_valuebasis_selected_y, ")")
  )
gg1 

gg2 <- ggplot(dat_plot3b, aes(Value_plot_x, Value_plot_y)) +
  geom_smooth(se = FALSE, method = 'lm', formula = 'y ~ x') +
  geom_point(aes(shape = Under_LOQ), size = 2) +
  scale_shape_manual(values = c(`None under` = 19, `x under` = 120, `y under` = 121, `xy under` = 6)) +
  labs(
    x = paste0(paramtext_x, " (", input$p3_valuebasis_selected_x, ")"),
    y = paste0(paramtext_y, " (", input$p3_valuebasis_selected_y, ")")
  )
gg2 + facet_wrap(vars(MYEAR))
gg2 + facet_wrap(vars(MYEAR), scales = "free_y")
gg2 + facet_wrap(vars(MYEAR), scales = "free")



# Set working directory back
setwd(here::here())


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 6. APPENDIX ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


#
# Data downloaded via OHAT page, link 'DOME raw data extractions' (https://www.ices.dk/sites/pub/Publication%20Reports/Forms/DispForm.aspx?ID=37129)  
# See https://dome.ices.dk/ohat/?assessmentperiod=2021  
#

if (FALSE){
  
  #
  # . Read published raw data and save the Norwegian part ----
  #
  # Do this only once -  reads all data and saves the Norwegian ones 
  
  fn <- "Files_to_ICES/Data_from_ICES/OSPAR_MIME_data_extraction_2021/OSPAR_MIME_AMAP_Biota_contaminants_effects_20210201/OSPAR_MIME_AMAP_Biota_contaminants_effects_20210201.txt"
  
  # Checking
  readLines(fn, n = 10, encoding = "UTF-8", skipNul = TRUE)
  
  # The file Didn't work
  # readr::read_tsv(fn, n_max = 10)
  # readr::read_delim(fn, delim = "\t", n_max = 10)
  
  # Didn't work (tried to read Norway only; see https://www.r-bloggers.com/2017/02/how-to-import-a-subset-of-a-too-huge-csv-file/)
  # library(sqldf)
  # dat <- sqldf::read.csv.sql(fn, header = TRUE, sep = "\t", sql = "select * from file where ÿþCountry = '\"Norway\"'")
  # dat <- sqldf::read.csv.sql(fn, header = TRUE, sep = "\t", sql = "select * from file where StationName = '\"14-S\"'")
  
  # Worked (test read)
  df <- read.table(fn, header = TRUE, sep = "\t", quote = "", nrows = 10, skipNul = TRUE)
  str(df)
  
  # Read all (40 seconds)  
  t0 <- Sys.time()
  dat_all_countries <- read.table(fn, header = TRUE, sep = "\t", quote = "", skipNul = TRUE)
  t1 <- Sys.time()
  t1-t0
  str(dat_all_countries)
  names(dat_all_countries)[1] <- "Country"
  
  dat <- subset(dat_all_countries, Country == "Norway")
  
  fn_save <- "Files_to_ICES/Data_from_ICES/OSPAR_MIME_data_extraction_2021/OSPAR_MIME_AMAP_Biota_contaminants_effects_20210201/OSPAR_MIME_AMAP_Biota_contaminants_effects_20210201_Norway.txt"
  write.csv(dat, fn_save, row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")
  
  test <- readr::read_csv(fn_save)
  unique(test$SD_StationName)
  
}

If (FALSE){
  
  #
  # . Test published raw data ----
  #
  
  fn <- "../Files_to_ICES/Data_from_ICES/OSPAR_MIME_data_extraction_2021/OSPAR_MIME_AMAP_Biota_contaminants_effects_20210201/OSPAR_MIME_AMAP_Biota_contaminants_effects_20210201_Norway.txt"
  dat_publ <- readr::read_csv(fn)

  dat_publ %>%
    filter(PARAM %in% param,
           MYEAR %in% 2019) %>% View()

  dat_publ %>%
    filter(StationName == "Varalds") %>% View()
  
  # Test "overview plot"
  
  param <- "CB118"
  dat_publ %>%
    filter(PARAM %in% param,
           BASIS %in% "W") %>% # View()
    mutate(Value_plot = Value) %>%
    # The rest is copied from 5 plot 1
  mutate(Station_name = fct_rev(Station_name)) %>%
    ggplot(aes(MYEAR, Station_name)) +
    geom_raster(aes(fill = log10(Value_plot))) +
    scale_fill_viridis_c("log10(concentration)")
    
  
}


