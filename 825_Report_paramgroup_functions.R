
#
# Data on sample level ----  
#

get_parametervalues <- function(paramgroup, return_group_names = FALSE){

  #
  # Parameter groups
  #
  lookup_paramgroup <- read_excel("Input_files_2020/Lookup table - substance groups.xlsx")
  
  if (return_group_names){
    return <- unique(lookup_paramgroup$Substance.Group)
  } else {
    lookup_paramgroup <- read_excel("Input_files_2020/Lookup table - substance groups.xlsx") %>%
      filter(grepl(paramgroup, Substance.Group, ignore.case = TRUE))
    
    # Parameters
    return <- unique(lookup_paramgroup$PARAM)
  }

  return  

}

if (FALSE){
  get_parametervalues(return_group_names = TRUE)
  get_parametervalues("metals")
  get_parametervalues("organobrom")
}


get_data_tables <- function(paramgroup, 
                            filename_109 = "Files_from_Jupyterhub_2021/109_adjusted_data_2022-06-04.rds",
                            filename_lookup_substancegroups = "Input_files_2020/Lookup table - substance groups.xlsx",
                            filename_lookup_stations= "Files_from_Jupyterhub_2020/Lookup for big excel - stations.xlsx",
                            filename_bigexcel = "Files_from_Jupyterhub_2020/Big_excel_table/Data_xl_2021-09-15_ver03.rds"){
  

  # Parameter groups
  lookup_paramgroup <- read_excel(filename_lookup_substancegroups) %>%
    filter(grepl(paramgroup, Substance.Group, ignore.case = TRUE))
  
  param_values <- unique(lookup_paramgroup$PARAM)

  # Raw data
  dat_all <- readRDS(filename_109) %>%
    filter(PARAM %in% param_values)

  # Stations
  lookup_stations <- read_excel(filename_lookup_stations) %>% # View()
    bind_rows(data.frame(STATION_CODE = "19B", Station.Name = "Isfjorden", County = "Svalbard")) %>%
    bind_rows(data.frame(STATION_CODE = "20B", Station.Name = "Longyearbyen", County = "Svalbard")) %>%
    bind_rows(data.frame(STATION_CODE = "97A3", Station.Name = "Mjelle, Bodø area", County = "Nordland"))
  
  # EQS and proref
  lookup_eqs_ww <- readRDS(filename_bigexcel) %>%
    filter(PARAM %in% param_values) %>%
    rename(Proref = Q95) %>%
    filter(
      Basis %in% "WW"
    ) %>%
    distinct(PARAM, LATIN_NAME, TISSUE_NAME, EQS, Proref) %>%
    mutate(TISSUE_NAME = case_when(
      TISSUE_NAME == "Muscle" ~ "Muskel",
      TISSUE_NAME == "Liver" ~ "Lever",
      TRUE ~ TISSUE_NAME)
    ) %>%
    rename(EQS_WW = EQS)

  check <- lookup_eqs_ww %>%
    add_count(PARAM, LATIN_NAME, TISSUE_NAME) %>%
    filter(n > 1)

  if (nrow(check) > 0){
    stop("More than one EQS per parameter!")
  }
  
  list(dat_all=dat_all, 
       lookup_paramgroup=lookup_paramgroup, 
       lookup_stations=lookup_stations, 
       lookup_eqs_ww = lookup_eqs_ww)

}

# Test
if (FALSE){
  X <- get_data_tables("metals")
}


get_data <- function(paramgroup, speciesgroup, min_obs = 100){
  
  X <- get_data_tables(paramgroup)
  
  dat_1 <- X$dat_all %>%
    left_join(X$lookup_paramgroup %>% select(PARAM, Substance.Group), 
              by = "PARAM") %>%
    add_count(PARAM) %>%
    filter(n >= min_obs) %>%
    # Add 'Station.Name'
    left_join(X$lookup_stations %>% select(STATION_CODE, Station.Name), 
              by = "STATION_CODE") %>%
    # Add 'Station.Name'
    mutate(Station = paste(STATION_CODE, Station.Name))

  dat_2 <- dat_1 %>%
    left_join(X$lookup_eqs_ww, by = c("PARAM", "LATIN_NAME", "TISSUE_NAME")) %>%
    mutate(
      Above_EQS = case_when(
        VALUE_WW > EQS_WW ~ "Over",
        VALUE_WW <= EQS_WW ~ "Under",
        TRUE ~ as.character(NA))
    ) 
  
  if (speciesgroup == "fish"){
    
    result <- dat_2 %>%
      filter(LATIN_NAME %in% c("Gadus morhua", "Platichthys flesus")) %>%
      left_join(lookup_region_fish, by = "Station") %>%    # Adds Region
      mutate(
        Station = factor(Station, levels = rev(station_order_fish)),
        Station2 = substr(Station, 1, 15),
        Station2 = factor(Station2, levels = rev(substr(station_order_fish, 1, 15))),
        Region = factor(Region, levels = region_order_fish)
        )
    
  } else if (speciesgroup == "mussel"){
    
    result <- dat_2 %>%
    filter(LATIN_NAME %in% c("Mytilus edulis")) %>%
    mutate(
      Station = factor(Station, levels = rev(station_order_mussel)),
      Station2 = substr(Station, 1, 15),
      Station2 = factor(Station2, levels = rev(substr(station_order_mussel, 1, 15))))
  } %>%
    mutate(Region = as.character(NA))    # TO FIX
  
  result

}

# Test
if (FALSE){
  x1 <- get_data("metals", "fish")
  x2 <- get_data("metals", "mussel")
}


#
# Data, medians per station/year
#


# * Including 'dat_medium_fish' and 'dat_medium_mussel' which will be used for "all-parameters + all-stations + last year" overviews  

get_medians <- function(data_samplelevel_fish, data_samplelevel_mussel){
  
  result <- bind_rows(data_samplelevel_fish, data_samplelevel_mussel) %>%
    group_by(Station) %>%
    mutate(n_after_2018 = sum(MYEAR >= 2018)) %>%
    filter(
      n_after_2018 > 0) %>%
    ungroup() %>%
    group_by(PARAM, LATIN_NAME, TISSUE_NAME, STATION_CODE, Station, Region, MYEAR, UNIT, EQS_WW, Proref) %>%
    summarize(
      VALUE_WW_med = median(VALUE_WW, na.rm = TRUE),
      VALUE_WW_min = min(VALUE_WW, na.rm = TRUE),
      VALUE_WW_max = max(VALUE_WW, na.rm = TRUE),
      N = n(),
      N_underLOQ = sum(FLAG1 %in% "<"),
      .groups = "drop") %>%
    mutate(
      Proref_ratio_WW = VALUE_WW_med/Proref,
      EQS_ratio_WW = VALUE_WW_med/EQS_WW,
      Prop_underLOQ = N_underLOQ/N,
      FLAG1 = case_when(
        Prop_underLOQ < 0.5 ~ as.character(NA),
        Prop_underLOQ >= 0.5 ~ "<"),
      Above_EQS = case_when(
        EQS_ratio_WW > 1 ~ "Over",
        EQS_ratio_WW <= 1 ~ "Under",
        TRUE ~ as.character(NA)),
      LOQ_label = ifelse(Prop_underLOQ >= 0.5, "<", "")
    )
  
  check <- xtabs(~MYEAR, result %>% filter(PARAM == "HG" & grepl("30A", Station)))
  if (sum(check > 1) > 0){
    stop("Error in median: more than one measurement per parameter/station/year")
  }
  
  result
  
}


#
# Station order ----
#

station_order_fish <- c(
  "19B Isfjorden", "20B Longyearbyen", "10B Varangerfjord", "45B2 Hammerfest (havn)", "43B2 Tromsø harbour",
  "98B1 Lofoten, Skrova", "96B Helgelandskysten area by Sandnessjøen", 
  "80B Munkholmen", "13B Kristiansand harbour", "28B Ålesund area by Hundsvær",
  "24B Bergen havn", "23B Bømlo north", "53B Inner Sørfjord", 
  "15B Farsund area", "71B Grenlandsfjorden Breviks area", 
  "36B Færder area", "30B Inner Oslofjord", "02B Kirkøy (north)")

# NOTE: must be in same order as 'station_order_fish'  
# https://www.huvo.no/filer/bilder/kart-vannregioner-i-full-storrelse.jpg?i=063
# We skip Kemijoki which makes up part of Varangerfjord
station_order_fish_region <- c(
  "Svalbard", "Svalbard", "Troms+Finnmark", "Troms+Finnmark", "Troms+Finnmark",
  "Nordland", "Nordland", 
  "Trøndelag", "Møre+Romsdal", "Møre+Romsdal",
  "Vestland", "Vestland", "Vestland", 
  "Agder", "Vestfold+Telemark", 
  "Vestfold+Telemark", "Innlandet+Viken", "Innlandet+Viken")

# station_region_fish %>% unique() %>% dput()
region_order_fish <- c("Svalbard", "Troms+Finnmark", "Nordland", "Trøndelag", "Møre+Romsdal", 
                       "Vestland", "Agder", "Vestfold+Telemark", "Innlandet+Viken")

# Only made "regions" for fish  
lookup_region_fish <- data.frame(
  Station = station_order_fish, 
  Region = station_order_fish_region)



# MUST CHECK/FIX: 76A2 and 28A2  
station_order_mussel <- c(
  "11X Brashavn", "10A2 Skallneset", "98A2 Lofoten, Svolvær", 
  "56A Kvalnes", 
  "97A3 Mjele, Bodø area", "97A2 Bodø harbour", 
  "22A Espevær",
  "91A2 Outer Trondheimsfjord", "26A2 Måløy", "I241 Nordnes", "64A Utne", 
  "76A2 NA", "28A2 NA",
  "65A Vikingneset", 
  "I131A Lastad", "71A Bjørkøya", "I304 Gåsøya", "15A Gåsøy", 
  "36A1 Tjøme", "I024 Kirkøy", "I023 Singlekalven", "31A Solbergstrand", 
  "I301 Akershuskaia", "30A Gressholmen" 
)

