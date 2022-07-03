
#
# Data on sample level ----  
#

get_parametervalues_singlegroup <- function(paramgroup, return_group_names = FALSE){

  #
  # Parameter groups
  #
  lookup_paramgroup <- readxl::read_excel("Input_files_2020/Lookup table - substance groups.xlsx")
  group_names <- unique(lookup_paramgroup$Substance.Group)
    
  if (return_group_names){
    return <- unique(lookup_paramgroup$Substance.Group)
  } else {
    sel_paramgroup <- grep(paramgroup, group_names, value = TRUE, ignore.case = TRUE)
    if (length(sel_paramgroup) > 1){
      stop(paramgroup, " fits several names: ", paste(sel_paramgroup, collapse = "; "))
    }
    lookup_paramgroup <- read_excel("Input_files_2020/Lookup table - substance groups.xlsx") %>%
      filter(Substance.Group %in% sel_paramgroup)
    
    # Parameters
    return <- unique(lookup_paramgroup$PARAM)
  }

  return  

}

if (FALSE){
  get_parametervalues_singlegroup(return_group_names = TRUE)
  get_parametervalues_singlegroup("metals")
  get_parametervalues_singlegroup("organobrom")
  get_parametervalues_singlegroup("chlor")      # should return error
  get_parametervalues_singlegroup("chlorobi")
  get_parametervalues_singlegroup("biological")
  get_parametervalues_singlegroup("biomarkers")
  get_parametervalues_singlegroup("paraffins")
  get_parametervalues_singlegroup("DDTs")
  get_parametervalues_singlegroup("cyclohexanes")
  get_parametervalues_singlegroup("organochlorine")
}

# Get parameter names for several groups
# If no argument given, returns group names
get_parametervalues <- function(paramgroups = NULL){
  
  if (is.null(paramgroups)){
    result2 <- get_parametervalues_singlegroup(return_group_names = TRUE)
  } else {
    result1 <- lapply(paramgroups, get_parametervalues_singlegroup)
    
    for (i in 1:length(result1)){
      if (i == 1){
        result2 <- result1[[1]]
      } else {
        result2 <- c(result2, result1[[i]])
      }
    }
  }
  result2
}

if (FALSE){
  get_parametervalues("organochlorine")
  get_parametervalues(c("organochlorine", "cyclohexanes"))
  get_parametervalues()
}

get_data_tables <- function(paramgroup, 
                            filename_109 = "Files_from_Jupyterhub_2021/109_adjusted_data_2022-06-04.rds",
                            filename_lookup_substancegroups = "Input_files_2020/Lookup table - substance groups.xlsx",
                            filename_lookup_stations= "Files_to_Jupyterhub_2019/Kartbase_edit.xlsx",
                            filename_bigexcel = "Files_from_Jupyterhub_2020/Big_excel_table/Data_xl_2021-09-15_ver03.rds"){
  

  # Parameter names
  param_values <- get_parametervalues(paramgroup)

  # Parameter groups
  lookup_paramgroup <- read_excel(filename_lookup_substancegroups) %>%
    filter(PARAM %in% param_values)
  
  # Raw data
  dat_all <- readRDS(filename_109) %>%
    filter(PARAM %in% param_values)

  # Stations
  lookup_stations <- read_excel(filename_lookup_stations)
  
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
  
  X$lookup_stations <- X$lookup_stations %>%
    arrange(Station_order) %>%
    mutate(
      Station = paste(STATION_CODE, Station_short),        # This will be shown in graphs - make changes here
      Station2 = substr(Station, 1, 15),                   # This will be shown in graphs - make changes here
      Station_name = forcats::fct_inorder(Station_name),
      Station = forcats::fct_inorder(Station),
      Station2 = forcats::fct_inorder(Station2),
      Water_region = forcats::fct_inorder(Water_region)
    )
  
  dat_1 <- X$dat_all %>%
    left_join(X$lookup_paramgroup %>% select(PARAM, Substance.Group), 
              by = "PARAM") %>%
    add_count(PARAM) %>%
    filter(n >= min_obs) %>%
    # Add 'Station.Name'
    left_join(X$lookup_stations, by = "STATION_CODE")
  
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
      filter(LATIN_NAME %in% c("Gadus morhua", "Platichthys flesus"))
    
  } else if (grepl("mussel", speciesgroup)){
    
    result <- dat_2 %>%
      filter(LATIN_NAME %in% c("Mytilus edulis"))
    
  } else {
    
    result <- dat_2
  }
  
  for (col in c("Station_name", "Station", "Station2", "Water_region"))
    result[[col]] <- droplevels(result[[col]])
  
  result

}

# Test
if (FALSE){
  # debugonce(get_data)
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
    group_by(PARAM, LATIN_NAME, TISSUE_NAME, STATION_CODE, Station, Station2, Water_region, MYEAR, UNIT, EQS_WW, Proref) %>%
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

# Test
if (FALSE){
  debugonce(get_medians)
  x1 <- get_data("metals", "fish")
  x2 <- get_data("metals", "mussel")
  test <- get_medians(x1, x2)
}

#
# Tables ----
#

# For ineractive use (tooltip, something doesn't work for some reason)
pargroup_median_table_TEST <- function(data_medians, fill, year, interactive = TRUE){
  
  data_medians$fill <- data_medians[[fill]]

  dat_plot <- data_medians %>%
    filter(MYEAR %in% year) %>%
    arrange(desc(PARAM)) %>%
    mutate(PARAM = forcats::fct_inorder(PARAM))

  cols <- c(RColorBrewer::brewer.pal(6, "Blues")[2],
            RColorBrewer::brewer.pal(6, "YlOrRd")[1:5])
  col_func <- function(x){cols[x]}
  
  if (interactive){
    gg <- ggplot(dat_plot, aes(Station2, PARAM, fill = fill)) +
      geom_tile_interactive(tooltip = VALUE_WW_med)
  } else {
    gg <- ggplot(dat_plot, aes(Station2, PARAM, fill = fill)) +
      geom_tile()
  }
  gg <- gg +
    geom_tile(data = subset(dat_plot, Above_EQS %in% "Over"),
              color = "red", size = 1, height = 0.9, width = 0.9) +
    geom_text(aes(label = round(VALUE_WW_med, 3)), nudge_y = -0.1, size = 3) +
    geom_text(aes(label = LOQ_label), size = 3, nudge_y = 0.3) +
    #scale_fill_viridis_b(trans = "log10", breaks = c(0.01,1,2,3,5,10,100), option = "plasma") +
    #scale_fill_binned(breaks = c(0.01,1,2,3,5,10,100)) +
    scale_fill_stepsn(breaks = c(0.01,1,2,3,5,10,100), colours = cols) +
    scale_color_manual(values = c("red", "white")) +
    scale_alpha_manual(values = c(1, 0)) +
    scale_y_discrete() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = -45, hjust = 0))
    labs(
      title = "Medians"
    )
  
  gg
  
}

pargroup_median_table <- function(data_medians, fill, year){
  
  data_medians$fill <- data_medians[[fill]]
  
  dat_plot <- data_medians %>%
    filter(MYEAR %in% year) %>%
    arrange(desc(PARAM)) %>%
    mutate(PARAM = forcats::fct_inorder(PARAM))
  
  cols <- c(RColorBrewer::brewer.pal(6, "Blues")[2],
            RColorBrewer::brewer.pal(6, "YlOrRd")[1:5])
  col_func <- function(x){cols[x]}
  
  gg <- ggplot(dat_plot, aes(Station2, PARAM, fill = fill)) +
      geom_tile()
  gg <- gg +
    geom_tile(data = subset(dat_plot, Above_EQS %in% "Over"),
              color = "red", size = 1, height = 0.9, width = 0.9) +
    geom_text(aes(label = round(VALUE_WW_med, 3)), nudge_y = -0.1, size = 3) +
    geom_text(aes(label = LOQ_label), size = 3, nudge_y = 0.3) +
    #scale_fill_viridis_b(trans = "log10", breaks = c(0.01,1,2,3,5,10,100), option = "plasma") +
    #scale_fill_binned(breaks = c(0.01,1,2,3,5,10,100)) +
    scale_fill_stepsn(breaks = c(0.01,1,2,3,5,10,100), colours = cols) +
    scale_color_manual(values = c("red", "white")) +
    scale_alpha_manual(values = c(1, 0)) +
    scale_y_discrete() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = -45, hjust = 0))
  labs(
    title = "Medians"
  )
  
  gg
  
}

if (F){
  # debugonce(pargroup_median_table)
  pargroup_median_table(dat_median_fish, fill = "Proref_ratio_WW", year = 2021)
}


make_boxplots <- function(data_medians, y, year, ylabel = NULL, main_title = NULL){
  
  data_medians$y <- data_medians[[y]]
  
  if (is.null(ylabel))
    ylabel = y
  
  dat_prorefplot2 <- data_medians %>%    # Change here for fish vs. mussel
    filter(MYEAR == year) %>%
    mutate(
      Unit = gsub("_P_", "/", UNIT, fixed = TRUE) %>% tolower(),
      Tooltip = paste0(Station, "<br>Conc.: (min-median-max): ", VALUE_WW_min, "-", VALUE_WW_med, "-", VALUE_WW_max, " ", Unit))
  
  # str(dat_prorefplot2)
  
  gg <- ggplot(dat_prorefplot2, aes(PARAM, y = y)) +
    geom_hline(yintercept = 1) +
    geom_boxplot() +
    geom_jitter_interactive(aes(fill = Water_region, tooltip = Tooltip, data_id = STATION_CODE), pch = 21, size = 2, width = 0.1) +
    # scale_fill_distiller("Along coast\n(far N/E = blue)", palette = "RdBu", direction = 1) +  # Geogr_position
    scale_fill_brewer("Water region", palette = "RdBu", direction = 1) +
    theme_bw() +
    ggeasy::easy_rotate_x_labels(angle = -45) +
    labs(y = ylabel, title = main_title)
  # gg
  
  # gg <- gg + coord_flip()
  # gg
  
  ggr <- girafe(ggobj = plot_grid(gg + guides(fill = "none") + labs(subtitle = "Medians, ordinary scale"),
                                  gg + scale_y_log10() + labs(subtitle = "Medians, log scale"), 
                                  rel_widths = c(1,1.35)), 
                width_svg = 10, height_svg = 4)
  
  ggr <- girafe_options(ggr, opts_hover(css = "fill:wheat;stroke:orange;r:5pt;") )
  
  ggr
  
}

if (FALSE){
  debugonce(make_boxplots)
  
  make_boxplots(dat_median_fish, y = "EQS_ratio_WW", year = 2021)

  make_boxplots(dat_median_mussel, y = "Proref_ratio_WW", year = 2021)
  
}