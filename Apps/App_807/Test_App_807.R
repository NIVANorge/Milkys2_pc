
#
# Code for testing App_802
# NOTE: this also has code for creating the files read by he app (part 2 below)
#

# Set working directory before rerunning code
setwd(here::here("App_807"))


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 1. Startup A ----
# Code that will run one time (when app is starting up)
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o



library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(forcats)
library(DT)
library(niRvana)


df_projects <- get_projects() %>%
  select(-ENTERED_BY) %>%
  arrange(PROJECT_NAME) %>%
  mutate(Menu_string = paste0(PROJECT_NAME, " (ID:", PROJECT_ID, ")"))

projects_available <- df_projects$Menu_string

#
# Tissue lookup table
# 
lookup_tissues <- get_nivabase_data(
  "select TISSUE_ID, TISSUE_NAME from NIVADATABASE.BIOTA_TISSUE_TYPES")

df_taxoncode_id <- get_nivabase_data(
  "select DISTINCT TAXONOMY_CODE_ID from NIVADATABASE.BIOTA_SINGLE_SPECIMENS;")

#
# Taxonomy lookup table
#
# use NAME (from TAXONOMY_CODES) instead of LATIN_NAME (from NIVADATABASE.TAXONOMY) as some 
#  things such as 'Zooplankton epilimnion' only has NAME
# When latin names does exist, NAME and LATIN_NAME seem to always be the same

lookup_taxonomy <- get_nivabase_data(paste(
  "select TAXONOMY_CODE_ID, NAME as TAXON_NAME",
  "from NIVADATABASE.TAXONOMY_CODES",
  "where TAXONOMY_CODE_ID in (",
  paste(sQuote(df_taxoncode_id$TAXONOMY_CODE_ID), collapse = ","), 
  ");"
))


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 2. Making and testing pre-made data ----
#    Not included in app
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 3. Startup B ----
# Code that will run one time (when app is starting up)
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Server ----
# Code that will run one time (when app is starting up)
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

output <- list()

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# UI - projects_selected ----
# Code that will run one time (when app is starting up)
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

input <- list()

# grep("CEMP", projects_available, value = TRUE)

input$projects_selected <- "CEMP_Biota (ID:3699)"

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Server ----
# Code triggered by UI.0
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#
# get_stations ---
#
# Triggered by selecting project
# For 'stations_selected' menu
#

get_stations <- function(){
  
  df_projects_sel <- df_projects %>%
    filter(Menu_string %in% input$projects_selected)
  id <- df_projects_sel %>%
    pull(PROJECT_ID)
  
  result <- get_nivabase_selection(
    "PROJECT_ID, STATION_ID, STATION_CODE, STATION_NAME, STATION_IS_ACTIVE, PROJECTS_STATION_ID",
    "PROJECTS_STATIONS",
    "PROJECT_ID",
    id) %>%
    mutate(
      Menu_string = paste0(STATION_CODE, " ", STATION_NAME, " (ID:", STATION_ID, ")")
    )

  result

}

get_stations() %>% nrow()
get_stations() %>% head()


#
# get_stations_years ---
#
# Triggered by get_stations (which is triggered by selecting project)
#
# get_stations_years <- reactive({
get_stations_years <- function(){
    df_stations <- get_stations() %>%
    distinct(STATION_ID, STATION_CODE, STATION_NAME, Menu_string)
  station_ids <- df_stations %>%
    pull(STATION_ID) %>%
    unique()
  # sql <-   paste(
  #   "select STATION_ID, extract(YEAR from DATE_CAUGHT) as YEAR, count(*) as N",
  #   "from NIVADATABASE.BIOTA_SINGLE_SPECIMENS",
  #   "WHERE STATION_ID in",
  #   paste("(", paste(station_id, collapse = ","), ")"),
  #   "group by STATION_ID, extract(YEAR from DATE_CAUGHT)",
  #   "order by STATION_ID"
  # )
  result <- get_nivabase_selection(
    "STATION_ID, extract(YEAR from DATE_CAUGHT) as YEAR, count(*) as N",
    "BIOTA_SINGLE_SPECIMENS",
    "STATION_ID",
    station_ids, 
    extra_sql = "group by STATION_ID, extract(YEAR from DATE_CAUGHT) order by STATION_ID"
  ) %>%
    left_join(
      df_stations %>% select(STATION_ID, STATION_CODE, STATION_NAME, Menu_string),
      by = "STATION_ID") %>%
    select(STATION_CODE, STATION_NAME, STATION_ID, YEAR, N, Menu_string)
  # browser()
  result
}


get_stations_years() %>% nrow()


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# UI - lastyear_data_series ----
# Selection menu 1 
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

input$lastyear_data_series <- 2020

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Server ----
# Code triggered by UI.1
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


#
# get_stations_years_select ---
#

# get_stations_years_select <- reactive({
get_stations_years_select <- function(){  
  df_stations_years <- get_stations_years()
  
  if (!is.null(input$lastyear_data_series)){
    df_stations_years <- df_stations_years %>%
      group_by(STATION_ID) %>%
      mutate(YEAR_last = max(YEAR)) %>%
      ungroup() %>%
      filter(YEAR_last >= input$lastyear_data_series)
  }
  
  df_stations_years
  
}


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# UI plot - station_years_plot ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# output$station_years_plot <- renderPlot({
output$station_years_plot <- function(){
  
  df_stations_years <- get_stations_years_select()
  
  # df <- mutate(STATION_ID = factor(STATION_ID))
  resultplot <- ggplot(df_stations_years, aes(YEAR, STATION_CODE, fill = N)) +
    geom_tile() +
    scale_fill_viridis_b()
  resultplot
  
}


output$station_years_plot() %>% print()



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# UI - years_selected + stations_selected----
# Selection menu 1 
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

input$years_selected <- c(2019, 2020)

df <- get_stations_years()

if (FALSE){
  # Check 'Menu_string' values 
df %>%
  filter(YEAR == 2020) %>%
  distinct(Menu_string)
  }

input$stations_selected <- "30B Inner Oslofjord (ID:46980)"

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Server ----
# Code triggered by UI.1
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#
# get_specimens ---
#
# Note: works by selecting either (or both of) stations and years   
# - if stations NOT selected, it returns specimens for all stations that year    
# - if stations ARE selected, it returns specimens for specific stations that year    
#

# get_specimens <- reactive({
get_specimens <- function(){
  
  # Make sure either years or stations is selected
  if (is.null(input$years_selected) & is.null(input$stations_selected)){
    validate(need(FALSE, "Please select years and/or stations"))
  }
  
  df_stations_years <- get_stations_years()
  df_stations <- get_stations()  # this have 'Menu_string" so we need this one too
  # browser()
  
  sql_years <- paste(" and extract(YEAR from DATE_CAUGHT) in (", 
                     paste(input$years_selected, collapse = ","),
                     ")")  
  
  if (is.null(input$stations_selected)){
    station_ids <- df_stations_years %>%
      filter(YEAR %in% input$years_selected) %>%
      pull(STATION_ID)
    extras_sql_string <- sql_years   
  } else if (is.null(input$years_selected)){
    station_ids <- df_stations %>%
      filter(Menu_string %in% input$stations_selected) %>%
      pull(STATION_ID)
    extras_sql_string <- ""
  } else {
    station_ids <- df_stations_years %>%
      filter(Menu_string %in% input$stations_selected) %>%
      pull(STATION_ID)
    extras_sql_string <- sql_years   
  }
  
  result_1 <- get_nivabase_selection(
    "STATION_ID, DATE_CAUGHT, SPECIMEN_NO, TAXONOMY_CODE_ID, SPECIMEN_ID",
    "BIOTA_SINGLE_SPECIMENS",
    "STATION_ID",
    station_ids, 
    extra_sql = extras_sql_string
  )
  # browser()
  
  result_2 <- result_1 %>%
    left_join(
      df_stations %>% select(STATION_ID, PROJECT_ID, STATION_CODE, STATION_NAME),
      by = "STATION_ID") %>% 
    left_join(
      lookup_taxonomy, by = "TAXONOMY_CODE_ID",
    ) %>%
    select(PROJECT_ID, STATION_CODE, STATION_NAME, TAXON_NAME, everything())      
  
  # browser()
  result_2 
  
}


df_specimens <- get_specimens()  
nrow(df_specimens)

#
# get_samples ---
#

# get_samples <- reactive({
get_samples <- function(){

  # Make sure either years or stations is selected
  if (is.null(input$years_selected) & is.null(input$stations_selected)){
    validate(need(FALSE, "Please select years and/or stations"))
  }
  
  df_specimens <- get_specimens()  
  
  specimen_ids <- df_specimens %>%
    pull(SPECIMEN_ID) %>%
    unique()
  
  result_samp_spec <- get_nivabase_selection(
    "SPECIMEN_ID, SAMPLE_ID, BIOTA_SAMPLES_SPECIMENS_ID",
    "BIOTA_SAMPLES_SPECIMENS",
    "SPECIMEN_ID",
    specimen_ids
  )
  
  result_specimens_summ <- df_specimens %>%
    left_join(
      result_samp_spec %>% select(SPECIMEN_ID, SAMPLE_ID),
      by = "SPECIMEN_ID") %>%
    group_by(SAMPLE_ID) %>%
    summarise(across(
      c(PROJECT_ID, STATION_CODE, STATION_NAME, TAXON_NAME, DATE_CAUGHT, SPECIMEN_NO, SPECIMEN_ID),
      .fn = ~paste(unique(.x), collapse = ",")
    ))
  
  result <- get_nivabase_selection(
    "SAMPLE_ID, TISSUE_ID, SAMPLE_DATE, SAMPLE_NO, REPNO",
    "BIOTA_SAMPLES",
    "SAMPLE_ID",
    unique(result_samp_spec$SAMPLE_ID)
  ) %>%
    left_join(result_specimens_summ, by = "SAMPLE_ID") %>%
    left_join(lookup_tissues, by = "TISSUE_ID") %>%
    arrange(PROJECT_ID, STATION_CODE, STATION_NAME, TAXON_NAME, DATE_CAUGHT, TISSUE_NAME, SAMPLE_NO, SPECIMEN_NO) %>%
    select(PROJECT_ID, STATION_CODE, STATION_NAME, TAXON_NAME, DATE_CAUGHT, TISSUE_NAME, SAMPLE_NO, SPECIMEN_NO, everything())
  
  # browser()
  result
  
}

# debugonce(get_samples)
df_samples <- get_samples()
nrow(df_samples)

xtabs(~TISSUE_NAME + DATE_CAUGHT, df_samples)

#
# get_parameters ---
#

# get_parameters <- reactive({
get_parameters <- function(){
  
  # Make sure either years or stations is selected
  if (is.null(input$years_selected) & is.null(input$stations_selected)){
    validate(need(FALSE, "Please select years and/or stations"))
  }
  
  df_samples <- get_samples()  
  
  sample_ids <- df_samples %>%
    pull(SAMPLE_ID) %>%
    unique()
  
  result <- get_nivabase_selection(
    "METHOD_ID, count(*) as N",
    "BIOTA_CHEMISTRY_VALUES",
    "SAMPLE_ID",
    sample_ids,
    extra_sql = "group by METHOD_ID"
  )
  
  df_methods <- get_nivabase_selection(
    "METHOD_ID, NAME, UNIT, LABORATORY, MATRIX",
    "METHOD_DEFINITIONS",
    "METHOD_ID",
    result$METHOD_ID)
  
  result <- result %>%
    left_join(
      df_methods, by = "METHOD_ID") %>%
    select(
      NAME, UNIT, LABORATORY, METHOD_ID, MATRIX, N) %>%
    mutate(Menu_string = paste0(NAME, " (", UNIT, "; id:", METHOD_ID, ")"))
  
  # browser()
  result
  
}

df_parameters <- get_parameters()


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# UI - parameters_selected ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


input$parameters_selected <- "PCB 118 (NG_P_G; id:36709)"
input$parameters_selected <- c("1-OH-pyren", "PYR1OH")

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Server ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


#
# get_measurements ---
#

# get_measurements <- reactive({

get_measurements <- function(){
  
  # Make sure either years or stations is selected
  if (is.null(input$years_selected) & is.null(input$stations_selected)){
    validate(need(FALSE, "Please select years and/or stations"))
  }
  validate(need(input$parameters_selected != "", "Please select parameters"))
  
  df_samples <- get_samples()  
  df_parameters <- get_parameters()
  
  sample_ids <- df_samples %>%
    pull(SAMPLE_ID) %>%
    unique()
  parameter_ids <- df_parameters %>%
    filter(Menu_string %in% input$parameters_selected) %>%
    pull(METHOD_ID) %>%
    unique()
  
  result <- get_nivabase_selection(
    "SAMPLE_ID, METHOD_ID, VALUE, FLAG1, DETECTION_LIMIT, UNCERTAINTY, QUANTIFICATION_LIMIT, VALUE_ID",
    "BIOTA_CHEMISTRY_VALUES",
    "SAMPLE_ID",
    sample_ids,
    extra_sql = paste("and METHOD_ID in (", paste(parameter_ids, collapse = ","), ")")
  )
  
  df_methods <- get_nivabase_selection(
    "METHOD_ID, NAME, UNIT, LABORATORY, MATRIX",
    "METHOD_DEFINITIONS",
    "METHOD_ID",
    unique(result$METHOD_ID))
  
  result <- result %>%
    left_join(
      df_methods, by = "METHOD_ID") %>%
    left_join(
      df_samples %>% select(PROJECT_ID, STATION_CODE, STATION_NAME, TAXON_NAME, DATE_CAUGHT, 
                            SPECIMEN_NO, SPECIMEN_ID, SAMPLE_ID, SAMPLE_NO), 
      by = "SAMPLE_ID") %>%
    select(
      PROJECT_ID, STATION_CODE, STATION_NAME, TAXON_NAME, DATE_CAUGHT, 
      SAMPLE_NO, SPECIMEN_NO, NAME, UNIT, VALUE, FLAG1, 
      DETECTION_LIMIT, UNCERTAINTY, QUANTIFICATION_LIMIT, 
      LABORATORY, MATRIX,
      SAMPLE_ID, SPECIMEN_ID, METHOD_ID, VALUE_ID)
  
  # browser()
  result
  
}

# debugonce(get_measurements)
df_meas <- get_measurements()

