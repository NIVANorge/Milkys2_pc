
# get_data_for_trend
# Flexible way of selecting rows of data for trend analysis
milkys_select_rows <- function(param = NULL, species = NULL, tissue = NULL, station = NULL, firstyear = NULL, data){
  data_sel <- data
  if (!is.null(param))
    data_sel <- data_sel %>% filter(PARAM %in% param)
  if (!is.null(species))
    data_sel <- data_sel %>% filter(LATIN_NAME %in% species)
  if (!is.null(tissue))
    data_sel <- data_sel %>% filter(TISSUE_NAME %in% tissue)
  if (!is.null(station)){
    # For station, we need only to give the first letters, e.g. "30B" for "30B Inner Oslofjord"
    # data_sel <- data_sel %>%
    #   mutate(Station_short = substr(Station, 1, nchar(station))) %>%
    #   filter(Station_short %in% station)
    # For station, we need only to give  partil match, e.g. "Inner Osl" for "30B Inner Oslofjord"
    data_sel <- data_sel %>%
      filter(grepl(station, Station, fixed = TRUE))
  }
  if (!is.null(firstyear))
    data_sel <- data_sel %>% filter(MYEAR >= firstyear)
  data_sel
}

get_tissue <- function(param, species){
  
  fish_species <- c("Gadus morhua", "Limanda limanda", 
                    "Somateria mollissima", "Platichthys flesus", "Modiolus modiolus")
  mollusc_species <- c("Mytilus edulis", "Nucella lapillus", "Littorina littorea")
  bird_species <- c("Somateria mollissima")
  bile_parameters <- c("AY", "PYR1O", "BILIV", "OHBIL", "PA1O", "BAP3O", "ABS380", 
                       "BAP3OH", "PA1OH", "PYR1OH", "PYR1", "AY380", "1-OH-fenantren")
  livermicrosome_parameters <- c("PROTV", "AY380")
  
  if (!species %in% c(fish_species, mollusc_species, bird_species)){
    stop("Species not found in the code of function 'get_tissue'. Check this function.")
  } 
  
  if (species %in% bird_species){
    stop("BIRD DATA - PICK TISSUE MANUALLY")
  } 
  
  if (param %in% "HG"){
    if (species %in% fish_species){
      result <- "Muskel"
    } else if (species %in% mollusc_species){
      result <- "Whole soft body"
    }
  } else if (param %in% bile_parameters){
    if (species %in% fish_species){
      result <- "Galle"
    } else {
      stop("Bile parameter measured in bird or mollusc? Check data.")
    }
  } else if (param %in% livermicrosome_parameters){
    if (species %in% fish_species){
      result <- "Liver - microsome"
    } else {
      stop("Liver microsome parameter measured in bird or mollusc? Check data.")
    }
  } else {
    if (species %in% fish_species){
      result <- "Lever"
    } else if (species %in% mollusc_species){
      result <- "Whole soft body"
    }
  }
  result
}

if (FALSE){
  get_tissue("HG", "Gadus morhua")
  get_tissue("CB118", "Gadus morhua")
  get_tissue("HG", "Mytilus edulis")
  get_tissue("CB118", "Mytilus edulis")
  get_tissue("PYR1O", "Gadus morhua")
  get_tissue("PYR1O", "Mytilus edulis")
}

# Get ordinary linear trend
# Selects rows of data, then performs trend analysis
get_trend <- function(param, species, tissue, station, firstyear = NULL, data,
                      y = "VALUE_WW_med"){
  data_sel <- milkys_select_rows(param=param, species=species, tissue=tissue, 
                                 station=station, firstyear = firstyear, data=data)
  sel_var <- names(data_sel) %in% y
  if (sum(sel_var) == 0)
    stop("Variable ", sQuote(y), "not found in dataset!")
  if (sum(sel_var) >= 2)
    stop("Several variables named ", sQuote(y), "found in dataset!")
  names(data_sel)[sel_var] <- "y"
  if (nrow(data_sel) >= 3){
    mod <- lm(log(y)  ~ MYEAR, data = data_sel)
    coef <- data.frame(summary(mod)$coef)
    result <- coef[2,]
    names(result) <- c("Est", "SE", "t", "P")
    result_meta <- data.frame(
      PARAM = param, LATIN_NAME = species, 
      TISSUE_NAME = tissue, Station = station)
    result <- cbind(result_meta, result)
  } else {
    result <- NULL
  }
  result
}

#
# get_trend for leftcensored data for one data set, assumed to be a single time series  
#
# Used by 'get_trendobj_station'
#
# The assumption that it's a single time series is checked   
# Selects rows of data, then performs trend analysis
#
get_trend_cens <- function(selected_data, firstyear = NULL,
                           several_series_is_error = TRUE,
                           ...){
  
  check <- unique(selected_data$PARAM)
  if (length(check) > 1 & several_series_is_error){
    stop("Series contains several parameters: ", paste(check, collapse = ", "))
  }
  check <- unique(selected_data$LATIN_NAME)
  if (length(check) > 1 & several_series_is_error){
    stop("Series contains several species: ", paste(check, collapse = ", "))
  }
  check <- unique(selected_data$TISSUE_NAME)
  if (length(check) > 1 & several_series_is_error){
    stop("Series contains several tissues: ", paste(check, collapse = ", "))
  }
  
  check <- unique(selected_data$Station)
  if (length(check) > 1 & several_series_is_error){
    stop("Series contains several stations: ", paste(check, collapse = ", "))
  }
  
  data_analysis <- leftcensored_prepare(
    data = selected_data, 
    var_year = "MYEAR", var_concentration = "VALUE_WW_med", var_LOQflag = "FLAG1")
  # Linear MCMC  
  result_full <- lc_linear(data_analysis, ...)
  result_full
}

#
# get_trend for leftcensored data, one time series  
#
# Selects only station - meant to be used for data with only one parameter (and species/tissue)
# Used by 'get_trendobj_parameter_species'  
#
# keep = the parts of the object we want to keep (to decrease memory usage)
# - set keep = "all" to keep all parts   

get_trendobj_station <- function(station, data, 
                                 keep = c("intercept", "slope", "plot_data"),
                                 ...){   
  data_select <- milkys_select_rows(station = station, data = data) 
  
  trendobj_complete <- get_trend_cens(data_select, ...) 
  # data_select
  
  if (keep == "all"){
    trendobj_complete
  } else {
    trendobj_complete[keep]
  }
}

# test
# trendobj1 <- get_trendobj_station("56A", dat_sel_medium)
# trendobj2 <- get_trendobj_station("56A", dat_sel_medium, firstyear = 2011)
#
# Should make error:
# test <- get_trendobj_station("56A", subset(dat_medium, PARAM %in% c("CD", "PB")))



#
# get_trend for leftcensored data, all time series for one parameter and species  
#
# Uses 'get_trend_cens'

get_trendobj_parameter_species <- function(param, species, data){
  
  data_sel <- data %>% filter(PARAM %in% param)
  
  # Vector of stations
  stations <- unique(data_sel$Station) %>% 
    set_names() # set names of 'stations' to stations (will be carried over to 'trend_results')
  
  # Filename for saving
  fn <- paste0("Trends ", unique(data_sel$PARAM), " ", species, ".rds")
  fn_full <- paste0("Data/824_trend_results/", fn)
  
  file_exists <- fn %in% dir("Data/824_trend_results")
  
  # If file of results exists, reada it; otherwise, perform anlysis and write it  
  if (file_exists){
    message("Trends have already been calculated and are read from ", sQuote(fn_full))
    trend_results <- readRDS(fn_full)
  } else {
    
    # Run all (takes some minutes)
    trend_results <- list()
    trend_results[[1]] <- purrr::map(stations, get_trendobj_station_s, data = data_sel)
    trend_results[[2]] <- purrr::map(stations, get_trendobj_station_s, data = data_sel, firstyear = 2011)
    
    saveRDS(trend_results, fn_full)
    
  }
  
  invisible(trend_results)
  
}



#
# Create function which creates relative class, based on two thresholds  
# - by default, works on log scale  
# - 'lowest' (= relclass 1.0)  could be taken to be just below the lowest value observed 
#
create_class_function <- function(thresh1, thresh2,  
                                  lowest = 0, log = TRUE,
                                  from_class = 1, to_class = 100){
  
  # Make class data
  df_classes_all <- data.frame(
    conc = c(lowest, thresh1, thresh2),
    class = 1:3
  )
  
  if (to_class > 3){
    df_classes_all <- bind_rows(
      df_classes_all,
      data.frame(
        conc = seq(2,(to_class-2))*thresh2,
        class = seq(4,to_class))
    )
  }
  
  df_classes <- df_classes_all %>%
    filter(class >= from_class & class <= to_class)
  
  if (log){
    fun <- approxfun(log(df_classes$conc), df_classes$class)
    result <- function(x){ fun(log(x))  }
  } else {
    result <- approxfun(df_classes$conc, df_classes$class)
  }
  
  result
  
}

# TEST
if (FALSE){
  f1 <- create_class_function(thresh1 = 20, thresh2 = 500, log = FALSE,
                              from_class = 1, to_class = 100)
  f1(c(8, 20, 30, 300, 500, 700))
  
  f2 <- create_class_function(thresh1 = 20, thresh2 = 500, log = TRUE,
                              from_class = 1, to_class = 100)
  f2(c(8, 20, 30, 300, 500, 700))
}
