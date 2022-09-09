

get_concentration_data <- function(result_one_series){
  
  # Point data  
  dat_all %>%
    filter(PARAM %in% result_one_series$PARAM, 
           STATION_CODE %in% result_one_series$STATION_CODE) %>%
    mutate(
      LOQ = case_when(
        is.na(FLAG1) ~ "Over LOQ",
        !is.na(FLAG1) ~ "Under LOQ"),
      LOQ = factor(LOQ, levels = c("Over LOQ", "Under LOQ"))
    ) %>%
    # Make sure that "<" points are plotted first (so over LOQ points are plotted on top)
    arrange(desc(LOQ))
  
}

get_sample_info <- function(concentration_data){
  
  concentration_data %>%
    group_by(MYEAR) %>%
    summarise(N_over = sum(is.na(FLAG1)),
              N_under = sum(!is.na(FLAG1)),
              N_text = paste0(N_over, "/", N_under), .groups = "drop")
  
}

plot_ts_log <- function(result_one_series, concentration_data, sampleinfo_data = NULL){
  
  # Plot time series, log-scale  
  gg <- ggplot(result_one_series$plot_data, aes(x, y)) +
    geom_ribbon(aes(ymin = y_lo, ymax = y_hi), fill = "lightblue") +
    geom_line() +
    geom_point(data = concentration_data, aes(x = MYEAR, y = log(VALUE_WW), shape = LOQ)) +
    scale_shape_manual(values = c(19, 6)) +
    labs(title = paste0(result$PARAM, " at ", result$STATION_CODE, ", estimated log(concentration) "), 
         x = "Year", y = "log(Conc)")
  
  if (!is.null(sampleinfo_data))
    gg <- gg +
      geom_text(data = sampleinfo_data, aes(x = MYEAR, y = -Inf, label = N_text), vjust = -1, size = 4)
  
  gg  
}

plot_ts <- function(result_one_series, concentration_data, sampleinfo_data = NULL){
  
  # Plot time series, ordinary scale  
  gg <- ggplot(result_one_series$plot_data, aes(x, exp(y))) +
    geom_ribbon(aes(ymin = exp(y_lo), ymax = exp(y_hi)), fill = "lightblue") +
    geom_line() +
    geom_point(data = concentration_data, aes(x = MYEAR, y = VALUE_WW, shape = LOQ)) +
    scale_shape_manual(values = c(19, 6)) +
    labs(title = paste0(result$PARAM, " at ", result$STATION_CODE, ", estimated concentration "), 
         x = "Year", y = "Conc")
  
  if (!is.null(sampleinfo_data))
    gg <- gg +
      geom_text(data = sampleinfo_data, aes(x = MYEAR, y = -Inf, label = N_text), vjust = -1, size = 4)
  
  gg  
}

