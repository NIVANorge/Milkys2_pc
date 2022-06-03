
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
  if (!is.null(station))
    data_sel <- data_sel %>% filter(Station %in% station)
  if (!is.null(firstyear))
    data_sel <- data_sel %>% filter(MYEAR >= firstyear)
  data_sel
}

# get_trend
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