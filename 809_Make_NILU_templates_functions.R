
#
# Note: the parameters will be in the same order as they are in 'nilu_data_long'
#
get_nilu_parameters <- function(nilu_data_long, example_sample_number, parameter_group){
  
  nilu_data_long %>%
    filter(Sample_no %in% example_sample_number & Group %in% parameter_group) %>%
    mutate(IPUAC_no = as.character(IPUAC_no)) %>%
    select(Parameter, IPUAC_no, PARAM) %>%
    rename(NIVA_name = PARAM) %>%
    as.data

}

if (FALSE){
  
  test_data <- structure(list(Sample_no = rep("Nr. 2020-08432", 3), 
                                    Group = rep("PBDE", 3),
                                    Parameter = c("TBA", "2,2',4-TriBDE", "2,4,4'-TriBDE"), 
                                    IPUAC_no = c(NA, "17", "28"), 
                                    PARAM = c("TBA", "BDE-17", "BDE-28"),
                                    Value = c(0.05, 0.10, 0.15)), 
                               row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame"))
  
  get_nilu_parameters(test_data, "Nr. 2020-08432", "PBDE")
  
}

make_sheet_bottomleft <- function(sample_data){
  
  df_samples_matrix <- as.matrix(sample_data, ncol = ncol(sampledata))
  # df_samples_matrix[1:5,]
  
  df_excel_1 <- matrix("", 
                       nrow = nrow(df_samples_matrix) + 3, 
                       ncol = ncol(df_samples_matrix))
  
  # dim(df_excel_1)
  df_excel_1[3, ] <- colnames(df_samples_matrix)
  df_excel_1[-(1:3),] <- df_samples_matrix[]
  
  df_excel_1
  
}

if (FALSE){
  
  df_samples_test <- structure(
    list(
      NILU_ID = c("", "", ""), 
      NIVA_ID = c("NR-2020-08432","NR-2020-08433", "NR-2020-08434"), 
      Prøvetype = c("Ærfugl Egg", "Ærfugl Egg", "Ærfugl Egg"), 
      Kommentar = c("", "", ""), Vekt = c("", "", "")
      ), 
    row.names = c(NA, 3L), class = "data.frame") 
  
  make_sheet_bottomleft(df_samples_test)
  
}

make_sheet_bottomright <- function(parameter_data, sample_data, unit){
  
  df_pars <- parameter_data %>%
    select(Parameter, IPUAC_no) %>%
    t()
  
  # dim(df_pars)
  
  df_excel_2 <- matrix("", 
                       nrow = nrow(sample_data) + 3, 
                       ncol = ncol(df_pars))
  df_excel_2[1:2,] <- df_pars
  df_excel_2[3,] <- unit
  
  df_excel_2
  
}

if (FALSE) {
  
  df_pars_test <- structure(
    list(
      Parameter = c("TBA", "2,2',4-TriBDE", "2,4,4'-TriBDE"), 
      IPUAC_no = c(NA, "17", "28"), 
      NIVA_name = c("TBA", "BDE-17", "BDE-28")), 
    row.names = c(NA, 3L), class = "data.frame")
  
  make_sheet_bottomright(parameter_data = df_pars_test, sample_data = df_samples_test, unit = "ng/g")
  
}


make_nilu_template_sheet <- function(sample_data, parameter_data, parametergroup_name, 
                                     unit = "",
                                     prosjektnr = "O-99999"){
  
  excel_bottomleft <- make_sheet_bottomleft(sample_data)
  excel_bottomright <- make_sheet_bottomright(parameter_data = parameter_data, 
                                              sample_data = sample_data, 
                                              unit = unit)
  
  
  #
  # left + right part of sheet (df_excel_bottom)  
  #
  
  if (nrow(excel_bottomleft) != nrow(excel_bottomright)){
    stop("The two df_excel parts have different number of rows!")
  }
  
  df_excel_bottom <- cbind(excel_bottomleft, excel_bottomright)
  
  #
  # combine to entire sheet (df_excel) 
  #
  
  df_excel_top <- matrix("", nrow = 4, ncol = ncol(df_excel_bottom))
  df_excel_top[1,1] <- paste(parametergroup_name, "i biologisk materiale")
  df_excel_top[3,1] <- paste("Prosjektnr:", prosjektnr)
  df_excel_top[4,1] <- paste("Rapportnr:")
  
  df_excel <- rbind(df_excel_top, df_excel_bottom)
  
  # Set cells that are not used to NA
  sel <- df_excel == ""
  df_excel[sel] <- NA
  
  
  df_excel

  }


if (FALSE){
  
  # df_samples_test, df_pars_test are defined above
  
  make_nilu_template_sheet(
    sample_data = df_samples_test, 
    parameter_data = df_pars_test, 
    parametergroup_name = "PBDE", 
    unit = "ng/g")[1:8, 1:7]
  
}