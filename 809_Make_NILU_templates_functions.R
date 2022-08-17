
# **NOTE: This has now been moved to project 'Lims_import_template'   
# https://github.com/NIVANorge/Lims_import_template
#

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

make_sheet_bottomleft <- function(sample_data, n_extra = 5){
  
  df_samples_matrix <- as.matrix(sample_data, ncol = ncol(sampledata))
  # df_samples_matrix[1:5,]
  
  df_excel_1 <- matrix("", 
                       nrow = nrow(df_samples_matrix) + n_extra, 
                       ncol = ncol(df_samples_matrix))
  
  # dim(df_excel_1)
  df_excel_1[n_extra, ] <- colnames(df_samples_matrix)
  df_excel_1[-(1:n_extra),] <- df_samples_matrix[]
  
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
  
  make_sheet_bottomleft(df_samples_test, n_extra = 5)
  
}

make_sheet_bottomright <- function(parameter_data, sample_data, param_group, unit = "", n_extra = 5){
  
  df_pars <- parameter_data %>%
    filter(Group %in% param_group) %>%
    mutate(IPUAC_no = as.character(IPUAC_no)) %>%
    select(Parameter_NILU, IPUAC_no, Parameter_LIMS, Analysis) %>%
    t()
  
  # dim(df_pars)
  
  df_excel_2 <- matrix("", 
                       nrow = nrow(sample_data) + n_extra, 
                       ncol = ncol(df_pars))
  df_excel_2[1:4,] <- df_pars   # as df_pars has 4 rows = variables
  df_excel_2[5,] <- unit
  
  df_excel_2
  
}

if (FALSE) {
  
  df_parameters <- readxl::read_excel(
    "Input_files_2020/NILU_template/Parameters_NILU_NIVA_2020.xlsx")
  
  make_sheet_bottomright(parameter_data = df_parameters, 
                         sample_data = df_samples_test, 
                         param_group = "HBCD", 
                         unit = "ng/g", n_extra = 5)
  
}


make_nilu_template_sheet <- function(sample_data, 
                                     parameter_data, 
                                     parametergroup_name, 
                                     unit = "",
                                     prosjektnr = "O-99999"){
  
  n_extra <- 5
  
  excel_bottomleft <- make_sheet_bottomleft(sample_data, 
                                            n_extra = n_extra)
  excel_bottomright <- make_sheet_bottomright(parameter_data = parameter_data, 
                                              sample_data = sample_data,
                                              param_group = parametergroup_name,
                                              unit = unit,
                                              n_extra = n_extra)
  
  #
  # left + right part of sheet (df_excel_bottom)  
  #
  
  if (nrow(excel_bottomleft) != nrow(excel_bottomright)){
    stop("The two df_excel parts have different number of rows!")
  }
  
  df_excel_bottom <- cbind(excel_bottomleft, excel_bottomright)
  
  # Add "headings" to the left of df_excel_2 (i.e. on the last column of df_excel_1):
  df_excel_bottom[1:5, ncol(excel_bottomleft)] <- 
    c("Parameter", "IUPAC no.", "NIVA_parameter", "NIVA_analysis", "Unit")
  
  
  #
  # combine to entire sheet (df_excel) 
  #
  
  df_excel_top <- matrix("", nrow = 4, ncol = ncol(df_excel_bottom))
  df_excel_top[1,1] <- paste(parametergroup_name, "i biologisk materiale")
  df_excel_top[3,1] <- paste("Prosjektnr:", prosjektnr)
  df_excel_top[4,1] <- paste("Rapportnr:")
  df_excel_top[3,5] <- paste("Verdier under LOQ kan skrives enten med '<' eller som negative tall. Eksempel: '<0.05' eller '-0.05'.")
  
  df_excel <- rbind(df_excel_top, df_excel_bottom)
  
  # Set cells that are not used to NA
  sel <- df_excel == ""
  df_excel[sel] <- NA
  
  
  df_excel

  }


if (FALSE){
  
  # df_samples_test, df_pars_test are defined above
  
  # debugonce(make_nilu_template_sheet)
  make_nilu_template_sheet(
    sample_data = df_samples_test, 
    parameter_data = df_parameters, 
    parametergroup_name = "HBCD", 
    unit = "ng/g")
  
}