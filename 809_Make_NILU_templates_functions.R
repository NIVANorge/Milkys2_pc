
make_top_rows <- function(header_main_part, parameter_group){
  
  sheet_rows_1a <- bind_cols(
    data.frame(
      V0a = c(
        "Encl. to measuring report:",
        "NILU-Sample number:",
        "Customer:",
        "Comment:",
        "Customers sample ID:",
        "Sample type:",
        "Sample amount:",
        "Analysed sample amount:",
        "Concentration units:",
        "Data files:"),
      # adds two extra empty columns coming before the 
      V0b = "",      
      V0c = ""),
    header_main_part)
  
  # Adds one row containg just the name of the parameter group, 
  # Adds two empty rows first ()
  sheet_rows_1 <- bind_rows(sheet_rows_1a[1:2,], sheet_rows_1a, sheet_rows_1a[1:2,])
  sheet_rows_1[1,1] <- parameter_group
  sheet_rows_1[2,1] <- ""
  sheet_rows_1[nrow(sheet_rows_1)-1,1] <- ""
  sheet_rows_1[nrow(sheet_rows_1),1] <- "Compound"
  sheet_rows_1
  
}

if (F){
  test_headers <- data.frame(
    LT1 = c("", "", "", "", "", "", "", "", "", ""), 
    V1 = c("", "", "MILKYS (NIVA)", "19N Breøyane - fugl egg 1", "NR-2020-08432", "Ærfugl Egg", "", "", "", ""), 
    LT2 = c("", "", "", "", "", "", "", "", "", ""), 
    V2 = c("", "", "MILKYS (NIVA)", "19N Breøyane - fugl egg 2", "NR-2020-08433", "Ærfugl Egg", "", "", "", "")
    )
  
  make_top_rows(test_headers, "PBDE")
}


#
# Note: the parameters will be in the same order as they are in 'nilu_data_long'
#
make_bottom_rows <- function(nilu_data_long, example_sample_number, parameter_group){
  
  nilu_data_long %>%
    filter(Sample_no %in% example_sample_number & Group %in% parameter_group) %>%
    mutate(IPUAC_no = as.character(IPUAC_no)) %>%
    select(Parameter, IPUAC_no, PARAM) %>%
    rename(NIVA_name = PARAM)

}

if (FALSE){
  
  test_data <- structure(list(Sample_no = rep("Nr. 2020-08432", 3), 
                                    Group = rep("PBDE", 3),
                                    Parameter = c("TBA", "2,2',4-TriBDE", "2,4,4'-TriBDE"), 
                                    IPUAC_no = c(NA, "17", "28"), 
                                    PARAM = c("TBA", "BDE-17", "BDE-28"),
                                    Value = c(0.05, 0.10, 0.15)), 
                               row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame"))
  
  make_bottom_rows(test_data, "Nr. 2020-08432", "PBDE")
  
}

make_nilu_template_sheet <- function(parameter_group, header_main_part, parameter_data, example_sample_number){
  
  sheet_rows_1 <- make_top_rows(header_main_part, parameter_group)
  sheet_rows_2 <- make_bottom_rows(parameter_data, example_sample_number, parameter_group)
  
  
  # Middle row
  sheet_rows_2_names <- colnames(sheet_rows_2)
  values_middle <- rep(c("Under LOQ ('<')", "Value"), ncol(header_main_part)/2)
  sheet_rows_middle <- matrix(c(sheet_rows_2_names, values_middle), nrow = 1) %>% as.data.frame()

  colnames(sheet_rows_2) <- colnames(sheet_rows_1)
  colnames(sheet_rows_middle ) <- colnames(sheet_rows_1)
  
  bind_rows(sheet_rows_1, sheet_rows_middle, sheet_rows_2)

}

if (FALSE){
  
  # test_headers, test_parameters are defined above
  
  # debugonce(make_nilu_template_sheet)
  make_nilu_template_sheet("PBDE", test_headers, test_parameters, "Nr. 2020-08432")

}