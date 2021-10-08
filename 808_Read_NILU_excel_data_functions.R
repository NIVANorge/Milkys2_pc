
library(dplyr)
library(tidyr)
library(readxl)

#
# read_excel_nilu1 - reads excel data where 
#   1) there is one sample per column, adnd one compund per row
#   2) there is a top "block" of "metadata" ("upper part"), and a main "block" of consentrations
#
# NOTE: The NILU file MUST be fixed to look like expained below:
#
# "Upper part" refers to the part of the excel file containing NILU and NIVA sample ID, tissue, unit 
# "Lower part" refers to the part of the excel file containing the concentrations 
#
# Column 1: 
#   upper part: "NILU-Sample number:" etc.
#   lower part: compund names
#
# Column 2: 
#   upper part: empty
#   lower part: IPUAC numbers
#
# Column 3 etcetera:
#   upper part: sample numbers, units, file names
#   lower part: concentrations (i.e. the data)
#
# (Also the row names of the upper part had to be fixed first, i.e. moved one row up)

get_value_with_lessthansign <- function(txt){
  lessthan <- substr(txt, 1, 1) == "<"
  i1 <- ifelse(lessthan, 2, 1)
  i2 <- nchar(txt)
  list(
    Flag1 = ifelse(lessthan, "<", NA),
    Value = as.numeric(substr(txt, i1, i2))
  )
}
# get_value_with_lessthansign(c("2.45", "<3.5", "8.1", "< 10.1"))

read_excel_nilu1 <- function(filename, sheetname,
                            lessthans_given_as_negative_number, 
                            find_upper_top = "NILU-sample number", # text to search for in first line of upper part
                            find_upper_bottom = "file",            # text to search for in last line of upper part
                            find_lower_top = "structure",          # text to search for in first line of lower part
                            name_Sample_no_NILU = "NILU-Sample number:",  # Names given in EXcle sheet (must fit exactly)
                            name_Sample_no = "Customers sample ID:",  
                            name_Tissue = "Sample type:", 
                            name_Sample_amount = "Sample amount:",
                            name_Unit = "Concentration units:"
                            ){
  # Info for user
  cat("\nReading file: ", filename, "\n")
  cat("Sheet: ", sheetname, "\n")

  # Test read used to set row numbers ----
  test_read <- read_excel(filename, sheet = sheetname, 
                          col_names = FALSE, col_types = "text") %>%
    as.data.frame()    # in order to be able to get columns as [,1] etc.
  
  # upper part
  row1a <- grep(find_upper_top, test_read[,1], ignore.case = TRUE)
  row1b <- grep(find_upper_bottom, test_read[,1], ignore.case = TRUE)
  # lower part
  row2 <- grep(find_lower_top, test_read[,1], ignore.case = TRUE)
  #
  # Upper part ----
  #
  dat_upper <- read_excel(filename, sheet = sheetname, 
                          skip = row1a - 1, n_max = row1b - row1a + 1, col_names = FALSE, col_types = "text") %>%
    as.data.frame()    # in order to be able to get columns as [,1] etc.
  row_sampleno <- grep(name_Sample_no, dat_upper[,1], ignore.case = TRUE)
  sample_no <- dat_upper[4,-(1:2)] %>% as.character()
  
  # Swap rows and columns
  dat_meta <- dat_upper[,-2] %>% as.matrix() %>% t() %>% .[-1,]
  # dat_meta <- dat_upper %>% dplyr::select(-X__2) %>% as.matrix() %>% t() %>% .[-1,]
  dat_meta <- data.frame(dat_meta, stringsAsFactors = FALSE)
  colnames(dat_meta) <- as.data.frame(dat_upper)[,1]
  
  # Change names for the columns we will use
  # dput(dat_upper[,1])
  
  sel <- colnames(dat_meta) %in% name_Sample_no_NILU; if (sum(sel)==0) stop(paste(name_Sample_no_NILU,"not found"))
  colnames(dat_meta)[sel] <- "Sample_no_NILU"
  sel <- colnames(dat_meta) %in% name_Sample_no; if (sum(sel)==0) stop(paste(name_Sample_no,"not found"))
  colnames(dat_meta)[sel] <- "Sample_no"
  sel <- colnames(dat_meta) %in% name_Tissue; if (sum(sel)==0) stop(paste(name_Tissue,"not found"))
  colnames(dat_meta)[sel] <- "Tissue"
  sel <- colnames(dat_meta) %in% name_Sample_amount; if (sum(sel)==0) stop(paste(name_Sample_amount,"not found"))
  colnames(dat_meta)[sel] <- "Sample_amount"
  sel <- colnames(dat_meta) %in% name_Unit; if (sum(sel)==0) stop(paste(name_Unit,"not found"))
  colnames(dat_meta)[sel] <- "Unit"
  
  #
  # Lower part ----
  #
  dat_lower <- read_excel(filename, sheet = sheetname, 
                          skip = row2, col_names = FALSE)
  colnames(dat_lower)[1:2] <- c("Parameter", "IPUAC_no")
  colnames(dat_lower)[3:ncol(dat_lower)] <- sample_no
  
  dat_meta <- dat_meta %>%
    dplyr::select(Sample_no_NILU, Sample_no, Tissue, Sample_amount, Unit)
  
  # Delete columns with no NIVA sample number
  sel <- !is.na(colnames(dat_lower))
  
  if (sum(!sel) > 0){
    warning(name_Sample_no, " lacking for ", sum(!sel), " samples. These will not be included!")
  }
  
  # Restructure data
  dat_result <- dat_lower[,sel] %>% 
    gather("Sample_no", "Value", -c(Parameter, IPUAC_no)) %>%
    left_join(dat_meta, by = c("Sample_no" = "Sample_no")) %>%
    mutate(IPUAC_no = as.numeric(IPUAC_no))
  
  # Set LOQ flag
  if (lessthans_given_as_negative_number){
    dat_result <- dat_result %>%
      mutate(Value = as.numeric(Value)) %>% 
      mutate(Flag1 = ifelse(Value < 0, "<", NA)) %>%
      mutate(Value = ifelse(Value < 0, -Value, Value)
      )
  } else {   # given with less-than sign
    resultlist <- get_value_with_lessthansign(dat_result$Value)
    dat_result <- dat_result %>%
      mutate(Flag1 = resultlist$Flag1,
             Value = resultlist$Value
      )
  }
  cat("Returning", nrow(dat_result), "rows of data\n")
  dat_result %>% 
    mutate(Sample_amount = as.numeric(Sample_amount)) %>%
    dplyr::select(Sample_no_NILU, Sample_no, Tissue, Sample_amount, Parameter, IPUAC_no, Value, Unit, Flag1)
  } 

# TEST
# debugonce(read_excel_nilu1)
# df <- read_excel_nilu1("Input_data_Norman/Rapportering MILKYS_2019.xlsx", "PBDE",
#                        lessthans_given_as_negative_number = TRUE)
# df <- read_excel_nilu1("Input_data_Norman/Rapportering MILKYS_2019.xlsx", "PCB",
#                        lessthans_given_as_negative_number = FALSE)


# 

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# read_excel_nilu2 - reads excel data where 
#   1) there is one compound per column, and one sample per row
#   2) there are two lines at the top, the first one with name of compound, the second with units
#   3) the first 3 columns are 1) NILU's sample ID, 2) NIVA's sample ID, and 3) Type of tissue
#   4) Column 4 is either "sample amount" (set contains_sample_amount = TRUE), or 
#      the first column of data (set contains_sample_amount = FALSE)
#

read_excel_nilu2 <- function(filename, sheetname, 
                             lessthans_given_as_negative_number, 
                             contains_sample_amount,
                             skip = 0){
  # Info for user
  cat("\nReading file: ", filename, "\n")
  cat("Sheet: ", sheetname, "\n\n")

  # Read file parts
  dat_upper <- read_excel(filename, sheet = sheetname, skip = skip, n_max = 1)
  dat_result <- read_excel(filename, sheet = sheetname, skip = skip + 1)

  # Add column names (some will later be values of Parameter)
  colnames(dat_result) <- colnames(dat_upper)

  # Change the 3 or 4 first column names (hard-coded!)
  if (contains_sample_amount){
    colnames(dat_result)[1:4] <- c("Sample_no_NILU", "Sample_no", "Tissue", "Sample_amount")
    cat("Changing variable names - check that old and new names correspond:\n")
    examples <- dat_result[1:3, 1:4] %>% purrr::map_chr(paste, collapse = ", ")
    tibble(`Original name` = colnames(dat_upper)[1:4], `New name` = colnames(dat_result)[1:4], Example = examples) %>% print()
    dat_result <- dat_result %>% mutate(Sample_amount = as.numeric(Sample_amount))
  }  else {
    colnames(dat_result)[1:3] <- c("Sample_no_NILU", "Sample_no", "Tissue")
    cat("Changing variable names - check that old and new names correspond:\n")
    examples <- dat_result[1:3, 1:3] %>% purrr::map_chr(paste, collapse = ", ")
    tibble(`Original name` = colnames(dat_upper)[1:3], `New name` = colnames(dat_result)[1:3], Example = examples) %>% print()
    dat_result <- dat_result %>% mutate(Sample_amount = NA)
  }
  # Restructure data
  dat_result <- dat_result %>%
    gather("Parameter", "Value", -c(Sample_no_NILU, Sample_no, Tissue, Sample_amount))
  # Set LOQ flag
  if (lessthans_given_as_negative_number){
    dat_result <- dat_result %>%
      mutate(Value = as.numeric(Value)) %>% 
      mutate(Flag1 = ifelse(Value < 0, "<", NA)) %>%
      mutate(Value = ifelse(Value < 0, -Value, Value)
      )
  } else {   # given with less-than sign
    resultlist <- get_value_with_lessthansign(dat_result$Value)
    dat_result <- dat_result %>%
      mutate(Flag1 = resultlist$Flag1,
             Value = resultlist$Value
      )
  }
  dat_unit <- tibble(Parameter = colnames(dat_upper), Unit = as.character(as.data.frame(dat_upper)[1,]))
  dat_result <- dat_result %>% 
    left_join(dat_unit, by = "Parameter") %>%
    mutate(IPUAC_no = as.numeric(NA))
  cat("\nReturning", nrow(dat_result), "rows of data\n")
  dat_result %>% dplyr::select(Sample_no_NILU, Sample_no, Tissue, Sample_amount, Parameter, IPUAC_no, Value, Unit, Flag1)
}

# TEST
# debugonce(read_excel_nilu2)
# df <- read_excel_nilu2("Input_data_Norman/Rapportering MILKYS_2019.xlsx", "HBCD",
#                       lessthans_given_as_negative_number = TRUE,
#                       contains_sample_amount = TRUE)
# df <- read_excel_nilu2("Input_data_Norman/Rapportering MILKYS_2019.xlsx", "CP",
#                       lessthans_given_as_negative_number = FALSE,
#                       contains_sample_amount = TRUE)
# df <- read_excel_nilu2("Input_data_Norman/Rapportering MILKYS_2019.xlsx", "Metaller",
#                       lessthans_given_as_negative_number = TRUE,
#                       contains_sample_amount = FALSE,
#                       skip = 2)


nilu_fix_units <- function(df){
  df_unit <- tibble(
    Unit = c("%", "mg/kg", "ng/g", "Rec %"),
    UNIT = c("PERCENT", "MG_P_KG", "UG_P_KG", "PERCENT")
  )
  df <- df %>% left_join(df_unit, by = "Unit")
  
  # Message
  n <- sum(!is.na(df$UNIT))
  cat("UNIT added for ", n, " records (", round(n/nrow(df)*100, 1), " percent)\n", sep = "")
  
  df
}

nilu_fix_hg_unit <- function(df){
  sel <- df$Parameter %in% "202  Hg  [ No Gas ]"
  df$Unit[sel] <- "mg/kg"
  df$UNIT[sel] <- "MG_P_KG"
  df$Value[sel] <- df$Value[sel]/1000
  cat("Hg unit changed from ug/kg to mg/kg for", sum(sel), "records\n")
  df
  }
 

nilu_fix_tissue <- function(df){
  df_tissue <- tibble(
    Tissue = c("Ærfugl blod", "Ærfugl egg", "Ærfuglblod", "Ærfuglegg", "Torskelever", "Torskefilet"),
    TISSUE_NAME = c("Blod", "Egg", "Blod", "Egg", "Lever", "Muskel")
  )
  df %>% left_join(df_tissue, by = "Tissue")
}

nilu_param_pcb_pbde <- function(df){
  # Create PARAM from IPUAC numbers
  df %>% 
    mutate(
      PARAM = 
        case_when(PARAM == "" & Group %in% "PCB" & !is.na(IPUAC_no) ~ paste0("PCB-", IPUAC_no),
                  PARAM == "" & Group %in% "PBDE" & !is.na(IPUAC_no) ~ paste0("BDE-", IPUAC_no),
                  TRUE ~ ""
        )
    )
}

nilu_param <- function(df){
  # Lookup table for the rest
  df_param_lookup <- tibble(
    Parameter = c("107  Ag  [ No Gas ]", "111  Cd  [ No Gas ]", "120  Sn  [ No Gas ]",
                  "202  Hg  [ No Gas ]", "208  Pb  [ No Gas ]", "52  Cr  [ He ]", 
                  "59  Co  [ He ]", "60  Ni  [ He ]",  "63  Cu  [ He ]",
                  "66  Zn  [ He ]", "75  As  [ He ]",
                  "MCCP", "SCCP", "HCB",
                  "Fett %",
                  "D4","D5","D6"),
    PARAM_new = c("AG", "CD", "SN",
                  "HG", "PB", "CR",
                  "CO", "NI", "CU",
                  "ZN", "AS",
                  "MCCP", "SCCP", "HCB",
                  "Fett",
                  "D4","D5","D6")
  )
  
  # Add new PARAM values
  df %>% 
    left_join(df_param_lookup) %>%                             # Add PARAM_new
    mutate(PARAM = ifelse(PARAM == "", PARAM_new, PARAM)) %>%  # replace PARAM with PARAM_new where there is no PARAM data
    dplyr::select(- PARAM_new)     
}
