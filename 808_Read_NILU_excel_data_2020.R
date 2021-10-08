#
# Reading NILU data from Excel
#
# - One single excel file, 7 sheets
# - Several different formats, some of which can be read using the functions
#   read_excel_nilu1 (data 1,2) and read_excel_nilu2 (5), some which are read 'manually' (the rest)
# 

# Eider duck data saved (and used in script 812) in part 9:
# - 808_dat_eiderduck_2020.rds
#
# Cod siloxans (part of data set 7) saved in part 2f:
# - 808_dat_cod-siloxans_2020.rds

# Based on 'Milkys' project script '31_Read_excel_data_2019.R'  
#

# 1a. Libraries and scripts ----
# 
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)

# install.packages("readxl")
# install.packages("cellranger")
library("readxl")

source('808_Read_NILU_excel_data_functions.R')

# Dropoed in this version
# source('01_Get_chemical_data_NIVAbasen_functions.R")  # for sum parameters


#
# Note: File fixed beforehand (see '31_Read_excel_data_functions.R')
# (Also the row names of the upper part had to be moved one row up)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 2. Read eider duck data from NILU ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
# . a1 Preparations ----
# Define fn and dat
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


fn <- "Input_files_2020/Samperapport_edited.xlsx"
fn <- "Input_files_2020/Samperapport 210621_edited.xlsx"
excel_sheets(fn)
# 1] "Fett"       "CP"         "HBCD"       "PBDE"       "PCB"        "Siloksaner" "Metaller"  

# Define data list that will be used 
dat <- vector("list", 7)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
# . a2 Data 1-2: data of type 1 (read_excel_nilu1) ----
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# debugonce(read_excel_nilu1)
dat[[1]] <- read_excel_nilu1(fn, "PBDE",
                            lessthans_given_as_negative_number = TRUE) %>%
  mutate(Group = "PBDE")
# View(dat[[1]])

xtabs(~dat[[1]]$Tissue)


# debugonce(read_excel_nilu1)

dat[[2]] <- read_excel_nilu1(fn, "PCB",
                           lessthans_given_as_negative_number = TRUE,
                           name_Sample_amount = "Analysed sample amount:",
                           ) %>%
  mutate(Group = "PCB")
# View(dat[[2]])


check_data(dat[[2]])


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
# . b Data 3, HBCD - manually ----   
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# File type 1, but with the variation that "<" is in a separate column....

# Define columns with concentrations (NOT counting the first "Component" column)
# NOTE: '60' is a HARD-CODED NUMBER - if fewer/more samples, adjust!
cols_conc <- seq(2,60,by = 2)        
cols_lessthans <- seq(1,60,by = 2)

# Metadata
df_meta <- read_excel(fn, sheet = "HBCD", n_max = 10, col_names = FALSE) %>%
  as.matrix() %>%
  t() %>%
  .[-1,]
sample_numbers_nilu <- df_meta[cols_conc, 2]
# tissue <- df_meta[cols_conc, 7]

# Data including less-thans
df2a <- read_excel(fn, sheet = "HBCD", skip = 13, n_max = 3, col_names = FALSE) %>%
  as.matrix() %>%
  t()
df2b <- df2a[-1,] %>%
  as.data.frame()
colnames(df2b) <- df2a[1,]

# Data on broad form
# Concentrations
df_data_broad <- df2b[cols_conc,]
df_data_broad$Sample_no_NILU <- sample_numbers_nilu
# df_data_broad$Tissue <- tissue   # will be added later

# Less-thans
df_data_broad_lt <- df2b[cols_lessthans,]
df_data_broad_lt$Sample_no_NILU <- sample_numbers_nilu

# Prepare  NIVA sample numbers + tissue from data set number 1 (Fett)
df_sampleno_niva <- dat[[1]] %>%
  distinct(Sample_no_NILU, Sample_no, Tissue)

# Prepare Less-thans, long (tidy) form
df_data_lt <- df_data_broad_lt %>%
  pivot_longer(-Sample_no_NILU, names_to = "Parameter", values_to = "Flag1")

# Finish data
df_data <- df_data_broad %>%
  pivot_longer(c(-Sample_no_NILU), names_to = "Parameter", values_to = "Value") %>%  # Concentrations, long (tidy) form
  mutate(
    Value = as.numeric(Value),
    Sample_no_NILU = sub("20", "21", Sample_no_NILU)
    ) %>%      # Sample_no_NILU start with '20', should be 21 
  left_join(df_data_lt, by = c("Parameter", "Sample_no_NILU")) %>%  # Add less-thans
  left_join(df_sampleno_niva, by = "Sample_no_NILU") %>%            # Add NIVA sample numbers + tissue
  mutate(
    Sample_amount = 1,
    Unit = "ng/g",
    Group = "HBCD"
  )

dat[[3]] <- df_data

xtabs(~addNA(Parameter) + addNA(Tissue), dat[[3]])

# View(dat[[3]])
# str(dat[[3]])

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
# . c Data 4 (CP) - manually ---- 
# Almost type 2, but "<" is given as separate column
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

df1 <- read_excel(fn, sheet = "CP", range = "A2:H31", 
                      col_names = c("Sample_no_NILU", "Matrix", "Sample_no", 
                                    "SCCP_lt", "SCCP", "MCCP_lt", "MCCP", "Sample_amount"))

# Prepare less-thans
df_data_lt <- df1 %>%
  select(Sample_no_NILU, SCCP_lt, MCCP_lt) %>%
  pivot_longer(-Sample_no_NILU, names_to = "Parameter", values_to = "Flag1") %>%  # long (tidy) form
  mutate(Parameter = sub("_lt", "", Parameter))   # needed for join below

df_data <- df1 %>%
  select(-SCCP_lt, -MCCP_lt) %>%
  pivot_longer(c(SCCP, MCCP), names_to = "Parameter", values_to = "Value") %>%  # Concentrations, long (tidy) form
  left_join(df_data_lt, by = c("Parameter", "Sample_no_NILU")) %>%              # Add less-thans
  mutate(
    Group = "CP",
    Tissue = case_when(
      grepl("egg", Matrix) ~ "Egg",
      grepl("blod", Matrix) ~ "Blod",
      TRUE ~ Matrix
    ),
    Unit = "mg/kg"
  ) %>%
  select(-Matrix)

dat[[4]] <- df_data

# View(dat[[4]])

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
# . d Data 5 (metals) data type 2 (read_excel_nilu2) ----  
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# You will get these two warnings, that is OK:
# warning for NA in `Sample_amount`
# warning for NA in `Value`

dat[[5]] <- read_excel_nilu2(fn, "Metaller",
                       lessthans_given_as_negative_number = TRUE, 
                       contains_sample_amount = TRUE,
                       skip = 5) %>%
  mutate(
    # Extract second "word", i.e. exctract "Cr" from "52  Cr  [ He ]"
    Parameter = stringr::str_extract(Parameter, "(?<=\\s)[^\\s]+(?=\\s)"),  
    Group = "Metaller"
    )

# xtabs(~addNA(Sample_no) + addNA(Parameter), dat[[5]])

tab <- xtabs(~Sample_no, dat[[5]])
cat("Number of measurements per sample: ", 
    paste(unique(tab), collapse = ","), "\n\n")

if (length(unique(tab)) > 1)
  warning("Different number of measurements per sample")

dat[[5]] <- dat[[5]] %>%
  filter(!is.na(Sample_no))

# unique(dat[[5]]$Parameter) %>% stringr::str_extract("(?<=\\s)[^\\s]+(?=\\s)")

# View(dat[[5]])

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
# . e Data 6 (fat) - manually ----
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

df <- read_excel(fn, "Fett")
colnames(df)
df <- df %>%
  rename(Sample_no_NILU = `Lab nr.`,
         Sample_no = `NIVA ID.nr.`,
         Tissue = Matrix,
         Value = `Fett %`) %>%
  mutate(Parameter = "Fett %",
         Sample_amount = NA, IPUAC_no = as.numeric(NA), Unit = "%", Flag1 = as.character(NA))
dat[[6]] <- df %>% 
  select(Sample_no_NILU, Sample_no, Tissue, Sample_amount, Parameter, IPUAC_no, Value, Unit, Flag1) %>% # line copied from read_excel_nilu1 or .2
  mutate(Group = "Fat")

# View(dat[[6]])

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
# . f Data 7 (siloxans) - manually ----  
#
# NOTE: This sheet also contains cod - saved as separate file in this section      
#
# Differs from 1-6 by not using sample ID, but sample numbers (1-15 and 90-95)
# - just using a new variable for this  
#
# Also contains LOD and LOQ 
# - Note that in the "edited" version of the excel file, I have reshuffled these columns
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

x <- c("D4", "D5", "D6")
varnames <- c("Sample", 
              x, paste0(x, "_LOD"), paste0(x, "_LOQ"),
              "Kommentar", "Tissue", "Specimen_label")

df1 <- read_excel(fn, "Siloksaner", range = "A2:M107",     # read all data, including cod
                 col_names = varnames,
                 col_types = c(rep("text",4), rep("numeric", 6), rep("text",3))
                 )

# Make long data set for value text  
df_data_txt <- as.data.frame(df1)[,c(1:4,11:13)] %>%
  pivot_longer(c(D4,D5,D6), names_to = "Parameter", values_to = "Value_txt")

# Prepare less-thans
df_data_lt <- df_data_txt %>%
  select(Parameter, Sample, Value_txt) %>%
  mutate(Flag1 = ifelse(
    grepl("<", Value_txt), "<", as.character(NA)
  ))

# Prepare LOD and LOQ
df_data_lod <- as.data.frame(df1)[,c(1, 5:7)] %>%
  pivot_longer(-Sample, names_to = "Parameter", values_to = "LOD") %>%
  mutate(Parameter = sub("_LOD", "", Parameter))
df_data_loq <- as.data.frame(df1)[,c(1, 8:10)] %>%
  pivot_longer(-Sample, names_to = "Parameter", values_to = "LOQ") %>%
  mutate(Parameter = sub("_LOQ", "", Parameter))

df_data <- df_data_txt %>%
  mutate(
    x1 = sub("<", "", Value_txt),
    x2 = sub(",", ".", x1),
    Value = as.numeric(x2)
    ) %>% # View()
  left_join(df_data_lt %>% select(-Value_txt), 
            by = c("Parameter", "Sample")) %>%                           # Add less-thans
  left_join(df_data_lod, by = c("Parameter", "Sample")) %>%              # Add LOD
  left_join(df_data_loq, by = c("Parameter", "Sample")) %>%              # Add LOQ
  mutate(Unit = "ng/g") %>%
  select(-x1, -x2)
# View(df_data)

# Check Tissue  
xtabs(~addNA(Tissue) + addNA(Parameter), df_data)

# Save for later use
# saveRDS(df_data, "Data/808_Siloxans_2020.rds")
# Read
# df_data <- readRDS("Data/808_Siloxans_2020.rds")

#
# Get Eider duck data
# - added to 'dat'
#
dat[[7]] <- df_data %>%
  filter(Tissue %in% c("Egg","Blod")) %>%
  mutate(Group = "Siloxans") %>%
  select(-Kommentar)

#
# Get cod data
# - saved as a separate file instead of adding to 'dat'
# - Note that the 'Sample_no' numbers are not correct and should just be ignored!
#   We will instead use Specimen_label (set manually in the excel file)
# - See script 814 for adding these data to Nivabasen
#
dat_cod <- df_data %>%
  filter(Tissue %in% c("Lever")) %>%
  mutate(
    Group = "Siloxans",
    Sample_no = paste("Nr.", substr(Sample, 1, 10))) %>%
  select(-Kommentar) %>%
  rename(SPECIMEN_NO = Specimen_label)   # when we made script 814, we used 'SPECIMEN_NO', so we stick to that

saveRDS(dat_cod, "Data/808_dat_cod-siloxans_2020.rds")

View(dat_cod)

#
# 3. Checks before combining the separate NILU files to a single file ----
#

# Check column types
get_coltypes <- function(dataframe){
  df <- dataframe %>% purrr::map_chr(class) %>% matrix(nrow = 1) %>% as.data.frame(stringsAsFactors = FALSE)
  colnames(df) <- colnames(dataframe)
  df
}
dat %>% purrr::map_df(get_coltypes)

# Remove some columns before combining
dat[[7]]$Sample <- NULL
dat[[7]]$Value_txt <- NULL

# Values of some columns
dat %>% purrr::map(~table(.$Tissue))
dat %>% purrr::map(~table(.$Unit))


#
# 4. Combine data 1-7 ----
#    Includes fixing units and tissues
#

dat_eiderduck <- bind_rows(dat) %>%
  select(-Sample_amount) %>%           # not needed (and not always with a value)
  mutate(
    TISSUE_NAME = case_when(
      grepl("egg", Tissue, ignore.case = TRUE) ~ "Egg",
      grepl("blod", Tissue, ignore.case = TRUE) ~ "Blod",
      TRUE ~ Tissue),
    UNIT = case_when(
      Unit %in% "mg/kg" ~ "MG_P_KG",
      Unit %in% "ng/g" ~ "UG_P_KG",
      Unit %in% "%" ~ "PERCENT")
  ) 

# Check unit
xtabs(~Group + addNA(Unit), dat_eiderduck)
xtabs(~Group + addNA(UNIT), dat_eiderduck)

# Check tissue
xtabs(~Group + addNA(Tissue), dat_eiderduck)
xtabs(~Group + addNA(TISSUE_NAME), dat_eiderduck)

#
# 5. Parameters (PARAM) ----
#    originally Parameter, we set PARAM here ('Parameter' is kept)
#

# table(dat_eiderduck$Parameter)

# Setting PARAM
dat_eiderduck$PARAM <- ""                             # Create PARAM
dat_eiderduck <- nilu_param_pcb_pbde(dat_eiderduck)   # Set PARAM for PBCs and PBDEs (using IPUAC nr)
dat_eiderduck <- nilu_param(dat_eiderduck) %>%        # Set PARAM for the most of the rest
  mutate(
    PARAM = case_when(
      Parameter %in% "Sum 7 PCB" ~ "Sum PCB7",
      TRUE ~ PARAM),
    PARAM = case_when(                              
      is.na(PARAM) ~ Parameter,                                     # PARAM = Parameter in the case of the rest (incl metals)
      !is.na(PARAM) ~ PARAM)
  )


#
# 6. Add METHOD_ID ----
#

if (FALSE){
  
  # Get existing NILU methods from METHOD_DEFINITIONS
  # One time only
  
  set_credentials()
  
  # Get df_methods for NILU
  df_methods_nilu <- get_nivabase_selection("*", "METHOD_DEFINITIONS", 
                                            "LABORATORY", "NILU", values_are_text = TRUE)
  
  saveRDS(df_methods_nilu, "Data/808_df_methods_nilu_2020.rds")

}

#
# 
df_methods_nilu <- readRDS("Data/808_df_methods_nilu_2020.rds") %>%
  filter(is.na(MATRIX) | (MATRIX %in% c("Biota","BIOTA")))


# table(addNA(df_methods_nilu$MATRIX))

#
# Select methods by METHOD_ID (makes 'df_methods_test')
#

meth_id <- 32375 + seq(-250,250)
meth_id <- c(meth_id, 10370, 19602, 27765:27767, 27710, 27711, 28434, 29338, 29339, 29340, 29341, 29342, 34204, 34206, 35084)
# range(meth_id)
df_methods_test <- df_methods_nilu %>%
  filter(METHOD_ID %in% meth_id) %>% 
  select(METHOD_ID, NAME, UNIT, METHOD_REF, ENTERED_BY, ENTERED_DATE)
# View(df_methods_test)
# plot(df_methods_test$METHOD_ID)

# Are NAME unique?
tab <- pull(df_methods_test, NAME) %>% table()
if (sum(tab>1) > 0){
  warning("Some NAME are not unique! Please remove some METHOD_ID values.")
  tab[tab>1]
}

# Are all PARAM values found? (Makes a 'test' by joining)
test <- dat_eiderduck %>%
  left_join(df_methods_test, by = c("PARAM" = "NAME"))
nrow(dat_eiderduck)
nrow(test)
test_lacking <- test %>% filter(is.na(METHOD_ID)) %>% pull(PARAM) %>% unique()
if (length(test_lacking) > 0){
  warning("Some PARAM does not have a corresponding NAME!")
  test_lacking
}

# If any, where are the lacking PARAM values found in 'df_methods_nilu'?
if (length(test_lacking) > 0){
  df_lacking <- df_methods_nilu %>%
    filter(NAME %in% test_lacking & grepl("w.w.", UNIT, fixed = TRUE)) %>%
    select(METHOD_ID, NAME, UNIT, METHOD_REF, ENTERED_BY, ENTERED_DATE) %>%
    arrange(METHOD_ID)
  View(df_lacking)
  df_lacking %>% pull(METHOD_ID) %>% .[-c(3,9)] %>% dput()
}

#
# Do the final join to add METHOD_ID
#

df_methods <- df_methods_test %>%
  select(NAME, METHOD_ID)

dat_eiderduck_joined <- dat_eiderduck %>%
  left_join(df_methods, by = c("PARAM" = "NAME"))

if (nrow(dat_eiderduck) < nrow(dat_eiderduck_joined)){
  stop("Error! Not all NAME are unique")
} else if (sum(is.na(dat_eiderduck_joined$METHOD_ID)) > 0){
  stop("Error! Not all PARAM are found")
  dat_eiderduck_joined 
} else {
  dat_eiderduck <- dat_eiderduck_joined
  cat("METHOD_ID successfuly added")
}

#
# 7. Check and fix sample identities ---- 
#    part b fixes 'Sample_no'  
#

#
# a. Sample_no vs Specimen_label
#    Only demonstrated here. This will be handled in script 812 '2020data'  
#

# Siloxans lack 'Sample_no'   
xtabs(~is.na(Sample_no) + Group, dat_eiderduck)

# But siloxans have 'Specimen_label' instead     
xtabs(~is.na(Specimen_label) + Group, dat_eiderduck)


#
# b. Sample_no values
#    Fixed here
#

# One sample lacks '.' after 'Nr'

# test
# sub("Nr ", "Nr. ", dat_eiderduck$Sample_no, fixed = TRUE) %>% table()
dat_eiderduck$Sample_no <- sub("Nr ", "Nr. ", dat_eiderduck$Sample_no, fixed = TRUE)


# After checking 'df_samples_eider' in script 812 (2020 version)
#   we find that all Eider duck sample numbers start with "NR-2020-084"
substr(df_samples_eider$TEXT_ID, 1, 11) %>% unique()

# ...while Sample_no it the table we just created mostly lack the '8' (the exception is a few PCBs)
substr(dat_eiderduck$Sample_no, 1, 11) %>% table()

# Let us insert that lacking '8'
dat_eiderduck$Sample_no <- sub("Nr. 2020-04", "Nr. 2020-084", dat_eiderduck$Sample_no, fixed = TRUE)

# Check table
# Note that some samples lacks PCB, while others have only PCB
xtabs(~Sample_no + Group, dat_eiderduck)

#
# 8. Check METHOD_ID ---- 
#     All OK
#

if (sum(is.na(dat_eiderduck$METHOD_ID)) > 0){
  stop("All data must have METHOD_ID!")
}

# Summary
xtabs(~addNA(TISSUE_NAME) + Group + is.na(METHOD_ID), dat_eiderduck)

# long output:
# xtabs(~METHOD_ID + Group, dat_eiderduck)

#
# 9. Save ----
#

# NOTE!! PCB data with negative values (actually data < LOQ) were not correcetd to 
#   positive numbers with FLAG1 = "<"
# Later corrected in the database, see 812 part 90  

# Version 1 - did not have  
# file.copy("Data/808_dat_eiderduck_2020.rds", "Data/808_dat_eiderduck_2020_ver01.rds")

saveRDS(dat_eiderduck, "Data/808_dat_eiderduck_2020.rds")

# dat_eiderduck <- readRDS("Data/808_dat_eiderduck_2020.rds")
# SEE NOTE ABOVE


