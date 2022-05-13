
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Make excel templates for the use in NILU
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


#
# 1. Packages ----
#

library(dplyr)
library(purrr)
library(niRvana)    # github 
library(lubridate)
library(openxlsx)

source("809_Make_NILU_templates_functions.R", encoding = "UTF-8")

lims_project_name <- "O 210330;ANA21 - MILKYS 2021; Hovedanalyser 2021"


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 2. Parameters ----
#
# NILU and NIVA names    
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

df_parameters <- readxl::read_excel(
  "Input_files_2020/NILU_template/Parameters_NILU_NIVA_2020.xlsx")

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 3. Get data on sample ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Get samples for last year
sql <- paste(
  "select *",
  "from NIVADATABASE.LABWARE_CHECK_SAMPLE",
  "where PROSJEKT =", sQuote(lims_project_name),
  "and SPECIES = 'Somateria mollissima'")

sql <- paste(
  "select ANALYSEOPPDRAG, TEXT_ID, SAMPLED_DATE, DESCRIPTION, TISSUE, BIOTA_SAMPLENO",
  "from NIVADATABASE.LABWARE_CHECK_SAMPLE",
  "where PROSJEKT =", sQuote(lims_project_name),
  "and SPECIES = 'Somateria mollissima'")

# sql

lims_eiderduck <- get_nivabase_data(sql) %>% arrange(TEXT_ID)

df_samples <- lims_eiderduck %>%
  mutate(Prøvetype = paste("Ærfugl", substring(TISSUE, 4)),
         NILU_ID = "",
         Kommentar = "",
         Vekt = "") %>%
  rename(NIVA_ID = TEXT_ID) %>%
  select(NILU_ID, NIVA_ID, Prøvetype, Kommentar, Vekt)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 4. Make list of parameter groups for excel sheets ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# test
# sheet_data <- make_sheet(header_4, dat_eiderduck_2020, "Nr. 2020-08432", "PBDE")

parametergroups <- unique(df_parameters$Group)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 5. Write to excel ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# . - List of parameter groups for excel sheets ----

# Will be one excel sheet per group


# . - Test function ----  
# 'make_nilu_template_sheet'     

test <- make_nilu_template_sheet(
  sample_data = df_samples,
  parameter_data = df_parameters,
  parametergroup_name = "HBCD",
  unit = "ng/g")

# dim(test)
# test[1:12, 1:7]

# END of test

# . - Make list of sheet data ---- 

sheet_list <- map(
  parametergroups,
  ~ make_nilu_template_sheet(
    sample_data = df_samples, 
    parameter_data = df_parameters, 
    parametergroup_name = ., 
    unit = "")
)

names(sheet_list) <- parametergroups

#
# . - Write sheets to excel ----
#

fn <- "Input_files_2020/NILU_template/Template_NILU_eider_2021.xlsx"
wb <- openxlsx::write.xlsx(
  sheet_list, 
  file = fn,
  colNames = FALSE)

#
# Increase font for first lines
#

style_big <- createStyle(fontSize = 15)
style_bold <- createStyle(textDecoration = "bold")
style_locked <- createStyle(locked = TRUE)

for (i in 1:length(sheet_list)){
  # Set column with for less-than columns and rewrite  
  openxlsx::setColWidths(wb, sheet = i, cols = c(1:3,5), widths = 18)
  addStyle(wb, sheet = i, style = style_big, rows = c(1,3,4), cols = 1)
  addStyle(wb, sheet = i, style = style_shaded, rows = 5:9, cols = 5)
  # Locking cells. This didn't work
  # addStyle(wb, sheet = i, style = style_bold, rows = 9, cols = 1:4)
  # for (row in 5:8){
  #   addStyle(wb, sheet = i, style = style_locked, 
  #            rows = row, cols = seq_len(ncol(sheet_list[[i]])) + 4)
  #   }
  # for (col in 2:3){
  #   addStyle(wb, sheet = i, style = style_locked, 
  #            rows = seq_len(nrow(sheet_list[[i]])) + 8, cols = col)
  # }
}
openxlsx::saveWorkbook(wb, fn, overwrite = TRUE)




#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# APPENDIX 1. Make Parameters_NILU_NIVA_2020.xlsx ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

df_parameters <- readxl::read_excel(
  "Input_files_2020/NILU_template/Parameters_NILU_NIVA_2020.xlsx")


if (FALSE){
  
  # . - NILU data ----
  dat_eiderduck_2020 <- readRDS("Data/808_dat_eiderduck_2020.rds")
  first_sample_number <- "Nr. 2020-08432"
  
  # getting 'first_sample_number'
  dat_eiderduck_2020$Sample_no %>% head(40)   # get 'first_sample_number'
  table(addNA(dat_eiderduck_2020$Group))
  
  #
  # . - Making 'Parameters_NILU_2020.xlsx' ----
  #
  # which was edited into 'Parameters_NILU_NIVA_2020.xlsx'
  dat_eiderduck_2020 %>%
    filter(Sample_no == "Nr. 2020-08432") %>%
    select(Parameter, IPUAC_no, Group) %>% # View()
    writexl::write_xlsx("Input_files_2020/NILU_template/Parameters_NILU_2020.xlsx")
  
  # . -  Reported_name and Analysis ----
  #
  # Code used to find  Reported_name and Analysis 
  #
  
  sql <- paste(
    "select distinct reported_name, analysis from labware.result",
    "where lower(reported_name) like ('%157%')")
  lims_reported_name <- get_nivabase_data(sql)
  # lims_reported_name
  
  sql <- paste(
    "select distinct reported_name, analysis from labware.result",
    "where lower(analysis) like ('%pcblike%')")
  lims_reported_name <- get_nivabase_data(sql) %>% arrange(ANALYSIS, REPORTED_NAME)
  # lims_reported_name
  
  sql <- paste(
    "select distinct name, laboratory, unit from nivadatabase.method_definitions",
    "where lower(name) like ('%hbcd%')")
  ndb_name <- get_nivabase_data(sql)
  # ndb_name
  
}



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# APPENDIX 2. Test sheet (PBDE) ----
#
# Used to make functions in '809_Make_NILU_templates_functions.R'  
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

param_group <- "PBDE"
prosjektnr <- "O 210330"

#
# . - left part of sheet (df_excel_1) ---- 
#

# Number of extra rows on top:
n_extra <- 5   

df_samples_matrix <- as.matrix(df_samples, ncol = ncol(df_samples))
# df_samples_matrix[1:5,]

df_excel_1 <- matrix("", 
                     nrow = nrow(df_samples_matrix) + n_extra, 
                     ncol = ncol(df_samples_matrix))

# dim(df_excel_1)
df_excel_1[n_extra, ] <- colnames(df_samples_matrix)
df_excel_1[-(1:n_extra),] <- df_samples_matrix[]
df_excel_1[1:7, 1:5]

#
# . - right part of sheet (df_excel_2) ---- 
#

df_pars <- df_parameters %>%
  filter(Group %in% param_group) %>%
  mutate(IPUAC_no = as.character(IPUAC_no)) %>%
  select(Parameter_NILU, IPUAC_no, Parameter_LIMS, Analysis) %>%
  t()

dim(df_pars)

df_excel_2 <- matrix("", 
                     nrow = nrow(df_samples) + n_extra,
                     ncol = ncol(df_pars))
df_excel_2[1:4,] <- df_pars
df_excel_2[5,] <- "ng/g"

df_excel_2[1:8,1:5]
# colnames(df_excel_2) <- df_pars_tall$Parameter

#
# . - left + right part of sheet (df_excel_bottom) ---- 
#

if (nrow(df_excel_1) != nrow(df_excel_2)){
  stop("The two df_excel parts have different number of rows!")
}

df_excel_bottom <- cbind(df_excel_1, df_excel_2)

df_excel_bottom[1:5, ncol(df_excel_1)] <- 
  c("Parameter", "IUPAC no.", "NIVA_parameter", "NIVA_analysis", "Unit")

df_excel_bottom[1:8, 1:9]

#
# . - combine to entire sheet (df_excel) ---- 
#

df_excel_top <- matrix("", nrow = 4, ncol = ncol(df_excel_bottom))
df_excel_top[1,1] <- paste(param_group, "i biologisk materiale")
df_excel_top[3,1] <- paste("Prosjektnr:", prosjektnr)
df_excel_top[4,1] <- paste("Rapportnr:")

df_excel <- rbind(df_excel_top, df_excel_bottom)
df_excel[1:10, 1:7]


