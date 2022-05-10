
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

source("809_Make_NILU_templates_functions.R")

lims_project_name <- "O 210330;ANA21 - MILKYS 2021; Hovedanalyser 2021"


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 2. 2020 data ----
#
# Just for getting parameter names  
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

dat_eiderduck_2020 <- readRDS("Data/808_dat_eiderduck_2020.rds")
first_sample_number <- "Nr. 2020-08432"

parametergroup_table <- table(dat_eiderduck_2020$Group)


if (FALSE){
  dat_eiderduck_2020$Sample_no %>% head(40)   # get 'first_sample_number'
  table(addNA(dat_eiderduck_2020$Group))
}

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

parametergroups <- names(parametergroup_table)  

parametergroups_list <- map(
  parametergroups,
  ~ get_nilu_parameters(
    nilu_data_long = dat_eiderduck_2020, 
    example_sample_number = "Nr. 2020-08432", 
    parameter_group = .)
  )

length(parametergroups_list)  

names(parametergroups_list) <- parametergroups

parametergroups_list[["CP"]]
parametergroups_list[["Siloxans"]] <- data.frame(
  Parameter = c("D4", "D5", "D6"),
  IPUAC_no = NA) %>%
  mutate(
    NIVA_name = Parameter)

)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 5. Write to excel ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# . - Test function ----  
# 'make_nilu_template_sheet'     

test <- make_nilu_template_sheet(
  sample_data = df_samples, 
  parameter_data = parametergroups_list[["PBDE"]], 
  parametergroup_name = "PBDE", 
  unit = "ng/g")

dim(test)
test[1:12, 1:7]

# . - Make list of sheet data ---- 

sheet_list <- map(
  parametergroups,
  ~ make_nilu_template_sheet(
    sample_data = df_samples, 
    parameter_data = parametergroups_list[[.]], 
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

style_big <- createStyle(fontSize = 15)

for (i in 1:length(sheet_list)){
  # Set column with for less-than columns and rewrite  
  openxlsx::setColWidths(wb, sheet = i, cols = 1:3, widths = 18)
  addStyle(wb, sheet = i, style = style_big, rows = c(1,3,4), cols = 1)
}
openxlsx::saveWorkbook(wb, fn, overwrite = TRUE)




#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# APPENDIX. Test sheet (PBDE) ----
#
# Used to make functions in '809_Make_NILU_templates_functions.R'  
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o



param_group <- "PBDE"
prosjektnr <- "O 210330"

#
# . - left part of sheet (df_excel_1) ---- 
#

df_samples_matrix <- as.matrix(df_samples, ncol = ncol(df_samples))
# df_samples_matrix[1:5,]

df_excel_1 <- matrix("", 
                     nrow = nrow(df_samples_matrix) + 3, 
                     ncol = ncol(df_samples_matrix))

# dim(df_excel_1)
df_excel_1[3, ] <- colnames(df_samples_matrix)
df_excel_1[-(1:3),] <- df_samples_matrix[]
df_excel_1[1:5, 1:5]

#
# . - right part of sheet (df_excel_2) ---- 
#

df_pars_tall <- get_nilu_parameters(
  nilu_data_long = dat_eiderduck_2020, 
  example_sample_number = "Nr. 2020-08432", 
  parameter_group = param_group) %>%
  as.data.frame()

df_pars <- df_pars_tall %>%
  select(Parameter, IPUAC_no) %>%
  t()

dim(df_pars)

df_excel_2 <- matrix("", 
                     nrow = nrow(df_samples) + 3, 
                     ncol = ncol(df_pars))
df_excel_2[1:2,] <- df_pars
df_excel_2[3,] <- "ng/g"

df_excel_2[1:5,1:5]
# colnames(df_excel_2) <- df_pars_tall$Parameter

#
# . - left + right part of sheet (df_excel_bottom) ---- 
#

if (nrow(df_excel_1) != nrow(df_excel_2)){
  stop("The two df_excel parts have different number of rows!")
}

df_excel_bottom <- cbind(df_excel_1, df_excel_2)
df_excel_bottom[]

df_excel_bottom[1:5, 1:9]

#
# . - combine to entire sheet (df_excel) ---- 
#

df_excel_top <- matrix("", nrow = 4, ncol = ncol(df_excel_bottom))
df_excel_top[1,1] <- paste(param_group, "i biologisk materiale")
df_excel_top[3,1] <- paste("Prosjektnr:", prosjektnr)
df_excel_top[4,1] <- paste("Rapportnr:")

df_excel <- rbind(df_excel_top, df_excel_bottom)
df_excel[1:10, 1:7]


