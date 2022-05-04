
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Make 
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


#
# 1. Packages ----
#

library(dplyr)
library(niRvana)    # github 
library(lubridate)
library(openxlsx)

source("809_Make_NILU_templates_functions.R")

lims_project_name <- "O 210330;ANA21 - MILKYS 2021; Hovedanalyser 2021"


#
# 2. 2020 data ----
#
# Just for getting parameter names  
#

dat_eiderduck_2020 <- readRDS("Data/808_dat_eiderduck_2020.rds")
first_sample_number <- "Nr. 2020-08432"

parametergroup_table <- table(dat_eiderduck_2020$Group)


if (FALSE){
  dat_eiderduck$Sample_no %>% head(40)
  table(addNA(dat_eiderduck_2020$Group))
}

#
# 3. Make header rows (header_4) ----
#

sql <- paste(
  "select ANALYSEOPPDRAG, TEXT_ID, SAMPLED_DATE, DESCRIPTION, TISSUE, BIOTA_SAMPLENO",
  "from NIVADATABASE.LABWARE_CHECK_SAMPLE",
  "where PROSJEKT =", sQuote(lims_project_name),
  "and SPECIES = 'Somateria mollissima'")

sql

lims_eiderduck <- get_nivabase_data(sql) %>% arrange(TEXT_ID)

dat_for_excel_tall <- lims_eiderduck %>%
  mutate(Customer = "MILKYS (NIVA)", 
         Sample_type = paste("Ærfugl", substring(TISSUE, 4))) %>%
  select(Customer, DESCRIPTION, TEXT_ID, Sample_type)

dat_for_excel_1 <- t(dat_for_excel_tall) %>%
  as.data.frame()

pargroup <- "PBDE"

header_1 <- dat_for_excel_tall %>%
  as.data.frame()
# colnames(header_1) <- c("Customer:", "Comment:", "Customers sample ID:", "Sample type:")

# Add two empty columns first and four after
# - after transforming (header_3), this becomes two empty rows above and four below
header_2 <- data.frame(
  data.frame(V1 = "", V2 = ""),
  header_1,
  data.frame(V7 = "", V8 = "", V9 = "", V10 = "")
)

# Transform rows -> columns  
header_3 <- t(header_2)

#
# Insert extra empty columns before each sample (for less-than sign)
#

# - the first sample
i <- 1
header_4 <- bind_cols(data.frame(V0 = ""), as.data.frame(header_3[,i]))
colnames(header_4) <- paste0(c("LT", "V"), i)

# - the rest of the samples
for (i in 2:ncol(header_3)){
  data_to_add <- bind_cols(data.frame(V0 = ""), as.data.frame(header_3[,i]))
  colnames(data_to_add) <- paste0(c("LT", "V"), i)
  header_4 <- bind_cols(header_4, data_to_add)
  }


#
# 4. Make list of data for excel sheets ----
#

# test
# sheet_data <- make_sheet(header_4, dat_eiderduck_2020, "Nr. 2020-08432", "PBDE")

parametergroups <- names(parametergroup_table)

sheet_data_list <- lapply(
  parametergroups,
  make_nilu_template_sheet,
  header_main_part = header_4, 
  parameter_data = dat_eiderduck_2020, 
  example_sample_number = "Nr. 2020-08432")
)

names(sheet_data_list) <- parametergroups

#
# 5. Write to excel ----
#

# Write to excel  
fn <- "Files_for_other_use/Template_NILU_eider_2021.xlsx"
wb <- openxlsx::write.xlsx(
  sheet_data_list, 
  file = fn,
  colNames = FALSE)

# Set column with for less-than columns and rewrite  
cols_narrow <- seq(4, by = 2, length = (ncol(sheet_data_list[[1]])-3)/2)
openxlsx::setColWidths(wb, sheet = 1, cols = cols_narrow, widths = 5)
openxlsx::saveWorkbook(wb, fn, overwrite = TRUE)

