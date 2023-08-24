
library(dplyr)
library(ggplot2)

# dat_all <- readRDS("Files_from_Jupyterhub_2021/Big_excel_table/Data_xl_2022-09-23_ver03.rds")
dat_all <- readRDS("Files_from_Jupyterhub_2021/Big_excel_table/Data_xl_2022-09-23_ver06.rds")

names(dat_all)

# dat <- dat_all %>% filter(!is.na(Yr_2021))
# xtabs(~PARAM, dat)
# xtabs(~PARAM + STATION_CODE, dat)

# Check MCCP 
param <- "MCCP eksl. LOQ"
param <- "SCCP eksl. LOQ"
dat <- dat_all %>% filter(!is.na(Yr_2021) & PARAM %in% param)
sel_cols <- c("PARAM", "STATION_CODE", "TISSUE_NAME", "Basis", 
              grep("Yr_20", names(dat), fixed = TRUE, value = TRUE)) 
              
# dat[sel_cols] %>% filter
dat[sel_cols] %>% filter(Basis == "WW") %>% arrange(TISSUE_NAME)

#
# Check data sent to Jutyerhub
#
dat_check_all <- readRDS("Files_to_Jupyterhub_2021/01_df_2021_notstandard_2022-09-23.rds")

dat_check <- dat_check_all %>% filter(NAME %in% param)

dat_check %>%
  group_by(STATION_CODE) %>%
  summarise(fraction_u_loq = mean(!is.na(FLAG1))) %>%
  as.data.frame() %>%
  arrange(fraction_u_loq)
# IS OK! only 3 have <50% under LOQ

# dat_check_st <- dat_check %>% filter(STATION_CODE %in% "30B")
 

#
# Write to csv
#


### HARD-CODED! OVERWRITING! ###

# Actually this DOESN'T work - when copying from csv to excel
fn <- "Files_from_Jupyterhub_2021/Big_excel_table/Data_xl_2022-09-23_ver06.csv"
write.csv(dat_all, fn, row.names = FALSE, quote = TRUE, na = "")

fn <- "Files_from_Jupyterhub_2021/Big_excel_table/Data_xl_2022-09-23_ver06_fromR.xlsx"
writexl::write_xlsx(dat_all, fn)


#
# => Make copy of excel file named "Template" someting  
# => Rename to same name as csv file above
# => Open csv in Notepad ++
# => Select all and copy to excel file  
# => Data : Text to columns in Excel
#

datxl_all <- readxl::read_excel("Files_from_Jupyterhub_2021/Big_excel_table/Data_xl_2022-09-23_ver03.xlsm")

# Check MCCP 
param <- "MCCP eksl. LOQ"
datxl <- datxl_all %>% 
  filter(!is.na(Yr_2021) & PARAM %in% param) %>%
  as.data.frame()

sel_cols <- c("PARAM", "STATION_CODE", "TISSUE_NAME", "Basis", 
              grep("Yr_20", names(dat), fixed = TRUE, value = TRUE)) 

datxl[sel_cols] %>% filter(Basis == "WW") %>% arrange(TISSUE_NAME)
