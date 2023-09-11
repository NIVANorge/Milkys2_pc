
#
# Make SQL code for adding lacking SCCP and MCCP measurements below LOQ ()  
#

#
# Packages etc. ----
#

library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(knitr)     # for kable()

# And then the niRvana package. If you need to install it, then run thee next line also:
# devtools::install_github("NIVANorge/niRvana")
library(niRvana)

source("802_Get_NIVAbasen_data_functions.R")
source("812_Import_til_NIVAbasen_NILU_functions.R")


#
# Labware ----
#

#
# . project string ----
#
projects <- c("O 16330 ANA15 MILKYS 2016", 
              "O 17330;ANA16 MILKYS 2016/17", 
              "O 18330;ANA17 Milkys 2018, analyser-hoved-2017pr", 
              "O 19330;ANA18 - MILKYS 2018", 
              "O 19330;ANA19 - MILKYS2019", 
              "O 210330;ANA21 - MILKYS 2021; Hovedanalyser 2021",
              "O 210200.ANAIN")

# Make project string
old.o <- options(useFancyQuotes = FALSE)

project_string_0 <- paste(sQuote(projects), collapse = ", ")
project_string <- paste("and PROSJEKT in (", project_string_0, ")")
project_string

options(old.o)


#
# . get data ----
#

vars <- "SAMPLE_TYPE, SAMPLED_DATE, AQUAMONITOR_ID, AQUAMONITOR_CODE, REPORTED_NAME, ENTRY_QUALIFIER, FORMATTED_ENTRY, NUMERIC_ENTRY, UNITS, DESCRIPTION, STATUS, STATUSX, TEXT_ID, PROSJEKT, ANALYSEOPPDRAG, SAMPLE_NUMBER, T_ANALYSIS_METHOD, ANALYS"

df_ccp_all <- get_nivabase_selection(
  vars,
  "LABWARE_IMPORT",
  "REPORTED_NAME",
  c("SCCP eksl. LOQ", "SCCP inkl. LOQ", "MCCP eksl. LOQ", "MCCP inkl. LOQ"), values_are_text = TRUE, 
  extra_sql = project_string) %>%
  mutate(
    Month = month(SAMPLED_DATE),
    Year = year(SAMPLED_DATE),
    MYEAR = case_when(
      Month <= 2 ~ Year-1,
      Month >= 3 ~ Year))

xtabs(~STATUSX + MYEAR, df_ccp_all)
xtabs(~STATUS + MYEAR, df_ccp_all)

#
# . statistics ----
#

xtabs(~REPORTED_NAME, df_ccp_all)

xtabs(~REPORTED_NAME + FORMATTED_ENTRY, df_ccp_all %>% filter(is.na(NUMERIC_ENTRY)))
xtabs(~Year + FORMATTED_ENTRY, df_ccp_all %>% filter(is.na(NUMERIC_ENTRY)))

#
# . plots ----
#
# See Appendix 1

#
# Get Nivabasen sample ID ---- 
#

# Values of TEXT_ID field  
text_id <- df_ccp_all %>% pull(TEXT_ID) %>% unique()

# Get corresponding SAMPLE_ID in table BIOTA_CHEMISTRY_VALUES in Nivadatabase
# These values are stored in the table LABWARE_BSID when data are imported from the LIMS
lookup_sample_id <- get_nivabase_selection(
  "BIOTA_SAMPLE_ID, LABWARE_TEXT_ID",
  "LABWARE_BSID",
  "LABWARE_TEXT_ID",
  text_id, values_are_text = TRUE)

#
# . add SAMPLE_ID to LIMS data ----
#

if (!"BIOTA_SAMPLE_ID" %in% names(df_ccp_all)){
  
  df_ccp_all <- df_ccp_all %>%
    left_join(lookup_sample_id, by = c("TEXT_ID" = "LABWARE_TEXT_ID"))
  
}
  


#
# Get METHOD_ID ----   
#

df_biota <- readRDS("Files_to_Jupyterhub_2022/01_df_2022_notstandard_2023-08-11.rds")

lookup_method_id <- df_biota %>%
  filter(NAME %in% c("SCCP eksl. LOQ", "SCCP inkl. LOQ", "MCCP eksl. LOQ", "MCCP inkl. LOQ")) %>%
  distinct(NAME, METHOD_ID)

kable(lookup_method_id)
# |NAME           | METHOD_ID|
#   |:--------------|---------:|
#   |SCCP inkl. LOQ |     36524|
#   |MCCP inkl. LOQ |     36525|
#   |SCCP eksl. LOQ |     36522|
#   |MCCP eksl. LOQ |     36523|

#
# . add METHOD_ID to LIMS data ----
#
if (!"METHOD_ID" %in% names(df_ccp_all)){
  
  df_ccp_all <- df_ccp_all %>%
    left_join(lookup_method_id, by = c("REPORTED_NAME" = "NAME"))
  
}

#
# Make data for insertion ----   
# 

#
# . get data without numeric value ----
#

# Make list with one data frame per method  
df_for_inserting_unfiltered <- df_ccp_all %>%
  filter(is.na(NUMERIC_ENTRY)) %>%
  select(BIOTA_SAMPLE_ID, METHOD_ID) %>%
  mutate(
    VALUE = 0,
    FLAG1 = as.character(NA)) %>%
  rename(SAMPLE_ID = BIOTA_SAMPLE_ID) %>%
  split(.$METHOD_ID) 

# No of rows  
map_int(df_for_inserting_unfiltered, nrow)

# Methods  
methodids <- map_dfr(df_for_inserting_unfiltered, head, n = 1) %>%
  pull(METHOD_ID)

#
# . remove those that are already in Nivabase ---- 
#

# Make list for filtered data (existing Sample_id removed)
df_for_inserting <- list()

# 
# Filter
#
for (i in seq_along(df_for_inserting_unfiltered)){
  # Method ID
  methodid <- methodids[i]
  # Sample ID for given method
  sampleids <- df_for_inserting_unfiltered[[i]] %>%
    pull(SAMPLE_ID) %>%
    unique()
  sampleids_existing <-  get_nivabase_selection(
    "SAMPLE_ID, METHOD_ID, VALUE, FLAG1",
    "BIOTA_CHEMISTRY_VALUES",
    "SAMPLE_ID",
    sampleids, extra_sql = paste("and METHOD_ID =", methodid)
  ) %>% 
    pull(SAMPLE_ID)
  df_for_inserting[[i]] <- df_for_inserting_unfiltered[[i]] %>% 
    filter(!SAMPLE_ID %in% sampleids_existing)
  cat("METHOD_ID =", methodid, "\n")
  cat("Number of values with ND values:", nrow(df_for_inserting_unfiltered[[i]]), "\n")
  cat("Number of these values not in Nivabase:", nrow(df_for_inserting[[i]]), "\n")
}


#
# . add to Nivabase ---- 
#

# For copy-pasting into SQL Developer  
# The code below puts SQL text on the clipboard,
#   so it can be 

# test  
make_sql_chemistry_values(1, df_for_inserting[[1]])  

df_for_inserting_comb <- bind_rows(df_for_inserting)

sql_list <- 1:nrow(df_for_inserting_comb) %>% 
  map_chr(make_sql_chemistry_values, data = df_for_inserting_comb)
cat("Number of sql sentences: \n") # 2340
length(sql_list) # 2340

i <- 1:length(sql_list)
sql <- paste(sql_list[i], collapse = ";\n") # join sqls together, with "new line" character between
sql <- paste0(sql, ";\n")                   # add final "new line" character at the end
writeLines(sql, "clipboard-1024")   # copies SQLs to clipboard - go to SQL Developer and paste
# "clipboard-1024" instead of "clipboard": increases avaliable
#    for the clipboard

cat("\n") # 2340
cat("First sql sentences: \n") # 2340
head(sql_list, 3)

cat("\n") # 2340
cat("Last sql sentence: \n") # 2340
tail(sql_list, 1)


#
# . checking ----  
#

# Run 'remove those that are already in Nivabase' again   
#
# Should give result where 
# 'Number of these values not in Nivabase' always equal 0:
#
# METHOD_ID = 36522 
# Number of values with ND values: 765 
# Number of these values not in Nivabase: 0 
# METHOD_ID = 36523 
# Number of values with ND values: 681 
# Number of these values not in Nivabase: 0 
# METHOD_ID = 36524 
# Number of values with ND values: 9 
# Number of these values not in Nivabase: 0 
# METHOD_ID = 36525 
# Number of values with ND values: 10 
# Number of these values not in Nivabase: 0 


# Make list for filtered data (existing Sample_id removed)
check <- list()

# 
# Filter
#
for (i in seq_along(df_for_inserting)){
  # Method ID
  methodid <- methodids[i]
  # Sample ID for given method
  sampleids_existing <-  get_nivabase_selection(
    "SAMPLE_ID, METHOD_ID, VALUE, FLAG1",
    "BIOTA_CHEMISTRY_VALUES",
    "SAMPLE_ID",
    sampleids, extra_sql = paste("and METHOD_ID =", methodid)
  ) %>% 
    pull(SAMPLE_ID)
  check[[i]] <- df_for_inserting[[i]] %>% 
    filter(!SAMPLE_ID %in% sampleids_existing)
  cat("METHOD_ID =", methodid, "\n")
  cat("Number of values with ND values:", nrow(df_for_inserting_unfiltered[[i]]), "\n")
  cat("Number of these values not in Nivabase:", nrow(df_for_inserting[[i]]), "\n")
}

check <-  get_nivabase_selection(
  "SAMPLE_ID, METHOD_ID, VALUE, FLAG1",
  "BIOTA_CHEMISTRY_VALUES",
  "SAMPLE_ID",
  239877)
)
sel <- check$METHOD_ID %in% methodids
sum(sel)
View(check[sel,])

#
# Appendix 1 - plots ----  
#

# All data  
gg <- df_ccp_all %>%
  ggplot(aes(MYEAR, NUMERIC_ENTRY)) +
  geom_jitter(width = 0.3) +
  facet_wrap(vars(REPORTED_NAME))
gg 
gg + scale_y_log10()  

# Time series of one station    
station <- "02B"
station <- "71A"
gg <- df_ccp_all %>%
  filter(AQUAMONITOR_CODE == station) %>%
  ggplot(aes(MYEAR, NUMERIC_ENTRY)) +
  geom_jitter(width = 0.3) +
  facet_wrap(vars(REPORTED_NAME))
gg 
gg + scale_y_log10()  

# All stations in one year      
year <- 2022
year <- 2015
gg <- df_ccp_all %>%
  filter(MYEAR == year) %>%
  ggplot(aes(AQUAMONITOR_CODE, NUMERIC_ENTRY)) +
  geom_jitter(width = 0.3) +
  facet_wrap(vars(REPORTED_NAME)) +
  ggeasy::easy_rotate_labels(angle = -45)
gg 
gg + scale_y_log10()  

# All stations, all years, all CCP parameters           
gg <- df_ccp_all %>%
  filter(!is.na(NUMERIC_ENTRY)) %>%
  group_by(AQUAMONITOR_CODE, MYEAR,REPORTED_NAME) %>%
  summarise(
    min_value = min(NUMERIC_ENTRY),
    log10_min_value = log10(min_value)) %>%
  ggplot(aes(AQUAMONITOR_CODE, MYEAR, fill = log10_min_value)) +
  geom_tile() +
  facet_wrap(vars(REPORTED_NAME)) +
  ggeasy::easy_rotate_labels(angle = -45)
gg

# All stations, all years, one CCP parameter           
param <- "SCCP eksl. LOQ"
gg <- df_ccp_all %>%
  filter(REPORTED_NAME %in% param & !is.na(NUMERIC_ENTRY)) %>%
  group_by(AQUAMONITOR_CODE, MYEAR,REPORTED_NAME) %>%
  summarise(
    min_value = min(NUMERIC_ENTRY),
    log10_min_value = log10(min_value)) %>%
  ggplot(aes(AQUAMONITOR_CODE, MYEAR, fill = log10_min_value)) +
  geom_tile() +
  ggeasy::easy_rotate_labels(angle = -45)
gg


#
# Appendix 2: Test ----
#

#
# . from Nivadatabase to LIMS ----  
#

df_liver2022 <- df_biota %>%
  filter(year(SAMPLE_DATE) == 2022 & LATIN_NAME == "Gadus morhua" & TISSUE_NAME == "Lever") %>%
  left_join(lookup_sample_id, by = c("SAMPLE_ID" = "BIOTA_SAMPLE_ID"))

cat("Stats for ccp data, Nivabasen \n")      
df_liver2022 %>% 
  filter(NAME %in% c("SCCP eksl. LOQ", "SCCP inkl. LOQ", "MCCP eksl. LOQ", "MCCP inkl. LOQ")) %>%
  count(NAME) %>%
  kable()

# All from one station  
# df_liver2022 %>% 
#   filter(STATION_CODE %in% "30B") %>%
#   count(NAME)

id_liver <- df_liver2022 %>%
  pull(LABWARE_TEXT_ID) %>%
  unique()

df_ccp_liver2022 <- df_ccp_all %>%
  filter(TEXT_ID %in% id_liver)

cat("Stats for ccp data, LIMS \n")      
df_ccp_liver2022 %>%
  mutate(Lacking_value = is.na(NUMERIC_ENTRY)) %>%
  count(REPORTED_NAME, Lacking_value) %>%
  kable()

df_for_inserting <- df_ccp_liver2022 %>%
  filter(is.na(NUMERIC_ENTRY)) %>%
  select(BIOTA_SAMPLE_ID, REPORTED_NAME) %>%
  mutate(
    VALUE = 0,
    FLAG1 = as.character(NA))
nrow(df_for_inserting)

# Check that these doesn't already exist in Nivabase



