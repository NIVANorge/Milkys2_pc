
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

source('210_Read_NILU_excel_data_functions.R')

# Dropoed in this version
# source('01_Get_chemical_data_NIVAbasen_functions.R")  # for sum parameters


#
# Note: File fixed beforehand (see '31_Read_excel_data_functions.R')
# (Also the row names of the upper part had to be moved one row up)

#
# 2a. Read data parts, NILU files ----
#

fn <- "Input_files_2020/Samperapport.xlsx"
excel_sheets(fn)
# 1] "Fett"       "CP"         "HBCD"       "PBDE"       "PCB"        "Siloksaner" "Metaller"  

dat <- vector("list", 7)

# Data 1-2: read_excel_nilu1
dat[[1]] <- read_excel_nilu1(fn, "PBDE",
                            lessthans_given_as_negative_number = TRUE) %>%
  mutate(Group = "PBDE")

dat[[2]] <- read_excel_nilu1(fn, "PCB",
                           lessthans_given_as_negative_number = FALSE,
                           name_Sample_amount = "Analysed sample amount:",
                           ) %>%
  mutate(Group = "PCB")

# Data 3-5: read_excel_nilu2
# Note 3rd data column for CP sheet (dat[[4]]): "Rec %", we call it "Rec_percent"
dat[[3]] <- read_excel_nilu2(fn, "HBCD",
                       lessthans_given_as_negative_number = TRUE,
                       contains_sample_amount = TRUE) %>%
  mutate(Group = "HBCD")

dat[[4]] <- read_excel_nilu2(fn, "CP",
                       lessthans_given_as_negative_number = FALSE,
                       contains_sample_amount = TRUE) %>%
  mutate(Group = "CP") %>%
  mutate(Parameter = ifelse(Parameter == "X__1", "Rec_percent", Parameter))

dat[[5]] <- read_excel_nilu2(fn, "Metaller",
                       lessthans_given_as_negative_number = TRUE,
                       contains_sample_amount = FALSE,
                       skip = 2) %>%
  mutate(Group = "Metaller")

# Data 6: we do manually
df <- read_excel(fn, "Fett %")
colnames(df)
df <- df %>%
  rename(Sample_no_NILU = `Lab nr.`,
         Sample_no = Comment,
         Tissue = Matrix,
         Value = `Fett %`) %>%
  mutate(Parameter = "Fett %",
         Sample_amount = NA, IPUAC_no = as.numeric(NA), Unit = "%", Flag1 = as.character(NA))
dat[[6]] <- df %>% 
  select(Sample_no_NILU, Sample_no, Tissue, Sample_amount, Parameter, IPUAC_no, Value, Unit, Flag1) %>% # line copied from read_excel_nilu1 or .2
  mutate(Group = "Fat")

#
# 2b. Read data parts, siloxans ----
#

# Two data sets in this sheet, which has a different format
#
# Top: NIVA-collected data (cod from stations 30B, 24B, 43B2, 19B, 10B)
#   Contains station + same sample number as used for *muscle* (see MILKYS+Lange tidsserier - analyser 2018.xlsx sheet 24B)
# Bottom: NILU-collected data (eider duck at 19N)
#   Contains station + Sample_no_NILU + Sample_no
#

data_siloksan <- read_excel(fn, sheet = "Siloksaner", skip = 1)

# Change column names (knowing that these names doesn't fit well for the top part, we'll fix that later)
colnames(data_siloksan)[1:3] <- c("Sample_no_NILU", "Sample_no", "Tissue")

# Pick rows 
first_letters <- substr(data_siloksan$Sample_no_NILU, 1, 3)   # look at first part of 'Sample_no_NILU'
tab <- table(first_letters)                  
first_letters_selected <- names(tab)[tab >= 10]               # pick those that are frequent
data_siloksan <- data_siloksan[first_letters %in% first_letters_selected,]  # pick those rows from file

# Check manually:
# data_siloksan %>% View()

# Fix data
data_siloksan_fix <- data_siloksan %>%
  select(Sample_no_NILU:D6) %>%
  tidyr::gather("Parameter", "Value_flag", D4:D6) %>%                   # reformat data to tall/narrow form
  mutate(Value = sub(" *< *", "", Value_flag),                          # Split value into value+flag
         Flag1 = ifelse(grepl("<", Value_flag), "<", as.character(NA))
  ) %>% 
  mutate(Value = sub(")", "", Value)) %>%                                   # only one case 
  mutate(Value = as.numeric(sub(",", ".", Value))) %>%                      # turn value into number
  mutate(Value = ifelse(Value_flag %in% "Below fieldblank", 0, Value)) %>%  # We set "Below fieldblank" to Value=0
  mutate(Unit = "ng/g")

table(substr(data_siloksan$Sample_no_NILU, 1, 3))

#
# Split data set in two
#
# table(substr(data_siloksan$Sample_no_NILU, 1, 3)) %>% names() %>% dput()
data_siloksan_cod <- data_siloksan_fix %>%
  filter(substr(Sample_no_NILU,1,3) %in% c("10B", "19B", "24B", "30B", "43B"))

data_siloksan_eider <- data_siloksan_fix %>%
  filter(substr(Sample_no_NILU,1,3) %in% "18/")

#
# Check that data set is split correctly
#
check <- nrow(data_siloksan_eider) + nrow(data_siloksan_cod) == nrow(data_siloksan_fix)
if (!check)
  cat("SOME ROWS ARE LACKING FROM COD OR EIDER DATASET")

dat[[7]] <- data_siloksan_eider %>%
  select(-Value_flag) %>%
  as.data.frame()

dat[[6]] %>% head(3)
dat[[7]] %>% head(3)
# dat[[7]] %>% View()



#
# 3. Combine the separate NILU files to a single file ----
#

# Check column types
get_coltypes <- function(dataframe){
  df <- dataframe %>% purrr::map_chr(class) %>% matrix(nrow = 1) %>% as.data.frame(stringsAsFactors = FALSE)
  colnames(df) <- colnames(dataframe)
  df
}
dat %>% purrr::map_df(get_coltypes)

#
# Combine data 1-7
#
dat_nilu <- bind_rows(dat)

#
# Save at this stage
#
saveRDS(dat_nilu, "Data/31_data_nilu_niluformat.rds")

#
# 4. Units and tissues ----
#

# xtabs(~Unit, dat_nilu)
# xtabs(~UNIT, df_2018)  # df_2018 from script 01!

# Parameter UNIT added (old "Unit" is kept)
dat_nilu <- nilu_fix_units(dat_nilu)
dat_nilu <- nilu_fix_hg_unit(dat_nilu)
sum(is.na(dat_nilu$UNIT))  # SHOULD BE ZERO
table(dat_nilu$UNIT)

# Parameter TISSUE_NAME added (old "Tissue" is kept)
dat_nilu <- nilu_fix_tissue(dat_nilu)
sum(is.na(dat_nilu$TISSUE_NAME))  # SHOULD BE ZERO

nrow(dat_nilu)  # 2610

#
# 5. Parameters (PARAM) ----
#    originally Parameter, we set PARAM here ('Parameter' is kept)
#

dat_nilu$PARAM <- ""                        # Create PARAM
dat_nilu <- nilu_param_pcb_pbde(dat_nilu)   # Set PARAM for PBCs and PBDEs (using IPUAC nr)
dat_nilu <- nilu_param(dat_nilu)            # Set PARAM for the most of the rest

# Finally, Sum PCB
sel <- dat_nilu2$Parameter %in% "Sum 7 PCB"; sum(sel)
dat_nilu$PARAM[sel] <- "CB_S7"

# All parameters
# dat_nilu %>% count(Parameter, PARAM) %>% View()

# Check remaining missing PARAM
dat_nilu %>% filter(PARAM == "") %>% count(Parameter, PARAM)    # none
dat_nilu %>% filter(is.na(PARAM)) %>% count(Parameter, PARAM)

# 1 PeCB        NA       30     # THESE ARE DROPPED IN THE FINAL DATASET! (part 23)
# 2 Rec_percent NA       30
# 3 Sum-HepCB   NA       30
# 4 Sum-HexCB   NA       30
# 5 Sum-PenCB   NA       30
# 6 Sum-TetCB   NA       30
# 7 Sum-TriCB   NA       30
# 8 Sum 7 PCB   NA       30
# 9 TBA         NA       30

#
# S
#
# Compare with 2018 data - if necessary
#
# tab2 <- xtabs(~PARAM, df_2018 %>% filter(!substr(PARAM,1,3) %in% c("PCB", "BDE")))  # df_2018 from script 01!
# x <- names(tab2)
# paste(x, collapse = ", ")
# grep("HBCD", x, value = TRUE)
# grep("CCP", x, value = TRUE)



#
# 6. Standardize Sample_no ----
#

# Change NILU numbers so they conform with Nivabase
# NILU:     Sample_no   "Nr. 2018-09655"
# Labware;  TEXT_ID     "NR-2018-09676"

# Testing....
# grepl("Nr[:blank:]*", c(" Nr.", "Nr.", "Nr. ", "Nr", "Nr.  "))
# sub("[:blank:]?Nr.*[:blank:]*", "NR-", c(" Nr.", "Nr.", "Nr. ", "Nr", "Nr.  "))
# sub("*Nr.*[:blank:]*", "NR-", c(" Nr.", "Nr.", "Nr. ", "Nr", "Nr.  "))

# dat_nilu_back <- dat_nilu  # Backup (if you need that)
# dat_nilu <- dat_nilu_back  # Restore from backup

# Make TEXT_ID (keep the old 'Sample_no')
dat_nilu <- dat_nilu %>%
  mutate(TEXT_ID = sub(".*Nr.+ +", "NR-", Sample_no)) %>%
  mutate(TEXT_ID = sub("Nr", "NR", TEXT_ID)) %>%
  mutate(TEXT_ID = sub("NR.", "NR-", TEXT_ID, fixed = TRUE)) %>%
  mutate(TEXT_ID = sub(" *2018", "2018", TEXT_ID))

xtabs(~TEXT_ID, dat_nilu)

#
# 7. Change HG unit from UG_P_KG to MG_P_KG ----
#

# Check
# dat_nilu %>% filter(PARAM %in% "HG")


# 1:7 %>% purrr::map_int(~dat[[.]] %>% filter(is.na(Parameter)) %>% nrow())


#
# 21. Get existing data ---- 
#   

# Existing chemical data
# Needed to check if df_samples captures all samples
dat_all <- readRDS(file = "Data/01_dat_all.rds")
df_2018 <- dat_all %>% filter(MYEAR %in% 2018)

# Get Labware sample file (df_samples) 
df_samples <- readRDS(file = "Data/01_df_samples.rds")

#
# Check that all samples (station + sample number) in data are found in df_samples 
#
df_2018_id <- paste(df_2018$STATION_CODE, df_2018$SAMPLE_NO) %>% unique()
df_samp_id <- paste(df_samples$AQUAMONITOR_CODE, df_samples$BIOTA_SAMPLENO) %>% unique
found <- df_2018_id %in% df_samp_id
mean(found)  # should be 1
# 1  


#
# Checks
# Note Somateria mollissima (eider duck) at 19N in df_samples 
#
df_samples %>%
  group_by(SPECIES) %>%
  summarise(paste(sort(unique(AQUAMONITOR_CODE)), collapse = ", "))

# 1 NA                  16R, 20R, RE02, RE04, RE08, RN2, RN4, RN5, RN6, RN7, RN9                                                                        
# 2 Gadus morhua        02B, 10B, 13B, 15B, 19B, 23B, 24B, 28B, 30B, 36B, 43B2, 45B2, 53B, 71B, 80B, 96B, 98B1                                          
# 3 Littorina littorea  71G                                                                                                                             
# 4 Mytilus edulis      10A2, 11X, 15A, 22A, 26A2, 28A2, 30A, 31A, 36A1, 51A, 52A, 56A, 57A, 64A, 65A, 71A, 76A2, 91A2, 97A2, 97A3, 98A2, I023, I024, I~
# 5 Nucella lapillus    11G, 131G, 15G, 227G2, 22G, 36G, 76G, 98G                                                                                       
# 6 Platichthys flesus  33F                                                                                                                             
# 7 Somateria mollissi~ 19N    

df_2018 %>%
  group_by(LATIN_NAME) %>%
  summarise(paste(sort(unique(STATION_CODE)), collapse = ", "))

# 1 Gadus morhua      02B, 10B, 13B, 15B, 19B, 23B, 24B, 28B, 30B, 36B, 43B2, 45B2, 53B, 71B, 80B, 96B, 98B1                                            
# 2 Littorina littor~ 71G                                                                                                                               
# 3 Mytilus edulis    10A2, 11X, 15A, 22A, 26A2, 28A2, 30A, 31A, 36A1, 51A, 52A, 56A, 57A, 64A, 65A, 76A2, 91A2, 97A2, 97A3, 98A2, I023, I024, I131A, I~
# 4 Nucella lapillus  11G, 131G, 15G, 227G2, 22G, 36G, 76G, 98G                                                                                         
# 5 Platichthys fles~ 33F  



#
# 22a. Add columns from df_samples ----
#    Creating dat_nilu2
#
head(dat_nilu, 2)
head(df_2018, 2)
head(df_samples, 2)

# COLUMN COMPARISON
# - - - - - - - - - - - - - - - - - - - - - - - - - 
# Nivabasen biota part - Labware table (df_samples)  
# - - - - - - - - - - - - - - - - - - - - - - - - - 
# STATION_CODE         - AQUAMONITOR_CODE
# STATION_ID           - AQUAMONITOR_ID
# STATION_NAME         - AQUAMONITOR_NAME
# SAMPLE_DATE          - SAMPLED_DATE (note the extra 'D')
# LATIN_NAME           - SPECIES
# TISSUE_NAME          - TISSUE (note: "Lever" = "LI-Lever" etc.)
# SAMPLE_ID            - not given
# SAMPLE_NO            - BIOTA_SAMPLENO (e.g. 1-15)
# REPNO                - X_BULK_BIO ???
# not always given     - TEXT_ID

df_samples_forjoin <- df_samples %>%
  rename(
    STATION_CODE = AQUAMONITOR_CODE,
    STATION_ID = AQUAMONITOR_ID,
    STATION_NAME = AQUAMONITOR_NAME,
    SAMPLE_DATE = SAMPLED_DATE,
    LATIN_NAME = SPECIES,
    TISSUE_NAME = TISSUE,
    SAMPLE_NO = BIOTA_SAMPLENO,
    REPNO = X_BULK_BIO          # not included at this stage (see 'select' below)
  ) %>%
  select(TEXT_ID, STATION_CODE, STATION_ID, STATION_NAME, SAMPLE_DATE, LATIN_NAME, SAMPLE_NO)

dat_nilu2 <- dat_nilu %>% 
  left_join(df_samples_forjoin, by = "TEXT_ID") %>%
  mutate(MYEAR = 2018,
         BASIS = "W")

# Check of number of rows after join
if (nrow(dat_nilu) != nrow(dat_nilu2))
  cat("TEXT_ID SEEMS NOT TO BE UNIQUE IN df_samples")

# Visual check of tissue
# dat_nilu2 %>% select(TISSUE_NAME.x, TISSUE_NAME.y) %>% View()

#
# 22b. Add sum variables ----
#

# Define sum parameters
pars_list <- get_sumparameter_definitions("Milkys_2018/01b_synonyms.csv")

# unique(dat_nilu2$PARAM)

# We already have "CB_S7" in the form of Parameter = 'Sum 7 PCB', so we delete it for definitions
sel <- names(pars_list) %in% "CB_S7"
pars_list <- pars_list[!sel]


# Check 
dat_nilu2 %>%
  filter(PARAM %in% names(pars_list)) %>%
  nrow()   # 0

# Add sum parameters (as extra rows)  
dat_nilu3 <- dat_nilu2 %>%
  rename(VALUE = Value, FLAG1 = Flag1) %>%
  mutate(UNCERTAINTY = NA,
         QUANTIFICATION_LIMIT = NA)

for (i in seq_along(pars_list)){
  dat_nilu3 <- add_sumparameter(i, pars_list, dat_nilu3)
 }


### Tables for number of cogeners per sum parameter  
# Change FALSE to TRUE to show tables
if (FALSE){
  data_all_updated %>%
    filter(MYEAR >= 2010 & PARAM %in% c("CB_S7", "BDE6S", "PFAS", "CB118")) %>%
    xtabs(~MYEAR + PARAM, .)

    for (i in 1:length(pars_list)){
    par <- names(pars_list)[i]
    print(par)
    print(
      xtabs(~MYEAR + N_par, data_all_updated %>% filter(PARAM %in% par & !is.na(VALUE)))
    )
  }
}


#
# 23. Add dat_nilu3 rows to dat_all ----
#

# Put dat_nilu3 data in same format as df_2018
# After check
dat_nilu3 <- dat_nilu3 %>% 
  rename(VALUE_WW = Value,
         FLAG1 = Flag1)

head(dat_nilu3, 2)
head(df_2018, 2)

dat_nilu3 <- dat_nilu3 %>% 
  select(MYEAR, STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, BASIS, SAMPLE_NO, FLAG1, UNIT, STATION_ID, 
         VALUE_WW, STATION_NAME, SAMPLE_DATE) %>%
  filter(!is.na(PARAM))


# . 23a. Save NILU eider duck data (2018 only)  ----
saveRDS(dat_nilu3, "Data/31_data_nilu_eider_nivaformat.rds")


# . 23b. Add to complete data ----
dat_all_updated1 <- bind_rows(dat_all, dat_nilu3)

nrow(dat_all)           # 464852
nrow(dat_all_updated1)  # 467552



#
# 24. Add cod siloxane rows to df_2018 ----
#     see section 2b
# 
head(dat_nilu3)
head(data_siloksan_cod)
xtabs(~Tissue, data_siloksan_cod)
xtabs(~TISSUE_NAME, df_2018)
xtabs(~Unit, data_siloksan_cod)
xtabs(~UNIT, df_2018)
xtabs(~Sample_no_NILU, data_siloksan_cod)

data_siloksan_cod2 <- data_siloksan_cod %>%
  rename(PARAM = Parameter,
         FLAG1 = Flag1,
         VALUE_WW = Value
  ) %>%
  mutate(MYEAR = 2018,
         STATION_CODE = stringr::str_extract(Sample_no_NILU, "[^_]+"),
         LATIN_NAME = "Gadus morhua",
         TISSUE_NAME = "Lever",
         BASIS = "W",
         SAMPLE_NO = as.numeric(Sample_no),
         UNIT = "UG_P_KG"
  ) %>%
  select(MYEAR, STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, BASIS, SAMPLE_NO, FLAG1, UNIT, VALUE_WW)


#
# Add STATION_NAME, SAMPLE_DATE
#
# Taken from df_2018
df_for_join <- df_2018 %>%
  filter(STATION_CODE %in% unique(data_siloksan_cod2$STATION_CODE)) %>%
  group_by(STATION_CODE) %>%
  summarise(STATION_NAME = first(STATION_NAME), SAMPLE_DATE = first(SAMPLE_DATE))

data_siloksan_cod2 <- data_siloksan_cod2 %>%
  left_join(df_for_join)


#
# Add to 2018 data
#

dat_all_updated2 <- bind_rows(dat_all_updated1, data_siloksan_cod2)

nrow(dat_all)           # 464869
nrow(dat_all_updated1)  # 467552
nrow(dat_all_updated2)  # 467807

# Save NILU cod data (2018 only) 
saveRDS(data_siloksan_cod2, "Data/31_data_nilu_cod_nivaformat.rds")


#
# 25. Save ----
#

# All data 
saveRDS(dat_all_updated2, "Data/31_dat_all.rds")



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# We go on to use this in script 34
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#
# 26. Checks etc. ----
#

a <- colnames(df_2018)
b <- colnames(dat_nilu3)
a[a %in% b]
a[!a %in% b]

xtabs(~addNA(BIOTA_SAMPLENO), df_samples %>% filter(AQUAMONITOR_CODE %in% "19N"))
xtabs(~addNA(SPECIES), df_samples %>% filter(AQUAMONITOR_CODE %in% "19N"))
xtabs(~addNA(TISSUE), df_samples %>% filter(AQUAMONITOR_CODE %in% "19N"))

df_2018_updated %>% filter(STATION_CODE %in% "19N") %>% View()

# Check same station using df_2018 from script 01
df_2018 %>%     # this object is from script 01!
  filter(STATION_CODE %in% "15B") %>%
  head(2)
df_samples %>% filter(AQUAMONITOR_CODE %in% "15B") %>% head(2)

head(df_2018, 2)

xtabs(~addNA(SPECIES), df_samples %>% filter(AQUAMONITOR_CODE %in% "15B"))
xtabs(~addNA(TISSUE), df_samples %>% filter(AQUAMONITOR_CODE %in% "15B"))


#
# 27. Get medians for sum parameters ----
#

# dat_nilu3 <- readRDS("Data/31_data_nilu_eider_nivaformat.rds")

dat_nilu3 %>% 
  filter(PARAM %in% c("CB_S7", "BDE6S", "HBCDD", "BDESS")) %>%
  group_by(PARAM) %>% 
  summarise(Median = median(VALUE), Over_LOQ = sum(is.na(FLAG1)))

