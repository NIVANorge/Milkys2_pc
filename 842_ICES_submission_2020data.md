---
title: "22 Submitting data to ICES"
output: 
  html_document:
    keep_md: true  
    toc: true  
    toc_float: true
---

For submission of 2020 data in autumn 2021  
  
Soure of main data (data_ind2):
- prepared in Jupyterhub script 101 (where it also gets input from scr. 101 in this folder)    
- exported from Jupyterhub to Files_from_Jupyterhub_2020 (in this folder) + to the Milkys2 folder  
- used in script 34 (2019 version) where uncertainty (UNCRT) is added  

Some important things to note:  
- There is an error in Labware for 53B - muscle data ar assigne to the wrong individuals  
- "HCHA", "HCHG", and "HCB" has not been submitted to ICES (see 'Table 10 a1') - should be resumbitted for previous years (HCHA and HCHG are lacking from OSPAR's app, for HCB there seems to be data up to 2014 only)  


_NOTE_: METOA should be brought into the 'data_ind2' data set on a much former stages. METOA is in the methods table of NIVAbasen - See '03extra01'.   
_however:_ cannot find it in this base? Seems to be in 'ICES_EXPORT.ANALYTICAL_METHODS_RECORD':
get_nivabase_data("select OWNER,TABLE_NAME from ALL_TAB_COLUMNS where column_name = 'METOA'")   
         OWNER                TABLE_NAME  
 1 ICES_EXPORT ANALYTICAL_METHODS_RECORD  
  
... but cannot access these data:   
get_nivabase_data(str)   
(...times out...)  


## Where files will be saved  

```r
save_folder_txt <- "Files_to_ICES/2020/Test"
save_folder_rds <- "Files_to_ICES/2020/Rdata"
save_folder_excel <- "Files_to_ICES/2020/Excel"
```

### Selected year  

```r
selected_year <- 2020
```


## 0. Libraries and functions

```r
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
```

```
## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
## ✔ tibble  3.1.8     ✔ dplyr   1.0.9
## ✔ tidyr   1.1.4     ✔ stringr 1.4.0
## ✔ readr   2.1.1     ✔ forcats 0.5.1
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
library(safejoin) # https://github.com/moodymudskipper/safejoin

# source("01_Get_chemical_data_NIVAbasen_functions.R")  # for get_standard_parametername()

source("844_ICES_submission_check_functions.R")
```


### Functions  
Between decimal and minute formats. Note the fixed format (2 signs for degrees, 6 signs for minutes) 

```r
decimal2minute <- function(x) {
  deg <- floor(x) %>% sprintf("%02.0f", .)
  min <- round(60*(x - floor(x)), 3) %>% sprintf("%02.3f", .)
  paste0("+", deg, " ", min) 
  }

minute2decimal <- function(x) {
  deg <- substr(x,2,3) %>% as.numeric()
  min <- as.numeric(substr(x,5,10) %>% as.numeric())/60
  deg + min
}

# Test
# x <- df_stat2$long_2018faktisk
# x <- subset(df_stat2, stasjon == "19B")$long_2018faktisk
# x <- xsel <- [!is.na(x%in%
# x %>% head()
# x %>% decimal2minute() %>% head()
# x %>% decimal2minute() %>% minute2decimal() %>% head()
```



## 1. Read data

### a1. Get data produced by script 841  
We only keep the 2019 data

```r
# dir("Data")
# Data at sample level (chemical data, biological efect parameters and VDSI)
# Also in "../Milkys2/Data/101_data_updated_2020-08-05.rds"

# '34_dat_all_2019.rds' 
#   was made in 
# '34_Add_uncertainty_2019data.Rmd'
#   which is based on
# 'Input_data_2019/101_data_updated_2020-08-05.rds'
#   which is copied from Jupyterhub

fn <- "Data/841_dat_all_2020"

cat("----------------------------------------\n")
```

```
## ----------------------------------------
```

```r
cat("file written at:\n")
```

```
## file written at:
```

```r
file.info(fn)$mtime
```

```
## [1] "2021-12-30 16:56:52 CET"
```

```r
cat("----------------------------------------\n")
```

```
## ----------------------------------------
```

```r
data_all <- readRDS(fn) %>%
  mutate(
    TISSUE_NAME = case_when(
      TISSUE_NAME %in% "Egg homogenate of yolk and albumin" ~ "Egg",
      TRUE ~ TISSUE_NAME),
    SAMPLE_NO = SAMPLE_NO2,
    PARAM = case_when(
      PARAM %in% "1-OH-fenantren" ~ "PA1OH",
      PARAM %in% "1-OH-pyren" ~ "PYR1OH",
      PARAM %in% "3-OH-benzo[a]pyren" ~ "BAP3OH",
      TRUE ~ PARAM),
    UNCRT = round(UNCRT, 3)
  )

#
# Data in the selected year
#
data_ind2 <- data_all %>%
  filter(MYEAR %in% selected_year)

#
# Couple of tables
#
xtabs(~ is.na(SAMPLE_NO), data_ind2)
```

```
## is.na(SAMPLE_NO)
## FALSE 
## 25055
```

```r
xtabs(~ addNA(LATIN_NAME), data_ind2)
```

```
## addNA(LATIN_NAME)
##         Gadus morhua   Littorina littorea       Mytilus edulis 
##                16337                   77                 5250 
##     Nucella lapillus   Platichthys flesus Somateria mollissima 
##                  144                  150                 3097 
##                 <NA> 
##                    0
```

```r
#
# Data on individual level of fish (lengt, weight, gonad weight)
#
# Must contain these exact column names:
# STATION_CODE, Fish_no, Length, Weight, Gonad_weight
#
# data_ind_fish_old <- readRDS("Data/03_Fish_individual_data.rds")
data_ind_fish <- readRDS(
  "Files_from_Jupyterhub_2020/Raw_data/150_Fish_individual_data_2020.rds") 

# table(data_ind_fish$Station)
# table(data_ind_fish$STATION_CODE)
```

#### Check PAH metabolites

```r
if (FALSE){
  
  data_ind2 %>%
    filter(PARAM %in% c(
      "PA1OH", "PA1O", "1-OH-fenantren", "PYR1OH",  "PYR1O", "1-OH-pyren", 
      "BAP3OH", "BAP3O", "3-OH-benzo[a]pyren")) %>%
    xtabs(~PARAM, .)

  data_ind2 %>%
    filter(PARAM %in% c("VDSI", "Intersex")) %>%
    xtabs(~PARAM + STATION_CODE, .)
  
}
```

### a2-0. Change Intersex to code `INTF%`  
- https://vocab.ices.dk/?CodeID=53682  
- VDSI is OK   


```r
sel <- data_ind2$PARAM %in% "Intersex"
data_ind2$PARAM[sel] <- "INTF%"

cat("Intersex changed to code INTF% for", sum(sel), "records")
```

```
## Intersex changed to code INTF% for 1 records
```


### a2-1. Get VDSI + intersex data   
- Intersex should have code `INTF%` (https://vocab.ices.dk/?CodeID=53682)   
- NOT NEEDED anymore  

```r
#
# NOT NEEDED (data has been added to database
#

if (FALSE){

folder_vdsi <- "K:/Prosjekter/Sjøvann/JAMP/2020/opparbeiding/Snegl"

#
# 1. Imposex in Littorina
#

fn <- dir(folder_vdsi, pattern = ".xlsx")
fn <- fn[!grepl("AAMAL", fn)]
fn <- fn[!grepl("LITTORINA", fn)]
fn <- fn[!grepl("~", fn, fixed = TRUE)]

# Names = Extract all characters to the left of the first underscore  
names(fn) <- stringr::str_extract(fn, "[^_]+")
names(fn)[names(fn) == "131"] <- "131G"
# fn


read_excel_snail <- function(fn){
  df <- readxl::read_excel(paste0(folder_vdsi, "/", fn), skip = 6)
  df <- df[1:6]
  df <- df[!is.na(df$`Analysert av`),]
  df
  }

dat_vdsi_list <- fn %>% map(read_excel_snail)

# 15G has 51 rows:
# dat_vdsi_list %>% map_int(nrow)

# Check 15G
# dat_vdsi_list[["15G"]] %>% View()
# dat_vdsi_list[["36G"]] %>% View()
dat_vdsi_list[["15G"]] <- dat_vdsi_list[["15G"]][1:50,]  # delete last line

# Pick only number, sex and Imposex 
# ...we will deal with length and penis length later  
dat_vdsi <- dat_vdsi_list %>% 
  map(~.[c(1,3,5)]) %>%
  bind_rows(.id = "STATION_CODE") %>%
  rename(VALUE_WW = VDSI, 
         Sex = Kjønn) %>%
  mutate(PARAM = "VDSI",
         UNIT = "idx",
         LATIN_NAME = "Nucella lapillus")

# dat_vdsi

cat("\n------------------------------------------\n")
cat("Imposex data: \n------------------------------------------\n")
xtabs(~is.na(VALUE_WW) + addNA(Sex), dat_vdsi)


#
# 2. Intersex
#
fn <- "71G_fugløy_intersex_LITTORINA_2020.xlsx"
df <- readxl::read_excel(paste0(folder_vdsi, "/", fn), range = "A3:G53")

dat_intersex <- df %>%
  filter(F == 1) %>%
  mutate(STATION_CODE = "71G", 
         Sex = "f",
         PARAM = "INTF%",
         UNIT = "PERCENT",
         LATIN_NAME = "Littorina littorea") %>%
  mutate(VALUE_WW = `ISI   (intersex-stadie)`*100) %>%
  select(STATION_CODE, Sex, VALUE_WW, PARAM, LATIN_NAME)

#
# 3. Combine and summrize 
#

dat_snail_sex <- bind_rows(dat_vdsi, dat_intersex) %>%
  filter(Sex == "f") %>%
  group_by(STATION_CODE, PARAM, LATIN_NAME) %>%
  summarise(VALUE_WW = mean(VALUE_WW))

dat_snail_sex$MYEAR <- selected_year

cat("\n------------------------------------------\n")
cat("Combined data: \n------------------------------------------\n")
xtabs(~PARAM, dat_snail_sex )

}
```

### a2-2. Add VDSI + intersex data   
- NOT NEEDED anymore  


```r
#
# NOT NEEDED (data has been added to database
#

if (FALSE){
  
check1 <- data_ind2 %>%
  filter(STATION_CODE %in% dat_snail_sex$STATION_CODE)

# View(check1)

# Get values of STATION_NAME, SAMPLE_DATE  
dat_snail_sex_lookup <- check1 %>%
  group_by(STATION_CODE) %>%
  summarise(across(c(STATION_NAME, SAMPLE_DATE), first))
# dat_snail_sex_lookup

data_to_add <- dat_snail_sex %>%
  left_join(dat_snail_sex_lookup) %>%
  mutate(
    TISSUE_NAME = "Whole soft body",
    SAMPLE_NO = 1,
    SAMPLE_NO2 = 1,
    REPNO = 1, 
    MYEAR = 2020,
    lab = "NIVA"
  )

# number of rows in data
n1 <- nrow(data_ind2)

# Check that VDSI + intersex have not already been added   
check_existing <- data_ind2 %>%
  filter(PARAM %in% c("VDSI", "INTF%"))

if (nrow(check_existing) == 0){
  
  data_ind2 <- bind_rows(data_ind2, data_to_add) 
  
}

n2 <- nrow(data_ind2)

message(n2-n1, " rows added to the data")

}
```




### a3. For checking single stations

```r
# debugonce(report_station)
data_all %>% report_station("43B2", "Lever", year = 2020, threshold_samples = 7)
```

```
## =======================================================================================
## Station: 43B2 
## 
## In total: 30 samples / 66 parameters 
## 
## 
## 65 parameters has 15 samples 
## 1 parameters has 1 samples 
## ----------------------------------------------------------------------------------------
## 
## 
## Parameters with 7 or less samples: 
## ---------------------------------
## MCCP eksl. LOQ
## 
## ----------------------------------------------------------------------------------------
## 1 samples has 63 parameters 
## 14 samples has 62 parameters 
## 15 samples has 3 parameters 
## 
## 
## Samples with < 50 parameters: 
## ---------------------------------------
## 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115
```

```r
# report_station("30B") 
```


### a4. Data in Aquamonitor format, for dates only   
We us these data to create ´df_dates´ containing ´SAMPLE_DATE´    
- NOT NEEDED  

```r
# 
# data_AqM <- readRDS("Input_data_2019/101_dat_new1_2020-08-05.rds")
# 
# data_AqM <- data_AqM %>%
#   mutate(
#     Month = month(CatchDateFirst),
#     MYEAR = case_when(
#       Month <= 2 ~ year(CatchDateFirst)-1,   # measurments in Jan-Feb belong to the year before
#       Month >= 3 ~ year(CatchDateFirst)),
#   ) %>%
#   filter(MYEAR == 2019)
# 
# # Dates for stations that have just one date
# df_dates_aqm <- data_AqM %>%
#   rename(STATION_CODE = StationCode, 
#          SAMPLE_DATE = CatchDateFirst) %>%
#   distinct(STATION_CODE, SAMPLE_DATE) %>%
#   # Remove stations with more than one date:
#   group_by(STATION_CODE) %>%
#   mutate(n = n()) %>%
#   filter(n == 1)
# 
# # Dates for eider station (19N), which has one date for
# #   blood and another for eggs
# # We call date 'SAMPLE_DATE2' so we can addi ti alongside SAMPLE_DATE
# df_dates_aqm_eider <- data_AqM %>%
#   rename(STATION_CODE = StationCode, 
#          TISSUE_NAME = TissueName, 
#          SAMPLE_DATE2 = CatchDateFirst) %>%  # Note the variable names
#   filter(STATION_CODE == "19N") %>%
#   distinct(STATION_CODE, TISSUE_NAME, SAMPLE_DATE2)
# 
# # Add SAMPLE_DATE using df_dates_aqm andSAMPLE_DATE2 using df_dates_aqm_eider
# df_dates <- data_ind2 %>%
#   distinct(STATION_CODE, TISSUE_NAME) %>%
#   # Add SAMPLE_DATE:
#   left_join(df_dates_aqm, by = "STATION_CODE") %>%
#   # Add SAMPLE_DATE2 for eider: 
#   left_join(df_dates_aqm_eider, by = c("STATION_CODE", "TISSUE_NAME"))
# 
# 
# # Change SAMPLE_DATE where suitable
# # The station "26A2" is lacking in AqM data so we add it manually
# df_dates <- df_dates %>%
#   mutate(
#     SAMPLE_DATE = case_when(
#       !is.na(SAMPLE_DATE2) ~ SAMPLE_DATE2,            # for eider
#       STATION_CODE %in% "26A2" ~ ymd_hms("2020-10-26 00:00:00"),
#       TRUE ~ SAMPLE_DATE)   
#   ) %>%
#   select(-SAMPLE_DATE2, -n) %>%
#   arrange(STATION_CODE)
# 
# # View(df_dates)
```


### a5. Add SAMPLE_DATE to data
- NOT NEEDED  

```r
# data_ind2 <- data_ind2 %>%
#   safe_left_join(df_dates,
#                  by = c("STATION_CODE", "TISSUE_NAME"), 
#                  na_matches = "never",
#                  check = "BCVm")
```


### a6. Check 227G2  
- NOT NEEDED  

```r
# data_ind2 %>%
#   filter(PARAM %in% "VDSI" & STATION_CODE %in% "227G2")  
```

### b. Checks  

Years

```r
table(data_ind2$MYEAR)
```

```
## 
##  2020 
## 25055
```

SAMPLE_NO 1

```r
data_all %>%
  filter(MYEAR %in% 2020) %>%
  group_by(STATION_CODE, TISSUE_NAME) %>% 
  summarise(Sample_no = paste(min(SAMPLE_NO), "-", max(SAMPLE_NO)),
            .groups = "drop") %>%
  spread(TISSUE_NAME, Sample_no)
```

```
## # A tibble: 56 × 8
##    STATION_CODE Blod  Egg   Galle  Lever   `Liver - microsome` Muskel Whole so…¹
##    <chr>        <chr> <chr> <chr>  <chr>   <chr>               <chr>  <chr>     
##  1 02B          <NA>  <NA>  <NA>   1 - 8   <NA>                1 - 15 <NA>      
##  2 10A2         <NA>  <NA>  <NA>   <NA>    <NA>                <NA>   1 - 3     
##  3 10B          <NA>  <NA>  <NA>   1 - 115 <NA>                1 - 15 <NA>      
##  4 11G          <NA>  <NA>  <NA>   <NA>    <NA>                <NA>   1 - 1     
##  5 11X          <NA>  <NA>  <NA>   <NA>    <NA>                <NA>   1 - 3     
##  6 131G         <NA>  <NA>  <NA>   <NA>    <NA>                <NA>   1 - 1     
##  7 13B          <NA>  <NA>  <NA>   1 - 8   <NA>                1 - 15 <NA>      
##  8 15A          <NA>  <NA>  <NA>   <NA>    <NA>                <NA>   1 - 3     
##  9 15B          <NA>  <NA>  1 - 15 1 - 15  <NA>                1 - 15 <NA>      
## 10 15G          <NA>  <NA>  <NA>   <NA>    <NA>                <NA>   1 - 1     
## # … with 46 more rows, and abbreviated variable name ¹​`Whole soft body`
```

SAMPLE_NO 2

```r
data_all %>%
  filter(MYEAR %in% 2020) %>%
  filter(TISSUE_NAME %in% c("Lever", "Muskel")) %>%
  group_by(STATION_CODE, TISSUE_NAME) %>% 
  summarise(Sample_no = paste(sort(unique(SAMPLE_NO)), collapse = ", "),
            .groups = "drop") %>%
  spread(TISSUE_NAME, Sample_no)
```

```
## # A tibble: 18 × 3
##    STATION_CODE Lever                                                     Muskel
##    <chr>        <chr>                                                     <chr> 
##  1 02B          1, 2, 3, 4, 5, 6, 7, 8                                    1, 2,…
##  2 10B          1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 17, 101,… 1, 2,…
##  3 13B          1, 2, 3, 4, 5, 6, 7, 8                                    1, 2,…
##  4 15B          1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15         1, 2,…
##  5 19B          1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 101, … 1, 2,…
##  6 23B          1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15         1, 2,…
##  7 24B          1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 101, 102,… 1, 2,…
##  8 28B          1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14             1, 2,…
##  9 30B          1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 101, 102, 104, 10… 1, 2,…
## 10 33F          1, 2, 3                                                   1, 2,…
## 11 36B          1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15         1, 2,…
## 12 43B2         1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 101, … 1, 2,…
## 13 45B2         1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14             1, 2,…
## 14 53B          1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15         1, 2,…
## 15 71B          1, 2, 3, 4, 5, 6, 7, 8, 9, 10                             1, 2,…
## 16 80B          1, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17       1, 3,…
## 17 96B          1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15         1, 2,…
## 18 98B1         1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15         1, 2,…
```

SAMPLE DATE (cod)

```r
data_ind2 %>%
  filter(TISSUE_NAME %in% c("Lever", "Muskel")) %>%
  group_by(STATION_CODE) %>% 
  summarise(Sample_date = paste(sort(unique(as.character(SAMPLE_DATE))), collapse = ", "),
            .groups = "drop")
```

```
## # A tibble: 18 × 2
##    STATION_CODE Sample_date
##    <chr>        <chr>      
##  1 02B          2020-08-18 
##  2 10B          2020-08-20 
##  3 13B          2020-11-01 
##  4 15B          2020-09-01 
##  5 19B          2020-09-27 
##  6 23B          2020-10-13 
##  7 24B          2020-10-27 
##  8 28B          2020-10-01 
##  9 30B          2020-11-02 
## 10 33F          2020-08-26 
## 11 36B          2020-11-01 
## 12 43B2         2020-11-15 
## 13 45B2         2020-09-13 
## 14 53B          2020-10-11 
## 15 71B          2020-12-01 
## 16 80B          2020-09-15 
## 17 96B          2020-10-22 
## 18 98B1         2020-08-16
```
  
#### Drop I964  
We drop I964 - Toraneskaien (B4), Mo i Rana (industry station)

```r
data_ind2 <- data_ind2 %>% filter(!STATION_CODE %in% "I964")
```

#### Drop new stations, OR NOT
we may or may not drop the following stations (as they are new and not in the station dictionary)
    + 19B (Svalbard)  78.33153483	15.14101617 (mail fra Norman torsdag 23. august 2019 10.15)
    + 28A2 Ålesund havn  
    + 97A3 Bodø havn  

```r
DROP_NEW_STATIONS <- FALSE

if (DROP_NEW_STATIONS)
  data_ind2 <- data_ind2 %>% filter(!STATION_CODE %in% c("19B", "28A2", "97A3"))
```

#### Drop NA stations  

```r
sel <- is.na(data_ind2$STATION_CODE)
# data_ind2[sel,]

# Delete these 
data_ind2 <- data_ind2[!sel,]

cat("Removed", sum(sel), "recods that had no station code\n")
```

```
## Removed 0 recods that had no station code
```

#### Parameter list

```r
# xtabs(~PARAM, data_ind2 %>% filter(MYEAR == 2020))
```

#### Check some perfluor compounds

```r
# grep("BD", unique(data_ind2$PARAM), value = TRUE) 
grep("PFH", unique(data_ind2$PARAM), value = TRUE) 
```

```
## [1] "PFHxA"                             "PFHpA"                            
## [3] "PFHxS"                             "7H-dodekafluorheptansyre (HPFHpA)"
## [5] "Perfluorheptansulfonat (PFHpS)"
```

```r
# None of these:
data_ind2 %>%
  filter(PARAM == "Perfluorheksansulfonat (PFHxS)") %>%
  select(MYEAR, PARAM, STATION_CODE, LATIN_NAME, TISSUE_NAME, 
         VALUE_WW, FLAG1, UNIT, DRYWT, FAT_PERC) %>%
  group_by(MYEAR, PARAM, STATION_CODE, LATIN_NAME, TISSUE_NAME) %>%
  summarise(N = n(), Median_ww = median(VALUE_WW), 
            Min_ww = min(VALUE_WW), max_ww = max(VALUE_WW), 
            Q_limit = median(VALUE_WW[!is.na(FLAG1)])) %>%
  select(-PARAM)
```

```
## Warning in min(VALUE_WW): no non-missing arguments to min; returning Inf
```

```
## Warning in max(VALUE_WW): no non-missing arguments to max; returning -Inf
```

```
## `summarise()` has grouped output by 'MYEAR', 'PARAM', 'STATION_CODE',
## 'LATIN_NAME'. You can override using the `.groups` argument.
## Adding missing grouping variables: `PARAM`
```

```
## # A tibble: 0 × 10
## # Groups:   MYEAR, PARAM, STATION_CODE, LATIN_NAME [0]
## # … with 10 variables: PARAM <chr>, MYEAR <dbl>, STATION_CODE <chr>,
## #   LATIN_NAME <chr>, TISSUE_NAME <chr>, N <int>, Median_ww <dbl>,
## #   Min_ww <dbl>, max_ww <dbl>, Q_limit <dbl>
```

```r
# None of these:
data_ind2 %>%
  filter(PARAM == "Perfluorheksansulfonat (PFHxS)" & LATIN_NAME %in% "Somateria mollissima") %>%
  select(MYEAR, PARAM, STATION_CODE, LATIN_NAME, TISSUE_NAME, VALUE_WW, FLAG1, UNIT, DRYWT, FAT_PERC) %>%
  arrange(TISSUE_NAME, VALUE_WW)
```

```
##  [1] MYEAR        PARAM        STATION_CODE LATIN_NAME   TISSUE_NAME 
##  [6] VALUE_WW     FLAG1        UNIT         DRYWT        FAT_PERC    
## <0 rows> (or 0-length row.names)
```

#### Check uncertainty (UNCRT)

```r
# str(data_ind2)
xtabs(~ is.na(UNCRT), data_ind2)
```

```
## is.na(UNCRT)
## FALSE  TRUE 
## 13310 11745
```

```r
df1 <- data_ind2 %>%
  mutate(UNCRT_lacking = is.na(UNCRT)) %>%
  group_by(PARAM) %>%
  summarise(UNCRT_lacking = mean(UNCRT_lacking)*100 %>% round(1),
            .groups = "drop")
df2 <- data_ind2 %>%
  mutate(UNCRT_lacking = is.na(UNCRT)) %>%
  group_by(PARAM, Lab) %>%
  summarise(UNCRT_lacking = mean(UNCRT_lacking)*100 %>% round(1),
            .groups = "drop") %>%
  pivot_wider(names_from = "Lab", values_from = "UNCRT_lacking")

df <- left_join(df1, df2)
```

```
## Joining, by = "PARAM"
```

```r
df %>%
  filter(UNCRT_lacking == 0)
```

```
## # A tibble: 93 × 5
##    PARAM  UNCRT_lacking Eurofins  NILU  NIVA
##    <chr>          <dbl>    <dbl> <dbl> <dbl>
##  1 ACNE               0        0    NA    NA
##  2 ACNLE              0        0    NA    NA
##  3 AG                 0        0     0    NA
##  4 ANT                0        0    NA    NA
##  5 AS                 0        0     0    NA
##  6 BAA                0        0    NA    NA
##  7 BAP                0        0    NA    NA
##  8 BBJF               0        0    NA    NA
##  9 BDE100             0        0     0    NA
## 10 BDE119             0        0     0    NA
## # … with 83 more rows
```


### b1. Get tables submitted to ICES in 2016
For comparison and getting the PARAM we want

```r
load(file = "../Milkys_2018/Input_data/22_ICES_tables_2016-10-06.RData")     # data file from script 22 used in 2017

data_03_2016 <- data_03
data_10_2016 <- data_10
data_21_2016 <- data_21
data_91_2016 <- data_91
```

### b2. Get tables submitted to ICES in 2019 (2018 data)

```r
folder_data <- "../Milkys/ICES/delivered to Mildir"
# dir(folder_data)
# fns <- dir(folder_data, ".NO", full.names = TRUE)
fn <- paste0(folder_data, "/NIVA2018CF_2nd_version.NO")
data_2018 <- read_ices_file(fn)  
data_2018 <- add_field_codes(data_2018)  

if (FALSE){
  
  names(data_2018)
  data_2018[["10"]] %>% xtabs(~PARAM, .)
  
  data_2018[["91"]] %>% xtabs(~STNNO, .)
  data_2018[["91"]] %>% xtabs(~STATN, .)

}
```




### c. Also get the newest 2017 data produced by script 10  
We call it 'data_ind2_10'  
We do this only to add uncertainty and quantification limit

```r
# NOT NEEDED - WE SHOULD ALREADY HAVE uncertainty and quantification limit
# str(data_ind2)

# Last year's statistics
# xtabs(~is.na(UNCRT), data_10_2016)   # UNCERTAINTY
# xtabs(~is.na(DETLI), data_10_2016)    # QUANTIFICATION_LIMIT

# xtabs(~is.na(QUANTIFICATION_LIMIT), data_ind2_10 %>% filter(PARAM %in% "EROD"))
# xtabs(~is.na(DETLI), data_10_2016 %>% filter(PARAM %in% "EROD"))
# data_10_2016 %>% filter(PARAM %in% "EROD") %>% pull(DETLI)
```

### d. Data used for submitting 2017 data

```r
df_ind_old <- read.csv("../Milkys_2018/Data/09_df_ind.csv", stringsAsFactors = FALSE)
df_liver_old <- read.csv("../Milkys_2018/Data/09_df_liver.csv", stringsAsFactors = FALSE)
df_muscle_old <- read.csv("../Milkys_2018/Data/09_df_muscle.csv", stringsAsFactors = FALSE)
df_liver_33F_old <- readRDS("../Milkys_2018/Data/09_df_liver_33F.RData")
df_muscle_33F_old <- readRDS("../Milkys_2018/Data/09_df_muscle_33F.RData")
```


### e1. 2019 fish  individual data

```r
# df_samples <- readRDS(file = "Data/01_df_samples_2019.rds")
df_samples <- readRDS("Files_to_Jupyterhub_2020/Labware_samples_2020_2021-07-02.rds")

# Nice way of viewing these data  
if (FALSE)
  df_samples %>% 
    select(AQUAMONITOR_CODE, TISSUE, DESCRIPTION, BIOTA_SAMPLENO, X_BULK_BIO) %>% 
    View()
```

### e2. Check for errors in df_samples    
Lacking X_BULK_BIO where there should have been a number   
- this is the case for 53B liver  

```r
check <- df_samples %>%
  # filter(AQUAMONITOR_CODE == "53B")  %>% 
  select(AQUAMONITOR_CODE, TISSUE, DESCRIPTION, BIOTA_SAMPLENO, X_BULK_BIO) %>% 
  mutate(DESCRIPTION = gsub("<f8>", "ø", DESCRIPTION, fixed = TRUE),
         DESCRIPTION = gsub("<c5>", "Å", DESCRIPTION, fixed = TRUE),
         DESCRIPTION = gsub("<e5>", "å", DESCRIPTION, fixed = TRUE),
         DESCRIPTION = gsub("<c6>", "Æ", DESCRIPTION, fixed = TRUE),
         DESCRIPTION = gsub("<e6>", "æ", DESCRIPTION, fixed = TRUE),
         DESCRIPTION2 = DESCRIPTION %>% stringr::str_sub(start = nchar(AQUAMONITOR_CODE) + 1),
         Fish_no = stringr::str_extract(DESCRIPTION2, "[0-9]+")) %>%
  filter(is.na(X_BULK_BIO)) %>%
  filter(Fish_no != BIOTA_SAMPLENO) %>%
  select(AQUAMONITOR_CODE, TISSUE, DESCRIPTION2, BIOTA_SAMPLENO, X_BULK_BIO, Fish_no)

check
```

```
## [1] AQUAMONITOR_CODE TISSUE           DESCRIPTION2     BIOTA_SAMPLENO  
## [5] X_BULK_BIO       Fish_no         
## <0 rows> (or 0-length row.names)
```

### e3. Fix df_samples  

```r
# Check that we are doing the right thing below
if (FALSE){
  df_samples %>%
    mutate(DESCRIPTION = gsub("<f8>", "ø", DESCRIPTION, fixed = TRUE),
           DESCRIPTION = gsub("<c5>", "Å", DESCRIPTION, fixed = TRUE),
           DESCRIPTION = gsub("<e5>", "å", DESCRIPTION, fixed = TRUE),
           DESCRIPTION = gsub("<c6>", "Æ", DESCRIPTION, fixed = TRUE),
           DESCRIPTION = gsub("<e6>", "æ", DESCRIPTION, fixed = TRUE),
           DESCRIPTION2 = DESCRIPTION %>% stringr::str_sub(start = nchar(AQUAMONITOR_CODE) + 1),
           Fish_no = stringr::str_extract(DESCRIPTION2, "[0-9]+")) %>%
    filter(AQUAMONITOR_CODE == "53B" & TISSUE == "MU-Muskel") %>%
    select(DESCRIPTION, BIOTA_SAMPLENO, X_BULK_BIO, Fish_no)
}

df_samples <- df_samples %>%
  mutate(DESCRIPTION = gsub("<f8>", "ø", DESCRIPTION, fixed = TRUE),
         DESCRIPTION = gsub("<c5>", "Å", DESCRIPTION, fixed = TRUE),
         DESCRIPTION = gsub("<e5>", "å", DESCRIPTION, fixed = TRUE),
         DESCRIPTION = gsub("<c6>", "Æ", DESCRIPTION, fixed = TRUE),
         DESCRIPTION = gsub("<e6>", "æ", DESCRIPTION, fixed = TRUE),
         DESCRIPTION2 = DESCRIPTION %>% stringr::str_sub(start = nchar(AQUAMONITOR_CODE) + 1),
         Fish_no = stringr::str_extract(DESCRIPTION2, "[0-9]+")) %>%
  mutate(X_BULK_BIO = case_when(
    AQUAMONITOR_CODE == "53B" & TISSUE == "MU-Muskel" ~ as.character(Fish_no),
    TRUE ~ X_BULK_BIO)
  )
```


## df_liver and df_muscle - for fish samples  
Basis: the file of LABWARE samples (df_samples)   

#### Liver except 33F

```r
# colnames(df_liver_old)
df_liver <- df_samples %>%
  filter(TISSUE %in% "LI-Lever") %>%
  filter(!AQUAMONITOR_CODE %in% "33F") %>%
  group_by(AQUAMONITOR_CODE, BIOTA_SAMPLENO, X_BULK_BIO) %>%
  summarise(STATION_CODE = first(AQUAMONITOR_CODE),
            SAMPLE_NO = first(BIOTA_SAMPLENO),
            Pooled_no = first(X_BULK_BIO),
            .groups = "drop") %>%
  select(STATION_CODE, SAMPLE_NO, Pooled_no) %>%
  mutate(Pooled = 
           case_when(is.na(Pooled_no) ~ "enkel",
                     grepl(",", Pooled_no) ~ "bland",
                     TRUE ~ "enkel"))

# Get each fish number 
fish_numbers <- strsplit(df_liver$Pooled_no, ",")
df_liver$FiskNr_1 <- fish_numbers %>% map_chr(~.[1]) %>% as.numeric()
df_liver$FiskNr_2 <- fish_numbers %>% map_chr(~.[2]) %>% as.numeric()
df_liver$FiskNr_3 <- fish_numbers %>% map_chr(~.[3]) %>% as.numeric()

# For unpooled samples, and if nothog is given in X_BULK_BIO,
#   we set FiskNr_1 = SAMPLE_NO
sel <- is.na(df_liver$FiskNr_1)
df_liver$FiskNr_1[sel] <- df_liver$SAMPLE_NO[sel]

# Fake it till you make it!
# Add some fake weight data (i.e. how the sample is combined from different fish)
df_liver$Vekt1 <- rnorm(nrow(df_liver), 12, 0.5)
df_liver$Vekt2 <- rnorm(nrow(df_liver), 12, 0.5)
df_liver$Vekt3 <- rnorm(nrow(df_liver), 12, 0.5)
df_liver$Vekt1[is.na(df_liver$FiskNr_1)] <- NA
df_liver$Vekt2[is.na(df_liver$FiskNr_2)] <- NA
df_liver$Vekt3[is.na(df_liver$FiskNr_3)] <- NA

df_liver$sum <- apply(df_liver[,c("Vekt1", "Vekt2", "Vekt3")], 1, sum, na.rm = TRUE)

df_liver <- df_liver %>%
  select(-Pooled_no)

df_liver
```

```
## # A tibble: 230 × 10
##    STATION_CODE SAMPLE_NO Pooled FiskN…¹ FiskN…² FiskN…³ Vekt1 Vekt2 Vekt3   sum
##    <chr>            <dbl> <chr>    <dbl>   <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl>
##  1 02B                  1 enkel        1      NA      NA  12.2  NA      NA  12.2
##  2 02B                  2 enkel        2      NA      NA  11.4  NA      NA  11.4
##  3 02B                  3 bland        9      14      NA  11.2  12.4    NA  23.5
##  4 02B                  4 bland        5       6      NA  12.1  12.0    NA  24.1
##  5 02B                  5 bland        7      16      NA  11.5  11.9    NA  23.4
##  6 02B                  6 bland        4      11      NA  12.3  12.0    NA  24.3
##  7 02B                  7 bland        3      12      NA  12.2  12.1    NA  24.3
##  8 02B                  8 bland        8      10      NA  11.8  11.9    NA  23.7
##  9 10B                  1 bland        1       2      NA  12.9  11.8    NA  24.7
## 10 10B                  3 bland        3      19      NA  11.4  12.0    NA  23.4
## # … with 220 more rows, and abbreviated variable names ¹​FiskNr_1, ²​FiskNr_2,
## #   ³​FiskNr_3
```


```r
df_muscle <- df_samples %>%
  filter(TISSUE %in% "MU-Muskel" & AQUAMONITOR_CODE != "33F")
```


```r
df_muscle_old <- df_muscle
```




#### Muscle (except 33F)

```r
# colnames(df_liver_old)
df_muscle <- df_samples %>%
  filter(TISSUE %in% "MU-Muskel" & AQUAMONITOR_CODE != "33F") %>%
  # filter(!AQUAMONITOR_CODE %in% "33F") %>%
  group_by(AQUAMONITOR_CODE, BIOTA_SAMPLENO, X_BULK_BIO) %>%
  summarise(STATION_CODE = first(AQUAMONITOR_CODE),
            SAMPLE_NO = first(BIOTA_SAMPLENO),
            Pooled_no = first(X_BULK_BIO),
            .groups = "drop") %>%
  select(STATION_CODE, SAMPLE_NO, Pooled_no) %>%
  mutate(Pooled = 
           case_when(is.na(Pooled_no) ~ "enkel",
                     grepl(",", Pooled_no) ~ "bland",
                     TRUE ~ "enkel"))

if (mean(df_muscle$Pooled == "enkel") == 1){
  cat("No pooled samples of muscle - OK to continue \n")
} else {
  cat(sum(df_muscle$Pooled == "bland"), "pooled samples of muscle! Check, and change code if necessary! \n")
}
```

```
## No pooled samples of muscle - OK to continue
```

```r
if (FALSE)
  df_muscle %>%
    filter(Pooled == "bland")

# Use this code only if no pooled samples of muscle
# Otherwise d as for liver
df_muscle <- df_muscle %>%
  mutate(Fish_no = 
           case_when(is.na(Pooled_no) ~ SAMPLE_NO,
                     !is.na(Pooled_no) ~ as.numeric(Pooled_no))
  )

df_muscle
```

```
## # A tibble: 255 × 5
##    STATION_CODE SAMPLE_NO Pooled_no Pooled Fish_no
##    <chr>            <dbl> <chr>     <chr>    <dbl>
##  1 02B                  1 <NA>      enkel        1
##  2 02B                  2 <NA>      enkel        2
##  3 02B                  3 <NA>      enkel        3
##  4 02B                  4 <NA>      enkel        4
##  5 02B                  5 <NA>      enkel        5
##  6 02B                  6 <NA>      enkel        6
##  7 02B                  7 <NA>      enkel        7
##  8 02B                  8 <NA>      enkel        8
##  9 02B                  9 <NA>      enkel        9
## 10 02B                 10 <NA>      enkel       10
## # … with 245 more rows
```


#### Liver 33F

```r
df <- df_samples %>%
  filter(TISSUE %in% "LI-Lever" & AQUAMONITOR_CODE %in% "33F") %>%
  group_by(AQUAMONITOR_CODE, BIOTA_SAMPLENO, X_BULK_BIO) %>%
  summarise(STATION_CODE = first(AQUAMONITOR_CODE),
            SAMPLE_NO = first(BIOTA_SAMPLENO),
            Pooled_no = first(X_BULK_BIO),
            .groups = "drop")

get_list <- function(i){
  list(
    SAMPLE_NO = df$SAMPLE_NO[i],
    Fish_no = strsplit(df$Pooled_no[i], ",")[[1]] %>% as.numeric()
    )
}
# get_list(1)

df_liver_33F <- 1:nrow(df) %>% map(get_list) 

# 2020: seems like X_BULK_BIO has not been filled out  
# We do it manually  
df_liver_33F[[1]]$Fish_no <- 1:5
df_liver_33F[[2]]$Fish_no <- 6:10
df_liver_33F[[3]]$Fish_no <- 11:15
```


#### Muscle 33F

```r
df <- df_samples %>%
  filter(TISSUE %in% "MU-Muskel" & AQUAMONITOR_CODE %in% "33F") %>%
  group_by(AQUAMONITOR_CODE, BIOTA_SAMPLENO, X_BULK_BIO) %>%
  summarise(STATION_CODE = first(AQUAMONITOR_CODE),
            SAMPLE_NO = first(BIOTA_SAMPLENO),
            Pooled_no = first(X_BULK_BIO),
            .groups = "drop")

get_list <- function(i){
  list(
    SAMPLE_NO = df$SAMPLE_NO[i],
    Fish_no = strsplit(df$Pooled_no[i], ",")[[1]] %>% as.numeric()
    )
}
# get_list(1)

df_muscle_33F <- 1:nrow(df) %>% map(get_list) 

# 2020: seems like X_BULK_BIO has not been filled out  
# We do it manually  
df_muscle_33F[[1]]$Fish_no <- 1:5
df_muscle_33F[[2]]$Fish_no <- 6:10
df_muscle_33F[[3]]$Fish_no <- 11:15
```

## df_ind - length and weight data  
Basis: the file of LABWARE samples (df_samples) and weight/length data (data_ind_fish)   
Fish individual Length/weight are added to df_ind  

```r
df_ind <- bind_rows(
  # Get STATION_CODE + Fish_no for each part
  df_liver %>% 
    select(STATION_CODE, FiskNr_1, FiskNr_2, FiskNr_3) %>%
    gather(Sampleno, Fish_no, -STATION_CODE) %>%
    mutate(Fish_no = as.numeric(Fish_no)) %>%
    select(-Sampleno),
  df_muscle %>% 
    count(STATION_CODE, Fish_no) %>%
    select(-n),
  tibble(STATION_CODE = "33F",
         Fish_no = df_liver_33F %>% purrr::transpose() %>% .$Fish_no %>% unlist() %>% sort()),
  tibble(STATION_CODE = "33F",
         Fish_no = df_muscle_33F %>% purrr::transpose() %>% .$Fish_no %>% unlist() %>% sort())
  ) %>%
  # Delete duplicates of STATION_CODE + Fish_no
  count(STATION_CODE, Fish_no) %>%
  select(-n) %>%
  filter(!is.na(Fish_no))

nrow(df_ind)
```

```
## [1] 331
```

```r
# Add fish length and weight to the data
df_ind <- df_ind %>%
  safe_left_join(    # using safe_lef_join, see https://github.com/moodymudskipper/safejoin 
    data_ind_fish %>% select(STATION_CODE, Fish_no, Length, Weight, Gonad_weight),
    by = c("STATION_CODE", "Fish_no"),
    na_matches = "never",
    check = "V"    # checks if y is unique on joining columns 
  )

range(df_ind$Length, na.rm = TRUE)  # must not include zero
```

```
## [1] 28.7 99.0
```

```r
range(df_ind$Weight, na.rm = TRUE)  # must not include zero
```

```
## [1]  232 8500
```

### Check where we lack data  
- Lack some gonad weight data  
- 2019 - also lacking 13B, 19B, 43B2 and 53B

```r
# df_ind
df_ind %>%
  group_by(STATION_CODE) %>%
  summarise_all(~mean(is.na(.)))
```

```
## # A tibble: 18 × 5
##    STATION_CODE Fish_no Length Weight Gonad_weight
##    <chr>          <dbl>  <dbl>  <dbl>        <dbl>
##  1 02B                0      0      0        0    
##  2 10B                0      0      0        1    
##  3 13B                0      0      0        0    
##  4 15B                0      0      0        0    
##  5 19B                0      0      0        0    
##  6 23B                0      0      0        0.238
##  7 24B                0      0      0        0.2  
##  8 28B                0      0      0        1    
##  9 30B                0      0      0        0.517
## 10 33F                0      0      0        1    
## 11 36B                0      0      0        0.158
## 12 43B2               0      0      0        0    
## 13 45B2               0      0      0        1    
## 14 53B                0      0      0        0.375
## 15 71B                0      0      0        0.652
## 16 80B                0      0      0        0.333
## 17 96B                0      0      0        0.6  
## 18 98B1               0      0      0        1
```


## Table 00

```r
data_00 <- data.frame(RECID = "00", RLABO = "NIVA", CNTRY = 58, MYEAR = selected_year, RFVER = "3.2.5")
```

## Table 03  
### Define SMPNO and SMLNK for each species   
    + SMPNO (sample number within station, links to table 04)
    + SMLNK (sample method, links to table 20)
    + I don't think these necessarily are linked to species, but in NIVA's case we let them be  
Updated with Eider duck Somateria mollissima in 2019 (must be updated also for 2018)  
    + SMPNO = 9 (More or less by random)  
    + SMLNK = 11 - links to table 20, where it refers to SMTYP = HAN (hand picking)   

```r
df_smpno <- read.table(textConnection("
            LATIN_NAME SMPNO  SMLNK
        'Gadus morhua'     1      8
     'Limanda limanda'     8      8
  'Littorina littorea'     3     11
      'Mytilus edulis'     0     11
    'Nucella lapillus'     3     11
  'Platichthys flesus'     6      9
'Somateria mollissima'     9     11
"), header = TRUE, stringsAsFactors = FALSE)
```

### Create data 03 table  
By aggregating the data  

```r
df_station <- data_ind2 %>%
  # filter(MYEAR %in% myear & !LATIN_NAME %in% 'Somateria mollissima') %>%  # NOTE: DELETES EIDER DUCK
  # filter(MYEAR %in% myear) %>%  # NOTE: DELETES EIDER DUCK
  group_by(STATION_CODE, LATIN_NAME) %>%
  summarise(n(), .groups = "drop")

####################################################
# Add
#   Sample identification SMPNO (for each species in haul) 
#   Sample method link: SMLNK
####################################################

df_station <- df_station %>% 
  left_join(df_smpno, by = "LATIN_NAME", .groups = "drop")

data_03 <- data.frame(RECID = "03", 
                      CRUIS = "AA711", 
                      STNNO = df_station$STATION_CODE,
                      DTYPE = "CF", 
                      SMPNO = df_station$SMPNO,
                      SMLNK = df_station$SMLNK,
                      ATIME = NA, 
                      NOAGG = NA, 
                      SPECI = df_station$LATIN_NAME,
                      RLIST = "ER", 
                      FINFL = "GDP", 
                      stringsAsFactors = FALSE
                      )
```

### Check that SMPNO has been set

```r
xtabs(~SPECI + is.na(SMPNO), data_03)
```

```
##                       is.na(SMPNO)
## SPECI                  FALSE
##   Gadus morhua            17
##   Littorina littorea       1
##   Mytilus edulis          28
##   Nucella lapillus         8
##   Platichthys flesus       1
##   Somateria mollissima     1
```


## Table 04
### a. Fish, test

```r
ex_station <- "30B"
# ex_station <- "53B"

df1 <- df_muscle %>%
  filter(STATION_CODE %in% ex_station)
df2 <- df_liver %>%
  filter(STATION_CODE %in% ex_station) %>%
  as.data.frame()

# 1. df_muscle
df1 %>% head()
```

```
## # A tibble: 6 × 5
##   STATION_CODE SAMPLE_NO Pooled_no Pooled Fish_no
##   <chr>            <dbl> <chr>     <chr>    <dbl>
## 1 30B                  1 <NA>      enkel        1
## 2 30B                  2 <NA>      enkel        2
## 3 30B                  3 <NA>      enkel        3
## 4 30B                  4 <NA>      enkel        4
## 5 30B                  5 <NA>      enkel        5
## 6 30B                  6 <NA>      enkel        6
```

```r
# 2. df_muscle
df2 %>% head()
```

```
##   STATION_CODE SAMPLE_NO Pooled FiskNr_1 FiskNr_2 FiskNr_3    Vekt1    Vekt2
## 1          30B         1  enkel        3       NA       NA 12.21746       NA
## 2          30B         2  enkel        5       NA       NA 12.95388       NA
## 3          30B         3  enkel        8       NA       NA 12.05124       NA
## 4          30B         4  enkel       14       NA       NA 11.71092       NA
## 5          30B         5  enkel       13       NA       NA 12.43485       NA
## 6          30B         6  bland        6       40       30 12.03629 12.06585
##      Vekt3      sum
## 1       NA 12.21746
## 2       NA 12.95388
## 3       NA 12.05124
## 4       NA 11.71092
## 5       NA 12.43485
## 6 11.99464 36.09677
```

```r
# 3. Tables
cat("Example tables from raw data: Station 30B \n--------------------------------------------\n\n")
```

```
## Example tables from raw data: Station 30B 
## --------------------------------------------
```

```r
cat("Liver \n")
```

```
## Liver
```

```r
data_ind2 %>%
  filter(STATION_CODE %in% ex_station & MYEAR %in% selected_year & TISSUE_NAME %in% "Lever") %>%
  xtabs(~SAMPLE_NO2, .)
```

```
## SAMPLE_NO2
##   1   2   3   4   5   6   7   8   9  10  11  12 101 102 104 105 106 107 108 109 
##  92  92  91  92  92  92  90  92  91  90  92  92   3   3   3   3   3   3   3   3 
## 110 111 112 113 114 115 
##   3   3   3   3   3   3
```

```r
cat("\nMuscle \n")
```

```
## 
## Muscle
```

```r
data_ind2 %>%
  filter(STATION_CODE %in% ex_station & MYEAR %in% selected_year & TISSUE_NAME %in% "Muskel") %>%
  xtabs(~SAMPLE_NO2, .)
```

```
## SAMPLE_NO2
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 
##  7  7  7  7  7  7  7  7  7  7  7  7  7  7  7
```

```r
# Muscle
i1 <- df1$Fish_no                                                     # all 'muscle' fish
cat("Muscle fish (specimen numbers): \n")
```

```
## Muscle fish (specimen numbers):
```

```r
i1
```

```
##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
```

```r
# Liver
bulkid <- with(df2, paste(FiskNr_1, FiskNr_2, FiskNr_3, sep = "~"))   # collect ind. numbers
bulkid <- gsub("~NA", "", bulkid, fixed = TRUE)                       # remove NA's
pooled <- grepl("~", bulkid)                                          # which are pooled? 
i2 <- df2$FiskNr_1[!pooled]                                           # get all unpooled fish
cat("Fish without pooled samples: \n")
```

```
## Fish without pooled samples:
```

```r
i2
```

```
## [1]  3  5  8 14 13
```

```r
# Muscle samples (assumed to be unpooled)
df <- tibble(SPECI = "Gadus morhua", Tissue = "Muscle", SUBNO = i1, BULKID = "")
cat("\nMake df: \n")
```

```
## 
## Make df:
```

```r
cat("- start with 'muscle' fish - nrow:", nrow(df), "\n")
```

```
## - start with 'muscle' fish - nrow: 15
```

```r
# Add unpooled liver samples NOT overlapping with muscle samples (quite rare situation)
liver_extra_unpooled_samples <- i2[!i2 %in% i1]
if (length(liver_extra_unpooled_samples) > 0)
  df <- rbind(
    df,
    tibble(SPECI = "Gadus morhua", Tissue = "Liver", SUBNO = liver_extra_unpooled_samples, BULKID = "")
  )
cat("- add unpooled liver samples NOT overlapping with muscle samples - nrow:", nrow(df), "\n")
```

```
## - add unpooled liver samples NOT overlapping with muscle samples - nrow: 15
```

```r
liver_pooled_samples_n <- sum(pooled)

# Add pooled liver samples 
if (liver_pooled_samples_n > 0){
  subno_liver <- max(i1, liver_extra_unpooled_samples) + seq(1, liver_pooled_samples_n)
  df <- rbind(
    df,
    tibble(SPECI = "Gadus morhua", Tissue = "Liver", SUBNO = subno_liver, BULKID = bulkid[pooled])
  )
}
cat("- pooled samples added - nrow:", nrow(df), "\n")
```

```
## - pooled samples added - nrow: 22
```

```r
# 4. Final df
df %>% as.data.frame()
```

```
##           SPECI Tissue SUBNO   BULKID
## 1  Gadus morhua Muscle     1         
## 2  Gadus morhua Muscle     2         
## 3  Gadus morhua Muscle     3         
## 4  Gadus morhua Muscle     4         
## 5  Gadus morhua Muscle     5         
## 6  Gadus morhua Muscle     6         
## 7  Gadus morhua Muscle     7         
## 8  Gadus morhua Muscle     8         
## 9  Gadus morhua Muscle     9         
## 10 Gadus morhua Muscle    10         
## 11 Gadus morhua Muscle    11         
## 12 Gadus morhua Muscle    12         
## 13 Gadus morhua Muscle    13         
## 14 Gadus morhua Muscle    14         
## 15 Gadus morhua Muscle    15         
## 16 Gadus morhua  Liver    16  6~40~30
## 17 Gadus morhua  Liver    17  7~19~27
## 18 Gadus morhua  Liver    18 11~44~37
## 19 Gadus morhua  Liver    19 10~22~23
## 20 Gadus morhua  Liver    20  21~1~26
## 21 Gadus morhua  Liver    21 12~41~24
## 22 Gadus morhua  Liver    22 15~39~33
```


### b. Fish, function    
Note:  
    + Muscle samples are assumed to be unpooled (except for 33F, which is treated specially below)  
    + The case of "unpooled liver samples NOT overlapping with muscle samples" has not been tested  
    + See "ICES/ICES - notes on SUBNO, BULKID etc.docx" for explanation  

```r
generate_04_fish_station <- function(station_code, latin_name, 
                                     df_individuals_muscle, df_individuals_liver,
                                     lookuptable_smpno = df_smpno){
  df1 <- df_individuals_muscle %>%
    filter(STATION_CODE %in% station_code)
  df2 <- df_individuals_liver %>%
    filter(STATION_CODE %in% station_code)
  fishno_muscle <- df1$Fish_no %>% sort()
  fishno_liver <- with(df2, c(FiskNr_1, FiskNr_2, FiskNr_3)) %>% unique() %>% sort()
  number_of_fish_liver <- apply(!is.na(df2[,c("FiskNr_1", "FiskNr_2", "FiskNr_3")]), 1, sum)
  pooled <- number_of_fish_liver > 1  
  # Start by listing all individual fish                           
  df <- tibble(STATION_CODE = station_code, LATIN_NAME = latin_name, 
               Fish_no = unique(c(fishno_muscle, fishno_liver)), NOIMP = 1)
  # Muscle samples (assumed to be unpooled)
  df <- df %>%
    left_join(df1 %>% select(Fish_no, SAMPLE_NO), by = "Fish_no") %>% 
    rename(SAMPLE_NO_Muscle = SAMPLE_NO)
  # Liver samples - create bulkid by joining Fisknr and removing "~NA"
  bulkid <- with(df2, paste(FiskNr_1, FiskNr_2, FiskNr_3, sep = "~")) %>%
    gsub("~NA", "", ., fixed = TRUE)
  # Unpooled liver samples overlapping with muscle samples
  df <- df %>%
    left_join(df2[!pooled, c("FiskNr_1", "SAMPLE_NO")], 
              by = c("Fish_no" = "FiskNr_1")) %>%
    rename(SAMPLE_NO_Liver = SAMPLE_NO)
  df$SUBNO <- df$Fish_no                         # for these samples SUBNO equals fish number Fish_no
  # BULKID for all unpooled samples is just 
  # Pooled liver samples 
  liver_pooled_samples_n <- sum(pooled)
  if (liver_pooled_samples_n > 0){
    df2p <- df2[pooled,]
    df2p$SUBNO <- max(df$SUBNO) + 1:nrow(df2p)    # make new SUBNO, starting where we left off
    df3 <- df2p %>% 
      select(SAMPLE_NO, SUBNO, FiskNr_1:FiskNr_3) %>% 
      gather("Part", "Fish_no", FiskNr_1:FiskNr_3) %>% 
      filter(!is.na(Fish_no))
    df3summ <- df3 %>% 
      group_by(Fish_no) %>% 
      summarise(BULKID = paste(SUBNO, sep = "~"))
    df_to_add <- tibble(
      STATION_CODE = station_code, LATIN_NAME = latin_name, 
      Fish_no = NA, 
      NOIMP = number_of_fish_liver[pooled],
      SAMPLE_NO_Muscle = NA,
      SAMPLE_NO_Liver = df2p$SAMPLE_NO,
      SUBNO = df2p$SUBNO,
      BULKID = "")
    df <- left_join(df, df3summ, by = "Fish_no")  # adds BULKID to the relevant fish
    df$BULKID[is.na(df$BULKID)] <- ""             # BULKID = NA is changed to BULKID = ""
    df <- rbind(df, df_to_add)
  }
  # Add SMPNO
  df %>% left_join(lookuptable_smpno %>% select(LATIN_NAME, SMPNO), by = "LATIN_NAME")
}
# debugonce(generate_04_fish_station)
# df <- generate_04_fish_station("24B", "Gadus morhua", df_muscle, df_liver)
df <- generate_04_fish_station("71B", "Gadus morhua", df_muscle, df_liver)
df <- generate_04_fish_station("02B", "Gadus morhua", df_muscle, df_liver)
df %>% as.data.frame()
```

```
##    STATION_CODE   LATIN_NAME Fish_no NOIMP SAMPLE_NO_Muscle SAMPLE_NO_Liver
## 1           02B Gadus morhua       1     1                1               1
## 2           02B Gadus morhua       2     1                2               2
## 3           02B Gadus morhua       3     1                3              NA
## 4           02B Gadus morhua       4     1                4              NA
## 5           02B Gadus morhua       5     1                5              NA
## 6           02B Gadus morhua       6     1                6              NA
## 7           02B Gadus morhua       7     1                7              NA
## 8           02B Gadus morhua       8     1                8              NA
## 9           02B Gadus morhua       9     1                9              NA
## 10          02B Gadus morhua      10     1               10              NA
## 11          02B Gadus morhua      11     1               11              NA
## 12          02B Gadus morhua      12     1               12              NA
## 13          02B Gadus morhua      13     1               13              NA
## 14          02B Gadus morhua      14     1               14              NA
## 15          02B Gadus morhua      16     1               15              NA
## 16          02B Gadus morhua      NA     2               NA               3
## 17          02B Gadus morhua      NA     2               NA               4
## 18          02B Gadus morhua      NA     2               NA               5
## 19          02B Gadus morhua      NA     2               NA               6
## 20          02B Gadus morhua      NA     2               NA               7
## 21          02B Gadus morhua      NA     2               NA               8
##    SUBNO BULKID SMPNO
## 1      1            1
## 2      2            1
## 3      3     21     1
## 4      4     20     1
## 5      5     18     1
## 6      6     18     1
## 7      7     19     1
## 8      8     22     1
## 9      9     17     1
## 10    10     22     1
## 11    11     20     1
## 12    12     21     1
## 13    13            1
## 14    14     17     1
## 15    16     19     1
## 16    17            1
## 17    18            1
## 18    19            1
## 19    20            1
## 20    21            1
## 21    22            1
```

```r
# For further exploration

explore_table_04 <- FALSE
if (explore_table_04){
  df_samples %>% 
    filter(AQUAMONITOR_CODE == "53B" & TISSUE %in% "MU-Muskel") %>% 
    arrange(BIOTA_SAMPLENO) %>%
    select(DESCRIPTION, BIOTA_SAMPLENO, X_BULK_BIO)
  df %>%
    filter(!is.na(SAMPLE_NO_Muscle)) %>%
    arrange(SAMPLE_NO_Liver)
}
```



### c. Create 'data_04_fish_main'  
I.e. data_04 for all fish stations except 33F ('main table')

```r
stat_fish <- unique(df_muscle$STATION_CODE)
df_station_species <- data_ind2 %>%
  filter(STATION_CODE %in% stat_fish & MYEAR %in% selected_year) %>%
  group_by(STATION_CODE, LATIN_NAME) %>%
  summarise(N = n())
```

```
## `summarise()` has grouped output by 'STATION_CODE'. You can override using the
## `.groups` argument.
```

```r
data_04_fish_main <- 1:nrow(df_station_species) %>%  
  map_df(~generate_04_fish_station(
    df_station_species$STATION_CODE[.x], df_station_species$LATIN_NAME[.x],
    df_muscle, df_liver)
    )

# Check
data_04_fish_main %>% filter(STATION_CODE == "36B") %>% as.data.frame()
```

```
##    STATION_CODE   LATIN_NAME Fish_no NOIMP SAMPLE_NO_Muscle SAMPLE_NO_Liver
## 1           36B Gadus morhua       1     1                1              NA
## 2           36B Gadus morhua       2     1                2              NA
## 3           36B Gadus morhua       3     1                3               3
## 4           36B Gadus morhua       4     1                4              NA
## 5           36B Gadus morhua       5     1                5               5
## 6           36B Gadus morhua       6     1                6               6
## 7           36B Gadus morhua       7     1                7              NA
## 8           36B Gadus morhua       8     1                8               8
## 9           36B Gadus morhua       9     1                9               9
## 10          36B Gadus morhua      10     1               10              10
## 11          36B Gadus morhua      12     1               12              12
## 12          36B Gadus morhua      13     1               13              13
## 13          36B Gadus morhua      14     1               14              14
## 14          36B Gadus morhua      15     1               15              15
## 15          36B Gadus morhua      16     1               11              11
## 16          36B Gadus morhua      11     1               NA              NA
## 17          36B Gadus morhua      17     1               NA              NA
## 18          36B Gadus morhua      20     1               NA              NA
## 19          36B Gadus morhua      21     1               NA              NA
## 20          36B Gadus morhua      NA     2               NA               1
## 21          36B Gadus morhua      NA     2               NA               2
## 22          36B Gadus morhua      NA     2               NA               4
## 23          36B Gadus morhua      NA     2               NA               7
##    SUBNO BULKID SMPNO
## 1      1     22     1
## 2      2     23     1
## 3      3            1
## 4      4     24     1
## 5      5            1
## 6      6            1
## 7      7     25     1
## 8      8            1
## 9      9            1
## 10    10            1
## 11    12            1
## 12    13            1
## 13    14            1
## 14    15            1
## 15    16            1
## 16    11     25     1
## 17    17     23     1
## 18    20     22     1
## 19    21     24     1
## 20    22            1
## 21    23            1
## 22    24            1
## 23    25            1
```
### d. Comparison with sample file   
One example station  

```r
example_station <- "02B"

df_samples %>%
  filter(AQUAMONITOR_CODE %in% example_station & TISSUE == "LI-Lever") %>% 
  select(AQUAMONITOR_CODE, TISSUE, DESCRIPTION, BIOTA_SAMPLENO, X_BULK_BIO) %>%
  arrange(TISSUE, BIOTA_SAMPLENO)
```

```
##   AQUAMONITOR_CODE   TISSUE                              DESCRIPTION
## 1              02B LI-Lever       02B Kirkøy (north) - torsk lever 1
## 2              02B LI-Lever       02B Kirkøy (north) - torsk lever 2
## 3              02B LI-Lever 02B Kirkøy (north) - torsk lever 9 og 14
## 4              02B LI-Lever  02B Kirkøy (north) - torsk lever 5 og 6
## 5              02B LI-Lever 02B Kirkøy (north) - torsk lever 7 og 16
## 6              02B LI-Lever 02B Kirkøy (north) - torsk lever 4 og 11
## 7              02B LI-Lever 02B Kirkøy (north) - torsk lever 3 og 12
## 8              02B LI-Lever 02B Kirkøy (north) - torsk lever 8 og 10
##   BIOTA_SAMPLENO X_BULK_BIO
## 1              1       <NA>
## 2              2       <NA>
## 3              3      9, 14
## 4              4       5, 6
## 5              5      7, 16
## 6              6      4, 11
## 7              7      3, 12
## 8              8      8, 10
```



### e. Same station in what will become table 04   
Uses 'example_station' from previous chunk  
Example of unpooled sample: 
- SAMPLE_NO_Liver = 6 (the SAMPLE_NO for the liver) - corresponds to fish specimen 7  
Example of pooled sample: 
- SUBNO = 22 (a single line) corresponds to a single pooled liver sample, with SAMPLE_NO = 8   
- BULKID = 22 (three lines) corresponds to the three fish (9,14,20) the liver sample is taken from,
each of which has a length and weight, at 9 and 14 has in addition a muscle sample   

```r
data_04_fish_main %>% 
  filter(STATION_CODE == example_station) %>%
  # select(STATION_CODE, SUBNO, BULKID) %>%
  mutate(Sort = case_when(
    is.na(BULKID) ~ SUBNO,
    BULKID == "" ~ SUBNO,
    !is.na(BULKID) ~ as.numeric(BULKID))
  ) %>%
  arrange(Sort) %>%
  select(-Sort)
```

```
## # A tibble: 21 × 9
##    STATION_CODE LATIN_NAME   Fish_no NOIMP SAMPLE_N…¹ SAMPL…² SUBNO BULKID SMPNO
##    <chr>        <chr>          <dbl> <dbl>      <dbl>   <dbl> <dbl> <chr>  <int>
##  1 02B          Gadus morhua       1     1          1       1     1 ""         1
##  2 02B          Gadus morhua       2     1          2       2     2 ""         1
##  3 02B          Gadus morhua      13     1         13      NA    13 ""         1
##  4 02B          Gadus morhua       9     1          9      NA     9 "17"       1
##  5 02B          Gadus morhua      14     1         14      NA    14 "17"       1
##  6 02B          Gadus morhua      NA     2         NA       3    17 ""         1
##  7 02B          Gadus morhua       5     1          5      NA     5 "18"       1
##  8 02B          Gadus morhua       6     1          6      NA     6 "18"       1
##  9 02B          Gadus morhua      NA     2         NA       4    18 ""         1
## 10 02B          Gadus morhua       7     1          7      NA     7 "19"       1
## # … with 11 more rows, and abbreviated variable names ¹​SAMPLE_NO_Muscle,
## #   ²​SAMPLE_NO_Liver
```


### f. Check number of individuals per sample 

```r
cat("Muscle - number of fish per sample:\n")
```

```
## Muscle - number of fish per sample:
```

```r
data_04_fish_main %>%
  filter(!is.na(SAMPLE_NO_Muscle)) %>%
  count(STATION_CODE, SUBNO, NOIMP) %>%
  xtabs(~NOIMP + STATION_CODE, .)
```

```
##      STATION_CODE
## NOIMP 02B 10B 13B 15B 19B 23B 24B 28B 30B 36B 43B2 45B2 53B 71B 80B 96B 98B1
##     1  15  15  15  15  15  15  15  15  15  15   15   15  15  15  15  15   15
```

```r
cat("\n\nLiver - number of fish per sample:\n")
```

```
## 
## 
## Liver - number of fish per sample:
```

```r
data_04_fish_main %>%
  filter(!is.na(SAMPLE_NO_Liver)) %>%
  count(STATION_CODE, SUBNO, NOIMP) %>%
  xtabs(~NOIMP + STATION_CODE, .)
```

```
##      STATION_CODE
## NOIMP 02B 10B 13B 15B 19B 23B 24B 28B 30B 36B 43B2 45B2 53B 71B 80B 96B 98B1
##     1   2  10   2  15  15   9   8  12   5  11   15    9  11   5  15  15   15
##     2   6   5   6   0   0   6   6   2   0   4    0    0   0   0   0   0    0
##     3   0   0   0   0   0   0   0   0   7   0    0    5   4   5   0   0    0
```

### g. Fish table, add 33F (special case)  
Assuming that 
    + Liver and muscle samples are from the same fish
    + All samples are pooled  

```r
#
# We cheat and just do this manually
#

# One line per fish, for length and weight measurements   
df1 <- tibble(STATION_CODE = "33F", LATIN_NAME = "Platichthys flesus",
              Fish_no = 1:15,
              NOIMP = 1,
              SAMPLE_NO_Muscle = NA, SAMPLE_NO_Liver = NA,
              SUBNO = as.numeric(1:15), BULKID = as.character(rep(16:18, each = 5)))

# One line per pooled sample (= 3 lines altogether))
df2 <- tibble(STATION_CODE = "33F", LATIN_NAME = "Platichthys flesus",
              Fish_no = NA,
              NOIMP = 5,
              SAMPLE_NO_Muscle = 1:3, SAMPLE_NO_Liver = 1:3,
              SUBNO = as.numeric(16:18), 
              BULKID = "")
df <- rbind(df1, df2)

data_04_fish_33F <-  df %>% left_join(df_smpno %>% select(LATIN_NAME, SMPNO), by = "LATIN_NAME")
```

### h. Check 33F

```r
# Sample file
df_samples %>%
  filter(AQUAMONITOR_CODE == "33F") %>% 
  select(AQUAMONITOR_CODE, TISSUE, DESCRIPTION, BIOTA_SAMPLENO, X_BULK_BIO) %>%
  arrange(TISSUE, BIOTA_SAMPLENO)
```

```
##   AQUAMONITOR_CODE    TISSUE                   DESCRIPTION BIOTA_SAMPLENO
## 1              33F  LI-Lever  33F Sande - Flatfisk lever 1              1
## 2              33F  LI-Lever  33F Sande - Flatfisk lever 2              2
## 3              33F  LI-Lever  33F Sande - Flatfisk lever 3              3
## 4              33F MU-Muskel 33F Sande - Flatfisk muskel 1              1
## 5              33F MU-Muskel 33F Sande - Flatfisk muskel 2              2
## 6              33F MU-Muskel 33F Sande - Flatfisk muskel 3              3
##   X_BULK_BIO
## 1       <NA>
## 2       <NA>
## 3       <NA>
## 4       <NA>
## 5       <NA>
## 6       <NA>
```

```r
# Table 04  
data_04_fish_33F %>%
  mutate(Sort = case_when(
    BULKID == "" ~ SUBNO,
    !is.na(BULKID) ~ as.numeric(BULKID))
  ) %>%
  arrange(Sort) %>%
  select(-Sort)
```

```
## # A tibble: 18 × 9
##    STATION_CODE LATIN_NAME      Fish_no NOIMP SAMPL…¹ SAMPL…² SUBNO BULKID SMPNO
##    <chr>        <chr>             <int> <dbl>   <int>   <int> <dbl> <chr>  <int>
##  1 33F          Platichthys fl…       1     1      NA      NA     1 "16"       6
##  2 33F          Platichthys fl…       2     1      NA      NA     2 "16"       6
##  3 33F          Platichthys fl…       3     1      NA      NA     3 "16"       6
##  4 33F          Platichthys fl…       4     1      NA      NA     4 "16"       6
##  5 33F          Platichthys fl…       5     1      NA      NA     5 "16"       6
##  6 33F          Platichthys fl…      NA     5       1       1    16 ""         6
##  7 33F          Platichthys fl…       6     1      NA      NA     6 "17"       6
##  8 33F          Platichthys fl…       7     1      NA      NA     7 "17"       6
##  9 33F          Platichthys fl…       8     1      NA      NA     8 "17"       6
## 10 33F          Platichthys fl…       9     1      NA      NA     9 "17"       6
## 11 33F          Platichthys fl…      10     1      NA      NA    10 "17"       6
## 12 33F          Platichthys fl…      NA     5       2       2    17 ""         6
## 13 33F          Platichthys fl…      11     1      NA      NA    11 "18"       6
## 14 33F          Platichthys fl…      12     1      NA      NA    12 "18"       6
## 15 33F          Platichthys fl…      13     1      NA      NA    13 "18"       6
## 16 33F          Platichthys fl…      14     1      NA      NA    14 "18"       6
## 17 33F          Platichthys fl…      15     1      NA      NA    15 "18"       6
## 18 33F          Platichthys fl…      NA     5       3       3    18 ""         6
## # … with abbreviated variable names ¹​SAMPLE_NO_Muscle, ²​SAMPLE_NO_Liver
```
### i1. Extra samples that may be needed for fish biological effects  

```r
fish_species <- "Gadus morhua"
sel_tissues <- c("Blod", "Galle", "Liver - microsome")

df1 <- data_ind2 %>%
  filter(LATIN_NAME %in% fish_species & 
           (TISSUE_NAME %in% sel_tissues | PARAM %in% "EROD"))  %>%
  # filter(STATION_CODE == "53B") %>%
  rename(Fish_no = SAMPLE_NO) %>%       # because for biological effects, the sample numbers refer to specimen number
  distinct(STATION_CODE, Fish_no)

df2 <- data_04_fish_main %>%
  # filter(STATION_CODE == "53B") %>%
  distinct(STATION_CODE, Fish_no) %>%
  mutate(In_table_04 = TRUE)

# df1
# df2

check <- df1 %>%
  left_join(df2, by = c("STATION_CODE", "Fish_no")) %>%
  filter(is.na(In_table_04))

cat(nrow(check), "specimens lack in 'data_04_fish_main'")
```

```
## 0 specimens lack in 'data_04_fish_main'
```

```r
if (nrow(check) > 0){
  
  stop("You must create 'data_04_fish_extra' for the lacking specimens. See nextchunk. (Error in section 'Table 04 i1')")
  
} else {
  
  cat("\nNo specimens lacking, no need to create extra table 04 data for biological effect data")
  
}
```

```
## 
## No specimens lacking, no need to create extra table 04 data for biological effect data
```

### i2. Fish table, fish specimens only used for biological effects  
- If any speciemns in 23B, 53B, 30B are only used for biological effects, they have not been included in df_samples and therefore they don't make it into 'data_04_fish_main'   
- Note: code is not finished - see comments    

```r
if (nrow(check) > 0){
  
  # NOTE: this code is not finished - must add new SUBNO numbers (wher it says 'extra_subno_numbers' in the code
  # These SUBNO numbers must continue where the former SUBNO numbers stop for this station
  
  data_04_fish_extra <- check %>%
    mutate(LATIN_NAME = "Gadus morhua",
           NOIMP = 1,
           SAMPLE_NO_Muscle = NA, SAMPLE_NO_Liver = NA,
           SUBNO = extra_subno_numbers, BULKID = "",
           SMPNO = 1)
  
} else {
  
  cat("These extra data ('data_04_fish_extra') is not needed in this case (see previous chunk) ")
  
}
```

```
## These extra data ('data_04_fish_extra') is not needed in this case (see previous chunk)
```


### j1. Fish table, join together  

```r
if (nrow(check) == 0){

  data_04_fish <- rbind(data_04_fish_main, data_04_fish_33F)
   
} else {

   data_04_fish <- rbind(data_04_fish_main, data_04_fish_33F, data_04_fish_extra)
  
}

cat("'data_04_fish' created - it has", nrow(data_04_fish), "lines")
```

```
## 'data_04_fish' created - it has 390 lines
```


### j2. Check fish table

```r
check <- data_04_fish %>%
    group_by(STATION_CODE, SMPNO, SUBNO) %>%
    mutate(n = n()) %>%
    filter(n > 1) %>%
    arrange(STATION_CODE, SMPNO, SUBNO)

if (nrow(check) > 0){
  cat(nrow(check), "duplicate data in 'data_04_fish' \n")
  stop("This will lead to duplicates in table 04. Please fix! (error occured in section 'Table 04 j2')")
} else {
  cat("No duplicates in 'data_04_fish' \n")
}
```

```
## No duplicates in 'data_04_fish'
```

```r
if (FALSE){

  check2 <- data_04_fish_main %>%
    group_by(STATION_CODE, SMPNO, SUBNO) %>%
    mutate(n = n()) %>%
    filter(n > 1) %>%
    arrange(STATION_CODE, SMPNO, SUBNO)
  nrow(check2)
  
  check2 <- data_04_fish_extra %>%
    group_by(STATION_CODE, SMPNO, SUBNO) %>%
    mutate(n = n()) %>%
    filter(n > 1) %>%
    arrange(STATION_CODE, SMPNO, SUBNO)
  nrow(check2)
  
}
```


### k. Mussels 

```r
data_04_bluemussel <- data_ind2 %>%
  filter(LATIN_NAME %in% "Mytilus edulis" & MYEAR %in% selected_year) %>%
  mutate(SAMPLE_NO2 = SAMPLE_NO) %>%
  group_by(STATION_CODE, LATIN_NAME, SAMPLE_NO2) %>%
  summarise(N = n(), .groups = "drop")
data_04_bluemussel <- data_04_bluemussel %>% 
  left_join(df_smpno %>% select(LATIN_NAME, SMPNO), 
            by = "LATIN_NAME")
```

### l. Snails

```r
data_04_snail <- data_ind2 %>%
  filter(LATIN_NAME %in% c("Nucella lapillus", "Littorina littorea") & MYEAR %in% selected_year) %>%
  group_by(STATION_CODE, LATIN_NAME) %>%
  summarise(SAMPLE_NO2 = first(SAMPLE_NO),
             .groups = "drop") %>%
  left_join(df_smpno %>% select(LATIN_NAME, SMPNO), 
            by = "LATIN_NAME")
data_04_snail
```

```
## # A tibble: 9 × 4
##   STATION_CODE LATIN_NAME         SAMPLE_NO2 SMPNO
##   <chr>        <chr>                   <dbl> <int>
## 1 11G          Nucella lapillus            1     3
## 2 131G         Nucella lapillus            1     3
## 3 15G          Nucella lapillus            1     3
## 4 227G2        Nucella lapillus            1     3
## 5 22G          Nucella lapillus            1     3
## 6 36G          Nucella lapillus            1     3
## 7 71G          Littorina littorea          1     3
## 8 76G          Nucella lapillus            1     3
## 9 98G          Nucella lapillus            1     3
```


### m. Eider duck  
For eider duck, the same SAMPLE_NO2 = 1 for blood and SAMPLE_NO2 = 1 for egg doesn't mean that this is the same individual (obviously). So we use SUBNO 1-15 for blood SAMPLE_NO 1-15, and SUBNO 16-30 for egg SAMPLE_NO 1-15.  

```r
data_04_eider <- 
  data_ind2 %>%
  filter(LATIN_NAME %in% c("Somateria mollissima") & MYEAR %in% selected_year) %>% # View()
  # excude siloxans - some samples have onky siloxans
  filter(!PARAM %in% c("D4", "D5", "D6")) %>%   
  count(STATION_CODE, LATIN_NAME, TISSUE_NAME, SAMPLE_NO) %>% # View()
  arrange(TISSUE_NAME, SAMPLE_NO) %>%            # first blood SAMPLE_NO 1-15, then egg SAMPLE_NO 1-15.   
  rename(SAMPLE_NO2 = SAMPLE_NO) %>%
  left_join(df_smpno %>% select(LATIN_NAME, SMPNO), by = "LATIN_NAME") %>%
  ungroup()
data_04_eider$SUBNO <- 1:nrow(data_04_eider)     # SUBNO given = 1-30
data_04_eider
```

```
##    STATION_CODE           LATIN_NAME TISSUE_NAME SAMPLE_NO2   n SMPNO SUBNO
## 1           19N Somateria mollissima        Blod          1 100     9     1
## 2           19N Somateria mollissima        Blod          2  99     9     2
## 3           19N Somateria mollissima        Blod          3  99     9     3
## 4           19N Somateria mollissima        Blod          4 101     9     4
## 5           19N Somateria mollissima        Blod          5 100     9     5
## 6           19N Somateria mollissima        Blod          6  99     9     6
## 7           19N Somateria mollissima        Blod          7  98     9     7
## 8           19N Somateria mollissima        Blod          8  98     9     8
## 9           19N Somateria mollissima        Blod          9 100     9     9
## 10          19N Somateria mollissima        Blod         10 101     9    10
## 11          19N Somateria mollissima        Blod         11 101     9    11
## 12          19N Somateria mollissima        Blod         12  97     9    12
## 13          19N Somateria mollissima        Blod         13  98     9    13
## 14          19N Somateria mollissima        Blod         14  99     9    14
## 15          19N Somateria mollissima        Blod         15  96     9    15
## 16          19N Somateria mollissima         Egg          1 101     9    16
## 17          19N Somateria mollissima         Egg          2 101     9    17
## 18          19N Somateria mollissima         Egg          3 101     9    18
## 19          19N Somateria mollissima         Egg          4 101     9    19
## 20          19N Somateria mollissima         Egg          5 101     9    20
## 21          19N Somateria mollissima         Egg          6 101     9    21
## 22          19N Somateria mollissima         Egg          7 101     9    22
## 23          19N Somateria mollissima         Egg          8 101     9    23
## 24          19N Somateria mollissima         Egg          9 101     9    24
## 25          19N Somateria mollissima         Egg         10 101     9    25
## 26          19N Somateria mollissima         Egg         11 101     9    26
## 27          19N Somateria mollissima         Egg         12 101     9    27
## 28          19N Somateria mollissima         Egg         13 101     9    28
## 29          19N Somateria mollissima         Egg         14 101     9    29
## 30          19N Somateria mollissima         Egg         15 101     9    30
```

### n. Put together data_04  
For blue mussel and snail, SUBNO = SAMPLE_NO2  
For fish, we have constructed SUBNO (see above)  
For eider, SUBNO 

```r
data_04_1 <- data.frame(
  RECID = "04", 
  CRUIS = "AA711", 
  STNNO = data_04_fish$STATION_CODE,
  SMPNO = data_04_fish$SMPNO,
  SUBNO = data_04_fish$SUBNO,
  NOIMP = data_04_fish$NOIMP,
  ORGSP = NA,
  SEXCO = NA,
  STAGE = NA,
  CONES = NA,
  ASTSA = NA,
  NODIS = NA,
  BULKID = data_04_fish$BULKID,
  stringsAsFactors = FALSE
  )

data_04_2 <- data.frame(
  RECID = "04", 
  CRUIS = "AA711", 
  STNNO = data_04_bluemussel$STATION_CODE,
  SMPNO = data_04_bluemussel$SMPNO,
  SUBNO = data_04_bluemussel$SAMPLE_NO2,
  NOIMP = 20,
  ORGSP = NA,
  SEXCO = NA,
  STAGE = NA,
  CONES = NA,
  ASTSA = NA,
  NODIS = NA,
  BULKID = "",
  stringsAsFactors = FALSE
  )

data_04_3 <- data.frame(
  RECID = "04", 
  CRUIS = "AA711", 
  STNNO = data_04_snail$STATION_CODE,
  SMPNO = data_04_snail$SMPNO,
  SUBNO = data_04_snail$SAMPLE_NO2,
  NOIMP = 50,
  ORGSP = NA,
  SEXCO = "F",
  STAGE = NA,
  CONES = NA,
  ASTSA = NA,
  NODIS = NA,
  BULKID = "",
  stringsAsFactors = FALSE
  )

data_04_4 <- data.frame(
  RECID = "04", 
  CRUIS = "AA711", 
  STNNO = data_04_eider$STATION_CODE,
  SMPNO = data_04_eider$SMPNO,
  SUBNO = data_04_eider$SUBNO,
  NOIMP = 1,
  ORGSP = NA,
  SEXCO = "F",
  STAGE = NA,
  CONES = NA,
  ASTSA = NA,
  NODIS = NA,
  BULKID = "",
  stringsAsFactors = FALSE
  )

# Without Somateria /Eider duck:
# data_04 <- rbind(data_04_1, data_04_2, data_04_3)

# With Somateria /Eider:
data_04 <- rbind(data_04_1, data_04_2, data_04_3, data_04_4)


# Checks
# data_04_1 %>% filter(STNNO %in% "36B")
# data_04 %>% filter(STNNO %in% "36B")
# data_04 %>% filter(is.na(SUBNO))
# data_04 %>% filter(SUBNO == "")
```

### o. Check the crucial columns

```r
check <- apply(
  is.na(data_04 %>% select( RECID,  CRUIS,  STNNO,  SMPNO,  SUBNO,  NOIMP)), 
  2, sum
  )
if (sum(check == rep(0, 6)) == 6){
  cat("All records have values for RECID,  CRUIS,  STNNO,  SMPNO,  SUBNO,  NOIMP\n\n")
} else {
  cat("ERROR! One or more columns RECID,  CRUIS,  STNNO,  SMPNO,  SUBNO,  NOIMP lack data\n\n")
}
```

```
## All records have values for RECID,  CRUIS,  STNNO,  SMPNO,  SUBNO,  NOIMP
```

```r
apply(is.na(data_04), 2, sum)
```

```
##  RECID  CRUIS  STNNO  SMPNO  SUBNO  NOIMP  ORGSP  SEXCO  STAGE  CONES  ASTSA 
##      0      0      0      0      0      0    511    472    511    511    511 
##  NODIS BULKID 
##    511     90
```

```r
# data_04 %>% filter(STNNO %in% c("24B", "53B", "96B")) %>% arrange(STNNO, SMPNO, SUBNO)


# data_04 %>% filter(is.na(SUBNO))
```

## Table 21
We make this table before table 10 in order to generate the AMLNK values we need    
Note that the AMLNK values are just random, i.e. alphabetical for non-biological effects and
then biological effects added in the end. They have only meaning internally in this data set
(for coupling to table 10)    
SO FAR WE JUST USE LAST YEAR'S TABLE :-)  

### Generate lookup

```r
df_amlnk <- data_10_2016 %>%
  group_by(PARAM, AMLNK) %>%
  summarise(n = n(),
            .groups = "drop")
df_amlnk$n <- NULL
```

### Add 'new' parameters  
- GOSOI (gonadal somatic index = (gonad weight/whole organism weight) x 100), https://vocab.ices.dk/?CodeID=51654     
- INTF% = intersex, https://vocab.ices.dk/?CodeID=53682       
- C13D and N15D = isotope ratio δ13C:12C (https://vocab.ices.dk/?CodeID=197194) and isotope ratio δ15N:14N (https://vocab.ices.dk/?CodeID=197195)   

```r
params_new <- c("GOSOI", "INT%", "C13D", "N15D")
# params_new <- c("GOSOI", "INT%")

for (param in params_new){
  if (sum(df_amlnk$PARAM %in% param) == 0){
    # add one row to df_amlnk
    df_amlnk <- bind_rows(
      df_amlnk,
      tibble(
        PARAM = param, 
        AMLNK = max(df_amlnk$AMLNK) + 1)    # add 1 to the last AMLNK number  
    )
  }
}
```



## Table 10  
Table 10 is the combined rows of the following tables:
    - data_10_fish (part c)  
    - data_10_bluemussel (part d)  
    - data_10_length2 (part g3)  
    - data_10_vdsi (part h)  

### a1. Make data_conc_1   
**Note make sure that all estimates are on wet-weight basis**    
- Note that siloxanes "D4", "D5", "D6" are not included because they are not in the ICES vocabulary  

```r
par_ices <- unique(data_10_2016$PARAM) %>% sort()
# Add some
par_ices <- c(par_ices, "HCHA", "HCHG", "HCB") %>% unique() %>% sort() 
#
# Should also have added "D4", "D5", "D6"
# But siloxans are not in the ICES vocabulary!
# (I also searched fro e.g. 'Decamethylcyclopentasiloxane' (= D5))

par_niva <- data_ind2 %>% filter(MYEAR %in% selected_year) %>% pull(PARAM) %>% unique() %>% sort()
par_ices[!par_ices %in% par_niva]
```

```
##  [1] "BD100"  "BD126"  "BD153"  "BD154"  "BD183"  "BD209"  "FATWT%" "LNMEA" 
##  [9] "PFDA"   "PFUnda" "TBBPA"  "TBP"    "TBTIN"  "WTMEA"
```

```r
data_conc_1 <- data_ind2 %>% 
  filter(MYEAR %in% selected_year) %>%
  mutate(SAMPLE_NO2 =                              # setting SAMPLE_NO2 (which is used in 2018 version)
           case_when(!is.na(SAMPLE_NO) ~  SAMPLE_NO,
                     is.na(SAMPLE_NO) ~  SAMPLE_NO2),
         BASIS = "W"                             # MAKE SURE ALL MEASUREMENS ARE WET WEIGHT 
  )

# data_ind2 %>% 
#   filter(STATION_CODE == "23B" & PARAM %in% "EROD") %>% 
#   select(STATION_CODE, PARAM, SAMPLE_NO, SAMPLE_NO2)
```

### a2. Change parameter names where ICES name differs  

```r
# Change BDE to BD for the following. NOTE: "BDE28"  "BDE47"  "BDE99" should NOT be changed (!)
sel <- data_conc_1$PARAM %in% c("BDE100", "BDE126", "BDE153", "BDE154", "BDE183", "BDE209")
data_conc_1$PARAM[sel] <- sub("BDE", "BD", data_conc_1$PARAM[sel])

sel <- data_conc_1$PARAM %in% "PFUdA"; sum(sel)
```

```
## [1] 192
```

```r
data_conc_1$PARAM[sel] <- "PFUnda"
sel <- data_conc_1$PARAM %in% "PFDcA"; sum(sel)
```

```
## [1] 192
```

```r
data_conc_1$PARAM[sel] <- "PFDA"

sel <- data_conc_1$PARAM %in% "TBT"; sum(sel)   # Note this is TBT cation weight = 2.44 * TBTIN (TBT tin weight)
```

```
## [1] 36
```

```r
data_conc_1$PARAM[sel] <- "TBSN+"

sel <- data_conc_1$PARAM %in% "Delta13C"; sum(sel)  
```

```
## [1] 340
```

```r
data_conc_1$PARAM[sel] <- "C13D"
sel <- data_conc_1$PARAM %in% "Delta15N"; sum(sel)  
```

```
## [1] 340
```

```r
data_conc_1$PARAM[sel] <- "N15D"

# This did NOT do in 2018:
# Fat:
sel <- data_conc_1$PARAM %in% "Fett"; sum(sel)
```

```
## [1] 332
```

```r
data_conc_1$PARAM[sel] <- "FATWT%"
```

### a2. Filter data to keep only those parameters that we need

```r
# Parameters 
par_niva2 <- data_conc_1 %>% pull(PARAM) %>% unique() %>% sort()
cat("\nCheck parameters not found in this year's data:  \n")
```

```
## 
## Check parameters not found in this year's data:
```

```r
par_ices[!par_ices %in% par_niva2]   # "FATWT%" "LNMEA"  "TBP"    "WTMEA"  
```

```
## [1] "LNMEA" "TBBPA" "TBP"   "TBTIN" "WTMEA"
```

```r
                                     # We will add "FATWT%" and "LNMEA" later

# Let us use TBSN+ instead of TBTIN
par_ices <- par_ices[!par_ices %in% "TBTIN"]
par_ices <- c(par_ices, "TBSN+")
# par_ices

# Filter data to keep only those parameters that we need
data_conc_2 <- data_conc_1 %>% filter(PARAM %in% par_ices)

cat("\nCheck final parameters:\n")
```

```
## 
## Check final parameters:
```

```r
tab <- data_conc_2 %>% xtabs(~PARAM, .)
tab
```

```
## PARAM
##   ACNE  ACNLE     AG   ALAD    ANT     AS    BAA    BAP BAP3OH   BBJF  BD100 
##     26     26    335     47     26    341     26     26     60     26    213 
##  BD126  BD153  BD154  BD183  BD209  BDE28  BDE47  BDE99  BGHIP    BKF  CB101 
##    213    213    213    213    213    213    213    213     26     26    331 
##  CB118  CB138  CB153  CB180   CB28   CB52     CD     CO     CR     CU  DBA3A 
##    331    331    331    331    331    331    341    334    340    341     26 
##  DDEPP  DDTPP DRYWT%   EROD FATWT%    FLE    FLU  HBCDA  HBCDB  HBCDG    HCB 
##    149    149    581     46    332     26     26    232    232    232    176 
##   HCHA   HCHG     HG   ICDP   MCCP    NAP     NI     PA  PA1OH     PB   PFBS 
##    146    146    371     26    230     26    340     26     60    341      6 
##   PFDA  PFHpA  PFHxA   PFNA   PFOA   PFOS  PFOSA PFUnda    PYR PYR1OH   SCCP 
##    192    192    192    192    192    192    192    192     26     60    230 
##     SN  TBSN+  TDEPP   VDSI     ZN 
##    335     36    149      8    341
```

```r
# cat("\n\nCheck excluded parameters:\n")
# data_conc_1 %>% filter(!PARAM %in% par_ices) %>% xtabs(~PARAM, .)

# Parameters with data over LOQ that are excluded:
cat("\n\nParameters with data over LOQ that are excluded:\n")
```

```
## 
## 
## Parameters with data over LOQ that are excluded:
```

```r
tab <- data_conc_1 %>% 
  filter(!PARAM %in% par_ices & is.na(FLAG1)) %>% 
  xtabs(~PARAM, .) %>% 
  sort(decreasing = TRUE)
tab
```

```
## PARAM
##                       % C                       % N                       C/N 
##                       340                       340                       340 
##                      C13D                      N15D                     CB_S7 
##                       340                       340                       325 
##                     HBCDD                     BDE6S                     BDESS 
##                       208                       201                       201 
##                     BDE49                      PFAS                     BDE66 
##                       186                       181                       154 
##                     DDTEP                    BDE119                  Dieldrin 
##                       145                       142                       117 
##                        D4                        D5          Nonaklor, trans- 
##                       106                       106                       104 
##                     Mirex      Oktaklorstyren (OCS)                        D6 
##                       100                        94                        93 
##                     PROTV        alfa-Klordan (cis)         Heptaklor epoksid 
##                        93                        92                        87 
##            MCCP eksl. LOQ        Toksafen Parlar 50                     DDDOP 
##                        84                        76                        69 
##        Toksafen Parlar 26                     DDTOP                Oxyklordan 
##                        60                        59                        54 
##                     AY380                     PYR1O            SCCP eksl. LOQ 
##                        51                        51                        51 
##     gamma-Klordan (trans)                      PA1O                     CB105 
##                        44                        34                        30 
##                     CB128                     CB156                     CB167 
##                        30                        30                        30 
##                     CB187                      CB99                 Sum-HepCB 
##                        30                        30                        30 
##                     CB170                      CB74                      PeCB 
##                        29                        29                        29 
##                     CB183                 Sum-HexCB                     BDE77 
##                        28                        28                        27 
##                     CB209                     BAP3O                     CB157 
##                        27                        26                        26 
##                 Sum-PenCB                     CB114     Tributyltinn (TBT)-Sn 
##                        25                        24                        24 
##                     CB123                      CB66                 Sum-TriCB 
##                        23                        23                        23 
##                     BDE17                      KPAH                       P_S 
##                        22                        22                        22 
##                     PAH16                 Sum-TetCB                     CB141 
##                        22                        22                        21 
##                     DDEOP                     CB189                     CB194 
##                        21                        20                        20 
##                     CB206                      CB31                      CB47 
##                        20                        20                        20 
##                    Krysen                     MBTIN    Monobutyltinn (MBT)-Sn 
##                        19                        18                        18 
##                     CB149                     DBTIN   Dibutyltinn-Sn (DBT-Sn) 
##                        16                        16                        16 
##                     PFHxS                       QCB                       TBA 
##                        15                        15                        15 
##        Toksafen Parlar 62                     TPTIN    Trifenyltinn (TPhT)-Sn 
##                        11                         9                         9 
##                     CB122                     BDE85 Total PFOS/PFOA inkl. LOQ 
##                         8                         7                         6 
##                      CB33                      CB37                      CB18 
##                         5                         5                         2 
##                    Endrin                    BDE184                     BDE71 
##                         2                         1                         1 
##                      HCHB                     INTF% 
##                         1                         1
```

```r
# dput(names(tab)) 
```



### a3. Check that basis is only W (wet weight basis)

```r
data_conc_2 %>% xtabs(~BASIS, .)
```

```
## BASIS
##     W 
## 13227
```

### a4. Check availability of dry weight  
- Should be lacking only for blood and egg   

```r
# Dry weight:
# sel <- data_conc_2$PARAM %in% "DRYWT%"; sum(sel)

# Check availability of dry weight for CB118 (as example for liver)
data_conc_2 %>%
  filter(PARAM %in% c("DRYWT%", "CB118")) %>%
  select(STATION_CODE, TISSUE_NAME, SAMPLE_NO2, PARAM, VALUE_WW) %>% 
  arrange(STATION_CODE, TISSUE_NAME, SAMPLE_NO2, PARAM) %>%
  spread(PARAM, VALUE_WW) %>%
  filter(!is.na(CB118)) %>%
  count(STATION_CODE, TISSUE_NAME, Lack_dryweight = is.na(`DRYWT%`)) %>%
  filter(Lack_dryweight)
```

```
##   STATION_CODE TISSUE_NAME Lack_dryweight  n
## 1          02B       Lever           TRUE  1
## 2          19N        Blod           TRUE 15
## 3          19N         Egg           TRUE 15
```

```r
# Check availability of dry weight for HG (as example for muscle) 
data_conc_2 %>%
  filter(PARAM %in% c("DRYWT%", "HG")) %>%
  select(STATION_CODE, TISSUE_NAME, SAMPLE_NO2, PARAM, VALUE_WW) %>% 
  arrange(STATION_CODE, TISSUE_NAME, SAMPLE_NO2, PARAM) %>%
  spread(PARAM, VALUE_WW) %>%
  filter(!is.na(HG)) %>%
  count(STATION_CODE, TISSUE_NAME, Lack_dryweight = is.na(`DRYWT%`)) %>%
  filter(Lack_dryweight)
```

```
##   STATION_CODE TISSUE_NAME Lack_dryweight  n
## 1          19N        Blod           TRUE 15
## 2          19N         Egg           TRUE 15
```

```r
# Check bird data
# data_conc_2 %>%
#   filter(STATION_CODE == "19N") %>%
#   count(TISSUE_NAME, PARAM)
```

### a5. Check uncertainty (UNCRT)   
If problems here, check either   
    - script 34 (for chemical variables from Eurofins or NILU)  
    - script 02 (for VDSI and the biological effect variables in cod)  

```r
labels <- c("1 - Under LOQ", "2 - Over LOQ, NA", 
          "3 - Over LOQ, zero", "4 - Over LOQ, OK")

# xxx
data_conc_2 %>% 
  filter(PARAM == "BDE47" & is.na(FLAG1)) %>%
  xtabs(~Lab + is.na(UNCRT), .)
```

```
##           is.na(UNCRT)
## Lab        FALSE
##   Eurofins   183
##   NILU        16
```

```r
df_uncertainty <- data_conc_2 %>%
  mutate(Uncert = 
           case_when(
             !is.na(FLAG1) ~ labels[1],
             is.na(UNCRT) ~ labels[2],
             UNCRT == 0 ~ labels[3],
             UNCRT != 0 ~ labels[4]),
         Uncert = factor(Uncert, levels = labels)
  )

tab <- df_uncertainty %>%
  count(PARAM, Lab, Uncert) %>%
  pivot_wider(names_from = "Uncert", values_from = "n", values_fill = 0, names_sort = TRUE)
tab
```

```
## # A tibble: 113 × 5
##    PARAM Lab      `1 - Under LOQ` `2 - Over LOQ, NA` `4 - Over LOQ, OK`
##    <chr> <chr>              <int>              <int>              <int>
##  1 ACNE  Eurofins              26                  0                  0
##  2 ACNLE Eurofins              18                  0                  8
##  3 AG    Eurofins              77                  0                228
##  4 AG    NILU                   0                  0                 30
##  5 ALAD  NIVA                   0                 47                  0
##  6 ANT   Eurofins               8                  0                 18
##  7 AS    Eurofins               0                  0                311
##  8 AS    NILU                   0                  0                 30
##  9 BAA   Eurofins               8                  0                 18
## 10 BAP   Eurofins              14                  0                 12
## # … with 103 more rows
```

```r
sel <- names(tab) %in% labels
tot <- apply(tab[sel], 1, sum, na.rm = TRUE)       # note hard-coded columns
sel <- names(tab) %in% c("2 - Over LOQ, NA", "3 - Over LOQ, zero")
not_ok <- apply(tab[sel], 1, sum, na.rm = TRUE)
sel <- names(tab) %in% c("2 - Over LOQ, NA", "3 - Over LOQ, zero", "4 - Over LOQ, OK")
over_loq <- apply(tab[sel], 1, sum, na.rm = TRUE)

tab <- tab %>%
  mutate(Total = tot,
         NotOK = round(not_ok/over_loq*100, 1)
         ) %>%
  filter(Total > 0) %>%
  arrange(NotOK)

tab
```

```
## # A tibble: 113 × 7
##    PARAM Lab      `1 - Under LOQ` `2 - Over LOQ, NA` 4 - Over LOQ,…¹ Total NotOK
##    <chr> <chr>              <int>              <int>           <int> <int> <dbl>
##  1 ACNLE Eurofins              18                  0               8    26     0
##  2 AG    Eurofins              77                  0             228   305     0
##  3 AG    NILU                   0                  0              30    30     0
##  4 ANT   Eurofins               8                  0              18    26     0
##  5 AS    Eurofins               0                  0             311   311     0
##  6 AS    NILU                   0                  0              30    30     0
##  7 BAA   Eurofins               8                  0              18    26     0
##  8 BAP   Eurofins              14                  0              12    26     0
##  9 BBJF  Eurofins               5                  0              21    26     0
## 10 BD100 Eurofins               1                  0             182   183     0
## # … with 103 more rows, and abbreviated variable name ¹​`4 - Over LOQ, OK`
```

```r
# To print list of parameters
# tab %>%
#   filter(NotOK == 100) %>%
#   pull(PARAM) %>% 
#   dput()
```


### a6-1. GOSOI - gonadal somatic index, defined as (gonad weight/whole organism weight) x 100   
    - Must be given for all EROD samples  
    - Taken from data_ind_fish
    - Creates `data_conc`    

```r
# Make data to go into 'data_to_add' below (Gonadal somatic index )
data_for_leftjoin <- data_ind_fish %>%
  mutate(VALUE_WW = Gonad_weight/Weight*100) %>%
  filter(!is.na(VALUE_WW)) %>%
  select(STATION_CODE, Fish_no, VALUE_WW)
  
# Check data 1
# data_conc_2[sel,] %>%
#  filter(PARAM %in% "EROD") %>%
#   count(STATION_CODE)

# Check data 2
# data_conc_2[sel,] %>%
#  filter(PARAM %in% "EROD") %>%
#  xtabs(~STATION_CODE + SAMPLE_NO2, .)

# Copy EROD data and replace the columns we need
# These will be extra rows in the data set
data_to_add <- data_conc_2 %>%
  filter(PARAM %in% "EROD") %>%
  mutate(TISSUE_NAME = "WO",
         MATRX = "WO",
         PARAM = "GOSOI",       
         BASIS = NA,
         UNIT = "idx") %>%       # for "index" (MUNIT will be set to the same)
  select(-VALUE_WW, -UNCRT) %>%  # this we replace by left_join below
  safe_left_join(data_for_leftjoin, 
                 by = c("STATION_CODE", "SAMPLE_NO2" = "Fish_no"),
                 na_matches = "never"
  ) %>%
  filter(!is.na(VALUE_WW)) %>%
  mutate(UNCRT = 5,     # We guess uncertainity is SD = 5%
         METCU = "%")

# Show data that will be added
if (FALSE){
  data_to_add %>%
    select(STATION_CODE, SAMPLE_NO2, PARAM, VALUE_WW, UNIT)
}

# Add to data (if not already added)
if (sum(data_conc_2$PARAM %in% "GOSOI") == 0){      # Make sure we add data only once
  data_conc <- bind_rows(data_conc_2, data_to_add)
}
```

### a6-2. Remove EROD without GOSOI    
- EROD withou GOSOI is an error in the OCES database ('Cross record/field check (condition not met)')    

```r
data_conc_back <- data_conc
# data_conc <- data_conc_back

data_conc <- data_conc %>%
  left_join(
    data_conc %>%
      filter(PARAM %in% "GOSOI") %>%
      select(STATION_CODE, SAMPLE_NO2, VALUE_WW) %>%
      rename(GOSOI = VALUE_WW),
    by = c("STATION_CODE", "SAMPLE_NO2")
  ) %>%
  filter(!(PARAM %in% "EROD" & is.na(GOSOI)))

message(nrow(data_conc_back) - nrow(data_conc), " EROD observations without GOSOI deleted")
```

```
## 2 EROD observations without GOSOI deleted
```






### b. Unit lookup table  
- From UNIT (Nivabase) to MUNIT (ICES)  

```r
xtabs(~UNIT, data_conc)
```

```
## UNIT
##                   %                 idx               Index             MG_P_KG 
##                 913                  44                   8                3760 
##   ng/min/mg protein pmol/min/mg protein             UG_P_KG 
##                  47                  44                8453
```

```r
xtabs(~MUNIT, data_10_2016)
```

```
## MUNIT
##                   %                   g               mg/kg                  mm 
##                 906                 306                3535                 306 
##                ng/g   ng/min/mg protein pmol/min/mg protein               ug/kg 
##                 180                  46                  45                7058
```

```r
# we need only these units, as the rest will be dropped
df_munit <- data.frame(
  UNIT = c("MG_P_KG", "NG_P_G", "UG_P_KG", "PERCENT",
           "ng/min/mg protein", "pmol/min/mg protein", 
           "idx"),
  MUNIT = c("mg/kg", "ng/kg", "ug/kg", "%",
            "ng/min/mg protein", "pmol/min/mg protein", 
            "idx"), 
  stringsAsFactors = FALSE)

#
# Check units for biol effects
#

# data_conc %>% 
#   filter(PARAM %in% c("PA1O", "PYR1O", "BAP3O", "PA1OH", "PYR1OH", "BAP3OH", "ALAD", "EROD")) %>%
#   count(PARAM, UNIT)
# 
# data_10_2016 %>% 
#   filter(PARAM %in% c("PA1O", "PYR1O", "BAP3O", "PA1OH", "PYR1OH", "BAP3OH", "ALAD", "EROD")) %>%
#   count(PARAM, MUNIT)
```


### c1. Fish, get SUBNO

#### c1a1. Definitions

```r
# Define fish species
fish_species <- c('Gadus morhua', 'Platichthys flesus', 'Limanda limanda')

# Define MATRX
df_matrx <- tibble(
  TISSUE_NAME = c("Muskel", "Blod", "Galle", "Liver - microsome", "Lever"),
  MATRX = c("MU", "BL", "BI", "LIMIC", "LI")
)

# xtabs(~MATRX + PARAM, data_10_2016 %>% filter(PARAM %in% c("ALAD", "BAP3OH", "EROD", "PA1OH", "PYR1OH")))
# xtabs(~MUNIT + PARAM, data_10_2016 %>% filter(PARAM %in% c("ALAD", "BAP3OH", "EROD", "PA1OH", "PYR1OH")))
```

#### c1a2. Check when SAMPLE_NO_Muscle =/= Fish_no  
This could potentially cause trouble, as biological effects follow Fish_no, not SAMPLE_NO_Muscle as assumed in c1a3.   
But this is no problem: only 2 cases, for 02B and 36B - which don't have biological effects   

```r
data_04_fish %>%
  filter(Fish_no != SAMPLE_NO_Muscle)
```

```
## # A tibble: 2 × 9
##   STATION_CODE LATIN_NAME   Fish_no NOIMP SAMPLE_NO…¹ SAMPL…² SUBNO BULKID SMPNO
##   <chr>        <chr>          <dbl> <dbl>       <dbl>   <dbl> <dbl> <chr>  <int>
## 1 02B          Gadus morhua      16     1          15      NA    16 "19"       1
## 2 36B          Gadus morhua      16     1          11      11    16 ""         1
## # … with abbreviated variable names ¹​SAMPLE_NO_Muscle, ²​SAMPLE_NO_Liver
```


#### c1a3. Muscle part (not including biological effects)    
(biological effects includes EROD, which is in liver, but samples are numbered following specimen number)
- Join "SAMPLE_NO2" (from concentration data) = "SAMPLE_NO_Muscle" (in 'data_04_fish')

```r
sel_tissues <- "Muskel"
data_conc_fishmuscle <- data_conc %>%
  filter(LATIN_NAME %in% fish_species & TISSUE_NAME %in% sel_tissues)  %>%
  left_join(df_matrx, by = c("TISSUE_NAME", "MATRX"))

# Add SMPNO and SUBNO
df_for_join <- data_04_fish %>% 
  select(STATION_CODE, LATIN_NAME, SAMPLE_NO_Muscle, SMPNO, SUBNO) %>%
  filter(!is.na(SAMPLE_NO_Muscle))

n1 <- nrow(data_conc_fishmuscle)
data_conc_fishmuscle <- data_conc_fishmuscle %>%
  left_join(df_for_join, by = c("STATION_CODE", "LATIN_NAME", "SAMPLE_NO2" = "SAMPLE_NO_Muscle")) %>%
  mutate(MATRX = "MU")

n2 <- nrow(data_conc_fishmuscle)
if (n1 == n2){
  cat("Creating data_conc_fishmuscle - join OK\n")
} else {
  cat("Creating data_conc_fishmuscle - join NOT OK!\n")
}
```

```
## Creating data_conc_fishmuscle - join OK
```

```r
# data_conc_fishmuscle %>% filter(STATION_CODE == "24B" & PARAM %in% "HG") %>% as.data.frame()
# data_conc_fishmuscle %>% filter(STATION_CODE == "23B" & PARAM %in% "EROD") %>% as.data.frame()

# data_conc_fishmuscle %>% 
#   filter(is.na(SMPNO)) %>% 
#   select(STATION_CODE, PARAM, SAMPLE_NO, SAMPLE_NO2, VALUE_WW)


# apply(is.na(data_conc_fishmuscle), 2, sum)

# Check SMPNO - lacking for one special case, where fish no 15 
#    has no ordinary muscle data, but has biol. effect data
not_ok <- is.na(data_conc_fishmuscle$SMPNO)
cat("Lacking SMPNO + SUBNO:", sum(not_ok), "records\n")  # 0
```

```
## Lacking SMPNO + SUBNO: 0 records
```

```r
#
# If >0 records lack SMPno + SUBNO, we can skip the manual write
#

manual_write <- FALSE

if (manual_write){
  
  # If these are all biol. effects...
  lacking_subno_is_biol_effects <- 
    data_conc_fishmuscle$PARAM[not_ok] %in% c("PA1OH", "PYR1OH", "BAP3OH", "ALAD", "EROD")
  
  # ...we fix this special case
  if (mean(lacking_subno_is_biol_effects) == 1){
    data_conc_fishmuscle$SUBNO[not_ok] <- data_conc_fishmuscle$SAMPLE_NO2[not_ok]  # set SUBNO to SAMPLE_NO2
    data_conc_fishmuscle$SMPNO[not_ok] <- 1  # set SMPN to 1 (assumong this is cod)
  }
  
}


if (sum(not_ok) == 0){
  cat("All OK\n")
} else {
  cat ("Still lacking SMPNO + SUBNO!")
}
```

```
## All OK
```

```r
#
# In first try, this resukts in "Still lacking SMPNO + SUBNO!"
# This code shows that this is lacking for BLod, Gall Liver - microsome and WO for 
#  SAMPLE_NO 2,9, 13 and 14 in station 53B
# It also shows that this is because 'data_04_fish' doesn't contain these 
#  (because there are noe liver + muscel samples from these fish)

if (FALSE){
  
  df <- data_conc_fishmuscle[not_ok,] %>%
    arrange(SAMPLE_NO, PARAM)
  xtabs(~TISSUE_NAME + SAMPLE_NO + STATION_CODE, df)
  

  # Check data_04_fish
  data_04_fish %>%
    filter(STATION_CODE == "53B")  


}

# Check if problems
```

#### c1a4. Biological effects + length and weight  
(biological effects includes EROD, which is in liver, but samples are numbered as muscle)  
- Join "SAMPLE_NO2" (from concentration data) = "Fish_no" (in 'data_04_fish')   
- I.e., SAMPLE_NO2 follows numbered following specimen number - in contrast with muscle data (see previous chunk)  

```r
sel_tissues <- c("Blod", "Galle", "Liver - microsome", "WO")
data_conc_fishbiol <- data_conc %>%
  filter(LATIN_NAME %in% fish_species & 
           (TISSUE_NAME %in% sel_tissues | PARAM %in% "EROD"))  %>%
  left_join(df_matrx, by = c("TISSUE_NAME", "MATRX"))

# Add SMPNO and SUBNO
df_for_join <- data_04_fish %>% 
  select(STATION_CODE, LATIN_NAME, Fish_no, SMPNO, SUBNO) %>%
  filter(!is.na(Fish_no))

n1 <- nrow(data_conc_fishbiol)
data_conc_fishbiol <- data_conc_fishbiol %>%
  left_join(df_for_join, by = c("STATION_CODE", "LATIN_NAME", "SAMPLE_NO2" = "Fish_no")) %>%
  mutate(MATRX = "MU")

n2 <- nrow(data_conc_fishbiol)
if (n1 == n2){
  cat("Creating data_conc_fishbiol - join OK\n")
} else {
  cat("Creating data_conc_fishbiol - join NOT OK!\n")
}
```

```
## Creating data_conc_fishbiol - join OK
```

```r
# data_conc_fishbiol %>% filter(STATION_CODE == "24B" & PARAM %in% "HG") %>% as.data.frame()
# data_conc_fishbiol %>% filter(STATION_CODE == "23B" & PARAM %in% "EROD") %>% as.data.frame()

# data_conc_fishbiol %>% 
#   filter(is.na(SMPNO)) %>% 
#   select(STATION_CODE, PARAM, SAMPLE_NO, SAMPLE_NO2, VALUE_WW)


# apply(is.na(data_conc_fishbiol), 2, sum)

# Check SMPNO - lacking for one special case, where fish no 15 
not_ok <- is.na(data_conc_fishbiol$SMPNO)
cat("Lacking SMPNO + SUBNO:", sum(not_ok), "records\n")  # 0
```

```
## Lacking SMPNO + SUBNO: 0 records
```

```r
#
# If >0 records lack SMPno + SUBNO, we can skip the manual write
#

manual_write <- FALSE

if (manual_write){
  
  # If these are all biol. effects...
  lacking_subno_is_biol_effects <- 
    data_conc_fishbiol$PARAM[not_ok] %in% c("PA1OH", "PYR1OH", "BAP3OH", "ALAD", "EROD")
  
  # ...we fix this special case
  if (mean(lacking_subno_is_biol_effects) == 1){
    data_conc_fishbiol$SUBNO[not_ok] <- data_conc_fishbiol$SAMPLE_NO2[not_ok]  # set SUBNO to SAMPLE_NO2
    data_conc_fishbiol$SMPNO[not_ok] <- 1  # set SMPN to 1 (assumong this is cod)
  }
  
}


if (sum(not_ok) == 0){
  cat("All OK\n")
} else {
  cat ("Still lacking SMPNO + SUBNO!")
}
```

```
## All OK
```

```r
#
# In first try, this resukts in "Still lacking SMPNO + SUBNO!"
# This code shows that this is lacking for BLod, Gall Liver - microsome and WO for 
#  SAMPLE_NO 2,9, 13 and 14 in station 53B
# It also shows that this is because 'data_04_fish' doesn't contain these 
#  (because there are noe liver + muscel samples from these fish)

if (FALSE){
  
  df <- data_conc_fishbiol[not_ok,] %>%
    arrange(SAMPLE_NO, PARAM)
  xtabs(~TISSUE_NAME + SAMPLE_NO + STATION_CODE, df)
  

  # Check data_04_fish
  data_04_fish %>%
    filter(STATION_CODE == "53B")  


}

# Check if problems
```


#### c1a5. Check 33F

```r
# Sample file
df_samples %>%
  filter(AQUAMONITOR_CODE == "33F") %>% 
  select(AQUAMONITOR_CODE, TISSUE, DESCRIPTION, BIOTA_SAMPLENO, X_BULK_BIO) %>%
  arrange(TISSUE, BIOTA_SAMPLENO)
```

```
##   AQUAMONITOR_CODE    TISSUE                   DESCRIPTION BIOTA_SAMPLENO
## 1              33F  LI-Lever  33F Sande - Flatfisk lever 1              1
## 2              33F  LI-Lever  33F Sande - Flatfisk lever 2              2
## 3              33F  LI-Lever  33F Sande - Flatfisk lever 3              3
## 4              33F MU-Muskel 33F Sande - Flatfisk muskel 1              1
## 5              33F MU-Muskel 33F Sande - Flatfisk muskel 2              2
## 6              33F MU-Muskel 33F Sande - Flatfisk muskel 3              3
##   X_BULK_BIO
## 1       <NA>
## 2       <NA>
## 3       <NA>
## 4       <NA>
## 5       <NA>
## 6       <NA>
```

```r
# Table 04  
data_04_fish_33F %>%
  mutate(Sort = case_when(
    BULKID == "" ~ SUBNO,
    !is.na(BULKID) ~ as.numeric(BULKID))
  ) %>%
  arrange(Sort) %>%
  select(-Sort)
```

```
## # A tibble: 18 × 9
##    STATION_CODE LATIN_NAME      Fish_no NOIMP SAMPL…¹ SAMPL…² SUBNO BULKID SMPNO
##    <chr>        <chr>             <int> <dbl>   <int>   <int> <dbl> <chr>  <int>
##  1 33F          Platichthys fl…       1     1      NA      NA     1 "16"       6
##  2 33F          Platichthys fl…       2     1      NA      NA     2 "16"       6
##  3 33F          Platichthys fl…       3     1      NA      NA     3 "16"       6
##  4 33F          Platichthys fl…       4     1      NA      NA     4 "16"       6
##  5 33F          Platichthys fl…       5     1      NA      NA     5 "16"       6
##  6 33F          Platichthys fl…      NA     5       1       1    16 ""         6
##  7 33F          Platichthys fl…       6     1      NA      NA     6 "17"       6
##  8 33F          Platichthys fl…       7     1      NA      NA     7 "17"       6
##  9 33F          Platichthys fl…       8     1      NA      NA     8 "17"       6
## 10 33F          Platichthys fl…       9     1      NA      NA     9 "17"       6
## 11 33F          Platichthys fl…      10     1      NA      NA    10 "17"       6
## 12 33F          Platichthys fl…      NA     5       2       2    17 ""         6
## 13 33F          Platichthys fl…      11     1      NA      NA    11 "18"       6
## 14 33F          Platichthys fl…      12     1      NA      NA    12 "18"       6
## 15 33F          Platichthys fl…      13     1      NA      NA    13 "18"       6
## 16 33F          Platichthys fl…      14     1      NA      NA    14 "18"       6
## 17 33F          Platichthys fl…      15     1      NA      NA    15 "18"       6
## 18 33F          Platichthys fl…      NA     5       3       3    18 ""         6
## # … with abbreviated variable names ¹​SAMPLE_NO_Muscle, ²​SAMPLE_NO_Liver
```


#### c1b. Liver part (EXCLUDING EROD, which is in liver but is numbered following specimen number)  

```r
data_conc_fishliver <- data_conc %>%
  filter(LATIN_NAME %in% fish_species & TISSUE_NAME %in% "Lever")

df_for_join <- data_04_fish %>% 
  select(STATION_CODE, LATIN_NAME, SAMPLE_NO_Liver, SMPNO, SUBNO) %>%
  filter(!is.na(SAMPLE_NO_Liver))

n1 <- nrow(data_conc_fishliver)
data_conc_fishliver <- data_conc_fishliver %>%
  left_join(df_for_join, by = c("STATION_CODE", "LATIN_NAME", "SAMPLE_NO2" = "SAMPLE_NO_Liver")) %>%
  mutate(MATRX = "LI") %>%
  filter(!PARAM %in% "EROD")   # exclude EROD
  
n2 <- nrow(data_conc_fishliver)
if (n1 == n2){
  cat("Join OK\n")
} else {
  cat("Join NOT OK\n")
}
```

```
## Join OK
```

```r
# Check SMPNO
not_ok <- is.na(data_conc_fishliver$SMPNO)
cat("Number of SMPNO not ok: ", sum(not_ok))  # should be zero
```

```
## Number of SMPNO not ok:  0
```

```r
# Check
# data_conc_fishmuscle[not_ok,]

if (sum(not_ok) != 0){
  data_conc_fishliver[not_ok,] %>%
    count(STATION_CODE, LATIN_NAME)
  
  df_liver %>% pull(STATION_CODE) %>% unique() %>% sort()
  # 19B, 28B, 43B2, 80B, 96B
  df_samples %>% pull(AQUAMONITOR_CODE) %>% unique() %>% sort()
  
  data_conc_fishliver[not_ok,] %>%
    count(STATION_CODE, LATIN_NAME, SAMPLE_NO2) %>% View("Liver samp")
  
}

# apply(is.na(data_conc_fishliver), 2, sum)
```

#### c1c. If some SMPNO lacking: Check data

```r
# data_04_fish %>% filter(STATION_CODE == "23B")
check <- FALSE

if (check){
  data_conc %>% 
    filter(STATION_CODE == "28B" & PARAM %in% "CB118") %>%
    arrange(SAMPLE_NO) %>%
    select(STATION_CODE, SAMPLE_NO)
  
  data_04_fish %>% 
    filter(STATION_CODE == "28B")
  
  data_04_fish_main %>% 
    filter(STATION_CODE == "28B")
  
}
```

#### c1d. Combine, add MUNIT and AMLNK   
- makes `data_conc_fish`  

```r
data_conc_fish <- rbind(data_conc_fishmuscle, data_conc_fishbiol, data_conc_fishliver)
# apply(is.na(data_conc_fish), 2, sum)

# Add MUNIT and AMLNK
data_conc_fish <- data_conc_fish %>%
  # add MUNIT
  safe_left_join(df_munit, by = "UNIT", 
                 na_matches = "never", check = "BCV") %>%
  # add AMLNK
  safe_left_join(df_amlnk, by = "PARAM", 
                 na_matches = "never", check = "BCV") %>%
  # Fix MUNIT for dry weight and fat weight  
  mutate(
    MUNIT = case_when(
      PARAM %in% c("DRYWT%", "FATWT%") ~ "%",
      TRUE ~ MUNIT)
    )

cat("'data_conc_fish' created with", nrow(data_conc_fish), "number of rows \n")
```

```
## 'data_conc_fish' created with 9116 number of rows
```

```r
# apply(is.na(data_conc_fish), 2, sum)

#
# Unit for con biol. effects - NOT NEEDED ANYMORE, FIXED IN df_munit!
#
# sel <- data_conc_fish$PARAM %in% c("BAP3OH", "PA1OH", "PYR1OH"); sum(sel)
# data_conc_fish$MUNIT[sel] <- "ng/g"
# sel <- data_conc_fish$PARAM %in% c("ALAD"); sum(sel)
# data_conc_fish$MUNIT[sel] <- "ng/min/mg protein"
# sel <- data_conc_fish$PARAM %in% c("EROD"); sum(sel)
# data_conc_fish$MUNIT[sel] <- "pmol/min/mg protein"

#
# Basis for EROD
#  PARAM EROD and CYP1A should be reported in MATRX LIS9 or LIMIC (WGBEC2013)
#
# Info from Anders in mail 22 NOv 2018: "Microsomalfraksjonen lages ved å ultrasentrifugere S9-fraksjonen" 
#
sel <- data_conc_fish$PARAM %in% c("EROD"); sum(sel)
```

```
## [1] 44
```

```r
data_conc_fish$MATRX[sel] <- "LIS9"
```

### c2. Set uncertainty = 0 to NA  
No longer needed, we set UNCRT directly

```r
# sel <- !is.na(data_conc_fish$UNCERTAINTY) & data_conc_fish$UNCERTAINTY == 0
# table(sel)
# data_conc_fish$UNCERTAINTY[sel] <- NA
```

### c3. data_10, fish part

```r
data_10_fish <- data.frame(
  RECID = "10", 
  CRUIS = "AA711", 
  STNNO = data_conc_fish$STATION_CODE,
  SMPNO = data_conc_fish$SMPNO,
  SUBNO = data_conc_fish$SUBNO,
  MATRX = data_conc_fish$MATRX,
  DEPHU = NA,
  DEPHL = NA,
  PARAM = data_conc_fish$PARAM,
  MUNIT = data_conc_fish$MUNIT,
  BASIS = case_when(data_conc_fish$PARAM %in% c("DRYWT%", "FATWT%") ~ "",
                    TRUE ~ "W"),
  AMLNK = data_conc_fish$AMLNK,
  VFLAG = NA,
  QFLAG = data_conc_fish$FLAG1,
  VALUE = data_conc_fish$VALUE_WW,
  PERCR = NA,
  SIGND = NA,
  UNCRT = data_conc_fish$UNCRT, 
  METCU = data_conc_fish$METCU,
  DETLI = NA,
  LMQNT = NA,      #  could use data_conc_fish$QUANTIFICATION_LIMIT but unclear whether 
  stringsAsFactors = FALSE
  )

xtabs(~PARAM +  is.na(MATRX), data_conc_fish)
```

```
##         is.na(MATRX)
## PARAM    FALSE
##   AG       231
##   ALAD      47
##   AS       231
##   BAP3OH    60
##   BD100    152
##   BD126    152
##   BD153    152
##   BD154    152
##   BD183    152
##   BD209    152
##   BDE28    152
##   BDE47    152
##   BDE99    152
##   CB101    223
##   CB118    223
##   CB138    223
##   CB153    223
##   CB180    223
##   CB28     223
##   CB52     223
##   CD       231
##   CO       230
##   CR       230
##   CU       231
##   DDEPP    105
##   DDTPP    105
##   DRYWT%   490
##   EROD      44
##   FATWT%   232
##   GOSOI     44
##   HBCDA    171
##   HBCDB    171
##   HBCDG    171
##   HCB      105
##   HCHA     105
##   HCHG     105
##   HG       258
##   MCCP     169
##   NI       230
##   PA1OH     60
##   PB       231
##   PFDA     138
##   PFHpA    138
##   PFHxA    138
##   PFNA     138
##   PFOA     138
##   PFOS     138
##   PFOSA    138
##   PFUnda   138
##   PYR1OH    60
##   SCCP     169
##   SN       231
##   TDEPP    105
##   ZN       231
```

### c4. Set quantification limit = 1 for EROD

```r
sel <- data_10_fish$PARAM == "EROD"; sum(sel)
```

```
## [1] 44
```

```r
data_10_fish$DETLI[sel] <- 1
```

### c5. Checks

```r
cat("Table 10 - missing values\n")
```

```
## Table 10 - missing values
```

```r
apply(is.na(data_10_fish), 2, sum)
```

```
## RECID CRUIS STNNO SMPNO SUBNO MATRX DEPHU DEPHL PARAM MUNIT BASIS AMLNK VFLAG 
##     0     0     0     0     0     0  9116  9116     0     0     0   315  9116 
## QFLAG VALUE PERCR SIGND UNCRT METCU DETLI LMQNT 
##  7221     0  9116  9116  2506     0  9072  9116
```

```r
# xtabs(~PARAM, data_10_fish)

cat("\n\nTable 10\n")
```

```
## 
## 
## Table 10
```

```r
data_10_fish %>% 
  count(STNNO, MATRX, SUBNO) %>% 
  xtabs(~MATRX + STNNO, .)
```

```
##       STNNO
## MATRX  02B 10B 13B 15B 19B 23B 24B 28B 30B 33F 36B 43B2 45B2 53B 71B 80B 96B
##   LI     8  15   8  15  15  15  14  14  12   3  15   15   14  15  10  15  15
##   LIS9   0   0   0   0   0  15   0   0  14   0   0    0    0  15   0   0   0
##   MU    15  15  15  15  15  17  15  15  15   3  15   15   15  15  15  15  15
##       STNNO
## MATRX  98B1
##   LI     15
##   LIS9    0
##   MU     15
```

```r
cat("\n\nOriginal data\n")
```

```
## 
## 
## Original data
```

```r
data_conc %>%
  count(STATION_CODE, TISSUE_NAME, SAMPLE_NO) %>%
  xtabs(~TISSUE_NAME + STATION_CODE, .)
```

```
##                    STATION_CODE
## TISSUE_NAME         02B 10A2 10B 11G 11X 131G 13B 15A 15B 15G 19B 19N 227G2 22A
##   Blod                0    0   0   0   0    0   0   0   0   0   0  15     0   0
##   Egg                 0    0   0   0   0    0   0   0   0   0   0  15     0   0
##   Galle               0    0   0   0   0    0   0   0  15   0   0   0     0   0
##   Lever               8    0  15   0   0    0   8   0  15   0  15   0     0   0
##   Liver - microsome   0    0   0   0   0    0   0   0   0   0   0   0     0   0
##   Muskel             15    0  15   0   0    0  15   0  15   0  15   0     0   0
##   Whole soft body     0    3   0   1   3    1   0   3   0   1   0   0     1   3
##   WO                  0    0   0   0   0    0   0   0   0   0   0   0     0   0
##                    STATION_CODE
## TISSUE_NAME         22G 23B 24B 26A2 28A2 28B 30A 30B 31A 33F 36A1 36B 36G 43B2
##   Blod                0  17   0    0    0   0   0  15   0   0    0   0   0    0
##   Egg                 0   0   0    0    0   0   0   0   0   0    0   0   0    0
##   Galle               0  16   0    0    0   0   0  14   0   0    0   0   0    0
##   Lever               0  15  14    0    0  14   0  12   0   3    0  15   0   15
##   Liver - microsome   0  15   0    0    0   0   0  14   0   0    0   0   0    0
##   Muskel              0  15  15    0    0  15   0  15   0   3    0  15   0   15
##   Whole soft body     1   0   0    3    3   0   3   0   3   0    3   0   1    0
##   WO                  0  15   0    0    0   0   0  14   0   0    0   0   0    0
##                    STATION_CODE
## TISSUE_NAME         45B2 53B 56A 57A 64A 65A 71A 71B 71G 76A2 76G 80B 91A2 96B
##   Blod                 0  15   0   0   0   0   0   0   0    0   0   0    0   0
##   Egg                  0   0   0   0   0   0   0   0   0    0   0   0    0   0
##   Galle                0  15   0   0   0   0   0   0   0    0   0   0    0   0
##   Lever               14  15   0   0   0   0   0  10   0    0   0  15    0  15
##   Liver - microsome    0  15   0   0   0   0   0   0   0    0   0   0    0   0
##   Muskel              15  15   0   0   0   0   0  15   0    0   0  15    0  15
##   Whole soft body      0   0   3   3   3   3   1   0   1    3   1   0    3   0
##   WO                   0  15   0   0   0   0   0   0   0    0   0   0    0   0
##                    STATION_CODE
## TISSUE_NAME         97A2 97A3 98A2 98B1 98G I023 I024 I131A I133 I241 I301 I304
##   Blod                 0    0    0    0   0    0    0     0    0    0    0    0
##   Egg                  0    0    0    0   0    0    0     0    0    0    0    0
##   Galle                0    0    0    0   0    0    0     0    0    0    0    0
##   Lever                0    0    0   15   0    0    0     0    0    0    0    0
##   Liver - microsome    0    0    0    0   0    0    0     0    0    0    0    0
##   Muskel               0    0    0   15   0    0    0     0    0    0    0    0
##   Whole soft body      3    3    3    0   1    3    3     3    3    3    3    3
##   WO                   0    0    0    0   0    0    0     0    0    0    0    0
##                    STATION_CODE
## TISSUE_NAME         I965 I969
##   Blod                 0    0
##   Egg                  0    0
##   Galle                0    0
##   Lever                0    0
##   Liver - microsome    0    0
##   Muskel               0    0
##   Whole soft body      3    3
##   WO                   0    0
```

```r
cat("\n\nMissing uncertainties  \n")
```

```
## 
## 
## Missing uncertainties
```

```r
data_10_fish %>%
  filter(is.na(UNCRT)) %>%
  xtabs(~PARAM, .)
```

```
## PARAM
##   ALAD BAP3OH  BD126  BD183 DRYWT%   EROD FATWT%  PA1OH   PFDA  PFHpA  PFHxA 
##     47     60    152    152    490     44    232     60    138    138    138 
##   PFNA   PFOA   PFOS  PFOSA PFUnda PYR1OH  TDEPP 
##    138    138    138    138    138     60    105
```

```r
cat("\n\nMissing units  \n")
```

```
## 
## 
## Missing units
```

```r
data_10_fish %>%
  filter(is.na(MUNIT)) %>%
  xtabs(~PARAM, .)
```

```
## < table of extent 0 >
```

```r
if (FALSE){
  
  cat("\n\nMissing values of VALUE:  \n")
  data_10_fish %>%
    filter(is.na(VALUE)) %>%
    View()
  
}
```
### c6. Check SMPNO

```r
xtabs(~is.na(SMPNO), data_10_fish)  
```

```
## is.na(SMPNO)
## FALSE 
##  9116
```

```r
xtabs(~is.na(SMPNO), data_conc_fish)  
```

```
## is.na(SMPNO)
## FALSE 
##  9116
```

```r
xtabs(~is.na(SMPNO), data_conc_fishmuscle)  
```

```
## is.na(SMPNO)
## FALSE 
##   516
```

```r
xtabs(~is.na(SMPNO), data_conc_fishliver)  
```

```
## is.na(SMPNO)
## FALSE 
##  8285
```

```r
xtabs(~STATION_CODE + is.na(SMPNO), data_conc_fish)  
```

```
##             is.na(SMPNO)
## STATION_CODE FALSE
##         02B    211
##         10B    405
##         13B    346
##         15B    450
##         19B    645
##         23B    830
##         24B    602
##         28B    492
##         30B    679
##         33F     81
##         36B    735
##         43B2   645
##         45B2   293
##         53B    825
##         71B    200
##         80B    636
##         96B    315
##         98B1   726
```

```r
# xtabs(~PARAM + is.na(SMPNO), data_conc_fish)  
```

### c7. Check last year (2018)

```r
# fn1 <- "../Milkys_2018/ICES/Test/NIVA2017CF.NO"
# dat1 <- read_ices_file(fn1) %>%  add_field_codes()
# dat1[["10"]] %>% 
#   count(STNNO, MATRX, SUBNO) %>% 
#   xtabs(~MATRX + STNNO, .)
```

### d. Blue mussel  
Add SMPNO, MUNIT, AMLNK

```r
data_conc_bluemussel <- data_conc %>%
  filter(LATIN_NAME %in% 'Mytilus edulis') %>%
  left_join(df_smpno %>% select(LATIN_NAME, SMPNO)) %>%
  left_join(df_munit) %>%
  left_join(df_amlnk) %>%
  # Fix MUNIT for dry weight and fat weight  
  mutate(
    MUNIT = case_when(
      PARAM %in% c("DRYWT%", "FATWT%") ~ "%",
      TRUE ~ MUNIT)
    )
```

```
## Joining, by = "LATIN_NAME"
## Joining, by = "UNIT"
## Joining, by = "PARAM"
```

### e1. Set uncertainty = 0 to NA  
No longer needed

```r
# sel <- !is.na(data_conc_bluemussel$UNCERTAINTY) & data_conc_bluemussel$UNCERTAINTY == 0
# table(sel)
# data_conc_bluemussel$UNCERTAINTY[sel] <- NA
```

### e2. data_10, blue mussel part
For blue mussel, we set SUBNO = SAMPLE_NO2

```r
data_10_bluemussel <- data.frame(
  RECID = "10", 
  CRUIS = "AA711", 
  STNNO = data_conc_bluemussel$STATION_CODE,
  SMPNO = data_conc_bluemussel$SMPNO,
  SUBNO = data_conc_bluemussel$SAMPLE_NO2,
  MATRX = "SB",
  DEPHU = NA,
  DEPHL = NA,
  PARAM = data_conc_bluemussel$PARAM,
  MUNIT = data_conc_bluemussel$MUNIT,
  BASIS = case_when(data_conc_bluemussel$PARAM %in% "DRYWT%" ~ "",
                    TRUE ~ "W"),
  AMLNK = data_conc_bluemussel$AMLNK,
  VFLAG = NA,
  QFLAG = data_conc_bluemussel$FLAG1,
  VALUE = data_conc_bluemussel$VALUE_WW,
  PERCR = NA,
  SIGND = NA,
  UNCRT = data_conc_bluemussel$UNCRT, 
  METCU = data_conc_bluemussel$METCU,
  DETLI = NA,
  LMQNT = NA,   # alternatively data_conc_bluemussel$QUANTIFICATION_LIMIT,
  stringsAsFactors = FALSE
  )

# xtabs(~PARAM + BASIS, data_10_bluemussel)
```


### f. Dry weight and fat
- Add dry weight and fat to the data  
- Dry weight and fat, add SUBNO
- Put on "table 10 form"
NOT NEEDED in 2019



### g. Length Ver. 2: Makes 'data_10_length2'  
Adds length and weight to the data using df_ind and data_04_fish

```r
df_fishlength <- data_04_fish %>%
  filter(!is.na(Fish_no)) %>%
  left_join(df_ind, by = c("STATION_CODE", "Fish_no")) %>%
  select(STATION_CODE, SMPNO, SUBNO, Length, Weight) %>%
  gather("Param", "VALUE", Length, Weight) %>%
  mutate(PARAM = case_when(Param == "Length" ~ "LNMEA",
                           Param == "Weight" ~ "WTMEA")) %>%
  mutate(MUNIT = case_when(Param == "Length" ~ "cm",
                           Param == "Weight" ~ "g")) %>%
  left_join(df_amlnk,  by = "PARAM")                 # Adds AMLNK


data_10_length2 <- data.frame(
  RECID = "10", 
  CRUIS = "AA711", 
  STNNO = df_fishlength$STATION_CODE,
  SMPNO = df_fishlength$SMPNO,
  SUBNO = df_fishlength$SUBNO,
  MATRX = "WO",
  DEPHU = NA,
  DEPHL = NA,
  PARAM = df_fishlength$PARAM,
  MUNIT = df_fishlength$MUNIT,
  BASIS = "",
  AMLNK = df_fishlength$AMLNK,
  VFLAG = NA,
  QFLAG = NA,
  VALUE = df_fishlength$VALUE,
  PERCR = NA,
  SIGND = NA,
  UNCRT = NA, 
  METCU = "",
  DETLI = NA,
  LMQNT = NA,
  stringsAsFactors = FALSE
  )
```


### h. Snail data: VDSI, TBSN+ and DRYWT%

```r
# data_conc %>% filter(PARAM %in% "TBSN+") %>% data.frame()
df_snail <- data_conc %>% 
  filter(LATIN_NAME %in% c("Nucella lapillus", "Littorina littorea")) %>% # xtabs(~PARAM, .)
  left_join(df_smpno) %>%
  left_join(df_amlnk) %>%
  mutate(SAMPLE_NO2 = 1) %>%     # Note - we just assume a single pooled sample from each snail station
  data.frame()
```

```
## Joining, by = "LATIN_NAME"
## Joining, by = "PARAM"
```

```r
# df_snail %>% head(6)

data_10_vdsi <- data.frame(
  RECID = "10", 
  CRUIS = "AA711", 
  STNNO = df_snail$STATION_CODE,
  SMPNO = df_snail$SMPNO,
  SUBNO = df_snail$SAMPLE_NO2,
  MATRX = "SB",
  DEPHU = NA,
  DEPHL = NA,
  PARAM = df_snail$PARAM,
  UNIT = df_snail$UNIT,
  BASIS = case_when(df_snail$PARAM %in% c("DRYWT%", "VDSI", "INTF%") ~ "",
                    TRUE ~ "W"),
  AMLNK = df_snail$AMLNK,
  VFLAG = NA,
  QFLAG = df_snail$FLAG1,
  VALUE = df_snail$VALUE_WW,
  PERCR = NA,
  SIGND = NA,
  UNCRT = df_snail$UNCRT, 
  METCU = df_snail$METCU,
  DETLI = NA,
  LMQNT = NA,    # alternatively df_snail$QUANTIFICATION_LIMIT,
  stringsAsFactors = FALSE
  )

data_10_vdsi <- data_10_vdsi %>%
  left_join(df_munit) %>%
  mutate(
    MUNIT = case_when(
      PARAM %in% c("DRYWT%", "FATWT%") ~ "%",
      PARAM %in% "VDSI" ~ "IDX",
      TRUE ~ MUNIT)
  ) %>% 
  select(-UNIT)
```

```
## Joining, by = "UNIT"
```

```r
cat("---------------------------------------------------------------\n")
```

```
## ---------------------------------------------------------------
```

```r
cat("Parameters: \n---------------------------------------------------------------\n")
```

```
## Parameters: 
## ---------------------------------------------------------------
```

```r
xtabs(~PARAM, data_10_vdsi)
```

```
## PARAM
##   ACNE  ACNLE     AG    ANT     AS    BAA    BAP   BBJF  BGHIP    BKF     CD 
##      1      1      1      1      1      1      1      1      1      1      1 
##     CO     CR     CU  DBA3A  DDEPP  DDTPP DRYWT% FATWT%    FLE    FLU    HCB 
##      1      1      1      1      1      1      9      1      1      1      1 
##   HCHA   HCHG     HG   ICDP    NAP     NI     PA     PB    PYR     SN  TBSN+ 
##      1      1      1      1      1      1      1      1      1      1      9 
##  TDEPP   VDSI     ZN 
##      1      8      1
```

```r
cat("\n\n---------------------------------------------------------------\n")
```

```
## 
## 
## ---------------------------------------------------------------
```

```r
cat("Units: \n---------------------------------------------------------------\n")
```

```
## Units: 
## ---------------------------------------------------------------
```

```r
xtabs(~addNA(MUNIT), data_10_vdsi)
```

```
## addNA(MUNIT)
##     %   IDX mg/kg ug/kg  <NA> 
##    10     8    11    30     0
```

### i. Eider duck    
  
- SUBNO 16-30 should have EH - Egg homogenate of yolk and albumin    
- SUBNO 1-15 should have BL - Blood  

```r
# Use left join to data_04_eider to add SUBNO 
# df_eider <- 
df1 <- data_conc %>% 
  filter(LATIN_NAME %in% c("Somateria mollissima")) %>% # View()
  mutate(MATRX = case_when(
    grepl("Egg", TISSUE_NAME) ~ "EH",
    grepl("Blod", TISSUE_NAME) ~ "BL")
  ) %>%
  as.data.frame()
nrow(df1)
```

```
## [1] 1253
```

```r
df2 <- df1 %>%
  left_join(df_smpno, by = "LATIN_NAME")    # adds SMPNO and SMLNK
df3 <- df2 %>%
  left_join(df_amlnk, by = "PARAM") %>%     # adds AMLNK
  left_join(df_munit, by = "UNIT")          # adds MUNIT
# Add SUBNO
data_conc_eider <- df3 %>%
  left_join(data_04_eider %>% select(LATIN_NAME, STATION_CODE, TISSUE_NAME, SAMPLE_NO2, SUBNO), 
            by = c("LATIN_NAME", "STATION_CODE", "TISSUE_NAME", "SAMPLE_NO2")
            )
# data_conc_eider %>%
#   select(LATIN_NAME, STATION_CODE, TISSUE_NAME, SAMPLE_NO2, SUBNO, PARAM, VALUE_WW) %>%
#   arrange(LATIN_NAME, STATION_CODE, PARAM, SUBNO) %>%
#   view()

if (nrow(df1) == nrow(data_conc_eider)){
  cat("Joins ok \n\n")
} else {
  cat("Joins NOT OK! \n\n")
}
```

```
## Joins ok
```

```r
# View(data_conc_eider)

data_10_eider <- data.frame(
  RECID = "10", 
  CRUIS = "AA711", 
  STNNO = data_conc_eider$STATION_CODE,
  SMPNO = data_conc_eider$SMPNO,
  SUBNO = data_conc_eider$SUBNO,
  MATRX = data_conc_eider$MATRX,
  DEPHU = NA,
  DEPHL = NA,
  PARAM = data_conc_eider$PARAM,
  MUNIT = data_conc_eider$MUNIT,
  BASIS = case_when(data_conc_eider$PARAM %in% "DRYWT%" ~ "",
                    TRUE ~ "W"),
  AMLNK = data_conc_eider$AMLNK,
  VFLAG = NA,
  QFLAG = data_conc_eider$FLAG1,
  VALUE = data_conc_eider$VALUE_WW,
  PERCR = NA,
  SIGND = NA,
  UNCRT = data_conc_eider$UNCRT, 
  METCU = data_conc_eider$METCU,
  DETLI = NA,
  LMQNT = NA,    # alternatively data_conc_eider$QUANTIFICATION_LIMIT
  stringsAsFactors = FALSE
  )

# xtabs(~PARAM + BASIS, data_10_bluemussel)
```


### j. Combine fish, blue mussel and fat weight/dryweight

```r
# Length version 1
# data_10 <- rbind(data_10_fish, data_10_bluemussel, data_10_fatdry, data_10_length, data_10_vdsi)

# Length version 2
# note: data_10_fatdry deleted
data_10 <- rbind(data_10_fish, data_10_bluemussel, data_10_length2, data_10_vdsi, data_10_eider)
  

# Round all values (5 decimals)
data_10$VALUE <- round(data_10$VALUE, 5)

# data_10 %>% filter(PARAM == "TBSN+") %>% View()
# data_10 %>% filter(PARAM == "VDSI") %>% View()

xtabs(~is.na(UNCRT), data_10)
```

```
## is.na(UNCRT)
## FALSE  TRUE 
##  9987  3944
```

### k1 - Fix some stuff

```r
# Delete records without value
sel <- is.na(data_10$VALUE); sum(sel)  
```

```
## [1] 0
```

```r
xtabs(~STNNO + PARAM, data_10[sel,])
```

```
## < table of extent 0 x 0 >
```

```r
data_10 <- data_10[!sel,]

# Set SMPNO to be numeric
data_10$SMPNO <- as.numeric(data_10$SMPNO)

# Change PFOSA (deprecated) to "PFOSD" (CAS 754-91-6)
# Description of PFOSA in ICES vocabulary:
#   Code: PFOSA
#   Description: 	Perfluorooctylsulfonate acid amide (deprecated)
#   Long Description: 	This is an old request which had no CAS number at the time. However, still 
#     cannot find CAS for perfluorooctylsulfonate acid amide in 2016. Is not a synonym of the 
#     commonly known PFOSA with CAS 754-91-6. Have to deprecate to clarify. See PFOSD.
# Description of PFOSD in ICES vocabulary: (seems that PFOSA has been misspelled PHOSA)
#   Code: 	PFOSD
#   Description: 	Perfluorooctanesulfonamide - branched + linear forms
#   Long Description: 	Perfluorooctanesulfonamide - Clarification that this is total, i.e., both 
#     forms, was added in 2018. This is the commonly known "PHOSA". ICES PHOSA has no CAS reference
#     and has been deprecated.

sel <- data_10$PARAM %in% "PFOSA"; sum(sel)
```

```
## [1] 192
```

```r
data_10$PARAM[sel] <- "PFOSD"
cat("PARAM changed from PFOSA to PFOSD for", sum(sel), "records\n")
```

```
## PARAM changed from PFOSA to PFOSD for 192 records
```

```r
# Check that all records have SMPNO
sum(is.na((data_10$SMPNO)))  # should be zero
```

```
## [1] 0
```

```r
# Set BASIS to "" for length, wet weigth etc. + VDSI
# xtabs(~PARAM, data_10)
sel <- data_10$PARAM %in% c("FATWT%", "DRYWT%", "LNMEA", "WTMEA", "VDSI"); sum(sel)
```

```
## [1] 1583
```

```r
data_10$BASIS[sel] <- ""
```

### k2. Check why we lack SMPNO (if needed)

```r
if (sum(is.na(data_10$SMPNO)) > 0){
  # Which stations lack SMPNO?
  xtabs(~STNNO + is.na(SMPNO), data_10)
  
  # Which part of the data lack SMPNO? 
  xtabs(~is.na(SMPNO), data_10_fish)  
  xtabs(~is.na(SMPNO), data_10_bluemussel)  
  xtabs(~is.na(SMPNO), data_10_length2)  
  xtabs(~is.na(SMPNO), data_10_vdsi)  
} else {
  cat("SMPNO is OK\n")
}
```

```
## SMPNO is OK
```

### k3 - METCU may not be filled in when UNCRT is blank

```r
cat("Before: \n")
```

```
## Before:
```

```r
xtabs(~is.na(UNCRT) + addNA(METCU), data_10)
```

```
##             addNA(METCU)
## is.na(UNCRT)         % <NA>
##        FALSE    0 9987    0
##        TRUE   662 3282    0
```

```r
# Set 
sel <- is.na(data_10$UNCRT)
data_10$METCU[sel] <- NA

cat("\nAfter: \n")
```

```
## 
## After:
```

```r
xtabs(~is.na(UNCRT) + addNA(METCU), data_10)
```

```
##             addNA(METCU)
## is.na(UNCRT)    % <NA>
##        FALSE 9987    0
##        TRUE     0 3944
```

### k4 - MATRX BL required for PARAM ALAD

```r
sel <- data_10$PARAM %in% "ALAD"
# table(addNA(data_10$MATRX[sel]))

data_10$MATRX[sel] <- "BL"
table(addNA(data_10$MATRX[sel]))
```

```
## 
##   BL <NA> 
##   47    0
```

### k5 - GOSOI   
    - Effects parameters LISOI and GOSOI should report MUNIT index in the valid range of 0-50
    - Effects parameter GOSOI should be reported in MATRX GO

```r
sel <- data_10$PARAM %in% "GOSOI"

# Isn't this OK?
table(addNA(data_10$MUNIT[sel]))
```

```
## 
##  idx <NA> 
##   44    0
```

```r
summary(data_10$VALUE[sel])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.04167 0.43389 0.75872 1.02157 1.26210 4.40075
```

```r
# Fix MUNIT
table(addNA(data_10$MATRX[sel]))
```

```
## 
##   MU <NA> 
##   44    0
```

```r
data_10$MATRX[sel] <- "GO"
```

### k6 - PARAM Bile metabolites, fix    
    - PARAM Bile metabolites must be reported in matrix BI (bile) (WGBEC2013)  
    - Recommended MUNIT for Bile metabolites is nanogram/gram (WGBEC2013) (BUT written "ng/g")  
    - PARAM bile metabolites require reporting of DETLI (WGBEC2013)  

```r
sel <- data_10$PARAM %in% c("PA1O", "PYR1O", "BAP3O", "PA1OH", "PYR1OH", "BAP3OH")

# Fix MATRX
table(addNA(data_10$MATRX[sel]))
```

```
## 
##   MU <NA> 
##  180    0
```

```r
data_10$MATRX[sel] <- "BI"

# Fix MUNIT
table(addNA(data_10$MUNIT[sel]))
```

```
## 
## ug/kg  <NA> 
##   180     0
```

```r
data_10$MUNIT[sel] <- "ng/g"

# DETLI is (SO FAR) set to zero
df_statistics <- data_10[sel,] %>%
  group_by(PARAM) %>%
  summarise(Min = min(VALUE), Median = median(VALUE), Max = max(VALUE),
            .groups = "drop")

# Add DETLI
data_10 <- data_10 %>%
  safe_left_join(df_statistics %>% select(PARAM, Min), 
                 na_matches = "never",
                 check = "V") %>%
  mutate(DETLI = case_when(!is.na(Min) ~ Min,
                           is.na(Min) ~ DETLI)
         ) %>%
  select(-Min)
```

```
## Joining, by = "PARAM"
```

```r
# Change QFLAG from < to D
# But no QFLAGS for these, so don't have to worry about that
```


### k7. UNCRT for DRYWT% FATWT%  LNMEA  WTMEA    
Set to 10% (SD = 5%)

```r
# Det.limit - never there
table(is.na(data_conc$DETECTION_LIMIT))
```

```
## 
##  TRUE 
## 13269
```

```r
# Lacking UNCRT:
#   DRYWT% FATWT%  LNMEA   PFBS  WTMEA
sel <- is.na(data_10$UNCRT)
xtabs(~PARAM, data_10[sel,])
```

```
## PARAM
##   ALAD BAP3OH  BD126  BD183  DBA3A DRYWT%   EROD FATWT%    HCB  LNMEA  PA1OH 
##     47     60    183    183     26    581     44    309     30    331     60 
##   PFBS   PFDA  PFHpA  PFHxA   PFNA   PFOA   PFOS  PFOSD PFUnda PYR1OH  TDEPP 
##      6    192    192    192    192    192    192    192    192     60    149 
##   VDSI  WTMEA 
##      8    331
```

```r
xtabs(~PARAM, data_10[sel,]) %>% names() %>% dput()
```

```
## c("ALAD", "BAP3OH", "BD126", "BD183", "DBA3A", "DRYWT%", "EROD", 
## "FATWT%", "HCB", "LNMEA", "PA1OH", "PFBS", "PFDA", "PFHpA", "PFHxA", 
## "PFNA", "PFOA", "PFOS", "PFOSD", "PFUnda", "PYR1OH", "TDEPP", 
## "VDSI", "WTMEA")
```

```r
# Set UNCRT
sel <- data_10$PARAM %in% c("DRYWT%", "FATWT%", "LNMEA", "WTMEA"); sum(sel)
```

```
## [1] 1575
```

```r
data_10$UNCRT[sel] <- 10
data_10$METCU[sel] <- "%"

# View(data_10[sel,])
```


### k8. DETLI for DRYWT% FATWT%  LNMEA  WTMEA    
Set to 10% (SD = 5%)

```r
# Det.limit - never there
table(is.na(data_conc$DETECTION_LIMIT))
```

```
## 
##  TRUE 
## 13269
```

### k9a. For less-thans (QFLAG = <), set UNCRT = NA

```r
cat("Before: \n")
```

```
## Before:
```

```r
xtabs(~addNA(QFLAG) + addNA(METCU), data_10)
```

```
##             addNA(METCU)
## addNA(QFLAG)    % <NA>
##         <    2175 1446
##         <NA> 9364  946
```

```r
sel_lt <- data_10$QFLAG %in% "<"; sum(sel)
```

```
## [1] 1575
```

```r
data_10$UNCRT[sel_lt] <- NA
data_10$METCU[sel_lt] <- NA

cat("\nAfter: \n")
```

```
## 
## After:
```

```r
xtabs(~addNA(QFLAG) + addNA(METCU), data_10)
```

```
##             addNA(METCU)
## addNA(QFLAG)    % <NA>
##         <       0 3621
##         <NA> 9364  946
```


### k9b. When UNCRT present, set METCU  

```r
# This is all old

# sel_uncrt <- !is.na(data_10$UNCRT)
# 
# 
# cat("UNCRT given vs less thans \n") 
# # (could also use sel_lt from previous :)
# table(sel_uncrt, sel_lt)
# 
# cat("\nCheck METCU \n") 
# table(addNA(data_10$METCU), sel_uncrt)
# 
# data_10$METCU[sel_uncrt] <- "SD"
# data_10$METCU[!sel_uncrt] <- NA
# 
# cat("\nCheck METCU again \n") 
# table(addNA(data_10$METCU), sel_uncrt)
```

### k9c. Clear DETLI for PAH metabolites   
- We use QFLAG instead  

```r
sel <- data_10$PARAM %in% c("PA1OH", "PYR1OH", "BAP3OH")  
sum(sel)
```

```
## [1] 180
```

```r
data_10$DETLI[sel] <- NA
```

### k9c. Change QFLAG and set LMQNT  
- Change QFLAG from < to Q   

```r
sel_uncrt <- !is.na(data_10$QFLAG)
message(sum(sel_uncrt), " values are under LOQ - we set QFLAG and LMQNT for these")  
```

```
## 3621 values are under LOQ - we set QFLAG and LMQNT for these
```

```r
data_10$QFLAG[sel_uncrt] <- "Q"
data_10$LMQNT[sel_uncrt] <- data_10$VALUE[sel_uncrt]
```


### k10a. Detection limits in tab 10 and original data   

```r
xtabs(~is.na(DETLI), data_10)
```

```
## is.na(DETLI)
## FALSE  TRUE 
##    44 13887
```

```r
# xtabs(~is.na(DETECTION_LIMIT), data_conc)
```
### k10b. Detection limits, metals in liver

```r
#
# Check metals in liver
#
# For getting metal parameters:
# pars <- data_10$PARAM %>% unique()
# pars[nchar(pars) == 2] %>% dput()

pars_metals <- c("HG", "SN", "CD", "AG", "CU", "CR", "AS", "PB", "CO", "NI", "ZN")

matrix <- "LI"
df <- data_10 %>%
  filter(PARAM %in% pars_metals & QFLAG %in% "Q" & MATRX %in% matrix) %>%
  ungroup() %>%
  group_by(PARAM) %>%
  summarise(
    loq_median = median(VALUE),
    loq_min =  min(VALUE),
    loq_max = max(VALUE)
    ) %>%
  arrange(loq_min)
df
```

```
## # A tibble: 7 × 4
##   PARAM loq_median loq_min loq_max
##   <chr>      <dbl>   <dbl>   <dbl>
## 1 CD         0.001   0.001   0.001
## 2 CO         0.001   0.001   0.001
## 3 PB         0.005   0.005   0.005
## 4 CR         0.01    0.01    0.01 
## 5 NI         0.01    0.01    0.01 
## 6 SN         0.01    0.01    0.01 
## 7 AG         0.05    0.05    0.05
```

```r
#
# Set DETLI for metals in liver, based on table shown below
#
# note that "CU" "AS" "ZN" had no data under LOQ, but were se from looking at graphs...

matrix <- "LI"
sel <- with(data_10, MATRX %in% matrix & PARAM %in% c("CD","CO"))
data_10$DETLI[sel] <- 0.001
sel <- with(data_10, MATRX %in% matrix & PARAM %in% "PB")
data_10$DETLI[sel] <- 0.005
sel <- with(data_10, MATRX %in% matrix & PARAM %in% c("CR","NI","SN", "CU"))
data_10$DETLI[sel] <- 0.010
sel <- with(data_10, MATRX %in% matrix & PARAM %in% c("AG", "ZN", "AS"))
data_10$DETLI[sel] <- 0.050

# pars_metals[!pars_metals %in% df$PARAM]
# "HG" "CU" "AS" "ZN"

# For use in the next chink (blue mussel)
df_loq_metals <- bind_rows(
  df %>% select(PARAM, loq_min),
  data.frame(
    PARAM = c("CU","AS","ZN"),
    loq_min = c(0.010, 0.050, 0.050)
  ))
```

### k10c. Detection limits, metals in blue mussel

```r
matrix <- "SB"
df1 <- data_10 %>%
  filter(PARAM %in% pars_metals & QFLAG %in% "Q" & MATRX %in% matrix) %>%
  ungroup() %>%
  group_by(PARAM) %>%
  summarise(
    loq_median = median(VALUE),
    loq_min =  min(VALUE),
    loq_max = max(VALUE)
    ) 
df1
```

```
## # A tibble: 1 × 4
##   PARAM loq_median loq_min loq_max
##   <chr>      <dbl>   <dbl>   <dbl>
## 1 AG          0.05    0.05    0.05
```

```r
df2 <- data_10 %>%
  filter(PARAM %in% pars_metals & is.na(QFLAG) & MATRX %in% matrix) %>%
  ungroup() %>%
  group_by(PARAM) %>%
  summarise(
    val_median = median(VALUE),
    val_min =  min(VALUE)
    ) %>%
  arrange(val_min)
df2 %>%
  left_join(df_loq_metals)
```

```
## Joining, by = "PARAM"
```

```
## # A tibble: 11 × 4
##    PARAM val_median val_min loq_min
##    <chr>      <dbl>   <dbl>   <dbl>
##  1 HG         0.014   0.005  NA    
##  2 SN         0.04    0.014   0.01 
##  3 CO         0.071   0.037   0.001
##  4 CR         0.2     0.069   0.01 
##  5 CD         0.16    0.07    0.001
##  6 PB         0.23    0.071   0.005
##  7 NI         0.2     0.089   0.01 
##  8 AG         0.2     0.2     0.05 
##  9 CU         1.35    0.51    0.01 
## 10 AS         2.15    1       0.05 
## 11 ZN        17.5    11       0.05
```

```r
#
# Conclusion: we use the same limits as we used for liver 
#
matrix <- "SB"
sel <- with(data_10, MATRX %in% matrix & PARAM %in% c("CD","CO"))
data_10$DETLI[sel] <- 0.001
sel <- with(data_10, MATRX %in% matrix & PARAM %in% "PB")
data_10$DETLI[sel] <- 0.005
sel <- with(data_10, MATRX %in% matrix & PARAM %in% c("CR","NI","SN", "CU"))
data_10$DETLI[sel] <- 0.010
sel <- with(data_10, MATRX %in% matrix & PARAM %in% c("AG", "ZN", "AS"))
data_10$DETLI[sel] <- 0.050
```

### k10d. Detection limits, mercury in muscle

```r
matrix <- "MU"
df1 <- data_10 %>%
  filter(PARAM %in% pars_metals & QFLAG %in% "Q" & MATRX %in% matrix) %>%
  ungroup() %>%
  group_by(PARAM) %>%
  summarise(
    loq_median = median(VALUE),
    loq_min =  min(VALUE),
    loq_max = max(VALUE)
    )  %>%
  arrange(loq_min)
```

```
## Warning in min(VALUE): no non-missing arguments to min; returning Inf
```

```
## Warning in max(VALUE): no non-missing arguments to max; returning -Inf
```

```r
df1
```

```
## # A tibble: 0 × 4
## # … with 4 variables: PARAM <chr>, loq_median <dbl>, loq_min <dbl>,
## #   loq_max <dbl>
```

```r
df2 <- data_10 %>%
  filter(PARAM %in% pars_metals & is.na(QFLAG) & MATRX %in% matrix) %>%
  ungroup() %>%
  group_by(PARAM) %>%
  summarise(
    val_median = median(VALUE),
    val_min =  min(VALUE)
    ) %>%
  arrange(val_min)
df2
```

```
## # A tibble: 1 × 3
##   PARAM val_median val_min
##   <chr>      <dbl>   <dbl>
## 1 HG         0.088   0.002
```

```r
matrix <- "MU"
sel <- with(data_10, QFLAG %in% "Q" & MATRX %in% matrix & PARAM %in% c("CD","CO"))
```

### k11. Set MUNIT for FAT% and test for remaining missing units    

```r
sel <- with(data_10, PARAM %in% c("DRYWT%","FATWT%") & !MUNIT %in% "%")
data_10$MUNIT[sel] <- "%"

cat("MUNIT set for", sum(sel), "DRYWT% records.")
```

```
## MUNIT set for 23 DRYWT% records.
```

```r
# Missing units
cat("\n\nMissing units: \n----------------------------------------------------------\n")
```

```
## 
## 
## Missing units: 
## ----------------------------------------------------------
```

```r
check <- data_10 %>%
  filter(is.na(MUNIT)) 
cat(nrow(check), "records lack MUNIT \n")
```

```
## 0 records lack MUNIT
```

```r
if (nrow(check) > 0){
  cat("Missing MUNIT - Parameters: \n")
  xtabs(~PARAM, check)
  stop("All records must have MUNIT! (Table 10 k11)")
}
```

### k12. Check 'VALUE <= DETLI but QFLAG is not specified'    
(As it is called in the DATSU check)  

```r
check <- data_10 %>%
  filter(VALUE <= DETLI) 
# nrow(check)

# All are PAH metabolites  
xtabs(~PARAM + is.na(QFLAG), check)
```

```
##       is.na(QFLAG)
## PARAM  FALSE TRUE
##   AG      77    1
##   CD       1    0
##   CO       1    0
##   CR       2    0
##   EROD     0    2
##   NI       7    0
##   PB      53    5
##   SN       6    0
```

```r
# Change DETLI to 90% of VALUE for these cases  
data_10 <- data_10 %>%
  mutate(DETLI = case_when(
    VALUE <= DETLI & is.na(QFLAG) ~ 0.9*VALUE,
    TRUE ~ DETLI)
  )
```

### k13. Check BASIS for GOSOI measurements     
DATSU: 'BASIS is for contaminant and chemical analysis only'  

```r
xtabs(~addNA(BASIS), data_10)
```

```
## addNA(BASIS)
##           W  <NA> 
##  1583 12348     0
```

```r
# Set BASIS to 'not available' for these cases  
data_10 <- data_10 %>%
  mutate(BASIS = case_when(
    PARAM %in% "GOSOI" ~ as.character(NA),
    TRUE ~ BASIS)
  )
```
### k14. Check type of error (METCU)      
DATSU: 'BASIS is for contaminant and chemical analysis only'  

```r
xtabs(~addNA(METCU), data_10)
```

```
## addNA(METCU)
##    % <NA> 
## 9364 4567
```

```r
# Set BASIS to 'not available' for these cases  
data_10 <- data_10 %>%
  mutate(BASIS = case_when(
    PARAM %in% "GOSOI" ~ as.character(NA),
    TRUE ~ BASIS)
  )
```


## Table 21 - analytic methods

### a. Check PARAM relative to table 10

```r
# data_21 <- data_21_2016  # restore from "backup"
data_21_back <- data_21  # make backup

# We use 2018 data instead
data_21 <- data_2018[["21"]] %>%
  mutate(AMLNK = as.numeric(AMLNK))

column_order_21 <- names(data_21)
```


### b1. Check if some parameters in data_10 lack AMLNK  

```r
check1 <- data_10 %>%
  count(PARAM, AMLNK) %>%
  filter(is.na(AMLNK))

check1
```

```
##   PARAM AMLNK   n
## 1   HCB    NA 176
## 2  HCHA    NA 146
## 3  HCHG    NA 146
## 4 TBSN+    NA  36
```

### b2. Check if some parameters in data_10 are lacking a data_21 entry  

```r
check2 <- data_10 %>%
  count(PARAM, AMLNK) %>%
  left_join(data_21 %>% select(AMLNK, ALABO), by = "AMLNK") %>% 
  filter(is.na(ALABO))

check2
```

```
##   PARAM AMLNK   n ALABO
## 1   HCB    NA 176  <NA>
## 2  HCHA    NA 146  <NA>
## 3  HCHG    NA 146  <NA>
## 4 TBSN+    NA  36  <NA>
```

### b3a. Modify AMLNK in data_10   
So the AMLNK for TBSN+ in data_10 points to the TBT line in data_21  

```r
if ("TBSN+" %in% check1$PARAM) {
  
  # First, find AMLNK for TBTIN (note that we use 'old' data, as these have been deleted from 'data_10'....)
  amlnk <- data_2018[["10"]] %>% 
    filter(PARAM == "TBTIN") %>% 
    pull(AMLNK) %>% unique()
  amlnk
  
  # Then, use this in the TBSN+ rows of table 10
  data_10 <- data_10 %>%
    mutate(AMLNK = case_when(
      PARAM %in% "TBSN+" ~ as.numeric(amlnk),  # use the AMLNK value we found...
      TRUE ~ AMLNK)                            # ...else, use existing AMLNK                         
    )
  
  cat("Modified AMLNK in data_10 so AMLNK for TBSN+ in data_10 points to the TBT line in data_21 ")
  
}
```

```
## Modified AMLNK in data_10 so AMLNK for TBSN+ in data_10 points to the TBT line in data_21
```
### b3b. Add extra parameters  
Must both add a new AMLNK to table 10 and add a line with this AMLNK to table 21

```r
#
# Functions
#

table_10_amlnk_get <- function(existing_data_10, param){
  max(existing_data_10$AMLNK, na.rm = TRUE) + 1
}

table_10_amlnk_set <- function(existing_data_10, param, amlnk){
  sel <- existing_data_10$PARAM %in% param
  if (sum(!is.na(existing_data_10$AMLNK[sel])) > 0){
    stop("AMLNK is given for this PARAM in table 10 (Table 21 b3b)")
  }
  existing_data_10$AMLNK[sel] <- amlnk
  cat("AMLNK set to", amlnk, "for", sum(sel), param, "records \n")
  existing_data_10
}

table_21_add <- function(existing_data_21, amlnk){
  if (amlnk %in% existing_data_21$AMLNK){
    stop("This AMLNK already exists in table 21 (Table 21 b3b)")
  }
  cat(length(amlnk), "row with AMLNK", amlnk, "added to table 21 \n")
  bind_rows(
    existing_data_21,
    tibble(
      RECID = "21",
      AMLNK = amlnk,
      ALABO = "NIVA",
      METFP = "NON")
  )
} 

#
# DO IT
#

amlnk <- data_10 %>% table_10_amlnk_get("HCHA")
data_10 <- data_10 %>% table_10_amlnk_set("HCHA", amlnk)
```

```
## AMLNK set to 74 for 146 HCHA records
```

```r
data_21 <- data_21 %>% table_21_add(amlnk)
```

```
## 1 row with AMLNK 74 added to table 21
```

```r
cat("\n")
```

```r
amlnk <- data_10 %>% table_10_amlnk_get("HCHG")
data_10 <- data_10 %>% table_10_amlnk_set("HCHG", amlnk)
```

```
## AMLNK set to 75 for 146 HCHG records
```

```r
data_21 <- data_21 %>% table_21_add(amlnk)
```

```
## 1 row with AMLNK 75 added to table 21
```

```r
cat("\n")
```

```r
amlnk <- data_10 %>% table_10_amlnk_get("HCB")
data_10 <- data_10 %>% table_10_amlnk_set("HCB", amlnk)
```

```
## AMLNK set to 76 for 176 HCB records
```

```r
data_21 <- data_21 %>% table_21_add(amlnk)
```

```
## 1 row with AMLNK 76 added to table 21
```


### b4. Repeat checks (same as in b1 and b2)  

```r
# Check if some parameters in data_10 lack AMLNK  
check1 <- data_10 %>%
  count(PARAM, AMLNK) %>%
  filter(is.na(AMLNK))


# Check if some parameters in data_10 are lacking a data_21 entry  
check2 <- data_10 %>%
  count(PARAM, AMLNK) %>%
  left_join(data_21 %>% select(AMLNK, ALABO), by = "AMLNK") %>% 
  filter(is.na(ALABO))

if (nrow(check1) == 0 & nrow(check2) == 0){
  cat("All data_10 data have AMLNK, and all point to a line in data_21")
} else {
  stop("Either some data_10 data lack AMLNK, or some AMLNK point to a non-existing line in data_21. You must edit one of them. (Error in section 'Table 21 b4')")
}
```

```
## All data_10 data have AMLNK, and all point to a line in data_21
```
### b5. Find records in table 21 tha are not used  

```r
# data_21 %>% colnames() %>% dput()
amlnk_missing <- data_21 %>% filter(!AMLNK %in% data_10$AMLNK) %>% pull(AMLNK)
param_in_21_but_not_in_10 <- data_10_2016 %>%
  filter(AMLNK %in% amlnk_missing) %>%
  group_by(PARAM, AMLNK) %>%
  summarise(N = n(), .groups = "drop")

cat("Parameters in data_21 that are not in data_10: \n")
```

```
## Parameters in data_21 that are not in data_10:
```

```r
param_in_21_but_not_in_10 %>% pull(PARAM)
```

```
## [1] "TBBPA"
```

```r
#  "TBBPA" "TBP"
```

### b6. Those records are deleted from table 21

```r
data_21 <- data_21 %>% 
  filter(!AMLNK %in% param_in_21_but_not_in_10$AMLNK)

cat(length(param_in_21_but_not_in_10$AMLNK), "lines deleted from data_21")
```

```
## 1 lines deleted from data_21
```


### c. Add PARAM and MUNIT (will be removed afterwards)  
These are fetched from data_10 

```r
# Backup (for debugging)
# data_21_back <- data_21  # make backup
# data_21 <- data_21_back  # restore from backup

# xtabs(~addNA(MUNIT), data_10)

# Add PARAM (will be removed afterwards)
n1 <- nrow(data_21)
data_21 <- data_21 %>%
  safe_left_join(data_10 %>% count(PARAM, AMLNK, MUNIT) %>% select(-n), 
            by = "AMLNK",
            na_matches = "never",
            check = "BCV")
n2 <- nrow(data_21)

if (FALSE){
  
  amlnk_dubl <- data_10 %>% distinct(PARAM, AMLNK, MUNIT) %>% count(AMLNK) %>% filter(n > 1) %>% pull(AMLNK)
  data_10 %>% count(PARAM, AMLNK, MUNIT) %>% filter(AMLNK == amlnk_dubl)

  amlnk_dubl <- data_10 %>% count(PARAM, AMLNK, MUNIT) %>% count(AMLNK) %>% filter(n > 1) %>% pull(AMLNK)
  data_10 %>% count(PARAM, AMLNK, MUNIT) %>% filter(AMLNK == amlnk_dubl)

  data_conc %>%
    filter(PARAM == "HG") %>%
    count(LATIN_NAME, UNIT)

  data_conc %>%
    filter(PARAM == "HG") %>%
    group_by(LATIN_NAME, TISSUE_NAME, UNIT) %>%
    summarise(min(VALUE_WW), median(VALUE_WW), max(VALUE_WW))
  # If warning - check AMLNK
  data_10 %>%
    count(PARAM, AMLNK, MUNIT) %>%
    count(AMLNK) %>%
    filter(n > 2)
}


# Check 
cat("Number of AMLNK without a corresponding PARAM (should be 0):", sum(is.na(data_21$PARAM)), "\n")
```

```
## Number of AMLNK without a corresponding PARAM (should be 0): 0
```

```r
# xtabs(~PARAM + MUNIT, data_10)
# xtabs(~PARAM + MUNIT, data_21)
# data_21 <- data_21 %>% select(-PARAM, -MUNIT)    # remove these variables (if error)
```

### d. Check existing METOA codes  
For biol. effects only

```r
xtabs(~METOA, data_21)
```

```
## METOA
##     AAS-CV      GC-MS     GC-MSD     GC-NCI        GRV    HPLC-FD HPLC-MS-MS 
##          1         13         15          9          4          3          3 
## HPLC-MS-NE     ICP-MS        KIN       VISO 
##          9         10          2          2
```

```r
# xtabs(~PARAM + METOA, data_21 %>% filter(METOA != ""))
```



### e1. Create 'ICES_METOA_codes2.xlsx'  

```r
#
# This was used to create 'ICES_METOA_codes2.xlsx' (based on 'ICES_METOA_codes.xlsx')
# Also some manual additions to that file was done (length, wight, VDSI etc.) so don't repeat this code
#

# # Methods - made using script "03extra01....R"
# df_metoa <- readxl::read_excel("Input_data/ICES_METOA_codes.xlsx")
# df_metoa$PARAM <- replace_parameter_synonyms(df_metoa$NAME)
# 
# # From 10(a) above
# # Change BDE to BD for the following. NOTE: "BDE28"  "BDE47"  "BDE99" should NOT be changed (!)
# sel <- df_metoa$PARAM %in% c("BDE100", "BDE126", "BDE153", "BDE154", "BDE183", "BDE209"); sum(sel)
# df_metoa$PARAM[sel] <- sub("BDE", "BD", df_metoa$PARAM[sel])
# 
# # Change some of the rest
# sel <- df_metoa$PARAM %in% "PFUdA"; sum(sel)
# df_metoa$PARAM[sel] <- "PFUnda"
# sel <- df_metoa$PARAM %in% "PFDcA"; sum(sel)
# df_metoa$PARAM[sel] <- "PFDA"
# sel <- df_metoa$PARAM %in% "TBT"; sum(sel)
# df_metoa$PARAM[sel] <- "TBSN+"
# 
# # Pick the first of these (simple way to handle duplicates...)
# pars <- c("AG", "AS", "CB118", "CD", "CO", "CR", "CU", "NI", "PB", "SN", "TDEPP", "ZN")
# df_metoa <- bind_rows(
#   df_metoa %>% filter(!PARAM %in% pars),
#   df_metoa %>% filter(PARAM %in% pars) %>% group_by(PARAM) %>% summarise_all(funs(first))
#   )
# 
# df_metoa %>% select(METHOD_ID, NAME, PARAM, everything()) %>% select(-PARAM_ICES) %>%
#   openxlsx::write.xlsx("Input_data/ICES_METOA_codes2.xlsx")
```

### e2. Read method data   

```r
df_metoa <- readxl::read_excel("../Milkys_2018/Input_data/ICES_METOA_codes2.xlsx")

# Change PFOSA to PFOSD in the method data
sel <- df_metoa$PARAM %in% "PFOSA"; sum(sel)
```

```
## [1] 1
```

```r
df_metoa$PARAM[sel] <- "PFOSD"
```

### f. Rename original METOA and add new ones  
By join with df_metoa. Note that both PARAM and UNIT are used for the join!  

```r
# data_21_back <- data_21  # make backup
# data_21 <- data_21_2016  # restore from backup

n1 <- nrow(data_21)
data_21 <- data_21 %>%
  rename(METOA_orig = METOA) %>%
  left_join(df_metoa %>% select(PARAM, UNIT_ICES, METOA), by = c("PARAM", "MUNIT" = "UNIT_ICES"))
n2 <- nrow(data_21)
if (n2 != n1)
  warning("W A R N I N G !  LEFT JOIN CHANGED THE NUMBER OF LINES!")

# table(is.na(data_21$METOA_orig))

sel_lacking <- with(data_21, is.na(METOA) & (is.na(METOA_orig) | METOA_orig %in% ""))
# Should be zero
cat("Number of parameters without METOA or METOA_orig (should be zero):", sum(sel_lacking), "\n")
```

```
## Number of parameters without METOA or METOA_orig (should be zero): 0
```

```r
# data_21[sel_lacking,] %>% select(PARAM, MUNIT, METOA, METOA_orig)

# Where we lack METOA, we use METOA_orig
sel <- is.na(data_21$METOA)
data_21$METOA[sel] <- data_21$METOA_orig[sel]
cat("Number of codes gotten from METOA from last year:", sum(sel), "\n")
```

```
## Number of codes gotten from METOA from last year: 8
```

```r
# Check
lacking_metoa <- is.na(data_21$METOA) | data_21$METOA %in% ""

# Should be zero
cat("Number of parameters without METOA (should be zero):", sum(lacking_metoa), "\n")
```

```
## Number of parameters without METOA (should be zero): 0
```

```r
if (sum(lacking_metoa) > 0){
  df <- data_21[lacking_metoa,]
  df
  data_10 %>% filter(AMLNK %in% df$AMLNK) %>% pull(PARAM) %>% unique()
}
```

### Add METOA for GOSOI, FATWT and VDSI

```r
sel <- data_21$PARAM %in% "VDSI"; sum(sel)
```

```
## [1] 1
```

```r
data_21$METOA[sel] <- "VISO"  # Visual observation/identification

sel <- data_21$PARAM %in% c("GOSOI", "FATWT%"); sum(sel)
```

```
## [1] 2
```

```r
data_21$METOA[sel] <- "GRV"
```



```r
# sel <- data_21$PARAM %in% "PFOSA"; sum(sel)
# data_21$PARAM[sel] <- "Perfluorooctanesulfonic acid amide"
# cat("PARAM changed from PFOSA to Perfluorooctanesulfonic acid amide for", sum(sel), "records\n")
```


### Pick correct variables in correct order  

```r
vars <- c("RECID", "AMLNK", "ALABO", "METDC", "REFSK", "METST", "METFP", 
  "METPT", "METCX", "METPS", "METOA", "AGDET", "SREFW", "SPECI", 
  "RLIST", "ORGSP", "SIZRF", "FORML")
data_21 <- data_21[vars]
```




## Table 20 - sampling methods  
Just 3 lines - we will just use last year's table  
NOte that the eider duck sampling (SMTYP) we will denote as 'HAN' = "by hand'


## Table 91 - stations  
We will base ourself on using last year's table  
It lacks  
    + 19B (Svalbard)
    + 28A2 Ålesund havn
    + 97A3 Bodø havn
      
**NOTE:** A check of the data in script 26 shows that **97A3 (Bodø havn)** is still lacking in table 91. See `Start making data_91` below.  

### a. Get 2018 data (submitted in 2019)  

```r
data_91 <- data_2018[["91"]]
column_order_91 <- names(data_91)
```
    
  
### b. Check lacking stations  

```r
sel <- data_03$STNNO %in% data_91$STNNO
cat("\nTable 03 stations not in table 91: \n")
```

```
## 
## Table 03 stations not in table 91:
```

```r
data_03[!sel,] %>% pull(STNNO)
```

```
## [1] "71A"  "97A3"
```

```r
sel <- data_10$STNNO %in% data_91$STNNO
cat("\nTable 10 stations not in table 91: \n")
```

```
## 
## Table 10 stations not in table 91:
```

```r
data_10[!sel,] %>% pull(STNNO) %>% unique()
```

```
## [1] "71A"  "97A3"
```



### c1. Add 97A3 (the actual Bodø harbour)     

```r
new_stations <- "97A3"
new_lat <- 67.2963
new_lon <- 14.3956

data_91_extra <- tibble(
  RECID = "91",
  CRUIS = "AA711",
  STNNO = new_stations, 
  LATIT = decimal2minute(new_lat),   # change to ICES format ("+67 17.778")
  LONGI = decimal2minute(new_lon),   # change to ICES format
  POSYS = "EST",
  SDATE = "",   # set later
  STATN = "97A3 Bodø harbour",
  MPROG = "CEMP",
  WLTYP = "CF", 
  MSTAT = "IH",       # impacted
  PURPM = "T~S"
)

check <- new_stations %in% data_91$STNNO

if (sum(check) == 0){
  data_91 <- bind_rows(data_91, data_91_extra)
  cat(nrow(data_91_extra), "new stations added\n")
} else if (sum(check) == 1){
  cat("Stations were already in data set\n")
} else {
  cat("Some, but not all, stations are already in data set. CHECK.\n")
}
```

```
## 1 new stations added
```
### c2. Add 71A (Bjørkøya, Grenland)     

```r
# data_2018[["91"]] %>% View()

new_stations <- "71A"
new_lat <- 59.02333	
new_lon <- 9.75367

data_91_extra <- tibble(
  RECID = "91",
  CRUIS = "AA711",
  STNNO = new_stations, 
  LATIT = decimal2minute(new_lat),   # change to ICES format ("+67 17.778")
  LONGI = decimal2minute(new_lon),   # change to ICES format
  POSYS = "EST",
  SDATE = "",   # set later
  STATN = "71A Bjørkøya (Risøyodden)",
  MPROG = "CEMP",
  WLTYP = "CF", 
  MSTAT = "RH",      # Representative of general conditions in terms of hazardous substances
  PURPM = "T~S"
)

check <- new_stations %in% data_91$STNNO

if (sum(check) == 0){
  data_91 <- bind_rows(data_91, data_91_extra)
  cat(nrow(data_91_extra), "new stations added\n")
} else if (sum(check) == 1){
  cat("Stations were already in data set\n")
} else {
  cat("Some, but not all, stations are already in data set. CHECK.\n")
}
```

```
## 1 new stations added
```

### d. Remove stations not needed  

```r
sel <- data_91$STNNO %in% data_03$STNNO  
cat("\nStations not in table 03: \n")
```

```
## 
## Stations not in table 03:
```

```r
data_91[!sel,] %>% pull(STNNO)
```

```
## [1] "51A"  "52A"  "I714"
```

```r
sel <- data_91$STNNO %in% unique(data_10$STNNO)  
cat("\nStations not in table 10: \n")
```

```
## 
## Stations not in table 10:
```

```r
data_91[!sel,] %>% pull(STNNO)
```

```
## [1] "51A"  "52A"  "I714"
```

```r
# Remove those data lines nor found in table 10  
data_91 <- data_91[sel,]
cat("\n", sum(!sel), "stations deleted from table 91 \n")
```

```
## 
##  3 stations deleted from table 91
```

### e. Fix coordinates of station 76A2  
ICES station dictionary: "Latitude, Longitude (58.7327 9.28104) Latitude range +/-, Longitude range +/- (0.00718, 0.01381)"  
- This also fits with "Kartbase.xlsx"  

```r
station <- "76A2" 
sel <- with(data_91, STNNO %in% station)

cat("Existing coordinates for", station, ":", 
    minute2decimal(data_91$LATIT[sel]), ",", minute2decimal(data_91$LONGI[sel]), "\n")
```

```
## Existing coordinates for 76A2 : 58.73013 , 9.738083
```

```r
data_91$LONGI[sel] <- decimal2minute(9.28104)

cat("New coordinates for", station, ":", 
    minute2decimal(data_91$LATIT[sel]), ",", minute2decimal(data_91$LONGI[sel]), "\n")
```

```
## New coordinates for 76A2 : 58.73013 , 9.281033
```


### f. Sample dates   
- Taken from raw data, 

```r
data_for_dates <- data_ind2 %>%
  filter(!is.na(SAMPLE_DATE)) %>%
  distinct(STATION_CODE, SAMPLE_DATE)

df_dates <- data_for_dates %>%
  rename(STNNO = STATION_CODE)

# Check stations with >1 date (19N has two)
check_dates_1 <- df_dates %>%
  group_by(STNNO) %>%
  mutate(n = n()) %>%
  filter(n > 1) %>%
  arrange(STNNO, SAMPLE_DATE)

# If some stations with >1 date:
if (nrow(check_dates_1) > 0){
  message("Some stations have >1 date")
  
  # Mean of dates for 19N
  df_dates <- df_dates %>%
    group_by(STNNO) %>%
    summarise(
      SAMPLE_DATE = mean(SAMPLE_DATE) %>% floor_date("day"),
      .groups = "drop") %>%
    mutate(
      Date = as.character(SAMPLE_DATE),
      SDATE = paste0(substr(Date,1,4),substr(Date,6,7),substr(Date,9,10))
    )
  
  # df_dates %>% filter(STNNO == "19N")
  # df_dates
  
  # Check again stations with >1 date
  check_dates_2 <- df_dates %>%
    group_by(STNNO) %>%
    mutate(n = n()) %>%
    filter(n > 1) %>%
    arrange(STNNO, SAMPLE_DATE)
  
  if (nrow(check_dates_2) == 0){
    warning("Mean date used for stations with more than have >1 date. Check 'check_dates_1'. \n")
  } else {
    stop("Some stations with more than have >1 date!")
  }
  
}
```

```
## Some stations have >1 date
```

```
## Warning: Mean date used for stations with more than have >1 date. Check 'check_dates_1'.
```

```r
cat("\n")
```

```r
# Check if we have all dates we need
sel <- data_91$STNNO %in% df_dates$STNNO  
if (sum(!sel) > 0){
  stop(sum(!sel), " stations not found in 'df_dates'!")
} else {
  message("All stations are found in 'df_dates'")
}
```

```
## All stations are found in 'df_dates'
```

### g. Add new dates

```r
data_91 <- data_91 %>%
  # Remove old date
  select(-SDATE) %>%
  # Add SAMPLE_DATE
  left_join(df_dates %>% select(STNNO, SDATE),  by = "STNNO")

# Correct column order
data_91 <- data_91[column_order_91]
```



## Table 90 - list of cruises  

### New table 90  
Original table is just 1 line - we will just use last year's table  
REVISION: Use
* SHIPC = AA36 unspecified fishing vessel for all fish stations except 30B  
* SHIPC = 58TB 	TRYGVE BRAARUD for 30B

```r
# Original
data_90
```

```
##   RECID SHIPC CRUIS OWNER PRDAT
## 1    90  AA71 AA711
```

```r
data_90 <- tibble(
  RECID = rep(90,3),
  SHIPC = c("AA36", "AA71", "58TB"),
  CRUIS = c(1,2,3),
  OWNER = NA,
  PRDAT = NA
)

data_90
```

```
## # A tibble: 3 × 5
##   RECID SHIPC CRUIS OWNER PRDAT
##   <dbl> <chr> <dbl> <lgl> <lgl>
## 1    90 AA36      1 NA    NA   
## 2    90 AA71      2 NA    NA   
## 3    90 58TB      3 NA    NA
```

### Also change CRUIS in other tables  
I.e.:  "data_90" "data_91" "data_03" "data_04" "data_10"

```r
# data_03

data_03 <- data_03 %>%
  mutate(CRUIS = case_when(
    grepl("B", STNNO) & STNNO != "30B" ~ 1,
    STNNO %in% "30B" ~ 3,
    grepl("F", STNNO) ~ 1,
    TRUE ~ 2)
  )

# For checking
if (FALSE){
  for (cr in unique(data_03$CRUIS)){
    cat(cr, "\n")
    print(data_03 %>% filter(CRUIS == cr) %>% pull(STNNO) %>% unique())
  }
}

data_04 <- data_04 %>%
  mutate(CRUIS = case_when(
    grepl("B", STNNO) & STNNO != "30B" ~ 1,
    STNNO %in% "30B" ~ 3,
    grepl("F", STNNO) ~ 1,
    TRUE ~ 2)
  )
    
    
data_10 <- data_10 %>%
  mutate(CRUIS = case_when(
    grepl("B", STNNO) & STNNO != "30B" ~ 1,
    STNNO %in% "30B" ~ 3,
    grepl("F", STNNO) ~ 1,
    TRUE ~ 2)
  )
    

data_91 <- data_91 %>%
  mutate(CRUIS = case_when(
    grepl("B", STNNO) & STNNO != "30B" ~ 1,
    STNNO %in% "30B" ~ 3,
    grepl("F", STNNO) ~ 1,
    TRUE ~ 2)
  )
    
# data_92 - fixed below

# For checking table 10
if (FALSE){
  for (cr in 1:3){
    cat(cr, "\n")
    print(data_10 %>% filter(CRUIS == cr) %>% pull(STNNO) %>% unique())
  }
}
```






## Table 92 - station info   
Only temp for 23B, 30B, 53B   
Line 21, PARAMs EROD and CYP1A require the reporting of bottom temperature (BotTemp) in record 92 (WGBEC2013)

```r
data_92 <- tibble(
  RECID = "92",
  CRUIS = c(1,3,1),          # See table 90
  STNNO = c("23B","30B","53B"),
  RSRVD = NA,
  MATRX = "SI",
  PARAM = "BotTemp",
  MUNIT = "degC",
  VALUE = 7.75,
  DCFLG = NA
  )
```



## Finishing


### Check 1 - duplicates in 04

```r
# Are there duplicates in table 04? 
check <- data_04 %>%
    group_by(CRUIS, STNNO, SMPNO, SUBNO) %>%
    mutate(n = n()) %>%
    filter(n > 1) %>%
    arrange(CRUIS, STNNO, SMPNO, SUBNO) %>%
    select(RECID:NOIMP, SUBNO, NOIMP, BULKID, everything())
  
cat("Number of duplicates in table 04: ", nrow(check), "\n")
```

```
## Number of duplicates in table 04:  0
```

```r
if (nrow(check) > 0){

  cat("Please remove the duplicates.\n\n")

  tab <- xtabs(~SUBNO + STNNO, check)
  tab
  
  # check

}

if (FALSE){

  data_04_fish %>%
    group_by(STATION_CODE, SMPNO, SUBNO) %>%
    mutate(n = n()) %>%
    filter(n > 1) %>%
    arrange(STATION_CODE, SMPNO, SUBNO)
  
  }
```


### Check 2 - duplicates in 10

```r
# Are there duplicates in table 10? 
check <- data_10 %>%
  group_by(CRUIS, STNNO, SMPNO, SUBNO, MATRX, PARAM) %>%
  mutate(n = n()) %>%
  filter(n > 1) %>%
  arrange(CRUIS, STNNO, SMPNO, SUBNO, MATRX, PARAM) %>%
  select(CRUIS, STNNO, SMPNO, SUBNO, MATRX, PARAM, VALUE, everything())

cat("Number of duplicates in table 10: ", nrow(check), "\n")
```

```
## Number of duplicates in table 10:  0
```

```r
if (nrow(check) > 0){

  cat("Please remove the duplicates.\n\n")

  tab <- xtabs(~PARAM + STNNO, check)
  cat("Parameters: \n-----------------------------\n")
  apply(tab, 1, sum) %>% sort(decreasing = TRUE) %>% print()
  cat("Stations: \n-----------------------------\n")
  apply(tab, 2, sum) %>% sort(decreasing = TRUE) %>% print()
  cat("Stations / parameters: \n-----------------------------\n")
  tab2 <- cbind(tab, Sum = apply(tab, 1, sum)) 
  tab2 <- tab2[order(tab2[,"Sum"]),]
  tab2
  
}
```


### Check 3 - join 10 + 91

```r
#
# Stations: Measurement table vs station table
#
check <- data_10 %>%
  safe_left_join(data_91, by = c("CRUIS", "STNNO"), 
                 na_matches = "never", check = "VMN")

cat("Checked measurement table (10) vs station table (91). If no error, check has passed.")
```

```
## Checked measurement table (10) vs station table (91). If no error, check has passed.
```

```r
# If lack of matches, do this:
if (FALSE){
  df <- full_join(
    tibble(STNNO = unique(data_10$STNNO), tab10 = 1),
    tibble(STNNO = unique(data_91$STNNO), tab91 = 1)
  ) %>%
    filter(is.na(tab10) | is.na(tab91))
  df
}
```


### Check 4 - join 10 + 04

```r
#
# Stations and sample numbers: Measurement table vs sample table
#
check <- data_10 %>%
  safe_left_join(data_04, by = c("CRUIS", "STNNO", "SMPNO", "SUBNO"), 
                 na_matches = "never", check = "VMN")

cat("Checked measurement table (10) vs sample table (04). If no error, check has passed.")
```

```
## Checked measurement table (10) vs sample table (04). If no error, check has passed.
```

```r
# If errors, do this:
if (FALSE){
  
  # If  y is not unique on CRUIS, STNNO, SMPNO and SUBNO, do this:
  data_04 %>%
    group_by(CRUIS, STNNO, SMPNO, SUBNO) %>%
    mutate(n = n()) %>%
    filter(n > 1) %>%
    arrange(CRUIS, STNNO, SMPNO, SUBNO) %>%
    select(RECID:NOIMP, SUBNO, NOIMP, BULKID, everything())
  
  # If  x has unmatched sets of joining values:
  full_join(
    data_10 %>% distinct(STNNO, SMPNO, SUBNO) %>% mutate(tab10 = 1),
    data_04 %>% distinct(STNNO, SMPNO, SUBNO) %>% mutate(tab04 = 1)
  ) %>%
    filter(is.na(tab10) | is.na(tab04))
}
```


### Check 5 - join 91 + 92

```r
check <- data_91 %>%
  safe_left_join(data_92, by = c("CRUIS", "STNNO"), 
                 na_matches = "never", check = "VN")

cat("Checked station table (91) vs station information table (92). If no error, check has passed.")
```

```
## Checked station table (91) vs station information table (92). If no error, check has passed.
```

```r
if (FALSE){
  data_04 %>%
    group_by(CRUIS, STNNO, SMPNO, SUBNO) %>%
    mutate(n = n()) %>%
    filter(n > 1)
}
```


### Check 6 - Missing values  

#### a. Number of missing values per variable

```r
# Missing values
cat("Missing values: \n----------------------------------------------------------\n")
```

```
## Missing values: 
## ----------------------------------------------------------
```

```r
cat("Table 04\n")
```

```
## Table 04
```

```r
apply(is.na(data_04), 2, sum)
```

```
##  RECID  CRUIS  STNNO  SMPNO  SUBNO  NOIMP  ORGSP  SEXCO  STAGE  CONES  ASTSA 
##      0      0      0      0      0      0    511    472    511    511    511 
##  NODIS BULKID 
##    511     90
```

```r
cat("Table 10\n")
```

```
## Table 10
```

```r
apply(is.na(data_10), 2, sum)
```

```
## RECID CRUIS STNNO SMPNO SUBNO MATRX DEPHU DEPHL PARAM MUNIT BASIS AMLNK VFLAG 
##     0     0     0     0     0     0 13931 13931     0     0    44     0 13931 
## QFLAG VALUE PERCR SIGND UNCRT METCU DETLI LMQNT 
## 10310     0 13931 13931  4567  4567 10798 10310
```

```r
cat("\n")
```

#### b. Missing uncertainties

```r
cat("Missing uncertainties: \n----------------------------------------------------------\n")
```

```
## Missing uncertainties: 
## ----------------------------------------------------------
```

```r
xtabs(~is.na(UNCRT) + addNA(QFLAG), data_10)
```

```
##             addNA(QFLAG)
## is.na(UNCRT)    Q <NA>
##        FALSE    0 9364
##        TRUE  3621  946
```

```r
cat("\n\n")
```

```r
cat("Missing uncertainties (data above LOQ) by parameter: \n----------------------------------------------------------\n")
```

```
## Missing uncertainties (data above LOQ) by parameter: 
## ----------------------------------------------------------
```

```r
data_10 %>% 
  mutate(Status = case_when(
    !is.na(QFLAG) ~ "Under LOQ",
    !is.na(UNCRT) ~ "Over LOQ, has UNCRT",
    is.na(UNCRT) ~ "Over LOQ, lacks UNCRT")
  ) %>%
  xtabs(~PARAM + Status, .)
```

```
##         Status
## PARAM    Over LOQ, has UNCRT Over LOQ, lacks UNCRT Under LOQ
##   ACNE                     0                     0        26
##   ACNLE                    8                     0        18
##   AG                     258                     0        77
##   ALAD                     0                    47         0
##   ANT                     18                     0         8
##   AS                     341                     0         0
##   BAA                     18                     0         8
##   BAP                     12                     0        14
##   BAP3OH                   0                    29        31
##   BBJF                    21                     0         5
##   BD100                  198                     0        15
##   BD126                    0                   138        75
##   BD153                   74                     0       139
##   BD154                  180                     0        33
##   BD183                    0                     4       209
##   BD209                    0                     0       213
##   BDE28                  167                     0        46
##   BDE47                  199                     0        14
##   BDE99                  164                     0        49
##   BGHIP                   16                     0        10
##   BKF                     15                     0        11
##   CB101                  272                     0        59
##   CB118                  319                     0        12
##   CB138                  297                     0        34
##   CB153                  314                     0        17
##   CB180                  253                     0        78
##   CB28                   210                     0       121
##   CB52                   250                     0        81
##   CD                     332                     0         9
##   CO                     333                     0         1
##   CR                     325                     0        15
##   CU                     341                     0         0
##   DBA3A                    0                     3        23
##   DDEPP                  145                     0         4
##   DDTPP                  122                     0        27
##   DRYWT%                 581                     0         0
##   EROD                     0                    44         0
##   FATWT%                 332                     0         0
##   FLE                      0                     0        26
##   FLU                     21                     0         5
##   GOSOI                   44                     0         0
##   HBCDA                  208                     0        24
##   HBCDB                   24                     0       208
##   HBCDG                   48                     0       184
##   HCB                    103                    30        43
##   HCHA                     2                     0       144
##   HCHG                     1                     0       145
##   HG                     371                     0         0
##   ICDP                    14                     0        12
##   LNMEA                  331                     0         0
##   MCCP                   225                     0         5
##   NAP                      0                     0        26
##   NI                     322                     0        18
##   PA                       6                     0        20
##   PA1OH                    0                    39        21
##   PB                     288                     0        53
##   PFBS                     0                     0         6
##   PFDA                     0                    22       170
##   PFHpA                    0                     0       192
##   PFHxA                    0                     0       192
##   PFNA                     0                     7       185
##   PFOA                     0                     0       192
##   PFOS                     0                   171        21
##   PFOSD                    0                   118        74
##   PFUnda                   0                    94        98
##   PYR                     18                     0         8
##   PYR1OH                   0                    60         0
##   SCCP                   224                     0         6
##   SN                     303                     0        32
##   TBSN+                   24                     0        12
##   TDEPP                    0                   132        17
##   VDSI                     0                     8         0
##   WTMEA                  331                     0         0
##   ZN                     341                     0         0
```

#### c. Missing units

```r
cat("Missing units: \n----------------------------------------------------------\n")
```

```
## Missing units: 
## ----------------------------------------------------------
```

```r
check <- data_10 %>%
  filter(is.na(MUNIT)) 
cat(nrow(check), "records lack MUNIT \n")
```

```
## 0 records lack MUNIT
```

```r
if (nrow(check) > 0){
  cat("Missing MUNIT - Parameters: \n")
  xtabs(~PARAM, check)
  stop("All records must have MUNIT! (Check 6c)")
}
```


### Check 7

```r
# Check GOSOI (gonado-somatic index) for EROD samples
check <- data_10 %>%
  filter(PARAM %in% c("EROD", "GOSOI")) %>%
  select(STNNO, SMPNO, SUBNO, PARAM, VALUE) %>%
  spread(PARAM, VALUE)

percent_ok <- mean(!is.na(check$GOSOI))*100

cat(percent_ok, "% of the EROD values have gonado-somatic index (GOSOI) \n")
```

```
## 100 % of the EROD values have gonado-somatic index (GOSOI)
```

```r
if (percent_ok < 100){
  warning("GOSOI is needed for all EROD values! (Check 7)")
}
```


### Check 8

```r
if (FALSE){

    check1 <- data_10 %>% left_join(data_04[,2:6])
  nrow(data_10) == nrow(check1)
  sum(is.na(data_04$NOIMP))
  sum(is.na(check1$NOIMP))
  
  check2 <- data_04 %>% left_join(data_10[,2:6])
  nrow(data_04) == nrow(check2)  # false but that is ok
  sum(is.na(data_10$MATRX))
  sum(is.na(check2$MATRX))
  
  xtabs(~STNNO + MATRX, subset(check1, is.na(NOIMP)))
  xtabs(~PARAM + MATRX, subset(check1, is.na(NOIMP) & STNNO == "13B"))
  xtabs(~SUBNO + MATRX, subset(check1, is.na(NOIMP) & STNNO == "13B"))
  
  xtabs(~STNNO, subset(data_03, STNNO %in% c("227G", "227G2")))
  xtabs(~STNNO, subset(data_04, STNNO %in% c("227G", "227G2")))
  xtabs(~STNNO + PARAM, subset(data_10, STNNO %in% c("227G", "227G2")))
  xtabs(~STATION_CODE + PARAM, subset(data_conc, STATION_CODE %in% c("227G", "227G2")))
  xtabs(~STATION_CODE + PARAM, subset(data_conc, STATION_CODE %in% c("97A2", "97A3")))
  
  subset(check1, STNNO == "13B") %>% View()
  
  subset(data_04, STNNO == "13B")
  subset(data_04, STNNO == "13B") %>% xtabs(~SUBNO, .)
  
  subset(data_conc_fishmuscle, STATION_CODE == "13B") %>% View()
  subset(data_conc_fishliver, STATION_CODE == "13B") %>% View()
  
  subset(data_conc_fishmuscle, STATION_CODE == "13B") %>% xtabs(~SUBNO, .)
  subset(data_conc_fishliver, STATION_CODE == "13B") %>% xtabs(~SUBNO, .)
  
  subset(data_conc_fish, STATION_CODE == "13B") %>% xtabs(~SUBNO, .)
  subset(data_10, STNNO == "13B") %>% xtabs(~SUBNO, .)
  
  subset(data_04, STNNO == "13B") %>% xtabs(~SUBNO, .)
  subset(data_10, STNNO == "13B") %>% xtabs(~SUBNO, .)
  subset(data_10_fish, STNNO == "13B") %>% xtabs(~SUBNO, .)
  subset(data_10_fatdry, STNNO == "13B") %>% xtabs(~SUBNO, .)
  subset(data_10_length2, STNNO == "13B") %>% xtabs(~SUBNO, .)
}

# xtabs(~PARAM + is.na(UNCRT), data_10)
```


### Write ICES submission file    
- automatically sets version number (finds the highest existing version number and adds one)  
- Set up to use UTF-8 when writing, but apparently we need to change the file manually to UTF-8 afterwards anyway (in Notepad++, set encoding to ANSI, then "convert to UTF-8")  

```r
write_ices_file <- FALSE
 write_ices_file <- TRUE

#
# Set "scipen" options so we avoid writing scientific notation (almost always)
# See ?write.table
#
cat("Set the way numbers are written as characters: \n")
```

```
## Set the way numbers are written as characters:
```

```r
cat("Existing setting: \n")
```

```
## Existing setting:
```

```r
as.character(0.000004)   # check
```

```
## [1] "4e-06"
```

```r
old_options <- options(scipen = 3)
cat("New setting: \n")
```

```
## New setting:
```

```r
as.character(0.000004)   # check again
```

```
## [1] "0.000004"
```

```r
cat("\n\n")
```

```r
if (write_ices_file){

  if (!dir.exists(save_folder_txt)){
    message("Creating folder '", save_folder_txt, "'")
    dir.create(save_folder_txt)
  }
  
  fns <- dir(save_folder_txt, paste0("NIVA", selected_year, "CF*"))
  existing_versions <- substr(fns, 12, 13) %>% as.numeric()
  
  if (length(existing_versions) == 0){
    version <- 1
  } else {
    version <- max(existing_versions) + 1
  }
  
  string <- paste0("NIVA", selected_year, "CF_%02i.NO")
  fn <- sprintf(string, version)
  icesfile <- file(paste0(save_folder_txt, "/", fn), "w+")
  write.table(data_00, icesfile, sep = ";", quote = FALSE, na = "", 
              col.names = FALSE, row.names = FALSE, fileEncoding = "UTF-8")
  write.table(data_03, icesfile, sep = ";", quote = FALSE, na = "", 
              col.names = FALSE, row.names = FALSE, fileEncoding = "UTF-8")
  write.table(data_04, icesfile, sep = ";", quote = FALSE, na = "", 
              col.names = FALSE, row.names = FALSE, fileEncoding = "UTF-8")
  write.table(data_10, icesfile, sep = ";", quote = FALSE, na = "", 
              col.names = FALSE, row.names = FALSE, fileEncoding = "UTF-8")
  write.table(data_20, icesfile, sep = ";", quote = FALSE, na = "", 
              col.names = FALSE, row.names = FALSE, fileEncoding = "UTF-8")
  write.table(data_21, icesfile, sep = ";", quote = FALSE, na = "", 
              col.names = FALSE, row.names = FALSE, fileEncoding = "UTF-8")
  write.table(data_90, icesfile, sep = ";", quote = FALSE, na = "",
              col.names = FALSE, row.names = FALSE, fileEncoding = "UTF-8")
  write.table(data_91, icesfile, sep = ";", quote = FALSE, na = "", 
              col.names = FALSE, row.names = FALSE, fileEncoding = "UTF-8")
  write.table(data_92, icesfile, sep = ";", quote = FALSE, na = "", 
              col.names = FALSE, row.names = FALSE, fileEncoding = "UTF-8")
  close(icesfile)
  cat("Version", version, "written as text file at", as.character(Sys.time()))

  } 
```

```
## Version 18 written as text file at 2022-09-27 15:34:10
```

```r
# Reset options
options(old_options)
```

### Save as R data

```r
if (write_ices_file){
  

  if (!dir.exists(save_folder_rds)){
    message("Creating folder '", save_folder_rds, "'")
    dir.create(save_folder_rds)
  }
  
  datalist <- list(data_00, data_03, data_04, data_10, data_20, 
                   data_21, data_90, data_91, data_92)
  names(datalist) <- c("00", "03", "04", "10", "20", "21", "90", "91", "92")
  
  string <- paste0("842_NIVA", selected_year, "CF_%02i.rds")
  fn_rds <- sprintf(string, version)
  
  saveRDS(datalist, paste0(save_folder_rds, "/", fn_rds))

  cat("Version", version, "written as rds (R data) file at", as.character(Sys.time()), "\n")
  cat("Filename: ",  paste0(save_folder_rds, "/", fn_rds), "\n")
  
}
```

```
## Version 18 written as rds (R data) file at 2022-09-27 15:34:10 
## Filename:  Files_to_ICES/2020/Rdata/842_NIVA2020CF_18.rds
```

```r
if (FALSE){
  
  # For reading file
  datalist <- readRDS("Files_to_ICES/2020/Rdata/842_NIVA2020CF_09.rds")
  
}
```


### Save as Excel file  

```r
if (FALSE){
  
  library(writexl)
  string <- paste0("842_NIVA", selected_year, "CF_%02i.xlsx")
  fn_xl <- sprintf(string, version)
  
  write_xlsx(datalist, paste0(save_folder_excel, "/", fn_xl))

  cat("Version", version, "written as excel file at", as.character(Sys.time()), "\n")
  cat("Filename: ",  paste0(save_folder_rds, "/", fn_rds), "\n")
  
}
```


## Post-submission


### Restore all tables from saved version  
NOTE:   
- Versions 1-36 are written using this script  
- Versions 38-40 are written using script `26_Fix_submitted_ICES_file`   
    - The order of the tables in the latter rds files ( a list) differs form the order in vesrion 1-36     
    - In these versions, 
- (Version 37 doesn't exist, at least not as rds file)

```r
# Set this to true for restoring the tables 
# NOTE: will overwrite existing data_XX objects
restore <- FALSE
# restore <- TRUE

  
# Pick version (if needed)

if (restore) {
  
  
  # Find newest rds file
  fns <- dir(save_folder_rds)
  existing_versions <- substr(fns, 16, 17) %>% as.numeric()
  if (length(existing_versions) == 0){
    version <- 1
  } else {
    version <- max(existing_versions)
  }
  
  cat("Reading newest version - 'version' =", version, "\n")
  
  # Make file names
  
  string <- paste0("842_NIVA", selected_year, "CF_%02i.rds")
  fn_rds <- sprintf(string, version)
  string <- paste0("NIVA", selected_year, "CF_%02i.NO")
  fn <- sprintf(string, version)
  
  data_list <- readRDS(paste0(save_folder_rds, "/", fn_rds))
  names(data_list) <- c("00", "03", "04", "10", "20", "21", "90", "91", "92")
  
  data_00 <- data_list[["00"]]
  data_03 <- data_list[["03"]]
  data_04 <- data_list[["04"]]   # sample table
  data_10 <- data_list[["10"]]   # concentration data
  data_20 <- data_list[["20"]]
  data_21 <- data_list[["21"]]
  data_90 <- data_list[["90"]]
  data_91 <- data_list[["91"]]   # station data
  data_92 <- data_list[["92"]]
  
}
```

## Checks of data  

### Check positions     
- ICES position strings getting the warning 'Station position appears to be on land'   
- Checked using norgeskart.no  

```r
# Turns ICES position string into decimal longitude, latitude
copy_decimal <- function(string){ 
  str2 <- strsplit(string, ",")[[1]]
  y1 <- strsplit(str2[1], " ")[[1]] %>% as.numeric()
  y <- y1[1] + y1[2]/60
  x1 <- strsplit(str2[2], " ")[[1]] %>% as.numeric()
  x <- x1[1] + x1[2]/60
  paste0(round(y, 5), ",", round(x,5)) %>% writeLines("clipboard")
}

# ICES position strings getting the warning 'Station position appears to be on land'   
# Run line by line
# For each line, paste into search box of norgeskart.no
"+59 53.737,+05 6.514" %>% copy_decimal()
"+69 39.180,+18 58.440" %>% copy_decimal()
```

```
## Warning in close.connection(con): unable to open the clipboard
```

```r
"+79 0.240,+12 6.600" %>% copy_decimal()
```


### Check specific lines in data

```r
check <- FALSE
# check <- TRUE
if (check) {
  dat <- readLines(fn_txt)  
  # Here, check which stations which "appears to be on land"
  lines <- c(1178, 1236, 1273, 1319, 1365)
  colnumber <- c(3,6,9,15)
  dat[lines] %>%
    strsplit(split = ";") %>%
    map(~.[colnumber])
}
```

### Check SHIPC references    
Mail from Anna (accessions@ices.dk) 4. sept 2019:  
Due to some critical errors, it is presently impossible to upload the NIVA 2018 CF file to DOME.
The SHIPC references in all records but record 90 are erroneous, and don’t match the ‘parent’ 90.SHIPC code.  

```r
if (FALSE){

# Which tables contain SHIPC?
sel <- data %>% map_lgl(~("SHIPC" %in% colnames(.)))
names(data)[sel] 

# Which tables contain CRUIS?
sel <- data %>% map_lgl(~("CRUIS" %in% colnames(.)))
names(data)[sel] 


data_90$CRUIS %>% unique() %>% sort()
data_91$CRUIS %>% unique() %>% sort()
data_03$CRUIS %>% unique() %>% sort()
data_04$CRUIS %>% unique() %>% sort()
data_10$CRUIS %>% unique() %>% sort()

}

if (FALSE){
  data_90
}
```

### Check STNNO references    
Mail from Anna (accessions@ices.dk) 12. sept 2019: 
'There is actually a problem with a missing record 91 for the STNNO=97A3 that is used in other record types'.  
Also lacking from record 92 but that doesnæt matter (only contains bottom temperatures)      

```r
if (FALSE){

  
# Which tables contain STNNO?
sel <- data %>% map_lgl(~("STNNO" %in% colnames(.)))
names(data)[sel] 

# Checks
for (station_code in c("97A2", "97A3")){
  cat(station_code, "\n")
  for (table in list(data_03, data_04, data_10, data_91, data_92)){
    cat(station_code %in% table$STNNO, "")
  }
  cat("\n")
}


}
```

### Check DRYWT 

```r
# Check parameters per matrix (excluding LI, MU)
df <- data_10 %>%
  filter(!MATRX %in% c("LI","MU")) %>%
  group_by(MATRX) %>%
  summarise(PARAM = paste(unique(PARAM), collapse = ", ")) %>%
  as.data.frame()
for (i in seq_len(nrow(df)))
  cat(df[i,1], "\n", df[i,2], "\n\n")
```

```
## BI 
##  PA1OH, PYR1OH, BAP3OH 
## 
## BL 
##  ALAD, CB118, NI, HCB, HBCDA, CD, ZN, PFHxA, PFOA, SN, BD126, BD154, BD209, PFOSD, SCCP, PFHpA, HBCDB, AS, CO, BD100, PFDA, CB180, CR, BDE99, PFOS, PFNA, CB138, BDE47, MCCP, PFUnda, CB101, HG, AG, CB153, HBCDG, BD153, CB52, PB, BD183, BDE28, FATWT%, CU, CB28 
## 
## EH 
##  CD, PB, CO, PFOS, CB138, PFOA, BD153, SN, CB118, CR, BD154, PFHxA, ZN, CB101, BDE47, HBCDG, AG, CU, HCB, SCCP, BD126, NI, PFUnda, PFHpA, PFDA, HG, FATWT%, CB52, PFOSD, BD209, AS, CB28, HBCDB, BDE99, PFNA, BD100, BDE28, MCCP, CB153, CB180, BD183, HBCDA 
## 
## GO 
##  GOSOI 
## 
## LIS9 
##  EROD 
## 
## SB 
##  CO, CU, CB118, CB138, PB, HG, CR, AS, SN, CB28, CD, CB153, NI, CB52, CB101, AG, CB180, FATWT%, ZN, DRYWT%, BBJF, BGHIP, PYR, ACNE, BAA, NAP, ACNLE, BAP, ICDP, DBA3A, PA, ANT, FLE, BKF, FLU, DDTPP, TDEPP, HCB, HCHG, DDEPP, HCHA, TBSN+, HBCDB, BDE28, BD183, BD154, HBCDG, BDE99, BD209, HBCDA, SCCP, BD100, MCCP, BD153, BD126, BDE47, PFHxA, PFOSD, PFHpA, PFDA, PFNA, PFOS, PFOA, PFUnda, PFBS, VDSI 
## 
## WO 
##  LNMEA, WTMEA
```

```r
# Count samples
data_10 %>%
  count(STNNO, SMPNO, SUBNO, MATRX) %>% 
  nrow()   # 1116 samples
```

```
## [1] 1138
```

```r
# Count measurements per matrix
data_10 %>%
  count(MATRX)
```

```
##   MATRX    n
## 1    BI  180
## 2    BL  670
## 3    EH  630
## 4    GO   44
## 5    LI 8285
## 6  LIS9   44
## 7    MU  516
## 8    SB 2900
## 9    WO  662
```

```r
# Percent of samples with dry weight, by matrix 
data_10 %>%
  group_by(STNNO, SMPNO, SUBNO, MATRX) %>% 
  summarise(Drywt_n = sum(PARAM %in% "DRYWT%"), .groups = "drop") %>%    # count(Drywt_n)  # just checking
  group_by(MATRX) %>%  
  summarise(Drywt_perc = 100*mean(Drywt_n), N = n(), .groups = "drop") 
```

```
## # A tibble: 9 × 3
##   MATRX Drywt_perc     N
##   <chr>      <dbl> <int>
## 1 BI           0      60
## 2 BL           0      62
## 3 EH           0      15
## 4 GO           0      44
## 5 LI          99.6   233
## 6 LIS9         0      44
## 7 MU         100     258
## 8 SB         100      91
## 9 WO           0     331
```

```r
  # Lacking for 23% of SB samples
  # Lacking for all:
  #   cod biol. effect parameters (MATRX = BI, BL, LIS9) 
  #   gonad somatic index GOSOI (MATRX = GO)
  #   fish length LNMEA and weight WTMEA (MATRX = WO)

# Percent of "SB" samples with dry weight, BY STATION 
data_10 %>%
  group_by(STNNO, SMPNO, SUBNO, MATRX) %>% 
  summarise(Drywt_n = sum(PARAM %in% "DRYWT%")) %>%    # ungroup() %>% count(Drywt_n)  # just checking
  group_by(STNNO, MATRX) %>%
  summarise(Drywt_perc = 100*mean(Drywt_n), N = n(), .groups = "drop") %>%
  filter(Drywt_perc < 100 & MATRX %in% "SB")
```

```
## `summarise()` has grouped output by 'STNNO', 'SMPNO', 'SUBNO'. You can override
## using the `.groups` argument.
```

```
## # A tibble: 0 × 4
## # … with 4 variables: STNNO <chr>, MATRX <chr>, Drywt_perc <dbl>, N <int>
```

```r
  # 19N ONLY (Eider duck)
```

### Check a parameter, inc.. uncertainty     

```r
if (FALSE){
  
  param <- "TBSN+"
  metcu <- "SD"     # standard deviation  
  
  
  unique(data[["10"]]$PARAM) %>% sort()
  head(data[["10"]])
  
  
  data[["10"]] %>%
    filter(PARAM %in% param & (METCU %in% metcu | is.na(METCU))) %>% # View()
    ggplot(aes(STNNO, VALUE, color = QFLAG)) +
    geom_pointrange(aes(ymin = VALUE - UNCRT, ymax = VALUE + UNCRT)) +
    facet_wrap(vars(MATRX))
  
  
  }
```


## Notes  


```

"BDE119", "BDE138", "BDE156", "BDE17", "BDE184", "BDE191", "BDE196", 
"BDE197", "BDE202", "BDE206", "BDE207", "BDE49", "BDE66", 
"BDE71", "BDE77", "BDE85", 
"CB105", "CB114", "CB122", "CB123", "CB128", "CB141", "CB149", "CB156", 
"CB157", "CB167", "CB170", "CB18", "CB183", "CB187", "CB189", 
"CB194", "CB206", "CB209", "CB31", "CB33", "CB37", "CB47", "CB66", 
"CB74", "CB99", 

"MCCP eksl. LOQ" ~ "MCCP", 
"SCCP eksl. LOQ" ~ "SCCP",
"Dieldrin" ~ "DIELD", 
"Mirex" = "MIREX", 
"Oktaklorstyren (OCS)" ~ "OCS",
"Oxyklordan" ~ "OCDAN", 

"Nonaklor, trans-" ~ "TNONC",
"alfa-Klordan (cis)" ~ "CCDAN"

"DDTEP" ~ "DDTEP", 
"DDTOP" ~ "DDTOP", 
"PROTV" ~ "PROTV"

"Tributyltinn (TBT)-Sn" ~ "TBTIN"
"MBTIN" ~ "MBSN+",

 
"D4", "D5", "D6", 
D4: octamethylcyclotetrasiloxane	556-67-2	
D5: decamethylcyclopentasiloxane	541-02-6	
D6: dodecamethylcyclohexasiloxane	540-97-6	


# "Heptaklor epoksid"
# only cis-heptachlorepoxide "(alpha)" and trans-heptachlorepoxide "(beta)"	


HCEPT 	 

 	trans-heptachlorepoxide "(beta)" 
, "MCCP eksl. LOQ", 
"Toksafen Parlar 50", "Toksafen Parlar 26", "Toksafen Parlar 62", - only TTOX, total toxaphene

"DDDOP" - not existing 

"Oxyklordan", "SCCP eksl. LOQ", "PROTV", "gamma-Klordan (trans)", 
"CB105", "CB128", "CB156", "CB167", "CB187", "CB99", "Sum-HepCB", 
"CB170", "CB74", "PeCB", "CB183", "Sum-HexCB", "BDE77", "CB209", 
"CB157", "Sum-PenCB", "CB114", , "CB123", 
"CB66", "Sum-TriCB", "BDE17", "KPAH", "P_S", "PAH16", "Sum-TetCB", 
"CB141", "DDEOP", "CB189", "CB194", "CB206", "CB31", "CB47", 
"Krysen", "MBTIN", "Monobutyltinn (MBT)-Sn", "CB149", "DBTIN", 
"Dibutyltinn-Sn (DBT-Sn)", "PFHxS", "QCB", "TBA", 
"TPTIN", "Trifenyltinn (TPhT)-Sn", "CB122", "BDE85", "Total PFOS/PFOA inkl. LOQ", 
"CB33", "CB37", "CB18", "Endrin", "BDE184", "BDE71", "HCHB", 
"INTF%")



"P_S", "PAH16", "PeCB", "Perfluor-3,7-dimetyloktansyre (PF37DMOA)", 
"Perfluorbutansyre (PFBA)", "Perfluordekansulfonat (PFDS)", "Perfluordodekansyre (PFDoA)", 
"Perfluorheptansulfonat (PFHpS)", "Perfluorpentansyre (PFPeA)", 
"Perfluortetradekansyre (PFTA)", "Perfluortridekansyre (PFTrA)", 
"PFAS", "PFHxS", "PROTV", "QCB", "SCCP eksl. LOQ", "Sum-HepCB", 
"Sum-HexCB", "Sum-PenCB", "Sum-TetCB", "Sum-TriCB", "TBA", "TCHT", 
"Tetrabutyltinn (TTBT)-Sn", "Toksafen Parlar 26", "Toksafen Parlar 50", 
"Toksafen Parlar 62", "Total PFOS/PFOA inkl. LOQ", "TPTIN", "trans-Heptaklorepoksid", 
"Tributyltinn (TBT)-Sn", "Trifenyltinn (TPhT)-Sn", "Trisykloheksyltinn (TCHT)-Sn", 
"TTBT")
> 
> cat("\n\nCheck excluded parameters:\n")

```
