
#
# Based in Milkys script 26
#

setwd(here::here("Apps/App_805"))

library(dplyr)
library(purrr)
library(readxl)
library(tidyr)
library(readr)
library(ggplot2)

source("../../844_ICES_submission_check_functions.R")
source("../../845_Check_ICES_submission_functions.R")   # set_numeric

summarize_sequence <- function(x){
  x <- sort(unique(x))
  dx <- diff(x)
  df <- tibble(
    x = x,
    index = cumsum(c(1, dx) > 1) + 1)
  df %>% 
    group_by(index) %>%
    summarize(Min = min(x),Max = max(x)) %>%
    mutate(Summ = ifelse(Min < Max, paste0(Min,"-",Max), Min)) %>%
    ungroup() %>%
    summarize(Summ = paste0(Summ, collapse = ",")) %>%
    pull(Summ)
}


#
# Read submitted ICES data ----    
#

#
# . choose file ----
#

folder_data <- "../../Files_to_ICES/Submission files 2015-2019"
dir(folder_data)
fns <- dir(folder_data, ".NO", full.names = FALSE)
fns
fn_bare <- "NIVA2017CF.no"
fn <- paste0(folder_data, "/", fn_bare)
fn

fn <- "../../Files_to_ICES/2020/Delivered/NIVA2020CF_11.NO"

#
# . read data ----
# 

# data_txt <- readLines(fn)
data_list <- read_ices_file(fn) %>%
  add_field_codes() %>%
  set_numeric()

# str(data_list, 1)

# Check links among tables
# check_all_links(data_list)

#
# . add PARAM_group ----
#
data_list[["10"]] <- data_list[["10"]] %>%
  mutate(
    PARAM_group = case_when(
      nchar(PARAM) == 2 ~ "Metals",
      substr(PARAM,1,2) == "BD" ~ "PBDE",
      substr(PARAM,1,2) == "PF" ~ "PFAS",
      substr(PARAM,1,2) == "CB" ~ "PCB",
      substr(PARAM,1,4) == "HBCD" ~ "Organobromines (other)",
      substr(PARAM,1,2) == "DD" ~ "Organochlorines (other)",
      PARAM %in% c("TDEPP") ~ "Organochlorines (other)",
      substr(PARAM,1,3) %in% c("HCH","HCB") ~ "Organochlorines (other)",
      PARAM %in% c("MCCP","SCCP") ~ "Chloroparaffins",
      PARAM %in% c("TBSN+","TBTIN") ~ "Organotins",
      PARAM %in% "VDSI" ~ "Imposex",
      PARAM %in% c("ACNE", "ACNLE", "ANT", "BAP", "BBJF", 
                   "BGHIP", "BKF", "BAA", "DBA3A", "FLE", 
                   "FLU", "ICDP", "NAP", "PYR") ~ "PAH",
      PARAM %in% c("PYR1OH","PA1OH","BAP3OH","EROD","ALAD") ~ "Biological effects (other)",
      PARAM %in% c("FATWT%","DRYWT%") ~ "Sample parameters",
      PARAM %in% c("LNMEA","WTMEA","GOSOI") ~ "Specimen parameters"
    )
  ) %>%
  arrange(PARAM_group, PARAM)

#
# Uncertainty measures ----
#

xtabs(~METCU + is.na(UNCRT), data_list[["10"]])

tab <- xtabs(~PARAM_group + is.na(UNCRT), data_list[["10"]])
tab

# Uncertainty given, by parameter group  
data_list[["10"]] %>%
  mutate(
    UNCRT_bool = ifelse(is.na(UNCRT), "Lacks UNCRT", "Has UNCRT")) %>%
  count(PARAM_group, UNCRT_bool) %>%
  pivot_wider(names_from = UNCRT_bool, values_from = n, values_fill = 0)

# Type of uncertainty, by parameter group  
data_list[["10"]] %>%
  filter(!is.na(UNCRT)) %>%
  count(PARAM_group, METCU) %>%
  pivot_wider(names_from = METCU, values_from = n, values_fill = 0)

# Type of uncertainty, by parameter group  
data_list[["10"]] %>%
  filter(!is.na(UNCRT)) %>%
  group_by(PARAM_group, METCU) %>%
  summarise(Min = min(UNCRT), Mean = mean(UNCRT), Max = max(UNCRT)) %>%
  pivot_wider(names_from = METCU, names_glue = "{.value}_{METCU}", values_from = c(Min, Mean, Max))

