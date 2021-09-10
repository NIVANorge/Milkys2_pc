library(dplyr)
library(tidyr)
library(purrr)
library(safejoin)   # https://github.com/moodymudskipper/safejoin

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Generic data tables ---- 
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# df_tissue <- readRDS("Data/71_df_tissue.rds") 
# df_species <- readRDS("Data/71_df_species.rds") 
# df_taxon <- readRDS("Data/71_df_taxon.rds") 
# df_taxoncodes <- readRDS("Data/71_df_taxoncodes.rds") 

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Species functions ---- 
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Creates a one-frame lookup table 
# Meant for only a single species input!
latin_to_taxid <- function(latin_name){
  df <- df_taxon %>% filter(LATIN_NAME %in% latin_name)
  x <- df_taxoncodes %>% filter(NIVA_TAXON_ID %in% df$NIVA_TAXON_ID) %>% pull(TAXONOMY_CODE_ID)
  tibble(LATIN_NAME = latin_name, TAXONOMY_CODE_ID = x)
}

# Creates a one-frame lookup table 
# Meant for only a single-number input!
taxid_to_latin <- function(taxonomy_code_id){
  df <- df_taxoncodes %>% filter(TAXONOMY_CODE_ID %in% taxonomy_code_id)
  x <- df_taxon %>% filter(NIVA_TAXON_ID %in% df$NIVA_TAXON_ID) %>% pull(LATIN_NAME)
  tibble(TAXONOMY_CODE_ID = taxonomy_code_id, LATIN_NAME = x)
}

# Tests
# latin_to_taxid("Gadus morhua")
# c("Gadus morhua", "Clupea harengus") %>% map_df(latin_to_taxid)
# taxid_to_latin(8850)
# c(8849,8850) %>% map_df(taxid_to_latin)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Functions for creating SQL ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


make_sql_single_specimen <- function(i, data){

  df <- as.data.frame(data)
  
  original_options <- options(useFancyQuotes = FALSE)
  txt <- paste0("insert into NIVADATABASE.BIOTA_SINGLE_SPECIMENS ",
                "(DATE_CAUGHT, STATION_ID, TAXONOMY_CODE_ID, SPECIMEN_NO)\n",  # \n for line shift
                "values (",
                "TO_DATE(", sQuote(df[i, 'DATE_CAUGHT']), ", 'YYYY-MM-DD'), ",
                df[i, 'STATION_ID'], ", ",
                df[i, 'TAXONOMY_CODE_ID'], ", ",
                df[i, 'SPECIMEN_NO'],
                ")"
  )
  options(original_options)
  txt
}

# make_sql_single_specimen(1, biota_single_specimens_eider)
# make_sql_single_specimen(2, biota_single_specimens_eider)



make_sql_sample <- function(i, data){
  
  df <- as.data.frame(data)
  
  original_options <- options(useFancyQuotes = FALSE)
  txt <- paste0("insert into NIVADATABASE.BIOTA_SAMPLES ",
                "(STATION_ID, TISSUE_ID, REPNO, TAXONOMY_CODE_ID, SAMPLE_DATE, SAMPLE_NO)\n",  # \n for line shift
                "values (",
                df[i, 'STATION_ID'], ", ",
                df[i, 'TISSUE_ID'], ", ",
                df[i, 'REPNO'], ", ",
                df[i, 'TAXONOMY_CODE_ID'], ", ",
                "TO_DATE(", sQuote(df[i, 'SAMPLE_DATE']), ", 'YYYY-MM-DD'), ",
                df[i, 'SAMPLE_NO'],
                ")"
  )
  options(original_options)
  txt
}


make_sql_samples_specimens <- function(i, data){
  
  df <- as.data.frame(data)
  
  original_options <- options(useFancyQuotes = FALSE)
  txt <- paste0("insert into NIVADATABASE.BIOTA_SAMPLES_SPECIMENS ",
                "(SAMPLE_ID, SPECIMEN_ID)\n",  # \n for line shift
                "values (",
                df[i, 'SAMPLE_ID'], ", ",
                df[i, 'SPECIMEN_ID'],
                ")"
  )
  options(original_options)
  txt
}

# Test
# make_sql_samples_specimens(1, biota_sample_specimens_eider)


#
# BIOTA_CHEMISTRY_VALUES
#

# "VALUE_ID"              - Let the database decide
# "SAMPLE_ID"             - From the database, after BIOTA_SAMPLES have been inserted
# "METHOD_ID"             - Lookup based on NAME and UNIT
# "VALUE"                 - From data
# "FLAG1"                 - From data
# "FLAG2"                 - NA
# "ENTERED_BY"            - DHJ
# "ENTERED_DATE"          - date, see above
# "REMARK"                - NA
# "DETECTION_LIMIT"       - NA
# "UNCERTAINTY"           - NA
# "QUANTIFICATION_LIMIT"  - NA
# "APPROVED"              - NA?


make_sql_chemistry_values <- function(i, data){
  
  df <- as.data.frame(data)
  
  original_options <- options(useFancyQuotes = FALSE)

  flag <- df[i, 'FLAG1']
  txt <- paste0("insert into NIVADATABASE.BIOTA_CHEMISTRY_VALUES ",
                "(SAMPLE_ID, METHOD_ID, VALUE, FLAG1, APPROVED)\n",  # \n for line shift
                "values (",
                df[i, 'SAMPLE_ID'], ", ",
                df[i, 'METHOD_ID'], ", ",
                round(df[i, 'VALUE'], 6), ", ",
                ifelse(is.na(flag), "NULL", sQuote(flag)), ", ",
                1,
                ")"
  )
  options(original_options)
  txt
}
# Test
# make_sql_chemistry_values(1, biota_chemistry_values_eider)

#
# For "select all" - NOT FINISHED!
#
# make_sql_chemistry_values_intoall <- function(lines, data){
#   
#   df <- as.data.frame(data)
#   data_section <- make_sql_chemistry_values_single <- function( data)
#   
#   original_options <- options(useFancyQuotes = FALSE)
#   txt <- paste0("insert all\n",
#                 data_section,
#                 "select 1 from dual"
#   )
#   options(original_options)
#   txt
# }

#
# For "select all" - NOT FINISHED!
#

# make_sql_chemistry_values_single <- function(i, data){
#   
#   df <- as.data.frame(data)
#   
#   original_options <- options(useFancyQuotes = FALSE)
#   
#   flag <- round(df[i, 'FLAG1'], 6)
#   txt <- paste0("    into NIVADATABASE.BIOTA_CHEMISTRY_VALUES ",
#                 "    (SAMPLE_ID, METHOD_ID, VALUE, FLAG1,APPROVED)\n",  # \n for line shift
#                 "    values (",
#                 df[i, 'SAMPLE_ID'], ", ",
#                 df[i, 'METHOD_ID'], ", ",
#                 round(df[i, 'VALUE'], 6), ", ",
#                 ifelse(is.na(flag), "NULL", sQuote(flag)),
#                 1,
#                 ")"
#   )
#   options(original_options)
#   txt
# }



make_sql_methods <- function(i, data){
  
  df <- as.data.frame(data)
  
  original_options <- options(useFancyQuotes = FALSE)
  
  name <- df[i, 'NAME']
  unit <- df[i, 'UNIT']
  lab <- df[i, 'LABORATORY']
  method_ref <- df[i, 'METHOD_REF']
  matrix <- df[i, 'MATRIX']
  cas <- df[i, 'CAS']
  txt <- paste0(
    "insert into NIVADATABASE.METHOD_DEFINITIONS ",
    "(NAME, UNIT, LABORATORY, METHOD_REF, MATRIX, CAS, MATRIX_ID)\n",  # \n for line shift
    "values (",
    ifelse(is.na(name), "NULL", sQuote(name)), ", ",
    ifelse(is.na(unit), "NULL", sQuote(unit)), ", ",
    ifelse(is.na(lab), "NULL", sQuote(lab)), ", ",
    ifelse(is.na(method_ref), "NULL", sQuote(method_ref)), ", ",
    ifelse(is.na(matrix), "NULL", sQuote(matrix)), ", ",
    ifelse(is.na(cas), "NULL", sQuote(cas)), ", ",
    df[i, 'MATRIX_ID'],
    ")"
  )
  options(original_options)
  txt
}

# See script 75:
# make_sql_methods(1, new_methods)


#
# Helper functions for making SQL parts
#
# Takes the unique values of a variable and puts them in a bracket (sql style)
# 
#

make_sql_ids <- function(data, variable){
  values <- data[[variable]] %>% unique()
  if (class(data[[variable]]) == "character"){
    original_options <- options(useFancyQuotes = FALSE)
    values <- sQuote(values)
    options(original_options)
  }
  paste0("(",
         values %>% paste(collapse = ","),
         ")")
}

# make_sql_ids(biota_samples, "STATION_ID")      
# "(46980,47221,50478,67807,69711)"
#
# make_sql_ids(biota_chemistry_values, "FLAG1")
# "('<','NA')"

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Functions for creating BIOTA_SAMPLES_SPECIMENS ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# NOTE:  
#  * Works only for a single station/species!! (but for several tissues)  
#    For several stations, you can use map() and map2(); see script 73 part 11d
#  * Assumes that data for BIOTA_SINGLE_SPECIMENS and BIOTA_SAMPLES has already been made 
#  * ...and also that we have entered these into the database and retrieved the correct SPECIMEN_ID and SAMPLE_ID  
#  * Input data for add_ids is 'sampletable', which is a table with one row per fish 
#    (with a uniquely numbered column named "SPECIMEN_NO" included)  
# * Furthermore, 'sampletable' must have has one column per tissue, names equal to tissue names or defined in 'columnnames'
# * I.e., in add_ids(), argument 'columnnames' is optional. If column names = TISSUE_NAME names (e.g. "Muskel"), you 
#      don't have to specify this lookup table. See script 70 for use of 'columnnames' and script 71 for an example where it is not used.

#
# Example of data set defining column names used in data 
#
# NOTE: 'data' in Must have column names "TISSUE_NAME", "Colname"
# UNCOMMENT NEXT LINES TO USE:
# columnnames_tissue <- read.table(textConnection("
# TISSUE_NAME        Colname
#      Muskel  Sample_muscle
#       Lever   Sample_liver
# "), header = TRUE, stringsAsFactors = FALSE)


# Input:
#   tissue name: one TISSUE_NAME    
#   biota_samples: one line per physical sample, 
#     columns SPECIMEN_NO, SAMPLE_NO, TISSUE_NAME
# Output:
#   Data frame with one line per physical sample for that tissue
#   2 columns: 1) SAMPLE_NO,  2) column named after tissue
get_tissue_samples <- function(tissue_name, biota_samples){
  result <- biota_samples %>% 
    filter(TISSUE_NAME == tissue_name) %>% 
    select(SAMPLE_NO, SAMPLE_ID) %>% 
    as.data.frame()
  colnames(result)[2] <- tissue_name
  result
}


# Test 1
# get_tissue_samples(23, ex_BIOTA_SAMPLES)
# Test 2
# tissuesamples_list <- c("Muskel", "Lever) %>% map(get_tissue_samples, ex_BIOTA_SAMPLES)


# Inputs:
#    'sampletable': a table with one line per specimen and one column per tissue  
#       The columns must either have names identical to TISSUE_NAME, or one
#       must supply a lookup table 'columnnames' (see example above)
#    single_specimens: has one line per specimen, columns SPECIMEN_ID, SPECIMEN_NO
#    tissuesamples: is a list of data frames, one per tissue type
#    tissues: character vector with the names of the tissues(in same order as tissuesamples)
#    columnnames: lookup table for column names
add_ids <- function(sampletable, 
                    single_specimens, 
                    tissuesamples, 
                    tissues,
                    columnnames = NULL){
  if (is.null(columnnames)){
    result <- sampletable[c("SPECIMEN_NO", tissues)]
  } else {
    colnames <- tibble(TISSUE_NAME = tissues) %>% 
      safe_left_join(columnnames) %>%
      pull(Colname)
    result <- sampletable[c("SPECIMEN_NO", colnames)]
  }
  result <- result %>%
    safe_left_join(single_specimens[c("SPECIMEN_ID", "SPECIMEN_NO")], by = "SPECIMEN_NO", check = "v")
  for (tissuesample in tissuesamples){
    tissue <- colnames(tissuesample)[2]
    if (is.null(columnnames)){
      colname <- tissue
    } else {
      colname <- subset(columnnames, TISSUE_NAME == tissue)$Colname
    }
    colnames(result)[colnames(result) == colname] <- "SAMPLE_NO"
    result <- result %>%
      left_join(tissuesample, by = "SAMPLE_NO") %>%
      select(-SAMPLE_NO)    # remove SAMPLE_NO, otherwise there will be confusion when the loop is in round 2
  }
  result
}
# Test
# id_table <- add_ids(ex_data, ex_BIOTA_SINGLE_SPECIMENS, tissuesamples_list)

gather_ids <- function(fish_to_id_table, tissues){
  df <- fish_to_id_table[c("SPECIMEN_ID", tissues)] 
  df %>% tidyr::gather("TISSUE_NAME", "SAMPLE_ID", -SPECIMEN_ID) %>%
    filter(!is.na(SAMPLE_ID)) %>%
    arrange(SPECIMEN_ID, SAMPLE_ID) 
}

# Test
# gather_ids(id_table)


#
# Functions for reading from database
#

# TEST
sql_test <- "
select s.STATION_CODE, a.STATION_ID, a.SPECIMEN_ID, a.SPECIMEN_NO, a.DATE_CAUGHT, a.TAXONOMY_CODE_ID, 
b.SAMPLE_ID, c.TISSUE_ID, 
d.METHOD_ID, d.VALUE, d.FLAG1,
e.NAME, e.UNIT, e.LABORATORY
from NIVADATABASE.PROJECTS_STATIONS s
join NIVADATABASE.BIOTA_SINGLE_SPECIMENS a on s.STATION_ID = a.STATION_ID
join NIVADATABASE.BIOTA_SAMPLES_SPECIMENS b on a.SPECIMEN_ID = b.SPECIMEN_ID
join NIVADATABASE.BIOTA_SAMPLES c on b.SAMPLE_ID = c.SAMPLE_ID
join NIVADATABASE.BIOTA_CHEMISTRY_VALUES d on c.SAMPLE_ID = d.SAMPLE_ID
left join NIVADATABASE.METHOD_DEFINITIONS e on d.METHOD_ID = e.METHOD_ID
where s.STATION_CODE = '19N'
and DATE_CAUGHT > TO_DATE('2017-03-01', 'YYYY-MM-DD')
and DATE_CAUGHT < TO_DATE('2018-03-01', 'YYYY-MM-DD')"

sql_get_biota_chemistry_station <- "
select s.STATION_CODE, a.STATION_ID, a.SPECIMEN_ID, a.SPECIMEN_NO, a.DATE_CAUGHT, a.TAXONOMY_CODE_ID, 
b.SAMPLE_ID, c.TISSUE_ID, 
d.METHOD_ID, d.VALUE, d.FLAG1,
e.NAME, e.UNIT, e.LABORATORY
from NIVADATABASE.PROJECTS_STATIONS s
join NIVADATABASE.BIOTA_SINGLE_SPECIMENS a on s.STATION_ID = a.STATION_ID
join NIVADATABASE.BIOTA_SAMPLES_SPECIMENS b on a.SPECIMEN_ID = b.SPECIMEN_ID
join NIVADATABASE.BIOTA_SAMPLES c on b.SAMPLE_ID = c.SAMPLE_ID
join NIVADATABASE.BIOTA_CHEMISTRY_VALUES d on c.SAMPLE_ID = d.SAMPLE_ID
left join NIVADATABASE.METHOD_DEFINITIONS e on d.METHOD_ID = e.METHOD_ID
where s.STATION_CODE = '%s'
and DATE_CAUGHT > TO_DATE('%i-03-01', 'YYYY-MM-DD')
and DATE_CAUGHT < TO_DATE('%i-03-01', 'YYYY-MM-DD')"

get_biota_chemistry_station <- function(station_code, myear){
  sql <- sprintf(sql_get_biota_chemistry_station, 
                 station_code, myear, myear + 1)
  df <- get_nivabase_data(sql) %>%
    rename(PARAM = NAME)
  # Get table with latin names
  df_latin <- df$TAXONOMY_CODE_ID %>% unique() %>% map_df(taxid_to_latin)
  df <- df %>%
    left_join(df_latin, by = "TAXONOMY_CODE_ID") %>%
    left_join(df_tissue, by = "TISSUE_ID") %>%
    select(STATION_CODE, DATE_CAUGHT, LATIN_NAME, SPECIMEN_NO, TISSUE_NAME, PARAM, VALUE, FLAG1, UNIT, 
           STATION_ID, SPECIMEN_ID, TAXONOMY_CODE_ID, SAMPLE_ID, TISSUE_ID, METHOD_ID, LABORATORY)
  }



