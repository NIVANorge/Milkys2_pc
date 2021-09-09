######################################################################################################
#
# Functions
#
######################################################################################################

fieldcodes <- list(
  c("RECID","RLABO","CNTRY","MYEAR","RFVER"),
  c("RECID","CRUIS","STNNO","DTYPE","SMPNO","SMLNK","ATIME","NOAGG","SPECI","RLIST","FINFL"),
  c("RECID","CRUIS","STNNO","SMPNO","SUBNO","NOIMP","ORGSP","SEXCO","STAGE","CONES","ASTSA","NODIS","BULKID"),
  c("RECID","CRUIS","STNNO","SMPNO","SUBNO","MATRX","DEPHU","DEPHL","PARAM","MUNIT","BASIS","AMLNK","VFLAG","QFLAG","VALUE","PERCR","SIGND","UNCRT","METCU","DETLI","LMQNT"),
  c("RECID","SLABO","SMLNK","SMTYP","NETOP","MESHS","SAREA","LNSMB","SPEED","PDMET","SPLIT"),
  c("RECID","AMLNK","ALABO","METDC","REFSK","METST","METFP","METPT","METCX","METPS","METOA","AGDET","SREFW","SPECI","RLIST","ORGSP","SIZRF","FORML","ACCRD","ACORG"),
  c("RECID","SHIPC","CRUIS","OWNER","PRDAT"),
  c("RECID","CRUIS","STNNO","LATIT","LONGI","POSYS","SDATE","STIME","ETIME","WADEP","STATN","MPROG","WLTYP","MSTAT","PURPM","EDATE"),
  c("RECID","AMLNK","QALNK","CONCH","CRMCO","CRMMB","CRMMV","MUNIT","CRMSD","CRMNM","CRMPE","DCFLG")
)

names(fieldcodes) <- c("00", "03", "04", "10", "20", "21", "90", "91", "93")


# c("RECID","SLABO","SMLNK","SMTYP","NETOP","MESHS","SAREA","LNSMB","SPEED","PDMET","SPLIT","OBSHT","DURAT","DUREX","ESTFR"),


# 00 = Header
# 03 = Sample record
# 04 = Biota specimen record
# 10 = Parameter/contaminant measurement record
# 20 = Sampling method record

#
# Add field codes to data
# When there are more field codes than variables in the data, the last field codes are just not used
# 
add_field_codes <- function(dataset){
  tbls <- names(dataset)
  for (tbl in tbls){
    # cat(tbl, "\n")
    nc <- ncol(dataset[[tbl]])
    colnames(dataset[[tbl]]) <- fieldcodes[[tbl]][1:nc]
  }
  dataset
}


# function used in strsplit2()
get_last_character <- function(x) substr(x, nchar(x), nchar(x))

#
# strsplit2
# As the base R function strsplit(), except that 
#    1) an extra empty string ("") is added when there is a trailing separator character (by default ";")
#    2) input msut be a single string
#    3) output is a vector, not a list
#
strsplit2 <- function(txt_vector, sep = ";"){
  result <- strsplit(txt_vector, sep)[[1]]
  if (get_last_character(txt_vector) == sep) 
    result <- c(result, "")
  result
}

# test on first line of this part
# strsplit2(txtall[line_no[1]])

# test on entire part
# txtsplit2 <- txtall[line_no] %>% map(strsplit2)
# txtsplit2 %>% map_int(length)

# Function to read a single table
# Input is a vector of strings (each is a line in the table)
txt_to_df <- function(txt_vector, sep = ";"){
  data.frame(matrix(strsplit2(txt_vector, sep = sep), nrow = 1), stringsAsFactors = FALSE)
}

# df <- txtall[line_no] %>% map_df(txt_to_df)
# View(df)

# read_file_part
# Read part of file, i.e. a part starting with a particular table_id
# Input:
#   1. text_vector: vector of text, each representing one line (output of readLines)
#   2. recid: vector of recids, same length as text_vector
#   3. which id we look for (in recid)
# Output: data frame
#
read_file_part <- function(text_vector, recid, table_id, sep = ";"){
  line_no <- which(recid %in% table_id)
  text_vector[line_no] %>% map_df(txt_to_df, sep = sep)
}

# read_file_part(txtall, recid, "03")

# Read all parts of the file into a list of data frames
read_ices_file <- function(fn, sep = ";"){
  txtall <- readLines(fn, encoding = "UTF-8")
  # Get RECID (first field)
  recid_all <- strsplit(txtall, sep) %>% map_chr(function(x) x[1])
  recids <- names(table(recid_all))
  df_list <- recids %>% map(function(x) read_file_part(txtall , recid_all, x, sep = sep))
  names(df_list) <- recids
  df_list
}





report_station <- function(data, station, tissue = "Lever", year = 2019,
                           threshold_samples = 5, threshold_parameters = 50){
  
  cat("=======================================================================================\n")
  cat("Station:", station, "\n")
  tab <- data %>%
    filter(STATION_CODE == station & MYEAR == year & TISSUE_NAME == tissue) %>%
    xtabs(~SAMPLE_NO2 + PARAM, .)
  
  cat("\n")
  cat("In total:", nrow(tab),"samples /", ncol(tab), "parameters \n")
  cat("\n")
  
  #
  # Number of parameters with the given number of samples 
  #
  sumcol <- apply(tab, 2, sum)
  sumcol_tab <- table(sumcol)
  sumcol_tab <- sumcol_tab[order(names(sumcol_tab) %>% as.numeric, decreasing = TRUE)]
  df_samples_per_parameter <- tibble(No_samples = names(sumcol_tab) %>% as.numeric, # number of samples
                                     No_parameters = sumcol_tab)   # number if parameters with the given number of samples  
  
  cat("\n")
  for (i in 1:nrow(df_samples_per_parameter)){
    with(df_samples_per_parameter[i,], cat(No_parameters, "parameters has", No_samples, "samples \n"))  
  }
  
  cat("----------------------------------------------------------------------------------------\n")
  cat("\n\nParameters with", threshold_samples, "or less samples: \n---------------------------------\n")
  names(sumcol)[sumcol <= threshold_samples] %>% paste(collapse = ", ") %>% cat()
  
  #
  # number of samples with the given number of parameters
  # 
  sumrow <- apply(tab, 1, sum)
  sumrow_tab <- table(sumrow) 
  sumrow_tab <- sumrow_tab[order(names(sumrow_tab) %>% as.numeric, decreasing = TRUE)]
  df_parameters_per_sample <- tibble(No_parameters = names(sumrow_tab) %>% as.numeric,  # number of parameters
                                     No_samples = sumrow_tab)   # number of samples with the given number of parameters 
  # Data shown, #2
  
  cat("\n\n")
  cat("----------------------------------------------------------------------------------------\n")
  for (i in 1:nrow(df_parameters_per_sample)){
    with(df_parameters_per_sample[i,], cat(No_samples, "samples has", No_parameters, "parameters \n"))  
  }
  
  cat("\n\nSamples with <", threshold_parameters, "parameters: \n---------------------------------------\n")
  names(sumrow)[sumrow <= threshold_parameters] %>% paste(collapse = ", ") %>% cat()
  cat("\n\n\n")
  
  invisible(tab)
  
}

# Test
# report_station("30B") 
# report_station("43B2", threshold_samples = 7)

