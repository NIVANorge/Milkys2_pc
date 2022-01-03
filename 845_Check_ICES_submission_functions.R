
#
# COPIED FROM Milkys PROJECT, SCRIPT 24_Check_ICES_files_functions.R
#

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
  c("RECID","SLABO","SMLNK","SMTYP","NETOP","MESHS","SAREA","LNSMB","SPEED","PDMET","SPLIT","OBSHT","DURAT","DUREX","ESTFR"),
  c("RECID","AMLNK","ALABO","METDC","REFSK","METST","METFP","METPT","METCX","METPS","METOA","AGDET","SREFW","SPECI","RLIST","ORGSP","SIZRF","FORML","ACCRD","ACORG"),
  c("RECID","SHIPC","CRUIS","OWNER","PRDAT"),
  c("RECID","CRUIS","STNNO","LATIT","LONGI","POSYS","SDATE","STIME","ETIME","WADEP","STATN","MPROG","WLTYP","MSTAT","PURPM","EDATE"),
  c("RECID","AMLNK","QALNK","CONCH","CRMCO","CRMMB","CRMMV","MUNIT","CRMSD","CRMNM","CRMPE","DCFLG")
)
names(fieldcodes) <- c("00", "03", "04", "10", "20", "21", "90", "91", "93")

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
    nc <- ncol(dataset[[tbl]]) - 1
    new_colnames <- c(fieldcodes[[tbl]][1:nc], "Line_no")
    colnames(dataset[[tbl]]) <- new_colnames
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
  # extract data frame from text strings  
  result <- text_vector[line_no] %>% map_df(txt_to_df, sep = sep)
  # add line number form original file
  result$Line_no <- line_no
  result
}

# read_file_part(txtall, recid, "03")


# Read all parts of the file into a list of data frames
read_ices_file <- function(fn, sep = ";"){
  txtall <- readLines(fn)
  # Get RECID (first field)
  recid_all <- strsplit(txtall, sep) %>% map_chr(function(x) x[1])
  recids <- names(table(recid_all))
  df_list <- recids %>% map(function(x) read_file_part(txtall , recid_all, x, sep = sep))
  names(df_list) <- recids
  df_list
}

#
# Set numeric
#
set_numeric <- function(list_of_data){
  
  # Set some columns to be numeric 
  vars_numeric <- c("SMPNO", "SUBNO", "DEPHU", "DEPHL", "AMLNK", "VALUE", "UNCRT", "DETLI")
  
  #  Assume 9 meaningful tables in list
  for (i in 1:9){
    #  For each table, go through columns
    for (j in 1:ncol(list_of_data[[i]])){
      # Set column to numeric, if its on the list:
      if (names(list_of_data[[i]])[j] %in% vars_numeric)
        list_of_data[[i]][,j] <- as.numeric(list_of_data[[i]][,j])
    }
  }
  list_of_data
}



# Write ICES file (based on output of read_ices_file)
# I.e. the reverse of read_ices_file()
write_ices_file <- function(data, fn){
  tables <- names(fieldcodes)
  icesfile <- file(fn, "w+")
  for (table in tables){
    write.table(data[[table]], icesfile, sep = ";", quote = FALSE, na = "", col.names = FALSE, row.names = FALSE)
  }
  close(icesfile)
}

#
# Checking which columns/variables that are present in both tables
# Variables listed in "drop" (as a string or string vector) not included
#

var_overlap <- function(tab1, tab2, drop = NULL){
  c1 <- colnames(tab1)  
  c2 <- colnames(tab2)  
  result <- c1[c1 %in% c2]
  if (!is.null(drop))
    result <- result[!result %in% drop]
}

# Test
# var_overlap(data[["04"]], data[["10"]])


#
# Check links between two tables (tab1 and tab2) 
# linked using variables listed in "by"
#
# Prints to screen if print = TRUE
# Silently returns named vector with 
#   number of rows (n1, n2)
#   number of rows with missing values in join variables (na1, na2)
#   lack_links1 lack_links1_comb      lack_links2 

check_link <- function(tab1, tab2, by, print = TRUE, print_values = FALSE, print_values_max = 50){
  # Select columns
  # Also set data to tibble, otherwise one-variable data frames becomes vectors - see below
  tab1_check <- as_tibble(tab1[c(by, "Line_no")])
  tab2_check <- as_tibble(tab2[c(by, "Line_no")])
  tab1_complete <- complete.cases(tab1[by])
  tab2_complete <- complete.cases(tab2[by])
  m1 <- nrow(tab1) - sum(tab1_complete)
  m2 <- nrow(tab2) - sum(tab2_complete)
  tab1_check <- tab1_check[tab1_complete,] # if data is *not* a tibble and there is only one variable, this will be a vector!
  tab2_check <- tab2_check[tab2_complete,]
  df1 <- anti_join(tab1_check, tab2_check, by = by)
  df2 <- anti_join(tab2_check, tab1_check, by = by)
  stat <- c(n1 = nrow(tab1), n2 = nrow(tab2), 
            na1 = m1, na2 = m2, 
            lack_links1 = nrow(df1),
            lack_links1_comb = df1 %>% count(across()) %>% nrow(),
            lack_links2 = nrow(df2),
            lack_links2_comb = df2 %>% count(across()) %>% nrow()
  )
  # missing_in_1 = tab2_check
  # print(stat)
  if (print){
    cat("Variables used for join:", paste(by, collapse = ", "), "\n")
    cat("Table 1 has ",  stat["n1"], " rows (", stat["na1"], " rows with missing values in join variables)\n", sep = "")
    cat("Table 2 has ",  stat["n2"], " rows (", stat["na2"], " rows with missing values in join variables)\n", sep = "")
    if (nrow(df1) == 0)
      cat("All variable combinations in table 1 exist in table 2\n")
    if (nrow(df1) > 0){
      cat("\nTable 1 has ",  stat["lack_links1"], " rows (", stat["lack_links1_comb"],  
          " variable combinations) without corresponding rows in table 2\n", sep = "")
      if (print_values)
        cat("Line numbers:", paste(df1$Line_no, collapse = ","), "\n")
      for (v in by){
        cat("  Number of ", v, " values in missing rows: ", length(unique(df1[[v]])), "\n", sep = "")
        if (print_values){
          unique_values <- unique(df1[[v]])
          cat("    - ", paste(head(unique_values, print_values_max), collapse = "; "), "\n", sep = "")
          if (length(unique_values) > print_values_max)
            cat("Only the first", print_values_max, "values are shown (for more, set print_values_max = ...) \n")
        }
      }
    }
    if (nrow(df2) == 0)
      cat("All variable combinations in table 2 exist in table 1\n")
    if (nrow(df2) > 0){
      cat("\nTable 2 has ",  stat["lack_links2"], " rows (", stat["lack_links2_comb"],  
          " variable combinations) without corresponding rows in table 1\n", sep = "")
      if (print_values)
        cat("Line numbers:", paste(df2$Line_no, collapse = ","), "\n")
      for (v in by){
        cat("  Number of ", v, " values in missing rows: ", length(unique(df2[[v]])), "\n", sep = "")
        if (print_values){
          unique_values <- unique(df2[[v]])
          cat("    - ", paste(head(unique_values, print_values_max), collapse = "; "), "\n", sep = "")
          if (length(unique_values) > print_values_max)
            cat("Only the first", print_values_max, "values are shown (for more, set print_values_max = ...) \n")
        }
      }
    }
  }
  invisible(stat)
}


#
# col_overlap - FOR TESTING 
#
# debugonce(check_link)
if (FALSE){
  # Get key variables (RECID is the table number, not a key variable for the link)
  cols <- col_overlap(data[["04"]], data[["10"]], drop = "RECID")
  
  # Example 1
  cat("All ok (All keys in table 1 are in table 2 and vice verca):\n")
  check_link(data[["04"]], 
             data[["10"]], 
             cols)
  
  # Example 2
  cat("\nTable 1 has missing rows:\n")
  check_link(data[["04"]] %>% filter(!STNNO == "15B"), 
             data[["10"]], 
             cols)
  
  cat("\n...with detailed output:\n")
  check_link(data[["04"]] %>% filter(!STNNO == "15B"), 
             data[["10"]], 
             cols,
             print_values = TRUE)
  
  # Example etc
  cat("\nTable 1 has key variables with missing data:\n")
  check_link(data[["04"]] %>% mutate(STNNO = ifelse(STNNO == "15B", NA, STNNO)), 
             data[["10"]], 
             cols)
  
  # Example etc
  cat("\nTable 2 has missing rows:\n")
  check_link(data[["04"]], 
             data[["10"]] %>% filter(!STNNO == "02B"), 
             cols)
  
  # Example etc
  cat("\nTable 1 and table 2 has missing rows:\n")
  check_link(data[["04"]] %>% filter(!STNNO == "15B"), 
             data[["10"]] %>% filter(!STNNO == "02B"), 
             cols,
             print_values = TRUE)
  
}


#
# Checking all links between tables
#

check_all_links <- function(data, print_values = FALSE, ...){
  
  # Get key variables (RECID is the table number, not a key variable for the link)
  cat("*** 03 (sample occasion per station) and 04 (samples for chemical analysis) ***\n")
  df1 <- data[["03"]]
  df2 <- data[["04"]]
  cols <- var_overlap(df1, df2, drop = c("RECID", "Line_no"))
  check_link(df1, df2, cols, print_values = print_values, ...)
  cat("\n")
  
  cat("*** 04 (samples for chemical analysis) and 10 (measurements by parameter) ***\n")
  df1 <- data[["04"]]
  df2 <- data[["10"]]
  cols <- var_overlap(df1, df2, drop = c("RECID", "Line_no"))
  check_link(df1, df2, cols, print_values = print_values, ...)
  cat("\n")
  
  cat("*** 10 (measurements by parameter) and 21 (chemical methods) ***\n")
  df1 <- data[["10"]]
  df2 <- data[["21"]]
  cols <- var_overlap(df1, df2, drop = c("RECID", "Line_no"))
  check_link(df1, df2, cols, print_values = print_values, ...)
  cat("\n")
  
  cat("*** 03 (sample occasion per station) and 20 (sampling methods) ***\n")
  df1 <- data[["03"]]
  df2 <- data[["20"]]
  cols <- var_overlap(df1, df2, drop = c("RECID", "Line_no"))
  check_link(df1, df2, cols, print_values = print_values, ...)
  cat("\n")
  
  cat("*** 03 (sample occasion per station) and 90 (cruises) ***\n")
  df1 <- data[["10"]]
  df2 <- data[["90"]]
  cols <- var_overlap(df1, df2, drop = c("RECID", "Line_no"))
  check_link(df1, df2, cols, print_values = print_values, ...)
  cat("\n")
  
  cat("*** 04 (samples for chemical analysis) and 91 (stations) ***\n")
  df1 <- data[["04"]]
  df2 <- data[["91"]]
  cols <- var_overlap(df1, df2, drop = c("RECID", "Line_no"))
  check_link(df1, df2, cols, print_values = print_values, ...)
  cat("\n")
  
}
