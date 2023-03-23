
#
# Functions for reading from ICES web service
#
# Used in scripts 26 and 86

#
# NOTE: The 'get_ices_data' function is old and doesn't use the current web service, soo it appears to returen a lot less data
# Also it is quit inefficiently programmed.
# Instead use 'get_ices_biotadata' further down (in the "NEW FUNCTIONS" section)
#


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# OLD FUNCTIONS ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# global var

library(XML)
# library(plyr)  # Loading this may mask a lot of dplyr commands (arrange, count, desc, failwith, id, mutate, rename, summarise, summarize)
                 # So we use plyr::, and assumed that it is installed

xml.url <- "http://ecosystemdata.ices.dk/webservices/EcoSystemWebServices.asmx/getListParameters"

get_ices_parameter_table <- function(){

  xmlfile <- xmlTreeParse(xml.url)
  xmltop = xmlRoot(xmlfile)

  # Make list(I don't know why we need to specify "1:length(xmltop)", if we write just "xmltop" it doesn't work
  df_par_list <- plyr::llply(xmltop[1:length(xmltop)], function(X) as.data.frame(t(xmlSApply(X, xmlValue)), stringsAsFactors = FALSE))

  # Turn list into data frame
  plyr::ldply(df_par_list, rbind)
  }

# Test
# df_par <- get_ices_parameter_table()
# Get mercury code
# subset(df_par, parameterName == "mercury")
# PYR in bile
# subset(df_par, grepl("PYR1", parameterCode))

  
# used by get_ices_data'
list_to_df <- function(obj) {
  plyr::ldply(obj, function(x) ifelse(is.na(x[1]), "NA", x[1]))
  }

#
# get_ices_data()
#   returns ICES data for a single parameter in all stations, one or all years
#   assumes that a parameter table (by default named df_par) exists
#
# param: ICES parameter code (e.g., "HG", "PYR1OH"). See table produced by 'get_ices_parameter_table()'
# year:  a single year, or "" for all years
# parameter_table: the output of get_ices_parameter_table()
#
# note 1: pretty slow, can probably be improved (see comments in code)
# note 2: handles some of the Norwegian letters ("??","??","??"), but not all (such as capital "??")
#
get_ices_data <- function(param, year, parameter_table = df_par){
  
  # if (missing(parameter_table))
  #   stop("'parameter_table' not defined - run 'get_ices_parameter_table()' to create a parameter table")
  
  sel <- parameter_table[,"parameterCode"] %in% param
  if (sum(sel)==0)
    stop("parameter ", param, " not found")
  key <- parameter_table[sel, "key"]
  xml.url <- paste0("http://ecosystemdata.ices.dk/webservices/EcoSystemWebServices.asmx/getICESDataPortalData?year=", year, 
                    "&datetime=&dataset=&datatype=&parametergroup=&parameter=", key, "&taxa=&matrix=&area=&minLatitude=&maxLatitude=&minLongitude=&maxLongitude=")
  
  
  # Use the xmlTreePares-function to parse xml file directly from the web
  xmlfile <- xmlTreeParse(xml.url)

  # the xml file is now saved as an object you can easily work with in R:
  # class(xmlfile)
  # Use the xmlRoot-function to access the top node
  xmltop = xmlRoot(xmlfile)
  
  # have a look at the XML-code of the first subnodes:
  # print(xmltop)[1:2]
  # print(xmltop)[1]
  # length(xmltop)
  # str(xmltop[1:3], 1)
  # str(xmltop[[1]], 1)
  
  # To extract the XML-values from the document, use xmlSApply:
  # 5 secs
  ices_data <- xmlSApply(xmltop, function(x) xmlSApply(x, xmlValue))
  
  # length(ices_data)
  # 1954
  
  # str(ices_data[1:3], 1)
  # str(ices_data[[1]], 1)
  
  
  #
  # Problem: not all elements have the same variables
  #   For instance:
  #     v$.id   # 29 variables
  #     list_to_df(ices_data[[5]])$.id   # 28 variables; Sex is lacking
  #
  # Solution: list_to_df() creates a "transposed" data frame where column 1 (.id) is the variables and column 2 is a single line of data
  #   We use merge, so the right data is added to the right columns, and since we use "all = TRUE", new variables are added as we go
  #   through the data set and new variables may show up.
  #   Afterwards the data frame is transposed back to the correct form, column names are added, and numeric strings are 
  #   turned into numeric variables.
  # Afterthought: one merge() per line is pretty slow. Might be faster to collect all records with the same variables and join them together by cbind(),
  #   and then merge the data with different variable sets using merge().
  #
  
  # n <- 200    # for test
  n <- length(ices_data)
  
  # Create list
  ices_list <- plyr::llply(ices_data[1:n], list_to_df)
  
  # Initialize transposed dataframe
  ices_df_trans <- ices_list[[1]]
  
  # Add records (actually columns) to transposed dataframe, one by one
  # Ca. 1 minute
  for (i in 2:n){
    colnames(ices_list[[i]])[2] <- paste0("V", i)
    ices_df_trans <- merge(ices_df_trans, ices_list[[i]], all = TRUE)
    }

  # ices_df_trans$.id
  
  #
  # The data frame is transposed back to the correct form, column names are added
  #
  ices_df <- data.frame(t(ices_df_trans[,-1]), stringsAsFactors = FALSE)
  colnames(ices_df) <- ices_df_trans$.id
  # head(ices_df)
  
  #
  # Numeric strings are turned into numeric variables
  #
  # dput(ices_df_trans$.id)    # used just to make a list of variables that can be edited
  
  numeric_cols <- c("Cruise", "DayCollected", "depth", "depthClass", "depthLower", "depthUpper", 
    "Latitude", "lengthClass", "Longitude", "MonthCollected", "noinp", "originalValue", 
    "value", "ValuePrecision", "YearCollected")
  
  for (col in numeric_cols){
    ices_df[,col] <- as.numeric(ices_df[,col])
    }

  ices_df[,"Station"] <- gsub("????", "??", ices_df[,"Station"], fixed = TRUE)
  ices_df[,"Station"] <- gsub("?????", "??", ices_df[,"Station"], fixed = TRUE)
  ices_df[,"Station"] <- gsub("????", "??", ices_df[,"Station"], fixed = TRUE)

  ices_df
  }

# Test

# Hg data for one year
# df <- get_ices_data("HG", 1991)
# PYR1OH data for all years
# df <- get_ices_data("PYR1OH", "")

# library(dplyr)
# get_ices_data_multyear <- function(param, years, parameter_table = df_par)
#   years %>% purrr::map_df(~get_ices_data(param=param, year = ., parameter_table = parameter_table))

# df_cd <- get_ices_data_multyear("CD", 1998:2007)
# head(df, 3)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# NEW FUNCTIONS ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# get_ices_biotadata
# New version (Jan 2020) of 'get_ices_data' for getting data (biota data only)
#
# 1. Using the following web service: http://dome.ices.dk/Webservices/index.aspx
# 2. Using url2 (which reads the data as UTF-8) insead of package XML
# 3. Much more effective code (using package purrr)
#
# Needs only PARAM to be set
#
# For other parameters, see examples below
# Note: don't know what it wants for "species"
#   (also tried code from http://ecosystemdata.ices.dk/webservices/EcoSystemWebServices.asmx/getListSpecies )
# 

# Country codes: https://vocab.ices.dk/?ref=22 
#   NOTE: downloaded to "00_read_ICES_webservice_functions_countries.csv"
# Matrix codes: MU, LI, SB etc. 
# Parameter groups (paramgroup): https://vocab.ices.dk/?ref=78 

# Example:
# All PYR1OH data from Norway  
# df1 <- get_ices_biotadata(param = "PYR1OH", country = 58)


get_ices_biotadata <- function(param = "", yearstart = NULL, yearend = yearstart, 
                               country = "", matrix = "", lab = "", species = "",
                               paramgroup = ""){
  if (is.null(yearstart)){
    yearstart <- ""
  } else {
    yearstart <- as.character(yearstart)
  }
  if (is.null(yearend)){
    yearend <- ""
  } else {
    yearend <- as.character(yearend)
  }
  if (is.numeric(country)){
    country <- sprintf("%02.f", country)
  }
  url_part1 <- "http://dome.ices.dk/Webservices/DOMEWebServices.asmx/selectContaminantsInBiota?"
  url_part2_txt <- "PARAM=%s&RLABO=%s&ALABO=&yearBegining=%s&yearEnd=%s&MATRX=%s&TAXA=%s&PURPM=&MPROG=&Area=&CNTRY=%s&ParamGroup=%s"
  url_part2 <- sprintf(url_part2_txt, 
                       param, lab, yearstart, yearend, matrix, species, country, paramgroup)
  xml_url <- paste0(url_part1, url_part2)
  cat("Querying dome.ices.dk using the following URL:\n")
  cat(xml_url)
  cat("\n")
  # OLD (using package XML)
  # xmlfile <- xmlTreeParse(xml_url, encoding = "UTF-8")
  # xmltop = xmlRoot(xmlfile)
  # list_of_vectors <- xmlSApply(xmltop, function(x) xmlSApply(x, xmlValue, encoding = "UTF-8"))
  # list_of_dataframes <- purrr::map(list_of_vectors, vector_to_dataframe)
  # NEW (using package xml2)
  df <- xml_to_dataframe(xml_url)
  if (!is.null(df)){
    numeric_vars <- c("MYEAR", "Latitude", "Longitude", "Value", "UNCRT", 
                      "SUBNO", "BULKID", 
                      "tblAnalysisID", "tblParamID", "tblBioID", "tblSampleID")
    for (var in numeric_vars){
      if(var %in% names(df))
        df[[var]] <- as.numeric(df[[var]])
    }
    df$DATE <- lubridate::dmy(df$DATE)
  }
  df
}

if (FALSE){
  # debugonce(get_ices_biotadata)
  X <- get_ices_biotadata("HG", yearstart = 2018, lab = "NIVA")   # 371 lines
  X <- get_ices_biotadata("HG", yearstart = 2018, country = 58)    # 371 lines (58 = Norway; see https://vocab.ices.dk/?ref=22)
  X <- get_ices_biotadata("HG", yearstart = 2018)   # 1309 lines (all countries)
  X <- get_ices_biotadata("HG", yearstart = 2018, lab = "NIVA")   # 371 lines
  X <- get_ices_biotadata("HG", yearstart = 2018, lab = "NIVA", species = "Gadus morhua")     # doesn't work
  X <- get_ices_biotadata("HG", yearstart = 2018, lab = "NIVA", species = "gadus morhua")     # doesn't work
  X <- get_ices_biotadata("HG", yearstart = 2018, lab = "NIVA", species = "Gadus%20morhua")   # doesn't work
  X <- get_ices_biotadata("HG", yearstart = 2018, lab = "NIVA", species = "5558")  # doesn't work either (code from http://ecosystemdata.ices.dk/webservices/EcoSystemWebServices.asmx/getListSpecies)
  X <- get_ices_biotadata("HG", yearstart = 2018, lab = "NIVA", matrix = "MU")   # 251 lines
  X <- get_ices_biotadata("HG", yearstart = 2009, yearend = 2010, lab = "NIVA", matrix = "MU")   # 251 lines
  # Error handling is not too good:
  X <- get_ices_biotadata("Bogus", yearstart = 2018, lab = "NIVA")
  X <- get_ices_biotadata("", paramgroup = "O-MET", yearstart = 2018, lab = "NIVA", matrix = "SB")   # all organic metals (TBT etc)
}


get_ices_sedimentdata <- function(param, yearstart = NULL, yearend = yearstart, 
                                  country = "", lab = "", 
                                  paramgroup = ""){
  if (is.null(yearstart)){
    yearstart <- ""
  } else {
    yearstart <- as.character(yearstart)
  }
  if (is.null(yearend)){
    yearend <- ""
  } else {
    yearend <- as.character(yearend)
  }
  # url_part1 <- "http://dome.ices.dk/Webservices/DOMEWebServices.asmx/selectContaminantsInBiota?"
  url_part1 <- "http://dome.ices.dk/Webservices/DOMEWebServices.asmx/selectContaminantsInSediment?"
  # "PARAM=CD&RLABO=ALUK&ALABO=&yearBegining=&yearEnd=&MATRX=&TAXA=&PURPM=&MPROG=&Area=&CNTRY=&ParamGroup="
  url_part2_txt <- "PARAM=%s&RLABO=%s&ALABO=&yearBegining=%s&yearEnd=%s&MATRX=&TAXA=&PURPM=&MPROG=&Area=&CNTRY=%s&ParamGroup=%s"
  url_part2 <- sprintf(url_part2_txt, 
                       param, lab, yearstart, yearend, country, paramgroup)
  xml_url <- paste0(url_part1, url_part2)
  cat("Querying dome.ices.dk using the following URL:\n")
  cat(xml_url)
  cat("\n")
  # OLD (using package XML)
  # xmlfile <- xmlTreeParse(xml_url, encoding = "UTF-8")
  # xmltop = xmlRoot(xmlfile)
  # list_of_vectors <- xmlSApply(xmltop, function(x) xmlSApply(x, xmlValue, encoding = "UTF-8"))
  # list_of_dataframes <- purrr::map(list_of_vectors, vector_to_dataframe)
  # NEW (using package xml2)
  df <- xml_to_dataframe(xml_url)
  if (!is.null(df)){
    numeric_vars <- c("MYEAR", "Latitude", "Longitude", "Value", "UNCRT", 
                      "SUBNO", "BULKID", 
                      "tblAnalysisID", "tblParamID", "tblBioID", "tblSampleID")
    for (var in numeric_vars){
      if(var %in% names(df))
        df[[var]] <- as.numeric(df[[var]])
    }
    df$DATE <- lubridate::dmy(df$DATE)
  }
  df
}
# Test
# X <- get_ices_sedimentdata("CD", yearstart = 1996, country = 74)    # 68 lines (74 = UK; see https://vocab.ices.dk/?ref=22)


if (FALSE){
  # debugonce(get_ices_biotadata)
  X <- get_ices_biotadata("HG", yearstart = 2018, lab = "NIVA")   # 371 lines
  X <- get_ices_biotadata("HG", yearstart = 2018, country = 58)    # 371 lines (58 = Norway; see https://vocab.ices.dk/?ref=22)
  X <- get_ices_biotadata("HG", yearstart = 2018)   # 1309 lines (all countries)
  X <- get_ices_biotadata("HG", yearstart = 2018, lab = "NIVA")   # 371 lines
  X <- get_ices_biotadata("HG", yearstart = 2018, lab = "NIVA", species = "Gadus morhua")     # doesn't work
  X <- get_ices_biotadata("HG", yearstart = 2018, lab = "NIVA", species = "gadus morhua")     # doesn't work
  X <- get_ices_biotadata("HG", yearstart = 2018, lab = "NIVA", species = "Gadus%20morhua")   # doesn't work
  X <- get_ices_biotadata("HG", yearstart = 2018, lab = "NIVA", species = "5558")  # doesn't work either (code from http://ecosystemdata.ices.dk/webservices/EcoSystemWebServices.asmx/getListSpecies)
  X <- get_ices_biotadata("HG", yearstart = 2018, lab = "NIVA", matrix = "MU")   # 251 lines
  X <- get_ices_biotadata("HG", yearstart = 2009, yearend = 2010, lab = "NIVA", matrix = "MU")   # 251 lines
  # Error handling is not too good:
  X <- get_ices_biotadata("Bogus", yearstart = 2018, lab = "NIVA")
  X <- get_ices_biotadata("", paramgroup = "O-MET", yearstart = 2018, lab = "NIVA", matrix = "SB")   # all organic metals (TBT etc)
}



#
# Used for old version of get_ices_biotadata
#
vector_to_dataframe <- function(x){
  df <- as.data.frame(matrix(x, nrow = 1), stringsAsFactors = FALSE)
  # colnames <- sub("X$", "", names(x), fixed = TRUE)
  # colnames <- sub("$", "", colnames, fixed = TRUE)
  names(df) <- names(x)
  df
}

xmlchild_to_dataframe <- function(i, xmlchildren){
  grandchildren <- xml2::xml_children(xmlchildren[[i]])
  df <- data.frame(matrix(xml2::xml_text(grandchildren), nrow = 1), stringsAsFactors = FALSE)
  colnames(df) <- xml2::xml_name(grandchildren)
  df
}

xml_to_dataframe <- function(url){
  full_xml <- xml2::read_xml(url)
  children <- xml2::xml_children(full_xml)
  if (length(children) > 0){
    list_of_dataframes <- seq_along(children) %>% purrr::map(~xmlchild_to_dataframe(., children))
    df <- dplyr::bind_rows(list_of_dataframes)
  } else {
    cat("No results for this database query\n")
    df <- NULL
  }
  df
  }
