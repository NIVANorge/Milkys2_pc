
#
# Data downloaded via OHAT page, link 'DOME raw data extractions' (https://www.ices.dk/sites/pub/Publication%20Reports/Forms/DispForm.aspx?ID=37129)  
# See https://dome.ices.dk/ohat/?assessmentperiod=2021  
#
# Same as 
#

# Do this just once -  reads all data and saves the Norwegian ones 

if (FALSE){
  
  fn <- "Files_to_ICES/Data_from_ICES/OSPAR_MIME_data_extraction_2021/OSPAR_MIME_AMAP_Biota_contaminants_effects_20210201/OSPAR_MIME_AMAP_Biota_contaminants_effects_20210201.txt"
  
  # Checking
  readLines(fn, n = 10, encoding = "UTF-8", skipNul = TRUE)
  
  # The file Didn't work
  # readr::read_tsv(fn, n_max = 10)
  # readr::read_delim(fn, delim = "\t", n_max = 10)
  
  # Didn't work (tried to read Norway only; see https://www.r-bloggers.com/2017/02/how-to-import-a-subset-of-a-too-huge-csv-file/)
  # library(sqldf)
  # dat <- sqldf::read.csv.sql(fn, header = TRUE, sep = "\t", sql = "select * from file where ÿþCountry = '\"Norway\"'")
  # dat <- sqldf::read.csv.sql(fn, header = TRUE, sep = "\t", sql = "select * from file where StationName = '\"14-S\"'")
  
  # Worked (test read)
  df <- read.table(fn, header = TRUE, sep = "\t", quote = "", nrows = 10, skipNul = TRUE)
  str(df)
  
  # Read all (40 seconds)  
  t0 <- Sys.time()
  dat_all_countries <- read.table(fn, header = TRUE, sep = "\t", quote = "", skipNul = TRUE)
  t1 <- Sys.time()
  t1-t0
  str(dat_all_countries)
  names(dat_all_countries)[1] <- "Country"
  
  dat <- subset(dat_all_countries, Country == "Norway")
  
  fn_save <- "Files_to_ICES/Data_from_ICES/OSPAR_MIME_data_extraction_2021/OSPAR_MIME_AMAP_Biota_contaminants_effects_20210201/OSPAR_MIME_AMAP_Biota_contaminants_effects_20210201_Norway.txt"
  write.csv(dat, fn_save, row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")
  
  test <- readr::read_csv(fn_save)
  unique(test$SD_StationName)
  
}

