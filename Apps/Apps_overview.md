
## Overview of apps and data sources  

* App_801 - Annual median concentrations from Jupyterhub script 110   
`dat_med <- readRDS("../Files_from_Jupyterhub_2020/Raw_data/110_mediandata_updated_2021-10-08.rds")`       
    
    
* App_802 - Raw data from ICES (csv file from Rob) 
`dat <- read_csv("../Files_to_ICES/Data_from_ICES/data_extraction_211008/Norway_added_columns.csv")`      
    - Most developed so far  
    
* App_803 - Raw data from CEMP Access database  
`dat_loc <- read_csv2("../../../CEMP/2016_Milkys/dbo_locality_ny.txt")` (several files)    
    - Files are joined together and saved in the app folder as `dat.csv`    
    
    
* App_804 - Raw data from NIVA (RData file from Jupyterhub script 109)    
`dat <- readRDS("../../Files_from_Jupyterhub_2020/Raw_data/109_adjusted_data_2021-09-15.rds")`    
    - In addition, station file   
    
    
* App_805 - Check ICES submission file      
`"../../Files_to_ICES/2020/Rdata"`    
    - Has some weight on checking measurement (analysis) error
    - Reads files from hard-coded folders  
    
* App_806 - Use for searching on a big SQL join? (see script 821)
    
    
* App_807 - Reading data directly from Nivabasen including Labware tables     
    - Main menus and main server functions finished  
    - Goes from project towards parameters (cannot search for parameter first)   
    - Select first project, then stations and/or years (but needs one of them), then parameter 
    - Can be made a bit quicker if samples are selected using a join in the database   
    
* Notes for Milkys app:  
    - Menu for parameter group + menu for parameter + station type (species/tissue) station  
    - That menu shold be in left margin  
    - Overview plot for levels (boxplot) + boxplot for EQS if possible (showing trends in some way?) 
    - Overview plot for trends (short/long)  
    - Stations sorted from N to S (prioritized before showing all old stations) 
    - Median numbers in add. to graph - show less-thans    
    - PROREF + EQS + long + short trend (this year + last year)  
    - Time series graph w/ ability to see only last x years  
    - Map 
    - Matrix plot (scatter) for parameters within parameter group  
    - Download button  
    
    
