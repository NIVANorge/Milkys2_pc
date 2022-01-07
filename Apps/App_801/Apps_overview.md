
## Overview of apps and data sources  

* App_801 - Annual median concentrations from Jupyterhub script 110   
`dat_med <- readRDS("../Files_from_Jupyterhub_2020/Raw_data/110_mediandata_updated_2021-10-08.rds")`       
    
    
* App_802 - Raw data from ICES (csv file from Rob) 
`dat <- read_csv("../Files_to_ICES/Data_from_ICES/data_extraction_211008/Norway_added_columns.csv")`      
    - Most developed so far  
    
    
* App_803 - Raw data from CEMP Access database  
`dat_loc <- read_csv2("../../../CEMP/2016_Milkys/dbo_locality_ny.txt")` (several files)    
    - Files are joined together and saved in the app folder as `dat.csv`    
    
    
* App_804  
``  
    
    
* App_805  
``  
    
    
* App_806  
``  
    
    
* App_807  
``  
    
    
