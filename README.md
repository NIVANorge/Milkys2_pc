# Milkys2_pc
R scripts and Excel templates for (1) fetching data to use in Milkys2 on Jupyterhub, and (2) post-processing files created in Milkys2 on Jupyterhub
- the scripts here are all numbered 8xx to avoid overlapping with the scripts in Milkys2 on Jupyterhub  
  
## Script overview     

801-810: Scripts for reading data from NIvabase or Excel   
811-820: Scripts for importing (inserting) data into Nivabase   
821-830: Various Nivabase-related   
831-840: Various Vannmlijø-related  
841-850: Scripts for submitting data to ICES  
851-860: Not used (yet)  
861-870: Various: making data files for co-workers/course, checking, testing     
871-880: Not used (yet)  
881-890: Not used (yet)  
891-899: Not used (yet)  

## Data flows  

### Data from NILU  

- Gotten by mail in excel: Folder 'Input_files_2020'  
- Get data from excel: 808  
- Insert in Nivabase: 812 + 814  
- Thereafter these data will be included when data is drawn from Nivabase using script 802   

### Data for biological effects / PAH metabolites in cod    

- Gotten by mail in excel: Folder 'Input_files_2020'  
- Get data from excel and insert in Nivabase: 815  
- Thereafter these data will be included when data is drawn from Nivabase using script 802   

### Data for imposex etc.      

- Data in excel on K, `K:/Prosjekter/Sjøvann/JAMP/2020/opparbeiding/Snegl`   
- Get data from excel and insert in Nivabase: 813  
- Thereafter these data will be included when data is drawn from Nivabase using script 802   

### Labware sample data

- Downloading data: 801  
- Used in 811, 812, 813, 814, 815   

## Coupling data from excel to existing samples in Nivabase   

(from DHJ's powerpoint 'CEMP database structures.pptx')  

![Overview of the table df_lookup_sample][pic1]

[pic1]: Info/df_lookup_sample.png "df_lookup_sample (from ppt 'CEMP database structures.pptx')"





