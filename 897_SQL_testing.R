
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(ggplot2)
library(niRvana)

test <- get_nivabase_data("
SELECT SAMPLE_ID, METHOD_ID, VALUE_ID, VALUE, FLAG1
FROM nivadatabase.biota_chemistry_values
WHERE method_id = 28115;
  ") 

test <- get_nivabase_data("
SELECT a.SAMPLE_ID, a.METHOD_ID, a.VALUE_ID, a.VALUE, a.FLAG1, b.TISSUE_ID, b.STATION_ID
FROM nivadatabase.biota_chemistry_values a
LEFT JOIN nivadatabase.biota_samples b 
ON a.SAMPLE_ID = b.SAMPLE_ID
WHERE a.method_id = 28115;
  ") 

test <- get_nivabase_data("
SELECT a.SAMPLE_ID, a.METHOD_ID, a.VALUE_ID, a.VALUE, a.FLAG1, b.TISSUE_ID, d.STATION_ID
FROM nivadatabase.biota_chemistry_values a
LEFT JOIN nivadatabase.biota_samples b ON a.SAMPLE_ID = b.SAMPLE_ID
LEFT JOIN nivadatabase.biota_samples_specimens c ON b.SAMPLE_ID = c.SAMPLE_ID
LEFT JOIN nivadatabase.biota_single_specimens d ON c.SPECIMEN_ID = d.SPECIMEN_ID
LEFT JOIN nivadatabase.biota_single_specimens d ON c.SPECIMEN_ID = d.SPECIMEN_ID
LEFT JOIN nivadatabase.projects_stations e ON d.STATION_ID = e.STATION_ID
WHERE a.method_id = 28115;
") 

test <- get_nivabase_data("
SELECT m.NAME, m.UNIT, m.LABORATORY, m.MATRIX, a.SAMPLE_ID, a.METHOD_ID, a.VALUE_ID, a.VALUE, a.FLAG1, 
b.TISSUE_ID, d.STATION_ID, d.SPECIMEN_NO, d.TAXONOMY_CODE_ID, d.DATE_CAUGHT, e.STATION_CODE, e.STATION_NAME
FROM nivadatabase.method_definitions m
LEFT JOIN nivadatabase.biota_chemistry_values a ON m.METHOD_ID = a.METHOD_ID
LEFT JOIN nivadatabase.biota_samples b ON a.SAMPLE_ID = b.SAMPLE_ID
LEFT JOIN nivadatabase.biota_samples_specimens c ON b.SAMPLE_ID = c.SAMPLE_ID
LEFT JOIN nivadatabase.biota_single_specimens d ON c.SPECIMEN_ID = d.SPECIMEN_ID
LEFT JOIN nivadatabase.biota_single_specimens d ON c.SPECIMEN_ID = d.SPECIMEN_ID
LEFT JOIN nivadatabase.projects_stations e ON d.STATION_ID = e.STATION_ID
WHERE m.NAME like '%PFOS%' and m.LABORATORY = 'NIVA_LABWARE' and m.MATRIX = 'BIOTA';
") 

table(test$UNIT)

test <- test %>%
  mutate(
    Conc_ng_p_g = case_when(
      UNIT == "NG_P_KG" ~ VALUE/1000,
      TRUE ~ VALUE)
  )

table(addNA(test$TISSUE_ID))
table(test$STATION_CODE)

test %>%
  add_count(STATION_CODE) %>%
  filter(n >= 50) %>%
  ggplot(aes(DATE_CAUGHT, Conc_ng_p_g, shape = FLAG1)) +
  geom_point() +
  facet_wrap(vars(STATION_CODE)) +
  scale_y_log10()
  



table(test$MATRIX)
table(test$METHOD_ID)

test2 <- get_nivabase_data("
SELECT *
FROM nivadatabase.method_definitions
WHERE NAME = 'Perfluoroktansyre (PFOA)' and LABORATORY = 'NIVA_LABWARE';
") 

test3 <- get_nivabase_selection("*", "method_definitions", "METHOD_ID", unique(test$METHOD_ID))

WHERE NAME = 'Perfluoroktansyre (PFOA)' and LABORATORY = 'NIVA_LABWARE';
") 

test <- get_nivabase_data("
SELECT a.SAMPLE_ID, a.METHOD_ID, a.VALUE_ID, a.VALUE, a.FLAG1, b.TISSUE_ID 
FROM nivadatabase.biota_chemistry_values a
LEFT JOIN nivadatabase.biota_samples b
ON a.SAMPLE_ID = b.SAMPLE_ID
WHERE a.METHOD_ID = 28115;
")

test <- get_nivabase_data("
SELECT a.SAMPLE_ID, a.METHOD_ID, a.VALUE_ID, a.VALUE, a.FLAG1, b.TISSUE_ID, d.SPECIMEN_ID 
FROM nivadatabase.biota_chemistry_values a
LEFT JOIN nivadatabase.biota_samples b ON a.SAMPLE_ID = b.SAMPLE_ID
LEFT JOIN nivadatabase.biota_samples_specimens c ON b.SAMPLE_ID = c.SAMPLE_ID
WHERE a.METHOD_ID = 28115;
")

test <- get_nivabase_data("
SELECT a.SAMPLE_ID, a.METHOD_ID, a.VALUE_ID, a.VALUE, a.FLAG1, b.TISSUE_ID, d.SPECIMEN_ID 
FROM nivadatabase.biota_chemistry_values a
LEFT JOIN nivadatabase.biota_samples b ON a.SAMPLE_ID = b.SAMPLE_ID
LEFT JOIN nivadatabase.biota_samples_specimens c ON b.SAMPLE_ID = c.SAMPLE_ID
LEFT JOIN nivadatabase.biota_single_specimens d ON c.SPECIMEN_ID = d.SPECIMEN_ID
WHERE a.METHOD_ID = 28115;
")
