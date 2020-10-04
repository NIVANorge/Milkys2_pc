
library(dplyr)
library(purrr)
library(lubridate)
library(stringr)
library(tidyr)
# library(ggplot2)
library(niRvana)
library(safejoin)   # https://github.com/moodymudskipper/safejoin

sql_from_jens <- "select 
 a.sample_id, a.value_id, a.flag1, a.value, a.remark as biota_chem_val_remark, a.approved, a.detection_limit, a.quantification_limit, a.method_id
, b.station_id, b.sample_date, b.sample_no, b.TISSUE_ID, b.repno
, c.project_id, c.station_code, c.station_name
, d.name, d.unit, d.laboratory, d.method_ref, d.detection_limit as m_detection_limit, d.quantification_limit as m_quantification_limit, d.uncertainty
, e.conversion_factor
, f.name as p_name, f.unit as p_unit
, h.tissue_name, h.niva_code
, i.taxonomy_code_id, i.taxonomy_domain_id, i.code, i.name as taxonomy_codes_name
--, j.species_id, j.NIVA_CODE as species_code
, L.biota_sampling_method_id , L.CAUGHT_BY , L.DATE_CAUGHT , L.REMARK , L.SAMPLE_POINT_ID , l.SAMPLING_METHOD_ID 
, L.SPECIES_ID as ss_species_id, L.SPECIMEN_ID , L.SPECIMEN_NO , L.STATION_ID as ss_station_id, L.TAXONOMY_CODE_ID as ss_taxonomy_code_id
from NIVADATABASE.biota_CHEMISTRY_VALUES a
join NIVADATABASE.biota_SAMPLES b on a.sample_id=b.sample_id
join NIVADATABASE.PROJECTS_STATIONS c on b.station_id=c.station_id
join nivadatabase.method_definitions d on a.method_id=d.method_id
left join NIVADATABASE.biota_PARAMETERS_METHODS e on e.method_id =d.method_id
left join NIVADATABASE.biota_PARAMETER_DEFINITIONS f on f.parameter_id=e.parameter_id
left join NIVADATABASE.BIOTA_TISSUE_TYPES h on h.tissue_id=B.TISSUE_ID
left join nivadatabase.taxonomy_codes i on i.taxonomy_code_id=b.taxonomy_code_id
left join nivadatabase.biota_samples_specimens k on k.sample_id=b.sample_id
left join nivadatabase.biota_single_specimens l on K.SPECIMEN_ID=L.SPECIMEN_ID"

sql_where <- "where p_name = 'CB118' and extract(YEAR from sample_date) = 2018"
sql_where <- "where extract(YEAR from sample_date) = 2015"

dftest <- get_nivabase_data(paste(sql_main, sql_where))

sql <- "select NAME,METHOD_ID from nivadatabase.method_definitions where NAME = 'CB118'"
get_nivabase_data(sql)


get_nivabase_data("select * from NIVADATABASE.METHOD_DEFINITIONS where rownum < 4")
get_nivabase_data("select * from NIVADATABASE.BIOTA_PARAMETERS_METHODS where rownum < 4")
get_nivabase_data("select * from NIVADATABASE.BIOTA_PARAMETER_DEFINITIONS where rownum < 4")

sql <- "select d.NAME as m_NAME, d.UNIT as m_UNIT, LABORATORY, d.METHOD_ID, 
e.CONVERSION_FACTOR, 
f.NAME as p_NAME, f.UNIT as p_UNIT, f.SORT_NAME from 
NIVADATABASE.METHOD_DEFINITIONS d
left join NIVADATABASE.BIOTA_PARAMETERS_METHODS e on e.method_id=d.method_id
left join NIVADATABASE.BIOTA_PARAMETER_DEFINITIONS f on f.parameter_id=e.parameter_id 
where d.NAME = 'CB118'"
get_nivabase_data(sql)

sql <- "select d.NAME as m_NAME, d.UNIT as m_UNIT, LABORATORY, d.METHOD_ID, 
e.CONVERSION_FACTOR, 
f.NAME as p_NAME, f.UNIT as p_UNIT, f.SORT_NAME from 
NIVADATABASE.BIOTA_CHEMISTRY_VALUES a
join NIVADATABASE.METHOD_DEFINITIONS d on a.method_id=d.method_id 
left join NIVADATABASE.BIOTA_PARAMETERS_METHODS e on e.method_id=d.method_id
left join NIVADATABASE.BIOTA_PARAMETER_DEFINITIONS f on f.parameter_id=e.parameter_id 
where d.NAME = 'CB118'"
get_nivabase_data(sql) %>% head()

sql <- "select b.SAMPLE_ID, b.SAMPLE_DATE, b.TISSUE_ID, b.STATION_ID, 
b.TAXONOMY_CODE_ID, b.SAMPLE_NO, b.REPNO,
a.VALUE_ID, a.VALUE, a.FLAG1, a.DETECTION_LIMIT, a.UNCERTAINTY, a.QUANTIFICATION_LIMIT,
d.NAME as m_NAME, d.UNIT as m_UNIT, LABORATORY, d.METHOD_ID, 
e.CONVERSION_FACTOR, 
f.NAME as p_NAME, f.UNIT as p_UNIT, f.SORT_NAME from 
NIVADATABASE.BIOTA_CHEMISTRY_VALUES a
join NIVADATABASE.BIOTA_SAMPLES b on a.sample_id=b.sample_id
join NIVADATABASE.METHOD_DEFINITIONS d on a.method_id=d.method_id 
left join NIVADATABASE.BIOTA_PARAMETERS_METHODS e on e.method_id=d.method_id
left join NIVADATABASE.BIOTA_PARAMETER_DEFINITIONS f on f.parameter_id=e.parameter_id 
where d.NAME = 'CB118'"
df <- get_nivabase_data(sql)
nrow(df)
head(df, 3)

#
# Full sql
#
# Note: this will return several rows per sample/parameter if we don't select on PROJECT_ID as well
#
sql_main_full <- "select m.project_name, c.project_id, c.station_code, c.station_name,
L.biota_sampling_method_id , L.CAUGHT_BY , L.DATE_CAUGHT , L.REMARK , L.SAMPLE_POINT_ID , l.SAMPLING_METHOD_ID,
L.SPECIES_ID as ss_species_id, L.SPECIMEN_ID , L.SPECIMEN_NO , L.STATION_ID as ss_station_id, L.TAXONOMY_CODE_ID as ss_taxonomy_code_id,
b.SAMPLE_ID, b.SAMPLE_DATE, b.TISSUE_ID, b.STATION_ID, 
b.TAXONOMY_CODE_ID, b.SAMPLE_NO, b.REPNO,
a.VALUE_ID, a.VALUE, a.FLAG1, a.DETECTION_LIMIT, a.UNCERTAINTY, a.QUANTIFICATION_LIMIT,
d.NAME as m_NAME, d.UNIT as m_UNIT, LABORATORY, d.METHOD_ID, 
e.CONVERSION_FACTOR, 
f.NAME as p_NAME, f.UNIT as p_UNIT, f.SORT_NAME, 
h.tissue_name, h.niva_code,
i.taxonomy_domain_id, i.code, i.name as taxonomy_codes_name from 
NIVADATABASE.BIOTA_CHEMISTRY_VALUES a
join NIVADATABASE.BIOTA_SAMPLES b on a.sample_id=b.sample_id
join NIVADATABASE.PROJECTS_STATIONS c on b.station_id=c.station_id
join NIVADATABASE.METHOD_DEFINITIONS d on a.method_id=d.method_id 
left join NIVADATABASE.BIOTA_PARAMETERS_METHODS e on e.method_id=d.method_id
left join NIVADATABASE.BIOTA_PARAMETER_DEFINITIONS f on f.parameter_id=e.parameter_id 
left join NIVADATABASE.BIOTA_TISSUE_TYPES h on h.tissue_id=b.tissue_id
left join nivadatabase.taxonomy_codes i on i.taxonomy_code_id=b.taxonomy_code_id
left join NIVADATABASE.BIOTA_SAMPLES_SPECIMENS k on k.sample_id=b.sample_id
left join NIVADATABASE.BIOTA_SINGLE_SPECIMENS l on k.specimen_id=l.specimen_id
left join NIVADATABASE.PROJECTS m on m.project_id=c.project_id"

sql_main_sel <- "select m.project_name, c.project_id, c.station_code, c.station_name,
h.tissue_name, L.DATE_CAUGHT, L.SPECIMEN_NO, 
b.TAXONOMY_CODE_ID, b.SAMPLE_NO, b.REPNO,
a.VALUE, a.FLAG1, 
d.NAME as m_NAME, d.UNIT as m_UNIT, LABORATORY, d.METHOD_ID, 
e.CONVERSION_FACTOR, 
f.NAME as p_NAME, f.UNIT as p_UNIT, f.SORT_NAME, 
i.name as taxonomy_codes_name, 
L.SPECIMEN_ID, b.SAMPLE_ID, a.VALUE_ID from 
NIVADATABASE.BIOTA_CHEMISTRY_VALUES a
join NIVADATABASE.BIOTA_SAMPLES b on a.sample_id=b.sample_id
join NIVADATABASE.PROJECTS_STATIONS c on b.station_id=c.station_id
join NIVADATABASE.METHOD_DEFINITIONS d on a.method_id=d.method_id 
left join NIVADATABASE.BIOTA_PARAMETERS_METHODS e on e.method_id=d.method_id
left join NIVADATABASE.BIOTA_PARAMETER_DEFINITIONS f on f.parameter_id=e.parameter_id 
left join NIVADATABASE.BIOTA_TISSUE_TYPES h on h.tissue_id=b.tissue_id
left join nivadatabase.taxonomy_codes i on i.taxonomy_code_id=b.taxonomy_code_id
left join NIVADATABASE.BIOTA_SAMPLES_SPECIMENS k on k.sample_id=b.sample_id
left join NIVADATABASE.BIOTA_SINGLE_SPECIMENS l on k.specimen_id=l.specimen_id
left join NIVADATABASE.PROJECTS m on m.project_id=c.project_id"

sql_where_1 <- "where d.NAME = 'CB118' and extract(YEAR from l.DATE_CAUGHT) > 2012"  # Select parameter from METHOD_DEFINITIONS
sql_where_2 <- "where f.NAME = 'CB118' and extract(YEAR from l.DATE_CAUGHT) > 2012"  # Select parameter from BIOTA_PARAMETER_DEFINITIONS

#
# Select parameter from METHOD_DEFINITIONS
#

# Returns 34 rows, but actually only 15 samples
df1a <- get_nivabase_data(paste(sql_main_full, sql_where_1))
nrow(df1a) # 34
# head(df1a, 3)

# Returns same rows, but fewer variables (is quite a bit faster)
df1b <- get_nivabase_data(paste(sql_main_sel, sql_where_1))
nrow(df1b) # 34
# head(df1b, 3)

#
# Note: The SQL above has returned several rows per sample/parameter, as we don't select on PROJECT_ID
# The following demostrates this - returns 15 samples
#
df_summ <- df1b %>% 
  group_by(STATION_CODE, STATION_NAME, DATE_CAUGHT, SAMPLE_ID) %>%
  summarise(n = n(), project_id = paste(PROJECT_ID, collapse = ", "), project_name = paste(PROJECT_NAME, collapse = ", "))
nrow(df_summ) # 15
View(df_summ)

#
# Get projects
#
df_proj <- niRvana::get_projects()

df_proj %>%
  filter(PROJECT_ID %in% unique(df1$PROJECT_ID)) %>%
  select(PROJECT_ID, PROJECT_NAME)
# 1       3699 CEMP_Biota                                                          2159
# 2       5962 CEMP_BIOTA_2012                                                     1631
# 3       4623 HARMONY_Cemp Biota                                                  1080
# 4       3240 "Kartlegging utvalgte nye organiske miljøgifter\r"                   266
# 5       5079 Screening of selected metals and new organic contaminants 2007       213
# 6      11506 NMDC Sørfjorden                                                      189
# 7       3239 Kartlegging av metaller og utvalgte nye organiske miljøgifter 2006   174
# 8      11407 NMDC Kristiansandfjorden                                             140
# 9       4660 Overvåking av miljøgifter i Kristiansandsfjorden                     133

#
# Select parameter from BIOTA_PARAMETER_DEFINITIONS
#

df2 <- get_nivabase_data(paste(sql_main_sel, sql_where_2))
nrow(df2) # 6543
# head(df2, 3)

df2 %>%
  count(PROJECT_ID, PROJECT_NAME) %>%
  arrange(desc(n))

#
# Get all 2018 data 
# Somewhat quicker than 
# 
#
sql_where_3 <- "where extract(YEAR from l.DATE_CAUGHT) = 2018 and m.PROJECT_NAME = 'CEMP_Biota'"  # Select parameter from BIOTA_PARAMETER_DEFINITIONS
t0 <- Sys.time()
df3 <- get_nivabase_data(paste(sql_main_sel, sql_where_3))
t1 <- Sys.time()
nrow(df3) # 40415
t1 - t0   # 20.69476 secs

df3 %>%
  filter(STATION_CODE %in% "19N") %>%
  count(M_NAME, P_NAME) %>%
  View()










#
# APPENDIX - trying to aggregate in SQL, like this: https://stackoverflow.com/a/4686600/1734247 
# Doesn't work in this test 
# May it's this: "Note: Out of the box, LISTAGG only works correctly with VARCHAR2 columns." (from above link)
#

sql_main_agg_sel <- "select listagg(c.project_id) within group (order by c.project_id) as PROJECT_IDS, c.station_code, c.station_name,
h.tissue_name, L.DATE_CAUGHT, L.SPECIMEN_NO, 
b.TAXONOMY_CODE_ID, b.SAMPLE_NO, b.REPNO,
a.VALUE, a.FLAG1, 
d.NAME as m_NAME, d.UNIT as m_UNIT, LABORATORY, d.METHOD_ID, 
e.CONVERSION_FACTOR, 
f.NAME as p_NAME, f.UNIT as p_UNIT, f.SORT_NAME, 
i.name as taxonomy_codes_name, 
L.SPECIMEN_ID, b.SAMPLE_ID, a.VALUE_ID from 
NIVADATABASE.BIOTA_CHEMISTRY_VALUES a
join NIVADATABASE.BIOTA_SAMPLES b on a.sample_id=b.sample_id
join NIVADATABASE.PROJECTS_STATIONS c on b.station_id=c.station_id
join NIVADATABASE.METHOD_DEFINITIONS d on a.method_id=d.method_id 
left join NIVADATABASE.BIOTA_PARAMETERS_METHODS e on e.method_id=d.method_id
left join NIVADATABASE.BIOTA_PARAMETER_DEFINITIONS f on f.parameter_id=e.parameter_id 
left join NIVADATABASE.BIOTA_TISSUE_TYPES h on h.tissue_id=b.tissue_id
left join nivadatabase.taxonomy_codes i on i.taxonomy_code_id=b.taxonomy_code_id
left join NIVADATABASE.BIOTA_SAMPLES_SPECIMENS k on k.sample_id=b.sample_id
left join NIVADATABASE.BIOTA_SINGLE_SPECIMENS l on k.specimen_id=l.specimen_id
left join NIVADATABASE.PROJECTS m on m.project_id=c.project_id"

# Returns same rows, but fewer variables (is quite a bit faster)
df1b <- get_nivabase_data(paste(sql_main_agg_sel, sql_where_1, "group by a.VALUE_ID"))
nrow(df1b) # 34

