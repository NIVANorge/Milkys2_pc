

sql <- paste("select * from LABWARE.PROJECT where DESCRIPTION like '%210200%'")
limsproject <- get_nivabase_data(sql)

sql <- paste("select * from LABWARE.SAMPLE where rownum < 4")
get_nivabase_data(sql)

sql <- paste("select * from LABWARE.RESULT where rownum < 4")
get_nivabase_data(sql)


sql <- paste("select * from LABWARE.SAMPLE where TEXT_ID = 'NR-2020-08432'")
get_nivabase_data(sql)

sql <- paste("select * from LABWARE.RESULT where TEXT_ID = 'NR-2020-08432'")
get_nivabase_data(sql)

sql <- paste("select * from LABWARE.SAMPLE where DESCRIPTION like '%210200%'")
get_nivabase_data(sql)

# Ikke tilgang til denne
# Har T_SHORT_NAME = Navn den får i Nivabasen    
sql <- paste("select * from LABWARE.COMPONENT where rownum < 4")
get_nivabase_data(sql)


lims_sample <- get_nivabase_selection(
  "*",
  "SAMPLE",
  "NAME",
  "1155-10616", values_are_text = TRUE, table = "LABWARE")

# All Milkys
sql <- "select project.name, sample.text_id, result.analysis, test.variation, result.reported_name, result.units, result.x_mu
from labware.sample, labware.result, labware.project, labware.test
where sample.project = project.name
--and labware.result.analysis in 'PFC'
and project.description = 'O 210200.ANAIN'
and sample.sample_number = result.sample_number
and test.test_number = labware.result.test_number
and sample.status not in ('X','A')
and result.status not in ('X','A')"
lims_analyses <- get_nivabase_data(sql)

# Types of analyses
library(dplyr)
lims_analyses %>%
  count(ANALYSIS)

# PFAS analyses 
lims_analyses %>%
  filter(ANALYSIS == "PFC") %>%
  count(REPORTED_NAME)



lims_sample <- get_nivabase_selection(
  "*",
  "SAMPLE",
  "NAME",
  "1155-10616", values_are_text = TRUE, table = "LABWARE")

