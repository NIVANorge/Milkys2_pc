
library(niRvana)
library(dplyr)
library(lubridate)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
## Milkys data ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

check <- get_nivabase_data("select * from NIVADATABASE.LABWARE_CHECK_SAMPLE where upper(PROSJEKT) like '%MILKYS%'")
table(check$PROSJEKT)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# . Sample data in Labware ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

df1 <- get_nivabase_data("select * from NIVADATABASE.LABWARE_CHECK_SAMPLE where PROSJEKT like '%MILKYS 2021%'")
nrow(df1) # 595
df1 %>%
  count(ACCOUNT_NUMBER, PROSJEKT)

df1_summ <- df1 %>%
  count(SAMPLED_DATE, ANALYSEOPPDRAG, SPECIES, TISSUE)
nrow(df1_summ)
head(df1_summ)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# . Concentration data in Labware ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

df2 <- get_nivabase_data("select * from NIVADATABASE.LABWARE_IMPORT where rownum < 4")

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Overview of Milkys data (using 30B as example)
# - note that SAMPLED_DATE is sometimes NA
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

df_projects <- get_nivabase_data(paste(
  "select extract(YEAR from RECD_DATE) as YEAR_rec, extract(YEAR from SAMPLED_DATE) as YEAR_samp, PROSJEKT, count(*)",
  "from NIVADATABASE.LABWARE_IMPORT where AQUAMONITOR_CODE = '30B'",
  "group by extract(YEAR from RECD_DATE), extract(YEAR from SAMPLED_DATE), PROSJEKT",
  "order by extract(YEAR from RECD_DATE)"))
df_projects

# Total number of records in these projects 
get_nivabase_data(paste(
  "select count(*)",
  "from NIVADATABASE.LABWARE_IMPORT where PROSJEKT in (", paste(sQuote(df_projects$PROSJEKT), collapse = ","), ")"))
# 148722

# Check whether AQUAMONITOR_CODE fits DESCRIPTION (in this case, it does)
get_nivabase_data("select count(*) from NIVADATABASE.LABWARE_IMPORT where AQUAMONITOR_CODE = '30B'")
get_nivabase_data("select count(*) from NIVADATABASE.LABWARE_IMPORT where DESCRIPTION like '%30B%'")

# Check if any of these lacks AQUAMONITOR_CODE  
# - none does
get_nivabase_data(paste(
  "select count(*)",
  "from NIVADATABASE.LABWARE_IMPORT where PROSJEKT in (", paste(sQuote(df_projects$PROSJEKT), collapse = ","), ")",
  "and AQUAMONITOR_CODE is NULL"))
# 0

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
## Other data ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# . Data not yet accepted ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# . - Check out STATUS and STATUSX ----
#
# Note that STATUS is different things in sample data and measurement data
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#
# Samples
#
get_nivabase_data(paste(
  "select STATUS, count(*)",
  "from NIVADATABASE.LABWARE_CHECK_SAMPLE where extract(YEAR from SAMPLED_DATE) = 2020",
  "group by STATUS"))

#        STATUS COUNT(*)
# 1      Ferdig    11731
# 2 Midlertidig      256

#
# Measurements
#
get_nivabase_data(paste(
  "select STATUSX, STATUS, count(*)",
  "from NIVADATABASE.LABWARE_IMPORT where extract(YEAR from SAMPLED_DATE) = 2020",
  "group by STATUSX, STATUS"))

#       STATUSX STATUS COUNT(*)
# 1      Ferdig      A   136568
# 2 Midlertidig      P      717
# 3 Midlertidig      I     1872


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# . - Check out 'Midlertidig' data ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

get_nivabase_data(paste(
  "select PROSJEKT, extract(YEAR from SAMPLED_DATE) as YEAR_samp, count(*)",
  "from NIVADATABASE.LABWARE_IMPORT where",
  "extract(YEAR from SAMPLED_DATE) = 2020",
  "and STATUSX = 'Midlertidig'",
  "group by PROSJEKT, extract(YEAR from SAMPLED_DATE)"))

#                     PROSJEKT YEAR_SAMP COUNT(*)
# 1 O 200199 PFAS i drikkevann      2020     1239
# 2         O 17146 Urbanfjord      2020     1350

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Urbanfjord data ----
#
# Contains STATUS Midlertidig as well as Ferdig
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


df_labware <- get_nivabase_data(paste(
  "select *",
  "from NIVADATABASE.LABWARE_IMPORT where",
  "PROSJEKT like '%17146%'",
  "and extract(YEAR from SAMPLED_DATE) = 2020"))

#
# Save for future use! Contains STATUS Midlertidig as well as Ferdig
#
# saveRDS(df_labware, "App_807/Urbanfjord_Labware_per_06-01-2022.rds")
# Read:
# df_labware <- readRDS("App_807/Urbanfjord_Labware_per_06-01-2022.rds")
#

#
# . Add PARAM_group and BIOTA_matrix ----
#
df_labware <- df_labware %>%
  mutate(
    PARAM_group = case_when(
      REPORTED_NAME %in% c("% C","% N", "W% C/N") ~ "C and N",
      REPORTED_NAME %in% c("Delta 13C","Delta 15N", "SIA") ~ "Isotopes",
      substr(REPORTED_NAME,1,2) == "UV" ~ "UV substances",
      grepl("EHMC", REPORTED_NAME) ~ "EHMC substances",
      REPORTED_NAME %in% c("<63 µm", "Totalt organisk karbon") ~ "Sediment parameters",
      REPORTED_NAME %in% c("Octocrylen", "Benzophenone-3", "STS") ~ "Others",
      TRUE ~ "PFAS"),
    BIOTA_matrix = case_when(
      grepl("Torskelever", DESCRIPTION) ~ "Torskelever",
      grepl("Torskefilet", DESCRIPTION) ~ "Torskefilet",
      grepl("Blåskjell", DESCRIPTION) ~ "Blåskjell",
      grepl("Blåskjell", AQUAMONITOR_CODE ) ~ "Blåskjell",
      grepl("Børstemark", DESCRIPTION) ~ "Børstemark",
      grepl("Krill", DESCRIPTION) ~ "Krill",
      grepl("Sild", DESCRIPTION) ~ "Sild",
      grepl("Reke", DESCRIPTION) ~ "Reke",
      AQUAMONITOR_NAME == "Skjælholmene" ~ "Måke"),
    Value_existing = 
      ifelse(is.na(NUMERIC_ENTRY), "Has_no_value", "Has_value"),
    LOQ = 
      ifelse(is.na(ENTRY_QUALIFIER), "Over LOQ", "Under LOQ")
  )

xtabs(~addNA(BIOTA_matrix), df_labware)
xtabs(~addNA(BIOTA_matrix), df_labware %>% filter(SAMPLE_TYPE == "BIOTA"))

#
# Check REPORTED_NAME + 
# - not all parameters have PARAM, but all PFAS has it

library(stringr)

#
# . Check deviant PARAM names ----
#
df_labware_names <- df_labware %>%
  mutate(
    # Extract paraneter name from parantheses
    #   (the first two lines handle parameters starting with N-Et and N-Me)
    PARAM_extract = case_when(
      grepl("N\\-Et", REPORTED_NAME) ~ str_match(REPORTED_NAME, "N\\-Et.+\\)") %>% sub(")", "", .),
      grepl("N\\-Me", REPORTED_NAME) ~ str_match(REPORTED_NAME, "N\\-Me.+\\)") %>% sub(")", "", .),
      TRUE ~ stringr::str_match(REPORTED_NAME, "\\((.+)\\)")[,2])
    ) %>%
  count(PARAM_group, REPORTED_NAME, PARAM_extract, PARAM) 

# df_labware_names %>% View()

#
# Deviant PARAM names
#
df_labware_names %>%
  filter(PARAM_group == "PFAS",
         PARAM_extract != PARAM)
# PARAM_group                          REPORTED_NAME PARAM_extract   PARAM  n
# 1  PFAS 6:2 Fluortelomersulfonat (FTS, H4PFOS)   FTS, H4PFOS 6:2 FTS 83
# 2  PFAS              Perfluordekansyre (PFDcA)         PFDcA    PFDA 83
# 3  PFAS            Perfluordodekansyre (PFDoA)         PFDoA  PFDoDA 83
# 4  PFAS             Perfluorpentansyre (PFPeA)         PFPeA    PFPA 83
# 5  PFAS          Perfluortetradekansyre (PFTA)          PFTA  PFTeDA 83
# 6  PFAS           Perfluortridekansyre (PFTrA)         PFTrA  PFTrDA 83
# 7  PFAS            Perfluorundekansyre (PFUdA)         PFUdA  PFUnDA 83

#
# Overview tables
#

# 
# Table of Station and SAMPLE_TYPE (AVLØPSVANN, BIOTA, SEDIMENT) 
#
xtabs(~addNA(AQUAMONITOR_CODE) + SAMPLE_TYPE, df_labware )

#
# Overview of status and whether values are given 
#
df_labware %>%
  count(SAMPLE_TYPE, AQUAMONITOR_CODE, BIOTA_matrix, STATUSX, Value_existing) %>%
  tidyr::pivot_wider(names_from = Value_existing, values_from = n, values_fill = 0)

df_labware %>%
  filter(AQUAMONITOR_CODE %in% "IO") %>%
  count(SAMPLE_TYPE, AQUAMONITOR_CODE, STATUSX, Value_existing) %>%
  tidyr::pivot_wider(names_from = Value_existing, values_from = n, values_fill = 0)

#
# . Urbanfjord: Plots of PFAS ----
#  All PFAS
#
df_labware %>%
  filter(
    !is.na(NUMERIC_ENTRY),
    AQUAMONITOR_CODE %in% "IO",
    PARAM_group %in% "PFAS") %>%
  ggplot(aes(REPORTED_NAME, NUMERIC_ENTRY, color = STATUSX, shape = LOQ)) +
  scale_color_manual(values = c("Ferdig" = "black", "Midlertidig" = "red2")) + 
  scale_shape_manual(values = c("Over LOQ" = 19, "Under LOQ" = 25)) +
  geom_jitter(height = 0, width = 0.2) +
  ggeasy::easy_rotate_x_labels(-45)

#
# Plot PFAS except PFOS + PFOSA at IO
#
df_labware %>%
  filter(
    !is.na(NUMERIC_ENTRY),
    AQUAMONITOR_CODE %in% "IO",
    PARAM_group %in% "PFAS",
    !PARAM %in% c("PFOS", "PFOSA")) %>%
  ggplot(aes(PARAM, NUMERIC_ENTRY, color = STATUSX, shape = LOQ)) +
  scale_color_manual(values = c("Ferdig" = "black", "Midlertidig" = "red2")) + 
  scale_shape_manual(values = c("Over LOQ" = 19, "Under LOQ" = 25)) +
  geom_jitter(height = 0, width = 0.2) +
  ggeasy::easy_rotate_x_labels(-45)

#
# Plot PFAS except PFOS + PFOSA at IO
#
df_labware %>%
  filter(
    !is.na(NUMERIC_ENTRY),
    AQUAMONITOR_CODE %in% "IO",
    PARAM_group %in% "PFAS",
    PARAM %in% c("PFDA", "PFDoDA", "PFDS")) %>%
  ggplot(aes(PARAM, NUMERIC_ENTRY, color = STATUSX, shape = LOQ)) +
  scale_color_manual(values = c("Ferdig" = "black", "Midlertidig" = "red2")) + 
  scale_shape_manual(values = c("Over LOQ" = 19, "Under LOQ" = 25)) +
  geom_jitter(height = 0, width = 0.2) +
  ggeasy::easy_rotate_x_labels(-45) +
  facet_wrap(vars(BIOTA_matrix))




#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# . Summarize "station-less" data ----
#
# Data with no AQUAMONITOR_CODE
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# Check if any lacks AQUAMONITOR_CODE at all
# - oh yes
if (FALSE){  # a bit slow so I put it in FALSE   
get_nivabase_data(paste(
  "select count(*)",
  "from NIVADATABASE.LABWARE_IMPORT where AQUAMONITOR_CODE is NULL"))
}
# 151766

# Records lacking AQUAMONITOR_CODE in 2020
get_nivabase_data(paste(
  "select PROSJEKT, count(*)",
  "from NIVADATABASE.LABWARE_IMPORT where AQUAMONITOR_CODE is NULL",
  "and extract(YEAR from SAMPLED_DATE) = 2020",
  "group by PROSJEKT"))
# n = 151766
# examples
# 1                                                                 O 15254 Øvre Jerpetjern       52
# 3                                                                      O 17146 Urbanfjord       41
# 4     O 180127;BEF20 - Gyroklor 2018-2020; Feltforsøk og Labundersøkelser - Befaring 2020       63
# 12                                                 O 190004;APNEM Lakselusmiddel Akvaplan       60
# 14                         O 200210 - Overvåking av forurensning i Ormevigtjern i Arendal      130
# 15                                           O 180082;WP1 - NorSOOP, Ships of Opportunity       22
# 18                  O 200089;HY_ANA - Økokyst DP Skagerrak; Hydrografi analyse og rapport        6
# 22                                  O 180001;WP8 - Data Cube Service for Copernicus - WP8      111
# 26                                       O 190004;RAM - 2019 Div 414; Ramboll Tyrifjorden       72
# 27                              O 200084;2 ØKOSTOR Basisovervåking av store innsjøer 2020      105
# 28                                                     413 Ringtest Akkrediterte analyser      788



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# . Check some "station-less" data ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#
# . - ØKOSTOR ----
# ØKOSTOR Basisovervåking av store innsjøer 2020
# Most but not all data have a station
#

df_check1 <- get_nivabase_data(paste(
  "select * from NIVADATABASE.LABWARE_IMPORT where",
  "PROSJEKT like '%200084%' and extract(YEAR from SAMPLED_DATE) = 2020"))
head(df_check1)
xtabs(~addNA(AQUAMONITOR_CODE) + SAMPLE_TYPE, df_check1 )
# 012-605-L      4921     FEM-P     GJE-P     LIM-P     RØS-P     SAL-P     SEL-P     SNÅ-P     TAK-P      <NA> 
#       26        27       449       354       307       316       433       391       360       345       105 

# Stations with no AQUAMONITOR_CODE
df_check1 %>% filter(is.na(AQUAMONITOR_CODE)) %>% head(3)
df_check1 %>% filter(is.na(AQUAMONITOR_CODE)) %>% pull(DESCRIPTION) %>% table()
# Prøve 11 Prøve 12 Prøve 13 Prøve 15 Prøve 16 Prøve 17 Prøve 18 

# Check sample table - no more information there
stns <- df_check1 %>% filter(is.na(AQUAMONITOR_CODE)) %>% pull(TEXT_ID) %>% unique()
df_check1_1 <- get_nivabase_selection(
  "*", "LABWARE_CHECK_SAMPLE", "TEXT_ID", stns, values_are_text = TRUE)

#
# . - Urbanfjord ----
# O 17146 Urbanfjord
# Again, most but not all data have a station
#

df_check2 <- get_nivabase_data(paste(
  "select * from NIVADATABASE.LABWARE_IMPORT where",
  "PROSJEKT like '%17146%' and extract(YEAR from SAMPLED_DATE) = 2020"))
head(df_check2)
xtabs(~addNA(AQUAMONITOR_CODE) + SAMPLE_TYPE, df_check2 )

# Stations with no AQUAMONITOR_CODE
df_check2 %>% filter(is.na(AQUAMONITOR_CODE)) %>% head(3)   
# DESCRIPTION says "Sediment-Cm21"

#
# . - Økokyst ----
# O 200089;HY_ANA - Økokyst DP Skagerrak; Hydrografi analyse og rapport
# Most but not all data have a station
#

df_check3 <- get_nivabase_data(paste(
  "select * from NIVADATABASE.LABWARE_IMPORT where",
  "PROSJEKT like '%200089%' and extract(YEAR from SAMPLED_DATE) = 2020"))
head(df_check3)
xtabs(~addNA(AQUAMONITOR_CODE) + SAMPLE_TYPE, df_check3 )

# Stations with no AQUAMONITOR_CODE
df_check3 %>% filter(is.na(AQUAMONITOR_CODE)) %>% head(3)
# DESCRIPTION says 'Blank'

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
## Quartzcorp data ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# check_sample <- get_nivabase_data("select * from NIVADATABASE.LABWARE_CHECK_SAMPLE where PROSJEKT = 'QuartzCorp overvåkning'")
check_sample <- get_nivabase_data("select * from NIVADATABASE.LABWARE_CHECK_SAMPLE where lower(PROSJEKT) like '%quartzcorp%'")

check_results <- get_nivabase_data("select * from NIVADATABASE.LABWARE_IMPORT where lower(PROSJEKT) like '%quartzcorp%'")


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
## Industry project data (Sigurd) ----
#
# Ranfjorden: O-210181, tre blåskjellstasjoner. Resultatene er klare der.
# Vefsnfjorden: O-210239, blåskjellstasjoner. Resultatene er klare.
# Årdalsfjorden: O-210266, blåskjellstasjoner. Resultatene ikke klare ennå.
# Høyangsfjorden: O-210293, blåskjellstasjoner. Resultatene ikke klare ennå.
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

check_results_rana <- get_nivabase_data(paste(
  "select * from NIVADATABASE.LABWARE_IMPORT where lower(PROSJEKT) like '%210181%'",
  "and extract(YEAR from SAMPLED_DATE) >= 2020"))

table(addNA(check_results_rana$STATUS))
table(addNA(check_results_rana$STATUSX))
xtabs(~addNA(AQUAMONITOR_CODE) + SAMPLE_TYPE, check_results_rana)
xtabs(~addNA(AQUAMONITOR_CODE) + SAMPLE_TYPE + year(SAMPLED_DATE), check_results_rana)
xtabs(~addNA(AQUAMONITOR_CODE) + SAMPLE_TYPE + year(SAMPLED_DATE) + STATUSX, check_results_rana)

check_results_aardal <- get_nivabase_data(paste(
  "select * from NIVADATABASE.LABWARE_IMPORT where lower(PROSJEKT) like '%210266%'",
  "and extract(YEAR from SAMPLED_DATE) >= 2020"))

table(addNA(check_results_aardal$STATUS))
table(addNA(check_results_aardal$STATUSX))
xtabs(~addNA(AQUAMONITOR_CODE) + SAMPLE_TYPE + year(SAMPLED_DATE), check_results_aardal)
xtabs(~addNA(AQUAMONITOR_CODE) + SAMPLE_TYPE + year(SAMPLED_DATE) + STATUSX, check_results_aardal)

check_results_hoyang <- get_nivabase_data(paste(
  "select * from NIVADATABASE.LABWARE_IMPORT where lower(PROSJEKT) like '%210266%'",
  "and extract(YEAR from SAMPLED_DATE) >= 2020"))

table(addNA(check_results_hoyang$STATUS))
table(addNA(check_results_hoyang$STATUSX))
xtabs(~addNA(AQUAMONITOR_CODE) + SAMPLE_TYPE + year(SAMPLED_DATE), check_results_hoyang)
xtabs(~addNA(AQUAMONITOR_CODE) + SAMPLE_TYPE + year(SAMPLED_DATE) + STATUSX, check_results_hoyang)

xtabs(~addNA(REPORTED_NAME) + SAMPLE_TYPE, check_results_hoyang)


