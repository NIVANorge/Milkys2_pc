---
title: "868_Plot_proportion_detected_2020"
author: "DHJ"
date: "8 12 2021"
output: 
  html_document:
    toc: true
    toc_float: true  
    keep_md: true
    code_folding: hide
    
---

## 0. Settings  

```r
current_year <- 2020

# Don't include code at all
# knitr::opts_chunk$set(echo = FALSE)  

knitr::opts_chunk$set(results = "hold")  
```


## 1. Packages  

```r
suppressPackageStartupMessages(library(dplyr))
library(viridisLite)
library(ggplot2)
library(stringr)    # str_extract
# library(forcats)    # 
# library(viridis)
```

## 2. Data  

```r
dat <- readRDS("Files_from_Jupyterhub_2020/Raw_data/109_adjusted_data_2021-09-15.rds")
```

### Parameter ordering

```r
substance_groups <- c("Metals and metalloids", 
                      "Chlorobiphenyls", 
                      "Polycyclic aromatic hydrocarbons (PAHs)", 
                      "Chlorinated paraffins", 
                      "Biological effects: molecular/biochemical/cellular/assays", "Biomarkers", 
                      "Dichloro-diphenyl-trichloroethane (DDTs)", 
                      "Organochlorines (general)",
                      "Organo-metallic compounds", 
                      "Organobromines", 
                      "Organofluorines", 
                      "Siloxanes", 
                      "Chlorinated flame retardants", 
                      "Dioxins", "Hexachlorocyclohexanes", 
                      "Isotopes", "Organic esters", 
                      "Phenols/chlorophenols", 
                      "Phosphorus flame retardant (PFR)", "Phthalates", 
                      "Triazines",
                      "Support parameters")

if (FALSE){
  
  dat_excel <- readRDS("Files_from_Jupyterhub_2020/Big_excel_table/Data_xl_2021-10-08_ver01.rds")
  
  df_pars_orig <- dat_excel %>%
    filter(!is.na(Yr_2020)) %>%
    distinct(Substance.Group, PARAM) %>%
    mutate(
      Substance.Group = factor(Substance.Group, levels = substance_groups),
      param_no = str_extract(PARAM, "[0-9]+") %>% as.numeric()
    ) %>%   # for ordering CBs and BDEs correctly
    arrange(Substance.Group, param_no, PARAM)
  
  # df_pars_orig$Substance.Group %>% unique() %>% dput()
  
  writexl::write_xlsx(df_pars_orig, "Data/868_df_pars_orig.xlsx")
  
  # edited manually to create '868_df_pars_edit.xlsx'
  # the 'param_no' variable was also edited to form the 'sorting' variable  
  
}

df_pars <- readxl::read_excel("Data/868_df_pars_edit.xlsx") %>%
  mutate(Substance.Group = factor(Substance.Group, levels = substance_groups)) %>%
  arrange(Substance.Group, Sorting)

param_levels <- df_pars %>% pull(PARAM)
# param_levels %>% dput()

# Remove sum and support parameters
param_exclude <- c("CB_S7", "Sum-HepCB", "Sum-HexCB", "Sum-PenCB", "Sum-TetCB", "Sum-TriCB", "PeCB", 
               "P_S", "PK_S", "PAHSS", "BDE6s", "BDESS", "PFAS", 
               "Delta13C", "Delta15N", "C/N", "DRYWT%", "% C", "% N", "Fett")
param_levels <- param_levels[!param_levels %in% param_exclude]

# Remove tin combounds measured as tin atom weight:
sel <- grepl("-Sn", param_levels, fixed = TRUE)
# param_levels[sel]
param_levels <- param_levels[!sel]

# param_levels <- factor(param_levels, levels = param_levels)
```

### Stations, for ordering  

```r
stations_table <- c(
  "30A", "I301", "I304", "31A", "36A", "I023", "I024",     # Blue mussel, Oslofjorden
  "71A", "I714", "76A2", "I131A", "I133", "15A",           # Blue mussel, Grenland - Sørlandet
  "51A", "52A", "56A", "57A", "64A", "65A", "22A",         # Blue mussel, Vestlandet - Trøndelag
     "I241", "26A2", "28A2", "91A2",                       #  - " -
  "97A2", "97A3", "98A2", "10A2", "11X",                   # Blue mussel, Nord-Norge
  "30B", "36B", "02B", "71B","13B", "15B",                 # Cod
    "53B", "23B", "24B", "28B", "80B", "96B",              # Cod            
    "98B1", "43B2", "45B2", "10B", "19B",                  # Cod
    "33F",                                                 # European flounder
  "19N",                                                   # Eider duck - twice (blood and egg)
  "71G",                                                   # Dog whelk / periwinkle
  "36G", "76G", "131G", "15G", "227G",                     # Dog whelk
    "22G", "98G", "11G")                                   # Dog whelk

# stations_table

# Get some line numbers for table - these are used later
a <- which(stations_table == "11X")    # last blue mussel
b <- which(stations_table == "33F")    # last cod and flounder
c <- which(stations_table == "19N")    # first bird    
d <- which(stations_table == "71G")    # first snail
e <- length(stations_table)            # last

stations_mussel <- stations_table[1:a]     # Not including I965, I969
stations_fish <- stations_table[(a+1):b]
```

## 3. Proportion over LOQ, by tissue   

### Calculate detection %  

```r
#
#
df_detect <- dat %>%
  filter(MYEAR %in% current_year &
           PARAM %in% param_levels) %>%
  mutate(PARAM = factor(PARAM, levels = param_levels)) %>%
  group_by(TISSUE_NAME, PARAM) %>%
  summarize(N = sum(!is.na(VALUE_WW)), Nondetect_n = sum(!is.na(FLAG1)), Detect = 100 - 100*Nondetect_n/N )
```

```
## `summarise()` has grouped output by 'TISSUE_NAME'. You can override using the `.groups` argument.
```

```r
# table(df_detect$TISSUE_NAME) %>% names() %>% dput()
```

### Plot    

```r
# tissues_plot <- c("Whole soft body", "Lever", "Muskel", "Blod", "Egg")  # Muskel is not so interesting
tissues_plot <- c("Whole soft body", "Lever", "Blod", "Egg")
tissues_plot_eng <- c("Whole soft body", "Liver", "Blood", "Egg")

gg <- ggplot(df_detect %>%
         filter(TISSUE_NAME %in% tissues_plot) %>%
         mutate(
           TISSUE_NAME = case_when(
             TISSUE_NAME == "Blod" ~ "Blood",
             TISSUE_NAME == "Lever" ~ "Liver",
             TRUE ~ TISSUE_NAME
           ),
           TISSUE_NAME = factor(TISSUE_NAME, levels = tissues_plot_eng),
           PARAM = factor(PARAM, levels = rev(param_levels)), # rev: for ordering top -> down
           Detect = cut(Detect, breaks = c(0, 20, 40, 60, 80, 99, 100), include.lowest = TRUE)
         ),
       aes(TISSUE_NAME, PARAM, fill = Detect)) +
  geom_raster() +
  scale_fill_viridis_d("Over LOQ (%)") +
  theme(axis.text.x = element_text(hjust = 0, angle = -35)) +
  theme(axis.text.y = element_text(size = 6)) +
  labs(
    x = "Tissue", y = "Parameter"
  )

ggsave("Figures/868_Prop_over_LOQ_by_tissue.png", gg, height = 16, width = 7, dpi = 300)

gg
```

![](868_Plot_proportion_detected_2020_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


## 4. Proportion over LOQ, by blue mussel station     

### Calculate detection %  

```r
df_detect_mussel <- dat %>%
  filter(MYEAR %in% current_year &
           PARAM %in% param_levels &
           LATIN_NAME == "Mytilus edulis" &     # not including I965, I969
           STATION_CODE %in% stations_mussel) %>%
  mutate(PARAM = factor(PARAM, levels = param_levels)) %>%
  group_by(STATION_CODE, PARAM) %>%
  summarize(N = sum(!is.na(VALUE_WW)), Nondetect_n = sum(!is.na(FLAG1)), Detect = 100 - 100*Nondetect_n/N )
```

```
## `summarise()` has grouped output by 'STATION_CODE'. You can override using the `.groups` argument.
```

```r
# table(df_detect$TISSUE_NAME) %>% names() %>% dput()
```

### Plot    

```r
# tissues_plot <- c("Whole soft body", "Lever", "Muskel", "Blod", "Egg")  # Muskel is not so interesting
tissues_plot <- c("Whole soft body", "Lever", "Blod", "Egg")

ggplot(df_detect_mussel %>%
         mutate(
           PARAM = factor(PARAM, levels = rev(param_levels)), # rev: for ordering top -> down
           STATION_CODE = factor(STATION_CODE, levels = stations_mussel),
           Detect = cut(Detect, breaks = c(0, 20, 40, 60, 80, 99, 100), include.lowest = TRUE)
         ),
       aes(STATION_CODE, PARAM, fill = Detect)) +
  geom_raster() +
  scale_fill_viridis_d() +
  theme(axis.text.x = element_text(hjust = 0, angle = -60)) +
  theme(axis.text = element_text(size = 7))
```

![](868_Plot_proportion_detected_2020_files/figure-html/unnamed-chunk-9-1.png)<!-- -->


## 5. Proportion over LOQ (liver), by fish station     

### Calculate detection %  

```r
df_detect_fish <- dat %>%
  filter(MYEAR %in% current_year &
           PARAM %in% param_levels &
           TISSUE_NAME == "Lever") %>%
  mutate(PARAM = factor(PARAM, levels = param_levels)) %>%
  group_by(STATION_CODE, PARAM) %>%
  summarize(N = sum(!is.na(VALUE_WW)), Nondetect_n = sum(!is.na(FLAG1)), Detect = 100 - 100*Nondetect_n/N )
```

```
## `summarise()` has grouped output by 'STATION_CODE'. You can override using the `.groups` argument.
```

```r
# table(df_detect$TISSUE_NAME) %>% names() %>% dput()
```

### Plot    

```r
# tissues_plot <- c("Whole soft body", "Lever", "Muskel", "Blod", "Egg")  # Muskel is not so interesting
tissues_plot <- c("Whole soft body", "Lever", "Blod", "Egg")

ggplot(df_detect_fish %>%
         mutate(
           PARAM = factor(PARAM, levels = rev(param_levels)), # rev: for ordering top -> down
           STATION_CODE = factor(STATION_CODE, levels = stations_fish),
           Detect = cut(Detect, breaks = c(0, 20, 40, 60, 80, 99, 100), include.lowest = TRUE)
         ),
       aes(STATION_CODE, PARAM, fill = Detect)) +
  geom_raster() +
  scale_fill_viridis_d() +
  theme(axis.text.x = element_text(hjust = 0, angle = -60)) +
  theme(axis.text = element_text(size = 7))
```

![](868_Plot_proportion_detected_2020_files/figure-html/unnamed-chunk-11-1.png)<!-- -->


## 6. Observed LOQ over time, blue mussel     

#### Functions for plotting 

```r
plot_loq_data <- function(relative_loq = "LOQ_rel2015", 
                     data =  df_loq_mussel_yr,
                     parameters = param_levels){
  
  data$LOQ_rel <- data[[relative_loq]]
  
  data %>%
    filter(PARAM %in% parameters) %>%
    mutate(
      LOQ_rel2 = case_when(
        is.na(LOQ) ~ -2,
        !is.na(LOQ) & is.na(LOQ_rel) ~ -1,
        TRUE ~ LOQ_rel,
      ),
      PARAM = factor(PARAM, levels = rev(parameters)), # rev: ordering top -> down
      Year = factor(MYEAR),
      LOQ_rel3 = cut(LOQ_rel2, 
                     breaks = c(-2.1, -1.1, 0, 0.5, 0.9, 1.1, 2, 4, 11, 1000), 
                     labels = c("No values <LOQ", "No LOQ reference", 
                                "LOQ down >50%", "LOQ down <50%", "LOQ unchanged",
                                "LOQ up <100%", "LOQ up <300%", "LOQ up <1000%", "LOQ up >1000%"),
                     include.lowest = TRUE),
      
    )

}

# plot_loq_data() %>% View()


plot_loq <- function(relative_loq = "LOQ_rel2015", 
                     legend_text = "LOQ rel. to 2010-2015",
                     data =  df_loq_mussel_yr,
                     parameters = param_levels){
  
  data <- plot_loq_data(relative_loq = relative_loq, 
                        data = data,
                        parameters = parameters)

  gg <- ggplot(data, aes(Year, PARAM, fill = LOQ_rel3)) +
    geom_raster() +
    geom_text(aes(label = round(LOQ, 3)), size = 3) +
    # scale_fill_brewer(palette = "RdYlBu", direction = -1) +
    scale_fill_manual("LOQ rel. to 2010-2015", values = fill_cols) +
    theme_bw() +
    theme(axis.text.x = element_text(hjust = 0, angle = -30)) +
    theme(axis.text.y = element_text(size = 7)) +
    labs(y = "Parameter")
  
  gg

}
```


### Calculate observed LOQ    
- Observed LOQ = median value of less than-values   
    * If no less than-values for a given year, no LOQ  
- Also extracting reference levels 2015 (2010-2015) and 2018 (2016-2018)

```r
# Get N_less_thans per year
# - in order to separate between years with no data, and years with no less-thans   
df_loq_mussel_yr_1 <- dat %>%
  filter(MYEAR %in% 2000:2020 &
           PARAM %in% param_levels &
           LATIN_NAME == "Mytilus edulis" &     # not including I965, I969
           STATION_CODE %in% stations_mussel) %>%
  group_by(PARAM, MYEAR) %>%
  summarise(
    N = length(FLAG1),
    N_less_thans = sum(!is.na(FLAG1))
    )
```

```
## `summarise()` has grouped output by 'PARAM'. You can override using the `.groups` argument.
```

```r
# LOQ for each year = median values of less than-values
df_loq_mussel_yr_2 <- dat %>%
  # filter(PARAM %in% c("ACNE", "ACNLE")) %>%
  filter(MYEAR %in% 2000:2020 &
           PARAM %in% param_levels &
           LATIN_NAME == "Mytilus edulis" &     # not including I965, I969
           STATION_CODE %in% stations_mussel &
           !is.na(FLAG1)) %>%
  group_by(PARAM, MYEAR) %>%
  summarise(LOQ = median(VALUE_WW), .groups = "drop")

# Add LOQ values to the first one 
df_loq_mussel_yr_3 <- df_loq_mussel_yr_1 %>%
  left_join(df_loq_mussel_yr_2, by = c("PARAM", "MYEAR"))

# Reference level 2015 = median values in the latest year of years 2010-2015
df_loq_mussel_yr_ref2015 <- df_loq_mussel_yr_2 %>%
  filter(MYEAR <= 2015) %>%
  group_by(PARAM) %>%
  summarise(LOQ_ref2015 = LOQ[MYEAR == max(MYEAR)], .groups = "drop")

# Reference level 2018 = median values in the latest year of years 2016-2018
df_loq_mussel_yr_ref2018 <- df_loq_mussel_yr_2 %>%
  filter(MYEAR %in% 2016:2018) %>%
  group_by(PARAM) %>%
  summarise(LOQ_ref2018 = LOQ[MYEAR == max(MYEAR)], .groups = "drop")

# Add LOQ_ref2015 and LOQ_ref2018 (reference levels 2015/2018) 
# Calculate LOQ_rel2015 and LOQ_rel2018 (LOQ relativ to reference levels) 
df_loq_mussel_yr <- df_loq_mussel_yr_3 %>%
  left_join(df_loq_mussel_yr_ref2015, by = "PARAM") %>%
  left_join(df_loq_mussel_yr_ref2018, by = "PARAM") %>%
  mutate(
    LOQ_rel2015 = LOQ / LOQ_ref2015,
    LOQ_rel2018 = LOQ / LOQ_ref2018)

# table(df_detect$TISSUE_NAME) %>% names() %>% dput()
```

### Parameter groups for 'selected groups'  

```r
i1 <- seq(which(param_levels == "Aldrin"), which(param_levels == "HCHD"))
i2 <- seq(which(param_levels == "PFOS"), which(param_levels == "PFUdA"))
i3 <- grep("Toksafen", param_levels)
param_levels_mussel1 <- param_levels[-c(i1,i2,i3)]
param_levels_mussel2 <- param_levels[c(i1,i2,i3)]
```

#### Define colors

```r
if (FALSE){
  par(mar = c(1,4,1,1))
  RColorBrewer::display.brewer.all()
  RColorBrewer::display.brewer.all(type = "seq")
  # RColorBrewer::display.brewer.pal(8, "YlOrRd")
}

# See plot_loq_data() above for categories
fill_cols <- c("grey80",
               RColorBrewer::brewer.pal(8, name = "Pastel1")[6],      
               RColorBrewer::brewer.pal(8, name = "Blues")[c(3,5)] %>% rev(),
               RColorBrewer::brewer.pal(8, name = "PuRd")[5],
               RColorBrewer::brewer.pal(9, name = "YlOrRd")[5:8])
```


### Plot relative to 2015, all    


```r
gg <- plot_loq()

ggsave("Figures/868_Median_LOQ_mussel_all.png", gg, width=11, height=16, dpi=300)
```

```
## Warning: Removed 394 rows containing missing values (geom_text).
```

```r
gg
```

```
## Warning: Removed 394 rows containing missing values (geom_text).
```

![](868_Plot_proportion_detected_2020_files/figure-html/unnamed-chunk-16-1.png)<!-- -->
  
 
### Plot relative to 2015, selected groups     
- groups that have a reference value  

```r
plot_loq(relative_loq = "LOQ_rel2015", 
         legend_text = "LOQ rel. to 2010-2015",
         data =  df_loq_mussel_yr,
         parameters = param_levels_mussel1)
```

```
## Warning: Removed 394 rows containing missing values (geom_text).
```

![](868_Plot_proportion_detected_2020_files/figure-html/unnamed-chunk-17-1.png)<!-- -->


### Plot relative to 2018, all     
- groups that have a reference value  

```r
plot_loq(relative_loq = "LOQ_rel2018", 
         legend_text = "LOQ rel. to 2016-2018",
         data =  df_loq_mussel_yr,
         parameters = param_levels)
```

```
## Warning: Removed 394 rows containing missing values (geom_text).
```

![](868_Plot_proportion_detected_2020_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

### Plot relative to 2018, selected groups     
- groups that have a reference value  

```r
plot_loq(relative_loq = "LOQ_rel2018", 
         legend_text = "LOQ rel. to 2016-2018",
         data =  df_loq_mussel_yr,
         parameters = param_levels_mussel2)
```

![](868_Plot_proportion_detected_2020_files/figure-html/unnamed-chunk-19-1.png)<!-- -->


## 7. Observed LOQ over time, fish liver       

### Calculate observed LOQ    
- Observed LOQ = median value of less than-values   
    * If no less than-values for a given year, no LOQ  
- Also extracting reference levels 2015 (2010-2015) and 2018 (2016-2018)

```r
# Get N_less_thans per year
# - in order to separate between years with no data, and years with no less-thans   
df_loq_fish_yr_1 <- dat %>%
  filter(MYEAR %in% 2000:2020 &
           PARAM %in% param_levels &
           TISSUE_NAME == "Lever" &     
           STATION_CODE %in% stations_fish) %>%
  group_by(PARAM, MYEAR) %>%
  summarise(
    N = length(FLAG1),
    N_less_thans = sum(!is.na(FLAG1))
    )
```

```
## `summarise()` has grouped output by 'PARAM'. You can override using the `.groups` argument.
```

```r
# LOQ for each year = median values of less than-values
df_loq_fish_yr_2 <- dat %>%
  # filter(PARAM %in% c("ACNE", "ACNLE")) %>%
  filter(MYEAR %in% 2000:2020 &
           PARAM %in% param_levels &
           TISSUE_NAME == "Lever" &     
           STATION_CODE %in% stations_fish &
           !is.na(FLAG1)) %>%
  group_by(PARAM, MYEAR) %>%
  summarise(LOQ = median(VALUE_WW), .groups = "drop")

# Add LOQ values to the first one 
df_loq_fish_yr_3 <- df_loq_fish_yr_1 %>%
  left_join(df_loq_fish_yr_2, by = c("PARAM", "MYEAR"))

# Reference level 2015 = median values in the latest year of years 2010-2015
df_loq_fish_yr_ref2015 <- df_loq_fish_yr_2 %>%
  filter(MYEAR <= 2015) %>%
  group_by(PARAM) %>%
  summarise(LOQ_ref2015 = LOQ[MYEAR == max(MYEAR)], .groups = "drop")

# Reference level 2018 = median values in the latest year of years 2016-2018
df_loq_fish_yr_ref2018 <- df_loq_fish_yr_2 %>%
  filter(MYEAR %in% 2016:2018) %>%
  group_by(PARAM) %>%
  summarise(LOQ_ref2018 = LOQ[MYEAR == max(MYEAR)], .groups = "drop")

# Add LOQ_ref2015 and LOQ_ref2018 (reference levels 2015/2018) 
# Calculate LOQ_rel2015 and LOQ_rel2018 (LOQ relativ to reference levels) 
df_loq_fish_yr <- df_loq_fish_yr_3 %>%
  left_join(df_loq_fish_yr_ref2015, by = "PARAM") %>%
  left_join(df_loq_fish_yr_ref2018, by = "PARAM") %>%
  mutate(
    LOQ_rel2015 = LOQ / LOQ_ref2015,
    LOQ_rel2018 = LOQ / LOQ_ref2018)

# table(df_detect$TISSUE_NAME) %>% names() %>% dput()
```


### Calculate observed LOQ     

```r
df_loq_fish_yr_1 <- dat %>%
  # filter(PARAM %in% c("ACNE", "ACNLE")) %>%
  filter(MYEAR %in% 2010:2020 &
           PARAM %in% param_levels &
           TISSUE_NAME == "Lever" &     # not including I965, I969
           STATION_CODE %in% stations_fish &
           !is.na(FLAG1)) %>%
  group_by(PARAM, MYEAR) %>%
  summarise(LOQ = median(VALUE_WW), .groups = "drop")

df_loq_fish_yr_ref2015 <- df_loq_fish_yr_1 %>%
  filter(MYEAR <= 2015) %>%
  group_by(PARAM) %>%
  summarise(LOQ_ref2015 = LOQ[MYEAR == max(MYEAR)], .groups = "drop")

df_loq_fish_yr_ref2018 <- df_loq_fish_yr_1 %>%
  filter(MYEAR %in% 2016:2018) %>%
  group_by(PARAM) %>%
  summarise(LOQ_ref2018 = LOQ[MYEAR == max(MYEAR)], .groups = "drop")

df_loq_fish_yr <- df_loq_fish_yr_1 %>%
  left_join(df_loq_fish_yr_ref2015, by = "PARAM") %>%
  left_join(df_loq_fish_yr_ref2018, by = "PARAM") %>%
  mutate(
    LOQ_rel2015 = LOQ / LOQ_ref2015,
    LOQ_rel2018 = LOQ / LOQ_ref2018)

# table(df_detect$TISSUE_NAME) %>% names() %>% dput()
```


### Parameter groups for 'selected groups'  

```r
i1 <- seq(which(param_levels == "Aldrin"), which(param_levels == "HCHD"))
# i2 <- seq(which(param_levels == "PFOS"), which(param_levels == "PFUdA"))
i3 <- grep("Toksafen", param_levels)
param_levels_fish1 <- param_levels[-c(i1,i3)]
param_levels_fish2 <- param_levels[c(i1,i3)]
```


### Plot relative to 2015, all    


```r
gg <- plot_loq(relative_loq = "LOQ_rel2015", 
               legend_text = "LOQ rel. to 2010-2015",
               data =  df_loq_fish_yr,
               parameters = param_levels_fish1)

ggsave("Figures/868_Median_LOQ_fish_all.png", gg, width=11, height=16, dpi=300)

gg
```

![](868_Plot_proportion_detected_2020_files/figure-html/unnamed-chunk-23-1.png)<!-- -->
  
 
### Plot relative to 2015, selected groups     
- groups that have a reference value  

```r
plot_loq(relative_loq = "LOQ_rel2015", 
         legend_text = "LOQ rel. to 2010-2015",
         data =  df_loq_fish_yr,
         parameters = param_levels_fish1)
```

![](868_Plot_proportion_detected_2020_files/figure-html/unnamed-chunk-24-1.png)<!-- -->


### Plot relative to 2018, all     
- groups that have a reference value  

```r
plot_loq(relative_loq = "LOQ_rel2018", 
         legend_text = "LOQ rel. to 2016-2018",
         data =  df_loq_fish_yr,
         parameters = param_levels)
```

![](868_Plot_proportion_detected_2020_files/figure-html/unnamed-chunk-25-1.png)<!-- -->

### Plot relative to 2018, selected groups     
- groups that have a reference value  

```r
plot_loq(relative_loq = "LOQ_rel2018", 
         legend_text = "LOQ rel. to 2016-2018",
         data =  df_loq_fish_yr,
         parameters = param_levels_fish2)
```

![](868_Plot_proportion_detected_2020_files/figure-html/unnamed-chunk-26-1.png)<!-- -->






## 8. Median values (including LOQ)   

* Median values (LOQ included)  
* Years/parameters where >50% are under LOQ are shown with smaller letters   

### Blue mussel, calculation    

```r
# Get N_less_thans per year
# - in order to separate between years with no data, and years with no less-thans   
df_medians_mussel <- dat %>%
  filter(MYEAR %in% 2000:2020 &
           PARAM %in% param_levels &
           LATIN_NAME == "Mytilus edulis" &     # not including I965, I969
           STATION_CODE %in% stations_mussel) %>%
  group_by(PARAM, MYEAR) %>%
  summarise(
    N = length(FLAG1),
    N_less_thans = sum(!is.na(FLAG1)),
    VALUE_WW = median(VALUE_WW), 
    log_VALUE_WW = log10(VALUE_WW), 
    .groups = "drop"
    )
```

### Blue mussel, plot      

```r
cat("Range of LOQ_rel2015: \n")
range(df_medians_mussel$log_VALUE_WW, na.rm = TRUE)
quantile(df_medians_mussel$log_VALUE_WW, na.rm = TRUE)
brs <- quantile(df_medians_mussel$log_VALUE_WW, seq(0,1,0.1), na.rm = TRUE)

# tissues_plot <- c("Whole soft body", "Lever", "Muskel", "Blod", "Egg")  # Muskel is not so interesting
tissues_plot <- c("Whole soft body", "Lever", "Blod", "Egg")

ggplot(df_medians_mussel %>%
         filter(PARAM %in% param_levels) %>%
         mutate(
           PARAM = factor(PARAM, levels = rev(param_levels)), # rev: ordering top -> down
           Year = factor(MYEAR),
           log_VALUE_WW = cut(log_VALUE_WW, 
                             breaks = brs, 
                             include.lowest = TRUE),
           VALUE_WW = case_when(
             VALUE_WW >= 10 ~ round(VALUE_WW, 1),
             VALUE_WW >= 1 ~ round(VALUE_WW, 2),
             VALUE_WW < 1 ~ round(VALUE_WW, 3),
           ),
           Over_LOQ = case_when(
             N_less_thans < 0.5*N ~ "Over LOQ", 
             N_less_thans >= 0.5*N ~ "Under LOQ")
         ), #%>%
       # filter(PARAM != "NAP"),
       aes(Year, PARAM, fill = log_VALUE_WW)) +
  geom_raster() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  geom_text(aes(label = round(VALUE_WW, 3), size = Over_LOQ)) +
  scale_size_manual(values = 3:2) +
  theme(axis.text.x = element_text(hjust = 0, angle = -30)) +
  theme(axis.text.y = element_text(size = 7))
```

![](868_Plot_proportion_detected_2020_files/figure-html/unnamed-chunk-28-1.png)<!-- -->

```
## Range of LOQ_rel2015: 
## [1] -5.000000  2.603144
##         0%        25%        50%        75%       100% 
## -5.0000000 -1.3010300 -0.6595604 -0.3005966  2.6031444
```


### Fish liver, calculation    

```r
# Get N_less_thans per year
# - in order to separate between years with no data, and years with no less-thans   
df_medians_fish <- dat %>%
  filter(MYEAR %in% 2000:2020 &
           PARAM %in% param_levels &
           TISSUE_NAME == "Lever" &     # not including I965, I969
           STATION_CODE %in% stations_fish) %>%
  group_by(PARAM, MYEAR) %>%
  summarise(
    N = length(FLAG1),
    N_less_thans = sum(!is.na(FLAG1)),
    VALUE_WW = median(VALUE_WW), 
    log_VALUE_WW = log10(VALUE_WW), 
    .groups = "drop"
    )
```

### Fish liver, plot      

```r
cat("Range of LOQ_rel2015: \n")
range(df_medians_fish$log_VALUE_WW, na.rm = TRUE)
quantile(df_medians_fish$log_VALUE_WW, na.rm = TRUE)
brs <- quantile(df_medians_fish$log_VALUE_WW, seq(0,1,0.1), na.rm = TRUE)

# tissues_plot <- c("Whole soft body", "Lever", "Muskel", "Blod", "Egg")  # Muskel is not so interesting
tissues_plot <- c("Whole soft body", "Lever", "Blod", "Egg")

ggplot(df_medians_fish %>%
         filter(PARAM %in% param_levels) %>%
         mutate(
           PARAM = factor(PARAM, levels = rev(param_levels)), # rev: ordering top -> down
           Year = factor(MYEAR),
           log_VALUE_WW = cut(log_VALUE_WW, 
                             breaks = brs, 
                             include.lowest = TRUE),
           VALUE_WW = case_when(
             VALUE_WW >= 10 ~ round(VALUE_WW, 1),
             VALUE_WW >= 1 ~ round(VALUE_WW, 2),
             VALUE_WW < 1 ~ round(VALUE_WW, 3),
           ),
           Over_LOQ = case_when(
             N_less_thans < 0.5*N ~ "Over LOQ", 
             N_less_thans >= 0.5*N ~ "Under LOQ")
         ), #%>%
       # filter(PARAM != "NAP"),
       aes(Year, PARAM, fill = log_VALUE_WW)) +
  geom_raster() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  geom_text(aes(label = VALUE_WW, size = Over_LOQ)) +
  scale_size_manual(values = 3:2) +
  theme(axis.text.x = element_text(hjust = 0, angle = -30)) +
  theme(axis.text.y = element_text(size = 7))
```

```
## Warning: Removed 3 rows containing missing values (geom_raster).
```

```
## Warning: Removed 3 rows containing missing values (geom_text).
```

![](868_Plot_proportion_detected_2020_files/figure-html/unnamed-chunk-30-1.png)<!-- -->

```
## Range of LOQ_rel2015: 
## [1] -2.096910  2.527037
##         0%        25%        50%        75%       100% 
## -2.0969100 -0.5934598  0.1139434  0.9637878  2.5270367
```

## 8. Median values, test   

### Fish liver, plot      

```r
cat("Range of LOQ_rel2015: \n")
range(df_medians_fish$log_VALUE_WW, na.rm = TRUE)
quantile(df_medians_fish$log_VALUE_WW, na.rm = TRUE)
brs <- quantile(df_medians_fish$log_VALUE_WW, seq(0,1,0.1), na.rm = TRUE)

# tissues_plot <- c("Whole soft body", "Lever", "Muskel", "Blod", "Egg")  # Muskel is not so interesting
tissues_plot <- c("Whole soft body", "Lever", "Blod", "Egg")

ggplot(df_medians_fish %>%
         filter(PARAM %in% param_levels) %>%
         mutate(
           PARAM = factor(PARAM, levels = rev(param_levels)), # rev: ordering top -> down
           Year = factor(MYEAR),
           log_VALUE_WW = cut(log_VALUE_WW, 
                             breaks = brs, 
                             include.lowest = TRUE),
           Over_LOQ = case_when(
             N_less_thans < 0.5*N ~ "Over LOQ", 
             N_less_thans >= 0.5*N ~ "Under LOQ")
         ), #%>%
       # filter(PARAM != "NAP"),
       aes(Year, PARAM, fill = log_VALUE_WW)) +
  geom_raster() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  geom_text(aes(label = VALUE_WW, size = Over_LOQ)) +
  scale_size_manual(values = 2:3) +
  theme(axis.text.x = element_text(hjust = 0, angle = -30)) +
  theme(axis.text.y = element_text(size = 7))
```

```
## Warning: Removed 3 rows containing missing values (geom_raster).
```

```
## Warning: Removed 3 rows containing missing values (geom_text).
```

![](868_Plot_proportion_detected_2020_files/figure-html/unnamed-chunk-31-1.png)<!-- -->

```
## Range of LOQ_rel2015: 
## [1] -2.096910  2.527037
##         0%        25%        50%        75%       100% 
## -2.0969100 -0.5934598  0.1139434  0.9637878  2.5270367
```
