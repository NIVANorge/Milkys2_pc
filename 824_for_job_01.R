

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(flextable)
library(glue)
library(readxl)
library(purrr)
library(leftcensored)   # DHJ's package (https://github.com/DagHjermann/leftcensored)

params <- unique(dat_medium_fish$PARAM)

for (param in params){
  X <- get_trendobj_parameter_species(param, "Gadus morhua", dat_medium)
}

