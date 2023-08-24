
library(dplyr)

library(ggplot2)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Data from CEMP Access database
#
# \\niva-of5\osl-userdata$\DHJ\Documents\seksjon 212\COCO\Analyser\Biol_effects\Data
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


dat <- read.csv2("Files_for_other_use/CEMP_biol_effects_2016-01-15.csv")

# test <- read.csv("Lookup table - substance groups.txt")

xtabs(~jmpst, dat)

xtabs(~is.na(PYR1O_BI) + myear, dat %>% filter(jmpst == "53B"))

dat %>% 
  filter(jmpst == "53B") %>%
  ggplot(aes(myear, PYR1O_BI)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_y_log10()


dat %>% 
  filter(jmpst == "53B") %>%
  ggplot(aes(myear, PYR1O_BI*AY_BI)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_y_log10()

dat %>% 
  filter(jmpst == "53B") %>%
  ggplot(aes(myear, PA1O_BI)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_y_log10()


dat %>% 
  filter(jmpst %in% c("53B", "23B", "30B")) %>%
  ggplot(aes(myear, PYR1O_BI)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_y_log10() +
  facet_wrap(vars(jmpst))

dat %>% 
  filter(jmpst %in% c("53B", "23B", "30B")) %>%
  ggplot(aes(myear, PA1O_BI)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_y_log10() +
  facet_wrap(vars(jmpst))
