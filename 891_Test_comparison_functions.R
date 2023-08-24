

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
#  Identify series to reanalyse, using compareDF package
# 
#  Conclusion: works nicely and quickly, see 'Test on all data' below
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


library(compareDF)

compare_df(a1, a2, c("a","b"))

dat1 <- readRDS("Files_from_Jupyterhub_2020/Raw_data/110_mediandata_updated_2021-10-08.rds") %>%
  filter(Basis == "WW")

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Test on just one series / one year ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

dat2 <- dat1
sel <- with(dat2, PARAM == "HG" & STATION_CODE == "30B" & MYEAR == 2020)

# Manipulate one value
i <- which(sel)
dat2$Value[i] <- dat2$Value[i]*1.3

# Compare
compare_df(dat2[sel,], dat1[sel,], c("MYEAR", "STATION_CODE", "LATIN_NAME", "TISSUE_NAME", "PARAM"))

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Test on three full series (all years) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

dat2 <- dat1

# Data to test
sel <- with(dat2, PARAM == "HG" & STATION_CODE %in% c("30B","23B","53B", "36B"))
dat1_test <- dat1[sel,]
dat2_test <- dat2[sel,]

# Manipulate one value
sel2 <- with(dat2_test, PARAM == "HG" & STATION_CODE == "30B" & MYEAR == 2020)
dat2_test$Value[sel2] <- dat2_test$Value[sel2]*1.3

# Shorten one series
sel3 <- with(dat2_test, PARAM == "HG" & STATION_CODE == "23B" & MYEAR == 2020); sum(sel3)
dat2_test <- dat2_test[!sel3,]

# Test plot
bind_rows(
  dat1_test %>% mutate(Dataset = "a"),
  dat2_test %>% mutate(Dataset = "b")
) %>% #View()
  ggplot(aes(MYEAR, Value, color = Dataset)) +
  geom_line() +
  geom_point() +
  facet_wrap(vars(STATION_CODE))

compare <- compare_df(dat2_test, dat1_test, c("MYEAR", "STATION_CODE", "LATIN_NAME", "TISSUE_NAME", "PARAM"))  

# Identify time series that have changed
compare$comparison_df %>%
  distinct(STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM)

# Identify time series that have been added
dat2_test %>%
  anti_join(dat1_test, by = c("STATION_CODE", "LATIN_NAME", "TISSUE_NAME", "PARAM")) %>%
  count(STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM)

#
# Also "Add" one series in data set 2 (delete one series in data set 1, now called 1b)
#

sel4 <- with(dat1_test, PARAM == "HG" & STATION_CODE == "53B"); sum(sel4)
dat1b_test <- dat1_test[!sel4,]

# Identify time series that have been added
dat2_test %>%
  anti_join(dat1b_test, by = c("STATION_CODE", "LATIN_NAME", "TISSUE_NAME", "PARAM")) %>%
  count(STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM)



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Test on all data ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


dat1_test <- dat1
dat2_test <- dat2

# Manipulate one value
sel2 <- with(dat2_test, PARAM == "HG" & STATION_CODE == "30B" & MYEAR == 2020)
dat2_test$Value[sel2] <- dat2_test$Value[sel2]*1.3

# Shorten one series
sel3 <- with(dat2_test, PARAM == "HG" & STATION_CODE == "23B" & MYEAR == 2020); sum(sel3)
dat2_test <- dat2_test[!sel3,]

# "Add" one series in data set 2 (delete one series in data set 1)
sel4 <- with(dat1_test, PARAM == "HG" & STATION_CODE == "53B"); sum(sel4)
dat1_test <- dat1_test[!sel4,]

# Remove one series in data set 2   
sel5 <- with(dat2_test, PARAM == "HG" & STATION_CODE == "36B"); sum(sel5)
dat2_test <- dat2_test[!sel5,]

# Identify time series that have been added
dat_series_added <- dat2_test %>%
  anti_join(dat1_test, by = c("STATION_CODE", "LATIN_NAME", "TISSUE_NAME", "PARAM")) %>%
  count(STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, name = "Records_changed")

dat2_test_b <- dat2_test %>%
  left_join(dat_series_added, by = c("STATION_CODE", "LATIN_NAME", "TISSUE_NAME", "PARAM"))

# Identify time series that have been removed
dat_series_removed <- dat1_test %>%
  anti_join(dat2_test, by = c("STATION_CODE", "LATIN_NAME", "TISSUE_NAME", "PARAM")) %>%
  count(STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, name = "Records_removed")

dat1_test_b <- dat1_test %>%
  left_join(dat_series_removed, by = c("STATION_CODE", "LATIN_NAME", "TISSUE_NAME", "PARAM"))

# 5 seconds or so
compare <- compare_df(
  dat2_test_b %>% filter(is.na(Records_changed)) %>% select(-Records_changed), 
  dat1_test_b %>% filter(is.na(Records_removed)) %>% select(-Records_removed), 
  c("MYEAR", "STATION_CODE", "LATIN_NAME", "TISSUE_NAME", "PARAM"))  

# Identify time series that have changed
dat_series_changed <- compare$comparison_df %>%
  distinct(STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, MYEAR) %>%
  count(STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, name = "Records_changed")

# Conclusion
# These series shall be reanalysed
dat_series_to_renalyse <- bind_rows(
  dat_series_added,
  dat_series_changed
)

# These series shall be removed from former analysis
dat_series_removed

