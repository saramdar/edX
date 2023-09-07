2.wrangling - 2.1 reshaping data
library(tidyverse)
library(dslabs)

#examine co2 data set - included in base R
co2
#not tidy, we need to wrangle it to have year, month, and value columns so each co2 observation has a row

#create co2_wide object
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))
co2_wide

#create tidy data set
co2_tidy <- pivot_longer(co2_wide, -year, names_to = "month", values_to = "co2")
co2_tidy

#plot co2 vs month with different curve for each year
co2_tidy %>%
  ggplot(aes(as.numeric(month), co2, color=year)) + geom_line()

#example admissions dataset from dslabs
data(admissions)
admissions
dat <- admissions %>% select(-applicants)
dat
#reshape data a few ways
dat_tidy <- pivot_wider(dat, names_from = gender, values_from = admitted)
dat_tidy

tmp <- admissions %>%
  pivot_longer(cols = c(admitted, applicants), names_to = "key", values_to = "value")
tmp

tmp2 <- unite(tmp, column_name, c(key, gender))
tmp2

pivot_wider(tmp2, names_from = c(column_name), values_from = value)

