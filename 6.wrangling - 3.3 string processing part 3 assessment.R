#6.wrangling 3.3 String processing part 3 - assessment
#Q1
day <- c("Monday", "Tuesday")
staff <- c("Mandy, Chris and Laura", "Steve, Ruth and Frank")
schedule <- data.frame(day, staff)
schedule
#split staff column into individual names
str_split(schedule$staff, ",|and ")
str_split(schedule$staff, ",\\s|\\sand\\s?")

#Q2 turn schedule into tidy table
tidy <- schedule %>% 
  mutate(staff = str_split(staff, ", | and ")) %>% 
  unnest(cols = c(staff))
tidy

#Q3 recode middle africa countries in the gapminder data set that have names longer than 12 letters
#find countries with names longer than 12 letters
gapminder %>%
  filter(region == "Middle Africa") %>%
  filter(str_length(country) >=12) %>%
  distinct(country)
#rename throughout dataset
dat <- gapminder %>% filter(region == "Middle Africa") %>%
  mutate(country_short = recode(country, 
                                "Central African Republic" = "CAR",
                                "Congo, Dem. Rep." = "DRC", 
                                "Equatorial Guinea" = "Eq. Guinea"))
dat %>% distinct(country_short)

#Q5 which expressions return FALSE for 19.5?
str_detect(19.5, "^1\\d*$") #FALSE
str_detect(19.5, "1\\d*") #TRUE
str_detect(19.5, "^1\\d+\\.\\d?$") #TRUE
str_detect(19.5, "[1-9]*\\.5") #TRUE

##Import raw Brexit ref polling data from Wiki
library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[6]] %>% html_table(fill = TRUE)
print(polls, n=134)
names(polls) <- c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes")
polls <-polls[str_detect(polls$remain, "%"),-9] #-9 removes the 9th notes column from the tibbe
nrow(polls)

#Q6 convert remain to a proportion - must first change to a number
as.numeric(str_replace(polls$remain, "%", ""))/100
parse_number(polls$remain)/100
head(polls)

#Q7 replace NAs in undecided column with 0
str_replace_all(polls$undecided, "N/A", "0")

#Q8 use regex to extract the end date and month from dates column
polls$dates
pattern <- "\\d{1,2}\\s[a-zA-Z]+"
temp <-str_extract_all(polls$dates, pattern)
end_date <-sapply(temp, function(x) x[length(x)]) # sapply function takes the last element
end_date
