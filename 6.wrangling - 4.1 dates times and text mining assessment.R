#6.wrangling - 4.1 dates times and text mining assessment
library(dslabs)
library(lubridate)
options(digits =3)

#Q3
data("brexit_polls")
head(brexit_polls)
#how many polls had a start date in April?
brexit_polls %>% filter(startdate >= ymd("2016-04-01")& startdate < ymd("2016-05-01")) %>%count()
#how many polls ended the week of 2016-06-12
sum(round_date(brexit_polls$enddate, "week") == "2016-06-12")

#Q4 on which weekday did the greatest number of polls end
brexit_polls %>%
  mutate(enddate_weekday = weekdays(brexit_polls$enddate)) %>% count(enddate_weekday)
#OR
table(weekdays(brexit_polls$enddate))

#Q5
data("movielens")
head(movielens)

movielens %>%
  mutate(new_timestamp = as_datetime(timestamp)) %>%
  mutate(review_year = year(new_timestamp)) %>%
  mutate(review_hour = hour(new_timestamp)) %>%
  count(review_hour) %>% #change to review_year or review_hour as needed
  arrange(desc(n))

#OR
dates <- as_datetime(movielens$timestamp)
reviews_by_year <- table(year(dates))    # count reviews by year
names(which.max(reviews_by_year))    # name of year with most reviews
         
reviews_by_hour <- table(hour(dates))    # count reviews by hour
names(which.max(reviews_by_hour))    # name of hour with most reviews         

##text mining and sentiment analysis
library(tidyverse)
library(gutenbergr)
library(tidytext)

gutenberg_metadata

#Q6 How many different ID numbers are returned for pride and prejudice?
gutenberg_metadata %>% filter(str_detect(title, "Pride and Prejudice"))

#Q7 use gutenberg_works to remove replicates and include only english language works - find ID number
gutenberg_works(title == "Pride and Prejudice")

#Q8 download the text for Pride and Prejudice and create a tidy table with all words in the text. How many words in the text?
book <- gutenberg_download(1342)
words <- book %>%
  unnest_tokens(word, text)
nrow(words)

#Q9 remove stop words and provide word count
words %>%
  filter(!word %in% stop_words$word) %>% nrow()
#OR
words <- words %>%anti_join(stop_words)
nrow(words)

#Q10 filter out digits and provide word count
words <- words %>% filter(!str_detect(word, "\\d+"))
nrow(words)

#Q11 analyize most frequent words in the novel
words %>%
  count(word) %>%
  arrange(desc(n)) %>% print(n=100)

#Q12 
#how many elements of words have sentiments in afinn lexicon?
afinn <- get_sentiments("afinn")
afinn_sentiments <- inner_join(words, afinn)
nrow(afinn_sentiments)

#what porportion of words in afinn_sentiments have positive value?
afinn_sentiments %>% filter(value > 0)
#OR
mean(afinn_sentiments$value >0)

#what porportion of words in afinn_sentiments have value = 4e?
afinn_sentiments %>% filter(value > 4)
#OR
sum(afinn_sentiments$value == 4)
