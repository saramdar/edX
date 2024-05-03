#8.machine learning - 6.2 recommendation systems comp check

library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")
head(movielens)
#Q1 compute the number of ratings for each movie and then plot it against the year the movie came out using a boxplot for each year.
#use the sequare root transformation on the y-axcis (number of ratings) when creating yourplot.

#what year has the highest median number of ratings?

movielens %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character((first(year)))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Q2 among movies that came out in 1993 or later, select the top 25 movies with the highest average number of ratings per year (n/year) 
# and calculate the average rating of each out them.  Use 2018 as the end uear.

movielens %>% filter(year >= 1993) %>% #filter out anything pre1993
  group_by(title) %>% 
  summarize(n=n(), year = 2018 - first(year), #count number of occurances in rows, determine number of years for rate calc
            rating = mean(rating)) %>% #average rating
  mutate(rate = n/year) %>% # number of ratings per year
  top_n(25, rate) %>% #view top 25 ratings per year
arrange(desc(rate)) %>%
  print(n=25)

#Q3 plot average rating vs ratings per year

movielens %>% 
  filter(year >= 1993) %>%
  group_by(title) %>%
  summarize(n = n(), years = 2018 - first(year),
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  ggplot(aes(rate, rating)) +
  geom_point() +
  geom_smooth()

#Q5 The movielens dataset also includes a time stamp. This variable represents the time and data in which 
# the rating was provided. The units are seconds since January 1, 1970. Create a new column date with the date.

# Which code correctly creates this new column?

movielens <- mutate(movielens, date = as_datetime(timestamp))
movielens %>% as.tibble()
  
#Q6 Compute the average rating for each week and plot this average against date. 
#Hint: use the round_date() function before you group_by().

# What type of trend do you observe?
movielens %>%
  mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarise(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) + geom_point() + geom_smooth()

#Q8 The movielens data also has a genres column. This column includes every genre 
#that applies to the movie. Some movies fall under several genres. Define a category 
#as whatever combination appears in this column. Keep only categories with more than 
#1,000 ratings. Then compute the average and standard error for each category. 
#Plot these as error bar plots.

#Which genre has the lowest average rating?
movielens %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genres = reorder(genres, avg)) %>% #not necessary, but makes for a chart that is ordered left to right
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

