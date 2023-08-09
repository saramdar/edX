# Load the libraries and data
library(dplyr)
library(dslabs)
data("polls_us_election_2016")

#1 create a 95% confidence interval for the spread, create a table (3.1)

# Create a table called `polls` that filters by  state, date, and reports the spread
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Create an object called `cis` that columns for the lower and upper confidence intervals. Select the columns indicated in the instructions.
cis <- polls%>% 
  mutate(X_hat = (spread+1)/2, se = 2*sqrt(X_hat*(1-X_hat)/samplesize), lower = spread - qnorm(.975)*se, upper = spread + qnorm(.975)*se) %>% 
  select(state, startdate, enddate, pollster, grade, spread, lower, upper)
head(cis)

#2 compare to actual results to determine how often the 95% confidence interval includes the actual result (3.1)
# Add the actual results to the `cis` data set
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

#3 Create an object called `p_hits` that summarizes the proportion of confidence intervals that contain the actual value. Print this object to the console.
p_hits <- ci_data %>% mutate(hit = actual_spread <= upper & actual_spread >= lower) %>% summarize(mean(hit))
p_hits

# Create an object called `p_hits` that summarizes the proportion of hits for each pollster that has at least 5 polls, order them from best to worst.  
# show the number of polls conducted by each pollster and the 538 grade of each pollster (3.1)
p_hits <- ci_data %>% 
  mutate(hit = actual_spread <= upper & actual_spread >= lower) %>% 
  group_by(pollster) %>% 
  filter(n()>=5) %>% 
  summarize(proportion_hits = mean(hit), n = n(), grade = first(grade)) %>%
  arrange(desc(proportion_hits))
p_hits

#4 stratify by state
p_hits <- ci_data %>%
  mutate(hit = actual_spread >= lower & actual_spread <= upper) %>%
  group_by(state) %>%
  filter(n() >= 5) %>%
  summarize(proportion_hits = mean(hit), n = n()) %>%
  arrange(desc(proportion_hits))
p_hits

#5 plot prediction results 
# make a barplot based on previous result. 
ggplot(p_hits, aes(state, proportion_hits)) + geom_bar(stat = "identity") + coord_flip()

#6 predict the winner
# add two columns to the cis table by computing, for each poll, 
#the difference between the predicted spread and the actual spread
# define a column hit that is true if the signs are the same
# examine the last 6 rows of the new table
head(ci_data)
errors <- ci_data %>% mutate(error = spread - actual_spread, hit = ifelse(sign(spread) == sign(actual_spread), TRUE, FALSE))
tail(errors)

#7 plot prediction results
# create an object called p_hits that contain the proportion of instances when the sign of the actual spread
# matches the predicted spread for states with 5 or more polls
# make a barplot based ont he result that shows the proportion of times the sign of the spread matched the 
# actual result for the data in p_hits
p_hits <- errors %>% 
  group_by(state) %>%
  filter(n()>=5) %>%
  summarize(proportion_hits = mean(hit), n = n())

p_hits %>% 
  arrange(desc(proportion_hits)) %>%
  ggplot(aes(state, proportion_hits)) + geom_bar(stat = "identity") + coord_flip() 

# most states' polls predict the corrct winner 100% of the time.  

#8 plotting the errors
# generate a histogram of the errors and find the median
head(errors)
hist(errors$error)
median(errors$error)

#9 plotting bias by state
head(errors)
errors %>%
  filter(grade %in% c("A+", "A", "A-", "B")) %>% 
  ggplot(aes(state, error)) +
  geom_boxplot() +
  geom_point() +
  coord_flip()

# filter error plot to only inlcude states with 5 or more good polls
errors %>%
  filter(grade %in% c("A+", "A", "A-", "B") & n()>=5) %>% 
  ggplot(aes(state, error)) +
  geom_boxplot() +
  geom_point() +
  coord_flip()
