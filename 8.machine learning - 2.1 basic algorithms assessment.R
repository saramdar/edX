#8.machine learning - 2.1 basics of evaluating ML algorithms assessment
library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

#Q1 what proportion of the inclass group is female? what proportion of the online group is female?
#question should really be: what proportion of females are online?  What proportion are inclass?
dat %>% group_by(type) %>% summarize(prop_female = mean(sex == "Female"))

#Q2 use type variable to predict sex.  Assume that for each class type the studends are either all male or female, basedon the most prevalent sex as calculated in Q1
y_hat <- ifelse(x == "online", "Male", "Female") %>% 
  factor(levels = levels(y))
mean(y_hat==y)
#same answer regardless of whether x == inclass or online

#Q3 use table function to show confusion matrix between y_hat and y
table(y_hat, y)

#Q4 what is the sensitivity of the prediction
library(caret)
sensitivity(y_hat, y)
#not working?
26/(26+42)

#Q5 what is the specificity of the prediction
library(caret)
specificity(y_hat, y)
#not working?
26/(26+42)

#Q6 what is the prevalance of females in the dat dataset?
mean(dat$sex == "Female")
#OR
mean(y == "Female")

library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

#Q7 what is the missing code?
set.seed(76)
# line of code
test_index <- createDataPartition(y, times = 1, p=0.5, list = FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]
