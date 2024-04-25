
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

#Q1a what proportion of the inclass group is female? what proportion of the online class is female?
 dat %>% group_by(type) %>% summarize(mean(sex == "Female"))

 #Q2 use type variable to predict sex
 y_hat <- ifelse(x == "inclass", "Female", "Male") %>% factor(levels = levels(y))
 mean(y == y_hat)
 
 #Q3 write a line of code using the table function to show the confusion matrix between y_hat and y
 table(y, y_hat)

#Q4 and Q5
#find sensitivity and specificity
 library(caret)
sensitivity(y_hat, y) 
specificity(y_hat, y)

#Q6 what is the prevalence of females in the dat dataset?
mean(y == "Female")

library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

#Q7 create a train and test data set
set.seed(76)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test<- iris[test_index,]
train<- iris[-test_index,]

#Q8 figure out which feature yields the greatest overall accuracy when predicting species
#using the train data set, find the cutoff that produces the highests accuracy, 
#predicting virginica if greater than the cutoff and versicolor otherwise
range(iris$Sepal.Length)
range(iris$Sepal.Width)
range(iris$Petal.Length)
range(iris$Petal.Width)

#plug ranges into cutoff, change variable in y_hat call
cutoff <- seq(3.0, 6.9, 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") 
  mean(y_hat == train$Species)
})
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

#OR all in one tidy little function
foo <- function(x){
  rangedValues <- seq(range(x)[1], range(x)[2], by=0.1)
  sapply(rangedValues, function(i){
    y_hat <- ifelse(x>i, 'virginica', 'versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5], 2, foo)
sapply(predictions, max)	

#Q9 calculate overall accuracy using the smart cutoff from Q8
y_hat <- ifelse(test$Petal.Width > best_cutoff, "virginica", "versicolor") %>% 
  factor(levels = levels(test$Species))
mean(y_hat == test$Species)


#Q10 see Q8 for variable with second highest accuracy
#Q11 do some exploratory data analysis
plot(iris, pch=21, bg=iris$Species)

#find optimum cutoff for Petal Length and Petal width
range(iris$Petal.Width)

#plug ranges into cutoff, change variable in y_hat call
cutoff <- seq(1.0, 2.5, 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") 
  mean(y_hat == train$Species)
})
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

#calculate accuracy when petal length is greater than length cutoff and width is greater than width cutoff
y_hat <- ifelse(test$Petal.Width > 1.5 & test$Petal.Length > 4.6, "virginica", "versicolor") %>% 
  factor(levels = levels(test$Species))
mean(y_hat == test$Species)
