#8.machine learning - 3.1 linear regression for prediction

#create a data set using this code:
library(tidyverse)
library(caret)

set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

#Q1 build 100 linear models using the data set and calculate the mean and standard dev of the combined models.
set.seed(1)
rmse <- replicate(100, { #replicate 100 times
          y <- dat$y
          test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE) #partition dataset into test and training sets
          test_set <- dat %>% slice(-test_index)
          train_set <- dat %>% slice(test_index)
          fit <- lm(y ~ x, data = train_set) #create linear model prediction y from x
          y_hat <- predict(fit, newdata = test_set) #generate predictions on test set
          sqrt(mean((y_hat - test_set$y)^2)) #calculate rmse
})
mean(rmse)
sd(rmse)

#Q2 repeat above but use larger datasets
# write a function that takes a size n, then
set.seed(1)
n <- c(100, 500, 1000, 5000, 10000)
res <- sapply(n, function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  rmse <- replicate(100, { 
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE) 
    test_set <- dat %>% slice(-test_index)
    train_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, newdata = test_set) 
    sqrt(mean((y_hat - test_set$y)^2)) 
})
  c(avg = mean(rmse), sd = sd(rmse))
})
res

#Q4 repeat the exercise from Q1, this time marking the correlation between x and y larger, as in the following code:
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1)
rmse <- replicate(100, { #replicate 100 times
  y <- dat$y
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE) #partition dataset into test and training sets
  test_set <- dat %>% slice(-test_index)
  train_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x, data = train_set) #create linear model prediction y from x
  y_hat <- predict(fit, newdata = test_set) #generate predictions on test set
  sqrt(mean((y_hat - test_set$y)^2)) #calculate rmse
 })

mean(rmse)
sd(rmse)

#Q6 create a data set using the following code:
set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

#observe data correlation
cor(dat)
#y is correlated with x1 and x2

set.seed(1)
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
test_set <- dat %>% slice(-test_index)
train_set <- dat %>% slice(test_index)
#x_1
fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))
#x_2
fit <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))
#x_1 + x_2
fit <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

#q8 repeat the exercise from Q6 but now create an example in which x_1 and x_2 are highly correlated
set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
cor(dat)
set.seed(1)
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
test_set <- dat %>% slice(-test_index)
train_set <- dat %>% slice(test_index)
#x_1
fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))
#x_2
fit <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))
#x_1 + x_2
fit <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

#observe that adding additional predictors does not improve the model substantially, this RMSE stays roughly the same.
