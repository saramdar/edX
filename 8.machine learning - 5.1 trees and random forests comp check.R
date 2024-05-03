#8.machine learning - 5.1 trees and random forests comp check

#Q1 create a simple dataset where the outcome grows 0.75 units on average for every
#increase in a predictor, using this code:
library(rpart)
n <- 1000
sigma <- 0.25
set.seed(1)
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

#which code correctly uses rpart() to fit a regresion tree and saves the results to fit?

fit <- rpart(y~., data=dat)
#Q2 plot the tree shape from Q1
plot(fit)
text(fit)

#Q3 complete the code to make a scatterplot of y vs x along iwth the predicted values based on the fit
dat %>%
  mutate(y_hat = predict(fit)) %>%
  ggplot() +
  geom_point(aes(x, y)) + # this is the line of the code that shows the actual values
  geom_step(aes(x, y_hat), col=2) # this is the line of code that shows the predicted values based on the fit

#Q4 run random forest instead of a regression and remake the scatterplot with the prediction line
library(randomForest)
fit <- randomForest(y~x, data = dat) # this line of code is the answer. 
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

#Q5 Use plot() to see if the random forest from Q4 has converged, or if we need more trees
  plot(fit)
  
#Q6 it seems the default values for the random forest result in an estimate that is too flexible (unsmooth)
  #re-run the random forest but this time with a node size of 50 and a max of 25 nodes.  Remake the plot.
  
  library(randomForest)
  fit <- randomForest(y~x, data = dat, 
                      nodesize = 50, maxnodes = 25) #this line of code is the answer
    dat %>% 
    mutate(y_hat = predict(fit)) %>% 
    ggplot() +
    geom_point(aes(x, y)) +
    geom_step(aes(x, y_hat), col = "red")
    