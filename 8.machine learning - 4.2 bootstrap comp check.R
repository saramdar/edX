#8.machine learning - 4.2 bootstrap comp check

#Q1 create the indexes for 10 bootstrap samples for the mnist_27 dataset like this:
library(dslabs)
library(caret)

data(mnist_27)

set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)
indexes

#how many times do 3, 4, and 7 appear in the first resampled index?
sum(indexes$Resample01 == 3)
sum(indexes$Resample01 == 4)
sum(indexes$Resample01 == 7)

#Q2 how many times does 3 appear in all of the resampled indexes?

sum(indexes$Resample01 == 3)
sum(indexes$Resample02 == 3)
sum(indexes$Resample03 == 3)
sum(indexes$Resample04 == 3)
sum(indexes$Resample05 == 3)
sum(indexes$Resample06 == 3)
sum(indexes$Resample07 == 3)
sum(indexes$Resample08 == 3)
sum(indexes$Resample09 == 3)
sum(indexes$Resample10 == 3)

#OR...
x <- sapply(indexes, function(ind){
  sum(ind  == 3)
})
sum(x)

#Q3 perform a monte carlo sim with 10,000 reps, generating a random data set and estimating the 75th quantile each time.  
#What is the expected value (mean) and standard error (sd) of the 75th quantile?

y <- rnorm(100, 0, 1) #generate random dataset of 100 numbers with mean = 0 and sd = 1
qnorm(0.75) # actual 75th quantile
quantile(y, 0.75) # estimated 75th quantile

set.seed(1)
library(gridExtra)
B <- 10000
q_75 <- replicate(B, {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})
mean(q_75)
sd(q_75)

# Q4 in practice we can't run a Monte Carlo sim.   
set.seed(1)
y <- rnorm(100, 0, 1)
#set seed to 1 again and use 10,000 botstrap samples to etimate the expected value and standard error of the 75th quantile.
set.seed(1)
indexes <- createResample(y, 10000) #create indexes for bootstrap sampling
q_75_b <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_b)
sd(q_75_b)
