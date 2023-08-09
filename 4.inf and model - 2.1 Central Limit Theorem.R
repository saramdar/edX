#1 find the average value of a random sample
take_sample <- function(p,N){
  x <- sample(c(1,0), N, replace = TRUE, prob = c(p, 1-p))
  mean(x)
}
p <- 0.45
N <- 100
take_sample(p,N)

#2 calculate the average of the differences between sample average and actual value p
B <- 10000
errors <- replicate(B, {
  p-take_sample(p,N)
})
mean(errors)

#3 create histogram of values contained in vector errors. They are all distributed around 0
hist(errors)

#4 calculate average error size, error = p-Xbar
errors <- replicate(B, p-take_sample(p, N))
mean(abs(errors))

#5 standard deviation of the spread from previous monte carlo sim
sqrt(mean(errors^2))

#6 estimating the standard error *theoretical prediction)
sqrt(p*(1-p)/N)

#7 standard error of the estimate (estimate of the theoretical prediction)
X <- sample(c(1, 0), N, replace = TRUE, prob = c(p, 1-p))
X_bar <- mean(X)
sqrt(X_bar*(1-X_bar)/N)

#8 plot the standard error
#results from monte carlo, theoretical prediction, and estimate of theo prediction are all close, tells us theory is working)
N <- seq(100, 5000, len = 100)
p <- 0.5
se <- sqrt(p*(1-p)/N)
plot(N, se)
#sample size would need to be 2500 to have standard error of ~1%

#11 qqplot of the errors to see if they follow a normal distribution
qqnorm(errors)
qqline(errors)

#12 estimate the probability of Xbar greater than 0.5
p <- 0.45
N <- 100
se <- sqrt(p*(1-p)/N)
se
1-pnorm(0.5, p, se)

#13 estimate the probability of a specific error size
#don't know p
N <- 100
x_hat <- 0.51

#what is the CLT approximation for the probability that the error size is equal or larger than 0.01?
se_hat <- sqrt(X_bar*(1-X_bar)/N)
1-pnorm(0.01/se_hat) + pnorm(-0.01/se_hat)

library(tidyverse)
library(dslabs)
library(gridExtra)

B <- 10000
N <- 1000
p <- 0.45
X_hat <- replicate (B, {
  X <- sample(c(0,1), size = N, replace = TRUE, prob=c(1-p, p))
  mean(X)
})

p1 <-data.frame(X_hat = X_hat) %>% ggplot(aes(X_hat)) +
  geom_histogram(binwidth = 0.005, color = "black")
p2 <- data.frame(X_hat=X_hat) %>% ggplot(aes(sample = X_hat))+
  stat_qq(dparams = list(mean=mean(X_hat), sd=sd(X_hat)))+
  geom_abline()+
  ylab("X_hat") +
  xlab("Theoretical normal")
grid.arrange(p1, p2, nrow=1)

#possible margin of error
N <- 100000
p <- seq(0.35, 0.65, length = 100)
SE <- sapply(p, function(x) 2*sqrt(x*(1-x)/N))
data.frame(p=p, SE=SE) %>% ggplot(aes(p, SE)) +
  geom_line()

