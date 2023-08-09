#Bayes' Theorem 
#Monte Carlo Sim
prev <- 0.00025    # disease prevalence
N <- 100000    # number of tests
outcome <- sample(c("Disease", "Healthy"), N, replace = TRUE, prob = c(prev, 1-prev))

N_D <- sum(outcome == "Disease")    # number with disease
N_H <- sum(outcome == "Healthy")    # number healthy

# for each person, randomly determine if test is + or -
accuracy <- 0.99
test <- vector("character", N)
test[outcome == "Disease"] <- sample(c("+", "-"), N_D, replace=TRUE, prob = c(accuracy, 1-accuracy))
test[outcome == "Healthy"] <- sample(c("-", "+"), N_H, replace=TRUE, prob = c(accuracy, 1-accuracy))

table(outcome, test)

#assessment Bayesian Statistics
library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
polls <- polls_us_election_2016 %>% filter(state == "Florida" & enddate >= "2016-11-04") %>% mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
head(polls)
results <- summarize(polls, avg = mean(spread), se = sd(spread)/sqrt(n()))
results

#estimate posterior distribution
mu <- 0
tau <- 0.01
sigma <- results$se
Y <- results$avg
B <- sigma^2/(sigma^2 + tau^2)
#calculate expected value of the posterior distribution
exp_value <- mu+(1-B)*(Y-mu)

#calculate standard error of the posterior distribution
se <- sqrt(1/(1/sigma^2 + 1/tau^2))

#construct a 95% credible interval
# ci = (estimate - qnorm(.975)*se, estimate + qnorm(.975)*se)
ci <- (mu+(1-B)*(Y-mu)+(c(-1, 1)*qnorm(0.975)*se))
ci

#what is the probability that Trump wins Florida?
#calculate the prob that the spread in Florida was less than 0
pnorm(0, exp_value, se)

#change variance tau to 100 values ranging from 0.005 to 0.05 and observe how probability of Trump winning changes by making a plot
taus <- seq(0.005, 0.05, len = 100)

p_calc <- function(tau){
  B <- sigma^2/(sigma^2 + tau^2)
  pnorm(0, mu+(1-B)*(Y-mu), sqrt(1/(1/sigma^2 + 1/tau^2)))
}
ps <- sapply(taus, p_calc)
plot(taus, ps)


