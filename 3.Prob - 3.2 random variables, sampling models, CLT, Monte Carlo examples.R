#3.Prob 3.2 random variables, sampling models, CLT, Monte Carlo examples

beads <- rep(c("red", "blue"), times = c(2, 3))
X <- ifelse(sample(beads, 1) == "blue", 1, 0)

#sampling model 1: define urn then sample
color <-rep(c("Black", "Red", "Green"), times = c(18, 18, 2))
n <- 1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)
X[1:10]

#sampling model 2: define urn inside sample function by noting probabilities
x <- sample(c(-1, 1), n, replace = TRUE, prob=c(9/19, 10/19))
S <- sum(x)
S
#use monte carlo sim to estimate prob of casino losing money
n < -1000 # number of players
B <- 10000 # number of monte carlo experiments
S <- replicate(B, {
  x <- sample(c(-1, 1), n, replace = TRUE, prob=c(9/19, 10/19)) #simulate 1000 spins
  sum(x) # determine total profit
})
mean(S < 0) #probability of casino losing money

#plot histogram of observed values of S and normal density curve
library(tidyverse)
s <- seq(min(S), max(S), length = 100) # sequence of 100 values across range of S
normal_density <- data.frame(s=s, f=dnorm(s, mean(S), sd(S))) #generate normal density for S
data.frame (S = S) %>% ggplot(aes(S, ..density..)) + geom_histogram(color = "black", binwidth = 10) + ylab("Probability") + geom_line(data = normal_density, mapping = aes(s, f), color = "blue")

p_green <- 2/38
p_not_green <- 1-p_green
n <- 100
avg <- n* (17*p_green + -1*p_not_green) # avg outcome of 100 spins if win $17 when ball lands on green, lose $1 if lands elsewhere
se <- sqrt(n) * (17--1)*sqrt(p_green*p_not_green) #standard error of sum of 100 outcomes

#compute probability that you win money betting on green 100 times
1-pnorm(0, avg, se)

#Monte Carlo sim that generates 10,000 outcomes of S, the sum of 100 bets
B <-10000
S <- replicate(B, {
  x <- sample(c(17, -1), n, replace = TRUE, prob = c(p_green, p_not_green))
  sum(x)
})
mean(S)
sd(S)
#calculate proportion of outcomes in vector S that exceed $0 / prob of winning money
mean(S > 0)
#create a random variable Y that contains average winnings per bet after betting on green 10,000 times
X <- sample(c(17, -1), 10000, replace = TRUE, prob = c(p_green, p_not_green))
Y <-mean(X)
Y
#what is the expected value of Y, the average outcome per bet?
17*p_green + -1*p_not_green
#standard error of Y after 10000 bets?
n <- 10000
abs(17--1)*sqrt(p_green*p_not_green)/sqrt(n)


