#define pockets for each color
green <- 2
black <- 18
red <- 18

#probability of ball landing in green pocket
p_green <- green / (green + black + red)
p_green
#probability of ball not landing in green
p_not_green <- (black + red) / (green + black + red)
p_not_green

#predict random variable X, your winnings from betting on green from one turn
X <- sample(c(17, -1), 1, prob = c(p_green, p_not_green))
X
#compute expected value of X ($17 for landing on green, -1 if any other color)
17*p_green + -1*p_not_green
#compute standard error of X
abs(17--1)*sqrt(p_green*p_not_green)

#compute random variable S that sums winnings after betting on green 1000 times
n <- 1000
X <- sample(c(17, -1), n, replace = TRUE, prob=c(p_green, p_not_green))
S <- sum(X)
S
#compute expected value of S
n*(17*p_green + -1*p_not_green)
#compute standard error of S
sqrt(n)*abs(17--1)*sqrt(p_green*p_not_green)
