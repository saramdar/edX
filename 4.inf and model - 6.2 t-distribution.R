# 6.2 assessment
#1 using the function pt, calculate the probabilty of seeing t-distributed
#random variables being more than 2 in absolute
#value when df=3

2*(1-pt(2,3))

#2 use sapply to compute the same probabilty for degrees of freedom from 3-50
df <- seq(3, 50, 1)
pt_func <- function(N){2*(1-pt(2, N))}
probs <- sapply(df, pt_func)
plot(df, probs)

#3 sample from a normal distribution
# what proportion of samples have cis that span the true pop mean?
library(dslabs)
library(dplyr)
data(heights)

#generate x, a vector of male heights
head(heights)
x <- heights %>% filter(sex == "Male") %>% .$height

#create variables for mean height, sample size, and number of times to run sim
mu <- mean(x)
N <- 15
B <- 10000

#generate log vector that contains the results of the monte carlo sim
res <- replicate(B, {
  X <- sample(x, N, replace = TRUE) #create sample
  interval <- mean(X) + c(-1, 1)*qnorm(0.975)*sd(X)/sqrt(N)
  between(mu, interval[1], interval[2])
})
mean(res)

#4 sample from the t-distribution
res <- replicate(B, {
  X <- sample(x, N, replace = TRUE) #create sample
  interval <- mean(X) + c(-1, 1)*qt(0.975, N-1)*sd(X)/sqrt(N)
  between(mu, interval[1], interval[2])
})
mean(res)
