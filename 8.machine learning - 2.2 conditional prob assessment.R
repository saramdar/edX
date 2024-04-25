8.machine learning - 2.2 conditional probs assessment

set.seed(1) 
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

#Q2 what is the probability that a test is positive?
table(disease, test)
mean(test)

#Q3 what is the probability that an individual has the disease if the test is negative?
mean(disease[test==0])

#Q4 what is the probability that an individual has the disease if the test is positive?
mean(disease[test==1])
#OR
mean(disease[test==1]==1)

#Q5 if a patients test is positive, by how many times does that incrase their risk of having the disease?
#first calculate the probability of having the disease given a postive result,
#then divide by the probability of having the disease
mean(disease[test==1]==1)/mean(disease == 1)

#Q6
#We are now going to write code to compute conditions probabilities for being male in the heights dataset
#round the heights to the nearest inch
#plot the estimated conditional probability

library(dslabs)
library(tidyverse)
data("heights")
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
qplot(height, p, data =.)

#Q7 we see high variability for low values of height. THis is because there are few datapoints. 
#now use the quantile and cut() function to assure each group has the same number ofpoints.
#Note that for any numeric vector x, you can create groups based on quantiles like this: cut(x, quantile(x, seq(0, 1, 0.1)), include.lowest = TRUE)
ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

#8generate data from bivariate normal distro using the MASS package
library(MASS)
Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
plot(dat)

#estimate the conditions expectations and make a plot
s <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)
