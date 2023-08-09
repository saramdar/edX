#1 calculate population avg and sd
library(dslabs)
data(heights)
head(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)
mean(x)
sd(x)

#2 calculate sample avg and sd using sample size 50
N <- 50
X <- sample(x, N, replace = TRUE)
mean(X)
sd(X)

#4 calculate 95% confidence interval using qnorm
se <- sd(X)/sqrt(length(X))
se
ci <- c(mean(X)-qnorm(.975)*se, mean(X)+qnorm(.975)*se)
ci

#5 Monte Carlo sim, compute 10000 cis, which proportion include mu? 
mu <- mean(x)
N <- 50
B <- 10000
res <- replicate(B, {
  X <- sample(x, N, replace = TRUE)
  interval <- c(mean(X) - qnorm(.975)*sd(X)/sqrt(N), mean(X)+qnorm(.975)*sd(X)/sqrt(N))
  between(mu, interval[1], interval[2])
})
mean(res)

#6 plot spreads for each poll
library(dplyr)
library(ggplot2)
data("polls_us_election_2016")

# These lines of code filter for the polls we want and calculate the spreads
polls <- polls_us_election_2016 %>% 
  filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research","The Times-Picayune/Lucid") &
           enddate >= "2016-10-15" &
           state == "U.S.") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) 

#make boxplot with points of the spread for each pollster
polls %>% ggplot(aes(pollster, spread)) + geom_boxplot() + geom_point()

#13 compute sd estimates for the two polls
sigma <- polls %>% group_by(pollster) %>% summarize(s = sd(spread))
sigma

#15 calculate 95% ci of the spreads
head(polls)
res <- polls %>% 
  group_by(pollster) %>% 
  summarize(avg = mean(spread), sd = sd(spread), n = n())
res
estimate <- res$avg[2]-res$avg[1]
estimate
se_hat <- sqrt(res$sd[2]^2/res$n[2]+res$sd[1]^2/res$n[1])
se_hat
ci <- c(estimate - qnorm(0.975)*se_hat, estimate + qnorm(0.975)*se_hat)
ci

#16 calculate the p-value
(1-pnorm(estimate/se_hat))*2

#17 filter original data set for polls with more than 5 surveys, compute avg and standard deviation of the spreads for each pollster
polls_5 <- polls_us_election_2016 %>% 
  filter(enddate >= "2016-10-15" &
           state == "U.S.") %>%
  group_by(pollster) %>%
  filter(n() >= 5) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ungroup()

var <- polls_5 %>% group_by(pollster) %>% summarize(avg = mean(spread), s = sd(spread))
var
