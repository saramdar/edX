# section 4
#model poll agregators
library(tidyverse)
library(dslabs)

d <- 0.039 #spread
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p <- (d+1)/2

# calculate confidence intervals of the spread
confidence_intervals <- sapply(Ns, function(N){
  X <- sample(c(0,1), size=N, replace=TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  2*c(X_hat, X_hat - 2*SE_hat, X_hat + 2*SE_hat) - 1
})
# generate a data frame storing results
polls <- data.frame(poll = 1:ncol(confidence_intervals),
                    t(confidence_intervals), sample_size = Ns)
names(polls) <- c("poll", "estimate", "low", "high", "sample_size")
polls

#alculate spread of combined polls
d_hat <- polls %>%
  summarize(avg = sum(estimate*sample_size) / sum(sample_size)) %>%
  .$avg

p_hat <- (1+d_hat)/2
moe <- 2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size))   
round(d_hat*100,1)
round(moe*100, 1)


data(polls_us_election_2016)
names(polls_us_election_2016)

# keep only national polls from week before election with a grade considered reliable
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade)))

# add spread estimate
polls <- polls %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
polls

#updated estimated spread
d_hat <- polls %>% summarize(d_hat = sum(spread*samplesize)/sum(samplesize)) %>% .$d_hat
d_hat
p_hat <- (d_hat+1)/2
moe <- 1.96*2*sqrt(p_hat*(1-p_hat)/sum(polls$samplesize))
moe

#hist of reported spreads
polls %>% ggplot(aes(spread))+geom_histogram(color="black", binwidth = .01)

# number of polls by pollster in time period
polls %>% group_by(pollster) %>% summarize(n())

#last reported result prior to election
one_poll_per_pollster <- polls %>% group_by(pollster) %>%filter(enddate == max(enddate)) %>% ungroup()
one_poll_per_pollster
qplot(spread, data = one_poll_per_pollster, binwidth = 0.01)
