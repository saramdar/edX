library(dslabs)
data("polls_us_election_2016")
head(polls_us_election_2016)

#1 confidence interval for p
#generate object "polls" that contains data filtered for polls that ended on or after oct 31 2016 in the US
polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.")
nrow(polls)

#assign sample size of 1st poll in "polls" to N
N <- polls$samplesize[1]
N
#assign estimated percentage of clinton voters to X_hat
X_hat <- polls$rawpoll_clinton[1]/100
X_hat
#calculate standard error
se_hat <- sqrt(X_hat*(1-X_hat)/N)
se_hat
#calculate the 95% confidence interval for proportion of Clinton voters. save lower and upper to variable "ci"
ci <- c(X_hat - qnorm(.975)*se_hat, X_hat + qnorm(.975)*se_hat)
ci

#2 create object pollster results that contains name, end date, proportion of voters who declared vote for clinton, se, lower and upper bounds of ci
head(polls)
pollster_results <- polls %>% mutate(X_hat = rawpoll_clinton/100, se_hat = sqrt(X_hat*(1-X_hat)/samplesize), lower = (X_hat-qnorm(.975)*se_hat),
                                     upper = X_hat+qnorm(.975)*se_hat) %>% select(pollster, enddate, X_hat, se_hat, lower, upper)
head(pollster_results)

#3 compare to actual results
avg_hit <- pollster_results %>% mutate(hit = lower<=0.482 & upper>=0.482) %>% summarize(mean(hit))
avg_hit

# 5 confidence interval for d
# add statement to code below to add d_hat, difference in proportion of voters
polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.") %>% mutate(d_hat = (rawpoll_clinton - rawpoll_trump)/100)
N <- polls$samplesize[1]
d_hat <- polls$d_hat[1]
X_hat <- (d_hat+1)/2
se_hat <- 2*sqrt(X_hat *(1-X_hat)/N)
ci <- c(d_hat - qnorm(0.975)*se_hat, d_hat + qnorm(0.975)*se_hat)
ci

#6 pollster results for d
pollster_results <- polls %>% mutate(X_hat = (d_hat+1)/2, se_hat = 2*sqrt(X_hat*(1-X_hat)/samplesize), lower = d_hat - qnorm(0.975)*se_hat, upper = d_hat + qnorm(0.975)*se_hat) %>% select(pollster, enddate, d_hat, lower, upper)
head(pollster_results)

#7 compare actual results to d
avg_hit <- pollster_results %>% mutate(hit = lower <= 0.021 & upper >= 0.021) %>% summarize(mean(hit))
avg_hit

#8 compare to actual results by pollster
errors <- polls %>% mutate(error = d_hat - 0.021) %>% ggplot(aes(x = pollster , y = error))+geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
errors
#9 compare to actual results by pollster - more than 5 polls
errors <- polls %>% mutate(error = d_hat - 0.021) %>% group_by(pollster) %>% filter(n() >= 5) %>% ggplot(aes(x = pollster , y = error))+geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
errors

