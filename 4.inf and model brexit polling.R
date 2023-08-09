library(tidyverse)
options(digits = 3)
library(dslabs)
data("brexit_polls")

p <- 0.481 # official proportion voting "Remain"
d <- 2*p-1 # official spread
N <- 1500 # number of voters in poll

#Q1
#expected total number of voters in sample who chose remain?
p*N
#se of total number of voters in sample
sqrt(N*p*(1-p))

#expected value of Xhat, the proportion of remain voters
p

#se of xhat, proportion of remain voters
sqrt((p*(1-p))/N)

#expected value of d
d
#se of d
2*sqrt(p*(1-p)/N)

#Q2 
head(brexit_polls)
#calculate x_hat for each poll, the estimate of the proportion of voters choosing "remain" given the observed spread data
brexit_polls <- brexit_polls %>% mutate(x_hat = (spread+1)/2)
head(brexit_polls)

#average and sd of observed spreads
mean(brexit_polls$spread)
sd(brexit_polls$spread)
#average and sd of x_hat, estimate of p
mean(brexit_polls$x_hat)
sd(brexit_polls$x_hat)

#Q3 confidence interval of a brexit poll
#calculate cis for first poll in data set
brexit_polls[1,]
brexit_polls$x_hat[1]+c(-1,1)*qnorm(0.975)*sqrt(brexit_polls$x_hat[1]*(1-brexit_polls$x_hat[1])/brexit_polls$samplesize[1])
#this confidence interval does not include p=0.5, it is greater and therefore predicts a a winner
#this confidence interval does not cover the true value of p (0.481)

#Q4 create data frame that only contains polls ending in June and later. 
#How many polls are in june?
june_polls <- brexit_polls %>% filter(enddate >="2016-06-01")
nrow(june_polls)

#what proportion of polls have a confidence interval that covers the value of 0?
june_polls <- june_polls %>%
  mutate(se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize), 
         se_spread = 2*se_x_hat, 
         ci_lower_spread = spread-qnorm(0.975)*se_spread, 
         ci_upper_spread = spread+qnorm(0.975)*se_spread)
june_polls
mean(june_polls$ci_lower_spread <0 & june_polls$ci_upper_spread >0) #(# of polls with lower spread less than 0 and upper spread more than 0) / total number of polls

#what proportion of polls predict "remain" (ci interval entirely above 0)?
mean(june_polls$ci_lower_spread >0)

#what proportion of polls have ci that covers the true value of d?
june_polls <- june_polls %>% 
  mutate(hit = d >=ci_lower_spread & d <= ci_upper_spread)
mean(june_polls$hit)

#5 group and summarize the june_polls by pollster to find the proportion of hits for each pollster and the number of polls per pollster.  Use arrange to sort by hit rate.
june_polls %>% group_by(pollster) %>% summarize(ph = mean(hit), n = n()) %>% arrange(ph)
#these results are consistent with a large general bias that affects all pollsters - there isn't a lot of consistency with poll performance

#6 make a boxplot of the spread in june_polls by type
june_polls %>% ggplot(aes(poll_type, spread)) + geom_boxplot() + geom_point()
# insights:
#telephone polls tend to show support for remain, spread >0
#telephone polls tend to show higher support for remain than online polls (higher spread)
#online polls have larger interquartile range for the spread than telephone polls, indicating they are more variable
#poll type introduces a bias that affects poll results

#7 combined spread across poll type
combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N, 
            p_hat = (spread+1)/2, 
            se_spread = 2*sqrt(p_hat*(1-p_hat)/N), 
            lower = spread - qnorm(0.975)*se_spread, 
            upper = spread + qnorm(0.975)*se_spread)
combined_by_type

combined_by_type %>%
  filter(poll_type == "Online") %>%
  pull(lower)

#9 Chi-squared p-value
#define brexit_hit, which computes the confidence intervals for all brexit polls in 2016 and calculates whether the confidence intervals covers the actual value of the the spread
head(brexit_polls)
brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread+1)/2, 
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize), 
         spread_lower = spread - qnorm(0.975)*se_spread,
         spread_upper = spread + qnorm(0.975)*se_spread, 
         hit = spread_lower < d & spread_upper > d) %>%
  select(poll_type, hit)
brexit_hit

brexit_chisq <- table(brexit_hit$poll_type, brexit_hit$hit)
brexit_chisq
chisq.test(brexit_chisq)$p.value

#online > telephone
hit_rate <- brexit_hit %>%
  group_by(poll_type) %>%
  summarize(avg = mean(hit))
hit_rate

odds_online <- (48/(37+48))/(37/(37+48))
odds_online
odds_telephone <- (10/(10+32))/(32/(10+32))
odds_telephone

odds_online/odds_telephone

# official solution
# convert to data frame
chisq_df <- as.data.frame(brexit_chisq)

online_true <- chisq_df$Freq[chisq_df$Var1 == "Online" & chisq_df$Var2 == "TRUE"]
online_false <- chisq_df$Freq[chisq_df$Var1 == "Online" & chisq_df$Var2 == "FALSE"]

online_odds <- online_true/online_false
online_odds
phone_true <- chisq_df$Freq[chisq_df$Var1 == "Telephone" & chisq_df$Var2 == "TRUE"]
phone_false <- chisq_df$Freq[chisq_df$Var1 == "Telephone" & chisq_df$Var2 == "FALSE"]

phone_odds <- phone_true/phone_false
phone_odds

online_odds/phone_odds

#11 plotting spread over time
# plot the spread over time, color by poll type

brexit_polls %>% 
  ggplot(aes(enddate, spread, col = poll_type)) + 
  geom_point() +
  geom_smooth(method = "loess", span = 0.4) +
  geom_hline(aes(yintercept = -.038))

#12 plotting raw percentages over time
brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))
brexit_long

brexit_long %>%
  ggplot(aes(enddate, proportion, col = vote)) +
  geom_smooth(method = "loess", span = 0.3) +
  geom_point()
