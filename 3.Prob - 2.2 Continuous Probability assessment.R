#2.Prob.2.2 Continuous Probability
options(digits = 3)
set.seed(16, sample.kind = "Rounding")
act_scores <- rnorm(10000, 20.9, 5.7) #simulate normally distributed data set with mean of 20.9 and sd of 5.7

#1a mean
mean(act_scores)
#1b standard deviation
sd(act_scores)
#1c how many perfect scores of 36?
length(which(act_scores >= 36))
#OR edX equation
sum(act_scores >= 36)
#1d what is the probability of an ACT score greater than 30?
1-pnorm(30, mean(act_scores), sd(act_scores))
#OR edX equation
mean(act_scores > 30)
#1e probability of an ACT score less than or equal to 10
mean(act_scores <= 10)

#2
#set x equal to sequence of integers 1 to 36
x <- seq(1:36)
f_x <- dnorm(x, 20.9, 5.7) #determine probability density 
plot(x, f_x)
#OR
data.frame(x, f_x) %>% ggplot(aes(x, f_x)) + geom_line()

#Q3 convert act_scores to Z scores
z <- scale(act_scores, mean(act_scores), sd(act_scores))
#OR
z_scores <- (act_scores - mean(act_scores))/sd(act_scores)
#3a probability of a z-score greater than 2? (2 sd above the mean)
mean(z >2)
mean(z_scores >2)
#3b what act score value corresponds to 2 standard deviations above the mean (Z=2)?
2*sd(act_scores)+mean(act_scores)
#3c what is the 97.5 percentile of act_scores?
qnorm(.975, mean(act_scores), sd(act_scores))

#4 create a CDF for ACT scores, apply it to range of scores 1 - 36
cdf <- sapply(1:36, function(x){mean(act_scores <=x)})
#4a which is the minimum integer score where the probability of that score or lower is at least .95?
min(which(cdf >=.95))
#4b use qnorm to determine expected 95th percentile given mean = 20.9 and sd = 5.7
#what is the expected 95th percentile of ACT scores
qnorm(0.95, 20.9, 5.7)
#4c make a vector containing the quantiles for the 1-99 percentiles of act_score data. 
p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)
#in what percentile is a score of 26?
names(sample_quantiles[max(which(sample_quantiles < 26))])
#4d make a qq plot graphing sample_quantiles (y axis) vs theoretical quantiles (x axis)
theoretical_quantiles <- qnorm(p, 20.9, 5.7)
qplot(theoretical_quantiles, sample_quantiles) + geom_abline()
