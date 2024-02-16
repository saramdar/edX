#7.linreg - 2.2 least squares estimate
set.seed(1983, sample.kind = "Rounding")
library(tidyverse)

#function that computes the Residual Sum of Squares for any pair of values for heights data
rss <- function(beta0, beta1, data){
  resid <-galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1, 
                      rss = sapply(beta1, rss, beta0 = 25))

results %>%ggplot(aes(beta1, rss)) + geom_line() + geom_line(aes(beta1, rss), col = 2)
#graph shows the minimum for b1 = 0.65 when b0 = 25

#lse
fit <- lm(son ~ father, data = galton_heights)
fit

summary(fit)

#monte carlo simulation
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    lm(son ~ father, data = .) %>% .$coef
})
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,])
library(gridExtra)
p1 <-lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black")
p2 <-lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "black")
grid.arrange(p1, p2, ncol = 2)

sample_n(galton_heights, N, replace = TRUE) %>%
  lm(son ~ father, data = .) %>% summary

lse %>% summarize(se_0 = sd(beta_0), se_1 = sd(beta_1))

#lse can be strongly correlated
lse %>% summarize(cor(beta_0, beta_1))

#standardize the fathers heights:
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    mutate(father = father - mean(father)) %>%
    lm(son ~ father, data = .) %>% .$coef 
})
cor(lse[1,], lse[2,])

# plot predictions and confidence intervals
galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")

# plot best fit line
galton_heights %>%
  mutate(Y_hat = predict(lm(son ~ father, data=.))) %>%
  ggplot(aes(father, Y_hat))+
  geom_line()

# predict Y directly
fit <- galton_heights %>% lm(son ~ father, data = .) 
Y_hat <- predict(fit, se.fit = TRUE)
names(Y_hat)
