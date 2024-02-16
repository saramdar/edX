#7.linreg - 2.4 Regression and Baseball Assessment
library(Lahman)
head(Teams)

#9a fit a multivariate linear regression model to obtain the effects of BB and HR on R in 1971
fit <- Teams %>%
  filter(yearID == 1971) %>%
  mutate(BB = BB/G, HR = HR/G, R = R/G) %>%
  lm(R ~ BB + HR, data=.) 

tidy(fit)
#use estimates for BB and HR

#10 repeat above to find effects of BB and HR on runs for every year from 1961 to 2018
res <- Teams %>%
  filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>%
  summarize(tidy(lm(R ~ BB + HR, data = across()))) %>%
  ungroup() 

res %>%
  filter(term == "BB") %>%
  ggplot(aes(yearID, estimate)) +
  geom_point() +
  geom_smooth(method = "lm")

#Q11 fit a linear model
res %>%
  filter(term == "BB") %>%
  summarize(tidy(lm(estimate ~ yearID, data = .), conf.int = TRUE))

            