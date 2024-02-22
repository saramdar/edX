#7.linreg - 2 linear models assessment

library(tidyverse)
library(broom)
library(Lahman)

Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G,
         R = R/G, 
         HR = HR/G)
#added HR/G and R/G to provided code

#Q1a predict increase in average attendance based on
#every 1 run scored per game
fit <- lm(avg_attendance ~ R, data = Teams_small)
fit$coef
#every 1 HR scored per game
fit <- lm(avg_attendance ~ HR, data = Teams_small)
fit$coef

#Q1b predict average attendance based on # of wins - do not normalize for number of games
fit <- lm(avg_attendance ~ W, data = Teams_small)
fit$coef

#Q1c predict average attendance based on year
fit <- lm(avg_attendance ~ yearID, data = Teams_small)
fit$coef

#Q2 
#what is the correlation coefficient for runs per game and wins?
Teams_small %>% summarize(cor(W,R))
#what is the correlation coefficient for HR per game and wins?
Teams_small %>% summarize(cor(W,HR))

#Q3 stratify teams_small data by wins - 
#divide number of wins by 10 and round to the nearest integer. 
#filter to keep only strata 5 through 10

#Q3a how many observations are in the 8 win strata?
dat <- Teams_small %>%
  mutate(wins_strata = round(W/10)) %>%
  filter(wins_strata >=5 & wins_strata<=10)
sum(dat$wins_strata == 8)

#Q3b calc slope of regression line predicting average attendance given runs per game for each of the win strata
dat %>%
  group_by(wins_strata) %>%
  summarize(slope = cor(R, avg_attendance)* sd(avg_attendance)/sd(R))

#Q3c calc slope of regression line predicting average attendance given HR per game for each of the win strata
dat %>%
  group_by(wins_strata) %>%
  summarize(slope = cor(HR, avg_attendance)* sd(avg_attendance)/sd(HR))

#Q4 fit a multivariate regression determining the effects of runs per game, home runs per game, wins 
#and year on average attendance
head(Teams_small)

fit <- lm(avg_attendance ~ R+HR+W+yearID, data = Teams_small)
tidy(fit)

#Q5
newdata <- data.frame(R=5, HR=1.2, W=80, yearID = 1960)
predict(fit, newdata)

#Q6
newdata <- Teams %>% filter(yearID == 2002) %>% 
  mutate(R=R/G, 
         HR = HR/G,
         avg_attendance = attendance/G)
preds <- predict(fit, newdata)
cor(preds, newdata$avg_attendance)
