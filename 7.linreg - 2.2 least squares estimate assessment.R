#7.linreg - 2.2 least squares estimate assessment

#q1 what is the LSE for B1 if we assume B0 = 36?
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)

#q3 load the lahman library and filter the Teams data to the years 1961-2001
#mutate the dataset to creater variables for:
#bases on balls per game
#runs per game
#home runs per game
#then run a linear model in R predicting the number of runs per game based on both the number of bases on ball per game and the number of home runs per game
#what is the coefficient for bases on balls per game?
library(Lahman)
Teams_small <- Teams %>% filter(yearID %in% 1961:2001) 

Teams_small %>%
  mutate(BB_per_game = BB/G, 
         R_per_game = R/G,
         HR_per_game = HR/G) %>%
  do(tidy(lm(R_per_game ~ BB_per_game + HR_per_game, data = .)))

#q5 R code that properly plots the predictions and condience intervals for our linear model of sons' heights
galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")

#this code uses the predict command tho generate predictions and 95% cofience intervals for the linear model
model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as_tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))

#Q7 define female_heights, a set of mother and daugther heights sampled from GaltonFamilies:
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

lm(mother ~ daughter, data = female_heights )

#Q8 predict modethers heights using the model from Q7 and the predict function
model <- lm(mother ~ daughter, data = female_heights)
#predict the height of the first mother in data set
prediction <- predict(model)[1]
prediction
actual height of first mother in data set
female_heights$mother[1]

#Q9 create a table for players, # of singles, and @ of bb for 2002
library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)
bat_02

#create the same table as above for years 1999-2001.  Keep only rows where players have more than 100 plate appearances. Calculate average single and BB rate per player for 3 year period
bat_9901 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarize(mean_singles = mean(singles), mean_bb = mean(bb))

#how many players have mean singles rate of greater than 0.2?
sum(bat_9901$mean_singles > 0.2)
#how many players have mean bb rate of greater than 0.2?
sum(bat_9901$mean_bb > 0.2)

#Q10 user inner_join to combine the bat_02 table with the new table.find correlation between 2002 rates and 1999-2001 rate averages
joined_data <- inner_join(bat_02, bat_9901, by = "playerID")
joined_data %>% summarize(cor(singles, mean_singles), cor(bb, mean_bb))

#Q11 make scatterplots of mean_singles vs singles and mean_bb vs bb.  Are these distributions bivariate normal?
joined_data %>%
  ggplot(aes(mean_singles, singles)) + geom_point()

joined_data %>%
  ggplot(aes(mean_bb, bb)) + geom_point()

#Q12 fit a linear model to predict 2002 sincles given the 1999-2001 mean_singles.  What is the coef of mean singles, the slope of the fit?
fit_singles <-lm(singles ~ mean_singles, data = joined_data)
fit_singles$coef[2]
fit_bb <- lm(bb ~ mean_bb, data = joined_data)
fit_bb$coef[2]
