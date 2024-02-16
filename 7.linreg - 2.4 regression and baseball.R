#7.lin reg - 2.4 Regression and baseball
library(tidyverse)
library(dplyr)
library(broom)
library(Lahman)
head(Teams)

#linear regression with 2 variables
fit <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB = BB/G, HR = HR/G, R = R/G) %>% 
  lm(R ~ BB + HR, data = .)

#view summary
tidy(fit, conf.int = TRUE)

#linear regression with multiple variables
fit <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB = BB/G,
         singles = (H - X2B - X3B - HR)/G, 
         doubles = X2B/G,
         triples = X3B/G,
         HR = HR/G, 
         R = R/G) %>% 
  lm(R ~ BB + singles + doubles + triples + HR, data=.)

coefs <-tidy(fit, conf.int = TRUE)
coefs

#how well does our metric predict runs?  Predict runs for each team in 2002 and plot
Teams %>% 
  filter(yearID %in% 2002) %>% 
  mutate(BB = BB/G, 
         singles = (H-X2B-X3B-HR)/G, 
         doubles = X2B/G, 
         triples =X3B/G, 
         HR=HR/G,
         R=R/G)  %>% 
  mutate(R_hat = predict(fit, newdata = .)) %>%
  ggplot(aes(R_hat, R, label = teamID)) + 
  geom_point() +
  geom_text(nudge_x=0.1, cex = 2) + 
  geom_abline()
#note that points from the observed versus predicted plot fall close to the identify line
#this indicates our model is good <- we can use model instead of Batting average, HR, etc.

#define player specific metrics
#compute average number of team plate appearances per game
head(Batting)
pa_per_game <- Batting %>%
  filter(yearID == 2002) %>%
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>%
  pull(pa_per_game) %>%
  mean()
pa_per_game

#compute per plate appearance rates for players available in 2002 using previous data
players <- Batting %>% filter(yearID %in% 1999:2001) %>%
  group_by(playerID) %>%
  mutate(PA = BB + AB) %>%
  summarise(G = sum(PA)/pa_per_game,
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G,
            triples = sum(X3B)/G,
            HR = sum(HR)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA)) %>%
  filter(PA >= 300) %>%
  select(-G) %>%
  mutate(R_hat = predict(fit, newdata = .))
players

#plot distribution of player specific predicted runs
qplot(R_hat, data = players, binwidth = 0.5, color = I("black"))
#OR
players %>% ggplot(aes(R_hat)) +
  geom_histogram(binwidth = 0.5, color = "black")
#note wide variability between players

#add 2002 salary of each player
head(Salaries)
players <- Salaries %>%
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by = "playerID")
players

#add position information
position_names <- c("G_p","G_c","G_1b","G_2b","G_3b","G_ss","G_lf","G_cf","G_rf")
head(Fielding)
pos <- Fielding %>%
  filter(yearID == 2002) %>%
  filter(!POS %in% c("OF", "P")) %>%
  group_by(playerID) %>%
  top_n(1, G) %>%
  filter(row_number(G)==1) %>%
  ungroup() %>%
  select(playerID, POS)
head(pos)

players <- right_join(players, pos, by = "playerID") %>%
  filter(!is.na(POS) & !is.na(salary))
head(players)

#add first and last name
head(People)
players <- People %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  mutate(debut = as.Date(debut)) %>%
  right_join(players, by="playerID")
head(players)

#top 10 players
players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>%
  arrange(desc(R_hat)) %>% top_n(10)

#players with higher metric have higher salaries
players %>% ggplot(aes(salary, R_hat, color = POS)) +
  geom_point() +
  scale_x_log10()
#remake plot with players that debuted before 1997
players %>% filter(year(debut) < 1998) %>%
  ggplot(aes(salary, R_hat, color = POS)) +
  geom_point() +
  scale_x_log10()

#linear programming
install.packages("reshape2")
install.packages("lpSolve")
library(reshape2)
library(lpSolve)
players <- players %>% filter(debut <= "1997-01-01" & debut > "1988-01-01")
constraint_matrix <- acast(players, POS ~ playerID, fun.aggregate = length)
npos <- nrow(constraint_matrix)
constraint_matrix <- rbind(constraint_matrix, salary = players$salary)
constraint_dir <- c(rep("==", npos), "<=")
constraint_limit <- c(rep(1, npos), 50*10^6)
lp_solution <- lp("max", players$R_hat,
                  constraint_matrix, constraint_dir, constraint_limit,
                  all.int = TRUE) 
our_team <- players %>%
  filter(lp_solution$solution == 1) %>%
  arrange(desc(R_hat))
our_team %>% select(nameFirst, nameLast, POS, salary, R_hat)

my_scale <- function(x) (x - median(x))/mad(x)
players %>% mutate(BB = my_scale(BB), 
                   singles = my_scale(singles),
                   doubles = my_scale(doubles),
                   triples = my_scale(triples),
                   HR = my_scale(HR),
                   AVG = my_scale(AVG),
                   R_hat = my_scale(R_hat)) %>%
  filter(playerID %in% our_team$playerID) %>%
  select(nameFirst, nameLast, BB, singles, doubles, triples, HR, AVG, R_hat) %>%
  arrange(desc(R_hat))

### Regression Fallacy
#does the data confirm the existence of a sophomore slump?
#examine batting averages of 2nd year rookie of the year award winners
#create table with playerID, names, most played positions
library(Lahman)
playerInfo <- Fielding %>%
  group_by(playerID) %>%
  arrange(desc(G)) %>%
  slice(1) %>%
  ungroup %>%
  left_join(People, by="playerID") %>%
  select(playerID, nameFirst, nameLast, POS)

#create table wtih only ROY award winners and adding batting statistics
ROY <- AwardsPlayers %>%
  filter(awardID == "Rookie of the Year") %>%
  left_join(playerInfo, by="playerID") %>%
  rename(rookie_year = yearID) %>%
  right_join(Batting, by="playerID") %>%
  mutate(AVG = H/AB) %>%
  filter(POS != "P")

#keep only rookie and sophomore season, remove players that didn't play sophomore season
ROY <- ROY %>%
  filter(yearID == rookie_year | yearID == rookie_year+1) %>%
  group_by(playerID) %>%
  mutate(rookie = ifelse(yearID == min(yearID), "rookie", "sophomore")) %>%
  filter(n() == 2) %>%
  ungroup %>%
  select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG)
head(ROY)

#create columns for rookie and sophomore batting avg
ROY <- ROY %>% spread(rookie, AVG) %>% arrange(desc(rookie))
head(ROY)

#compute proportion of players that have lower batting avg their sophomore year
mean(ROY$sophomore - ROY$rookie <=0)

#look at all players and see if trend is there
two_years <- Batting %>%
  filter(yearID %in% 2013:2014) %>%
  group_by(playerID, yearID) %>%
  filter(sum(AB) >= 130) %>%
  summarize(AVG = sum(H)/sum(AB)) %>%
  ungroup %>%
  spread(yearID, AVG) %>%
  filter(!is.na(`2013`) & !is.na(`2014`)) %>%
  left_join(playerInfo, by="playerID") %>%
  filter(POS!="P") %>%
  select(-POS) %>%
  arrange(desc(`2013`)) %>%
  select(nameFirst, nameLast, `2013`, `2014`)
two_years
#batting avg go down in the 2nd year for top performers

#look at worse performers
arrange(two_years, `2013`)
#batting averages go up

#no sophomore slump
two_years %>% ggplot(aes(`2013`, `2014`)) + geom_point()
#OR
qplot(`2013`, `2014`, data = two_years)

#data is correlated, but not perfectly
summarize(two_years, cor(`2013`,`2014`))

#on average, we expect high performers to do a little worse the next year due to chance


###
# Measurement Error Models
# common to have a nonrandom co variate, such as time
# randomness is introduced from measurement error, rather than sampling or natural variability

library(dslabs)
falling_object <- rfalling_object()
head(falling_object)

#draw the trajectory of the ball falling from the tower of Pisa
falling_object %>%
  ggplot(aes(time, observed_distance))+
  geom_point() +
  ylab("Distance in meters") +
  xlab("Time in seconds")

#use lm() to estimate coefs
fit <- falling_object %>%
  mutate(time_sq = time^2) %>%
  lm(observed_distance~time+time_sq, data=.)
tidy(fit)

#check if estimated parabola fits the data
augment(fit) %>%
  ggplot() +
  geom_point(aes(time, observed_distance)) +
  geom_line(aes(time, .fitted), col = "blue")

#see the summary statistic of the regression
tidy(fit, conf.int = TRUE)
