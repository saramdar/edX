1.1 Baseball as a motivating example
install.packages("Lahman")
library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

#do teams that hit more home runs score more runs?
head(Teams)
Teams %>% filter(yearID %in% 1962:2001) %>%
  mutate(HR_per_game = HR/G, R_per_game = R/G) %>%
  ggplot(aes(HR_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

#do teams that steal more bases score more runs?
Teams %>% filter(yearID %in% 1962:2001) %>%
  mutate(SB_per_game = SB/G, R_per_game = R/G) %>%
  ggplot(aes(SB_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

#do teams that have more bases on ball score more runs?
Teams %>% filter(yearID %in% 1962:2001) %>%
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>%
  ggplot(aes(BB_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

#Q4 make a scatterplot showing at bats vs runs per game
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

#Q5 ?Teams to find out info about data set

#Q6 make a scatterplot showing runs per game vs at bats per game
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

#Q7 use the filtered Teams data from q6, make a scatterplot of win rate vs number of fielding errors per game
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(W_per_game = W/G, E_per_game = E/G) %>%
  ggplot(aes(W_per_game, E_per_game)) + 
  geom_point(alpha = 0.5)

#Q8 use the filtered teams data from q6, make a scatter plot of triples (X3B) per game vs doubles (X2B) per game
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(triples_per_game = X3B/G, doubles_per_game = X2B/G) %>%
  ggplot(aes(triples_per_game, doubles_per_game)) + 
  geom_point(alpha = 0.5)
