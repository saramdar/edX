#7.linreg - 2.0 linear models
library(tidyverse)
library(Lahman)
get_slope <- function(x,y) cor(x,y)*sd(y)/sd(x)

#find regression line for predicting runs from BBs
bb_slope <- Teams %>%
  filter(yearID %in% 1962:2001) %>%
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>%
  summarize(slope = get_slope(BB_per_game, R_per_game))
bb_slope

#find regression line for predicting runs from singles
singles_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>%
  mutate(Singles_per_game = (H-HR-X2B-X3B)/G, R_per_game = R/G) %>%
  summarize(slope = get_slope(Singles_per_game, R_per_game))

singles_slope 

#calculate correlation between HR, BB, and singles
Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>%  
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB,Singles))

#stratify HR per game to nearest 10, filter out stata with few points
dat <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
           mutate(HR_strata = round(HR/G, 1),
                  BB_per_game = BB/G,
                  R_per_game = R/G) %>%
           filter(HR_strata >=0.4 & HR_strata <=1.2)

#scatterplot for each HR strata
dat %>%
  ggplot(aes(BB_per_game, R_per_game)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~HR_strata)

#calculate slope of regression line after stratifying by HR
dat %>%
  group_by(HR_strata) %>%
  summarize(slope = cor(BB_per_game, R_per_game)*sd(R_per_game)/sd(BB_per_game))

#stratify BB per game to nearest 10, filter out stata with few points
dat <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB_strata = round(BB/G, 1),
         HR_per_game = HR/G,
         R_per_game = R/G) %>%
  filter(BB_strata >=2.8 & BB_strata <=3.9)

#scatterplot for each BB strata
dat %>%
  ggplot(aes(HR_per_game, R_per_game)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~BB_strata)

#calculate slope of regression line after stratifying by BB
dat %>%
  group_by(BB_strata) %>%
  summarize(slope = cor(HR_per_game, R_per_game)*sd(R_per_game)/sd(HR_per_game))


#2.1 assessment
install.packages("HistData")    
library(HistData)
data("GaltonFamilies")
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

#Q2
lm(son ~ father, data = galton_heights)

#Q3
galton_heights <- galton_heights %>%
  mutate(father_centered = father - mean(father))

lm(son ~ father_centered, data = galton_heights)
