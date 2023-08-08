#load libraries and data
options(digits = 3)
library(tidyverse)
library(titanic)

#density plot of age grouped by sex
titanic %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(Age, fill = Sex)) +
  geom_density(alpha = 0.2) 

#density plot of age grouped by sex, change to count on y axis
titanic %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(Age, y = ..count.., fill = Sex)) +
  geom_density(alpha = 0.2) 

#barplot male vs female
titanic %>%
  ggplot(aes(Sex)) +
  geom_bar() 

#histogram male vs female ages
titanic %>%
  ggplot(aes(Age, fill = Sex)) +
  geom_histogram(position = "dodge")

#create dparams argument for qq-plot
params <- titanic %>% 
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

#create qq-plot of passenger age (*this isn't correct, wasn't able to figure out how to incorporate params in geom_qq)
titanic %>% 
  ggplot(aes(sample = scale(Age))) +
  geom_qq() +
  geom_abline()

#barplot for number survived by sex
titanic %>%
  ggplot(aes(Sex, fill = Survived)) +
  geom_bar(position = position_dodge())

#density plot of survival by age, change to count on y axis
titanic %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(Age, y = ..count.., fill = Survived)) +
  geom_density(alpha = 0.2) 

#bloxplot of fare grouped by survival status
titanic %>%
  filter(Fare > 0) %>%
  ggplot(aes(Survived, Fare)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2) +
  scale_y_continuous(trans = "log2") 

#survival by passenger class
#barplot for number survived by class
titanic %>%
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar()

#barplot for proportion survived by class
titanic %>%
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar(position = position_fill())

#barplot for survival by class
titanic %>%
  ggplot(aes(Survived, fill = Pclass)) +
  geom_bar(position = position_fill())

#density plot of survival by age, change to count on y axis
titanic %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(Age,y=..count.., fill = Survived)) +
  geom_density(alpha = 0.2) +
  facet_grid(Pclass~Sex)
