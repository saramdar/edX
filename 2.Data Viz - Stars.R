library(tidyverse)
library(dslabs)
library(ggrepel)
data(stars)
options(digits =3)

#Q1
mean(stars$magnitude)
sd(stars$magnitude)

#Q2 denisty plot of magnitude
stars %>%
  ggplot(aes(magnitude)) +
  geom_density()

#Q3 examine distribuition of star temp
stars %>%
  ggplot(aes(temp)) +
  geom_density()

#Q4 scatterplot of temp,magnitude
stars %>%
  ggplot(aes(temp, magnitude, col = type)) +
  geom_point()

#Q5 scatterplot with flipped y axis (lower values of mag are at top of axis)
stars %>%
  ggplot(aes(log10(temp), magnitude)) +
  geom_point()+
  scale_y_reverse() +
  scale_x_reverse()

#Q8 add labels = star
stars %>%
  ggplot(aes(log10(temp), magnitude, label = star)) +
  geom_point()+
  geom_text_repel() +
  scale_y_reverse() +
  scale_x_reverse()

#Q9 add col = type
stars %>%
  ggplot(aes(log10(temp), magnitude, col = type)) +
  geom_point()+
  scale_y_reverse() +
  scale_x_reverse() 

                      
                      