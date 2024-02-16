#7.linreg - 2.3 advanced dplyr assessment
#Q5
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2)

get_slope <- function(data) {
  fit <- lm(R ~ BB, data = data)
  sum.fit <- summary(fit)
  
  data.frame(slope = sum.fit$coefficients[2, "Estimate"], 
             se = sum.fit$coefficients[2, "Std. Error"],
             pvalue = sum.fit$coefficients[2, "Pr(>|t|)"])
}

#what code will add the coefficient, standard error, and p-value in the model?
dat %>% 
  group_by(HR) %>% 
  summarize(get_slope(across()))
#note that this delivers a tibble 9 X 4 <- 4 columns of data

#Q7 does the relationship between home runs and runs per game vary by league?
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         R = R/G) %>%
  select(lgID, HR, BB, R)

dat %>% 
  group_by(lgID) %>%
  summarize(tidy(lm(R ~ HR, data = across()), conf.int = T)) %>% 
  filter(term == "HR") 

#Q8 explore relationships between parent and child height
#filter data to first son, first daughter, father, mother
library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton

#how many father/daughter combinations?  Mother son combinations?
galton %>% 
  group_by(pair) %>%
             summarize(n=n())

#Q9 calculate the correlation coefficients for fathers and daugthers, fathers and sons, mothers and daugthers, and mothers and sons
galton %>%
  group_by(pair) %>%
  summarize(cor(childHeight, parentHeight))

#Q10 use lm() and broom to fit regression lines for each parent-child pair type.  
#compute the LSE, standard errors, confidence intervals and p-values for the parentHeight coefficient for each pair
galton %>%
  group_by(pair) %>%
  summarize(tidy(lm(childHeight ~ parentHeight), conf.int = TRUE)) %>%
  filter(term == "parentHeight")
#parent height coefficient is the estimate = slope\


#create a chart of confidence intervals to help with data review
galton %>%
  group_by(pair) %>%
  summarize(tidy(lm(childHeight ~ parentHeight), conf.int = TRUE)) %>%
  filter(term == "parentHeight") |>
  select(pair, estimate, conf.low, conf.high) |>
  ggplot(aes(pair, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()



