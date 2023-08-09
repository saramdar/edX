library(tidyverse)
library(dslabs)
data("research_funding_rates")
head(research_funding_rates)

#compute totals that were successful or not
totals <- research_funding_rates %>%
  select(-discipline) %>%
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men, 
            no_men = applications_men - awards_men, 
            yes_women = awards_women, 
            no_women = applications_women - awards_women)
#compare percentages of men/women with awards
totals %>% summarize(percent_men = yes_men/(yes_men + no_men), 
                     percent_women = yes_women/(yes_women + no_women))

# 2x2 table and p-value for lady tasting tea problem
tab <- matrix(c(3,1,1,3),2,2)
rownames(tab) <- c("poured before", "poured after")
colnames(tab) <- c("guessed before", "guessed after")
tab

#p-value calc with fishers exact test
fisher.test(tab, alternative = "greater")

#overall funding rate
funding_rate <- totals %>%
  summarize(percent_total =
              (yes_men+yes_women)/(yes_men+no_men+yes_women+no_women))%>%
  .$percent_total
funding_rate

# will we see a difference between funding for men and women if funding was assigned at random using the above rate?
# chi-squared test answers this question
#create 2x2 table with observed data
two_by_two <- tibble(awarded = c("no", "yes"),
                     men = c(totals$no_men, totals$yes_men),
                     women = c(totals$no_women, totals$yes_women))
two_by_two
#recreate 2x2 with expected result at overall funding rate
tibble(awarded = c("no", "yes"),
       men = (totals$no_men+totals$yes_men)*
  c(1-funding_rate, funding_rate),
womens = (totals$no_women+totals$yes_women)*
  c(1-funding_rate, funding_rate))

two_by_two %>%
  select(-awarded) %>%
  chisq.test()

#odds ratio
#odds of getting funded for men
odds_men <- (two_by_two$men[2]/sum(two_by_two$men))/(two_by_two$men[1]/sum(two_by_two$men))
odds_men

#odds of getting funded for women
odds_women <- (two_by_two$women[2]/sum(two_by_two$women))/(two_by_two$women[1]/sum(two_by_two$women))
odds_women

#odds ratio
odds_men/odds_women

# The 'totals' data have already been loaded. Examine them using the `head` function.
head(totals)

# Generate a variable called `odds_C` that contains the odds of getting the prediction right for grade C- polls
odds_C <- totals$'C-'[1]/sum(totals$C-)
odds_C

# Generate a variable called `odds_A` that contains the odds of getting the prediction right for grade A- polls
odds_A <- totals$'A-'[1]/sum(totals$A-)
odds_A

# Calculate the odds ratio to determine how many times larger the odds ratio is for grade A- polls than grade C- polls
odds_A/odds_C