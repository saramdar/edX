#7.linreg - 3 confounding assessment
library(dslabs)
data("research_funding_rates")
research_funding_rates

#Q1 create a two by two table showing award status by gender using totals across disciplines
totals <- research_funding_rates %>%
  select(-discipline) %>%
  summarize_all(sum) %>%
  summarize(yes_men = awards_men,
            no_men = applications_men - awards_men,
            yes_women = awards_women,
            no_women = applications_women - awards_women)
totals

two_by_two <- data.frame(awarded = c("no", "yes"),
             men = c(totals$no_men, totals$yes_men),
             women = c(totals$no_women, totals$yes_women))
two_by_two             

#Q2 compute percentages of men awarded versus women awarded

totals %>% summarize(percent_men = yes_men/(yes_men + no_men), 
                     percent_women = yes_women/(yes_women + no_women))
#OR
two_by_two %>% 
  mutate(men = round(men/sum(men)*100, 1), women = round(women/sum(women)*100, 1)) %>%
  filter(awarded == "yes") %>%
  pull(men)

#Q3 run a chi squared test on the 2x2 table to determine whether the difference in the
# two funding awarded rates is significant (evaluate p-value)
two_by_two %>% select(-awarded) %>% chisq.test() %>%tidy()
#looks like it could be sigificant, but need to stratify and look deeper

#Q4
dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  pivot_longer(-discipline) %>%
  separate(name, c("type", "gender")) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  filter(gender != "total")
dat

# plot success rate vs disciplines with colors for gender and size for number of applications
#which fields do men have a higher success rate than women?
#which fields have the most applications from women?
dat %>% 
  ggplot(aes(discipline, success, size = applications, color = gender)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_point()

# which fields have the lowest funding rates?
dat %>%
  ggplot(aes(discipline, y = success)) + geom_col()
