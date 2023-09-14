6. Data Wrangling - 2.2 Combining tables

library(tidyverse)
library(ggrepel)
library(dslabs)
library(dplyr)
ds_theme_set()
data(murders)
head(murders)
data("polls_us_election_2016")
head(results_us_election_2016)

tab <- left_join(murders, results_us_election_2016, by = "state")
head(tab)

tab %>% ggplot(aes(population/10^6, electoral_votes, label = abb))+
  geom_point()+
  geom_text_repel()+
  scale_x_continuous(trans = "log2")+
  scale_y_continuous(trans = "log2")+
  geom_smooth(method = "lm", se = FALSE)

tab1 <- slice(murders, 1:6) %>% select(state, population)
tab1
tab2 <- slice(results_us_election_2016, c(1:3, 5, 7:8)) %>% select(state, electoral_votes)
tab2
left_join(tab1, tab2) #only keeps rows that have information in the first table
tab1 %>% right_join(tab2) #only keeps rows that have information in the second table
inner_join(tab1,tab2) #keep only the rows that have information in both tables
full_join(tab1, tab2) #keeps all rows
semi_join(tab1, tab2) #keeps part of the first table for which we have information in the second.  does not add new columns
anti_join(tab1, tab2) #keeps elements of the first table for which there is no information in the second.  Does not add new columns

#Assessment
head(murders)
#create tables to match assessment
tab1 <- slice(murders, c(1:3, 8:9)) %>% select(state, population)
tab1

#OR
tab1 <- murders %>% 
  filter(state %in% c("Alabama", "Alaska", "Arizona", "Delaware", "District of Columbia")) %>%
  select(state, population)
tab1

tab2 <- results_us_election_2016 %>% 
  filter(state %in% c("Alabama", "Alaska", "Arizona", "California", "Colorado", "Connecticut")) %>% 
  select(state, electoral_votes)
tab2

dim(tab1)
dim(tab2)
dat <- left_join(tab1, tab2, by = "state")
dat
dat <- semi_join(tab1, tab2, by = "state")
dat

df1 <- bind_cols(x = c("a","b"), y = c("a","a"))
df1
df2 <- bind_cols(x = c("a", "a"), y = c("a", "b"))
df2
final <- setdiff(df1, df2)
final

#Q5-7
install.packages("Lahman")
library(Lahman)
top <- Batting %>%
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%
  slice(1:10)
top %>% as_tibble()
People %>% as_tibble()

#Q5 create a combined table of names and stats of the top 10 home run hitters for 2016
top_names <- top %>% 
  left_join(People) %>% #left join joins to the first table (top) and leaves out NAs
  select(playerID, nameFirst, nameLast, HR)
top_names

#Q6 add salary column from "Salaries" data frame to top_names table
top_salary <- Salaries %>%
  filter(yearID == 2016) %>%
  right_join(top_names) %>%
  select(nameFirst, nameLast, teamID, HR, salary)
top_salary

#Q7 inspect Awardsplayers table. filter awares to only 2016
head(AwardsPlayers)
tab_awards <- AwardsPlayers %>% 
  filter(yearID == 2016)
tab_awards
#how many top 10 players won awards
inner_join(top_names, tab_awards)
#OR
length(intersect(tab_awards$playerID, top_names$playerID))
#how many players won awards that weren't top 10?
length(setdiff(tab_awards$playerID, top_names$playerID))
