#6.wrangling - 2.3 web scraping assessment
library(rvest)
library(dplyr)
url  <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")
class(nodes)
html_text(nodes[[8]])
html_table(nodes[[8]])

#Q1 convert the first 4 tables in nodes to data frames and identify which include team payroll?
html_table(nodes[[4]]) #change number from 1 to 4
sapply(nodes[1:4], html_table) #do all at once

#Q2
html_table(nodes[[length(nodes)-2]])
html_table(nodes[[length(nodes)-1]])
html_table(nodes[[length(nodes)]])

#Q3
tab_1 <- html_table(nodes[[10]])
tab_2 <-html_table(nodes[[19]])
tab_1
tab_2

#remove extra column from tab_1
tab_1 <- tab_1[,2:4]
# remove 1st row from tab_1
tab_1 <-tab_1[2:31,]
#rename columns in tab_1
tab_1 <- tab_1 %>% setNames(c("Team", "Payroll", "Average"))
head(tab_1)
#remove 1st row from tab_2
tab_2 <-tab_2[2:31,]

#rename columns in tab_2
tab_2 <- tab_2 %>% setNames(c("Team", "Payroll", "Average"))
head(tab_2)
joined_table <- full_join(tab_1, tab_2, by = "Team")

#Q4
library(rvest)
library(tidyverse)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
length(tab)

#Q5
sapply(tab[1:9], html_table, fill = TRUE)
tab[[6]] %>% html_table(fill = TRUE) %>% names()
