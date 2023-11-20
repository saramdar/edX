#6.wrangling - 3.1 string processing + assessment part 1
# read in raw murders data from Wikipedia
url <- "https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States_by_state&direction=prev&oldid=810166167"
murders_raw <- read_html(url) %>% 
  html_nodes("table") %>% 
  html_table() %>%
  .[[1]] %>%
  setNames(c("state", "population", "total", "murder_rate"))

# inspect data and column classes
head(murders_raw)
class(murders_raw$population)
class(murders_raw$total)

#define strings in R- use double or single quotes
s <- "hello!"
cat(s)
s <- 'hello!'
cat(s)
s <- '10"'
cat(s)
s <- "5'"
cat(s)
#use \ to esc a character if needed both the ' and "
s <- "5'10\""
cat(s)

#try to change population to numbers, doesn't work because of the commas in data
as.numeric(murders_raw$population[1:3])

#detect and locate comma and replace with empty character in murders_raw table
commas <- function(x) any(str_detect(x, ","))
murders_raw %>% summarize_all(list(commas))
#use function to remove and replace commas
test_1 <- str_replace_all(murders_raw$population, ",", "")
test_1 <- as.numeric(test_1)
test_2 <- parse_number(murders_raw$population)
identical(test_1, test_2)

murders_new <- murders_raw %>% mutate_at(2:3, parse_number)
murders_new%>% head
class(murders_new$population)

#assessment
#Q2 cat("LeBron James is 6'\8"")
cat(" LeBron James is 6'8\" ")

#Q4
a <- c("January", "February", "March", "April", "May")
b <- c("$128,1","$109,1", "$115", "$122", "$117")
c <- c("$16", "$12", "$17,1", "$15,1", "$15")

dat <-bind_cols(a, b, c) %>% setNames(c("Month", "Sales", "Profit"))
dat
#change sales and profit to numeric
dat %>% mutate_at(2:3, parse_number) %>% head()
#OR
dat %>% mutate_at(2:3, funs(str_replace_all(., c("\\$|,"), ""))) %>% mutate_at(2:3, as.numeric) %>% head()
#sales and profit are now dbl (double) variables

#Q5 define a string
cat("10\"")
