#6.wrangling - string processing part 3
#separate with regex
library(tidyverse)
library(dslabs)
data(reported_heights)

s <- c("5'10", "6'1")
tab <- data.frame(x = s)
tab %>% separate(x, c("feet", "inches"), sep = "'")
#equivalent code using extract
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")

##Using Groups and Quantifiers
yes <- c("5", "6", "5")
no <-c("5'", "5''", "5'4")
s <- c(yes, no)
str_replace(s, "([56])'?$", "\\1'0")

pattern <- "[4-7]\\s*'\\s*\\d{1,2}$"

#testing and improving
data(reported_heights)
#function that captures all entries that can't be converted into numbers
not_inches_or_cm <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- !is.na(inches) &
    ((inches >= smallest & inches <= tallest) |
       (inches/2.54 >= smallest & inches/2.54 <= tallest))
  !ind
}
problems <- reported_heights |>
  filter(not_inches_or_cm(height)) |>
  pull(height)
length(problems)

#fix some errors and check the proportion of numbers that fit pattern:
converted <- problems |> str_replace("feet|foot|ft", "'") |> str_replace("inches|in|''|\"", "") |> str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") 
pattern <- "^[4-7]\\s*'\\s*(\\d{1,2})$"
index <- str_detect(converted, pattern)
mean(index)
#examine remaining cases
converted[!index]

convert_format <- function(s){s %>%
    str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
    str_replace_all("inches|in|''|\"|cm|and", "") %>%  #remove inches and other symbols
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") %>% #change x.y, x,y x y
    str_replace("^([56])'?$", "\\1'0") %>% #add 0 when to 5 or 6
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>% #change european decimal
    str_trim() #remove extra space
  }

#words to numbers function
words_to_numbers <- function(s){
  str_to_lower(s) %>% 
    str_replace_all("zero", "0") %>%
    str_replace_all("one", "1") %>%
    str_replace_all("two", "2") %>%
    str_replace_all("three", "3") %>%
    str_replace_all("four", "4") %>%
    str_replace_all("five", "5") %>%
    str_replace_all("six", "6") %>%
    str_replace_all("seven", "7") %>%
    str_replace_all("eight", "8") %>%
    str_replace_all("nine", "9") %>%
    str_replace_all("ten", "10") %>%
    str_replace_all("eleven", "11")
  }

converted <- problems %>% words_to_numbers() %>% convert_format()
remaining_problems <-converted[not_inches_or_cm(converted)]
pattern <- "^[4-7]\\s*'\\s*\\d+\\.?\\d*$"
index <- str_detect(remaining_problems, pattern)
remaining_problems[!index]

## cleaning it up
#write a function that cleans up strings so all feet and inches formats use the name x'y format
pattern <-"^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"
smallest <- 50
tallest <- 84
new_heights <- reported_heights %>%
  
  mutate(original = height,
         
         height = words_to_numbers(height) %>% convert_format()) %>%
  
  extract(height, c("feet", "inches"), regex = pattern, remove = FALSE) %>%
  
  mutate_at(c("height", "feet", "inches"), as.numeric) %>%
  
  mutate(guess = 12*feet + inches) %>%
  
  mutate(height = case_when(
    
    !is.na(height) & between(height, smallest, tallest) ~ height,
    
    !is.na(height) & between(height/2.54, smallest, tallest) ~ height/2.54,
    
    !is.na(height) & between(height*100/2.54, smallest, tallest) ~ height*100/2.54,
    
    !is.na(guess) & inches <12 & between(guess, smallest, tallest) ~ guess,
    
    TRUE ~ as.numeric(NA))) %>%
  
  select(-guess)



#check converted entries using code:

new_heights %>%
  
  filter(not_inches_or_cm(original)) %>%
  
  select(original, height) %>%
  
  arrange(height) %>%
  
  View()



#view shortest students:

new_heights %>% arrange(height) %>% head(n=7)



#

#

# string splitting

#read csv file line by line using base R function:

filename <- system.file("extdata/murders.csv", package = "dslabs")

lines <-readLines(filename)

lines %>% head()



#extract values that are separated by commas

x <- str_split(lines, ",")

x %>% head()



#separate column names

col_names <- x[[1]]

x <- x[-1]



#extract first entry of each element in x

library(purrr)

map(x, 1) %>% head()



#create data frame

dat <- data.frame(map_chr(x, 1),
                  
                  map_chr(x, 2),
                  
                  map_chr(x, 3),
                  
                  map_chr(x, 4),
                  
                  map_chr(x, 5)) %>%
  
  mutate_all(parse_guess) %>%
  
  setNames(col_names)

dat %>% head()



#

#

# case study - extraction a table from a PDF
library(dslabs)
data("research_funding_rates")
research_funding_rates 


#download the data
library("pdftools")
temp_file <- tempfile()
url <- "https://www.pnas.org/action/downloadSupplement?doi=10.1073%2Fpnas.1510159112&file=pnas.201510159SI.pdf"
download.file(url, temp_file, mode="wb")
txt <- pdf_text(temp_file)
txt
file.remove(temp_file)
raw_data_research_funding_rates <- txt[2]

#looking at the download
raw_data_research_funding_rates %>% head()
tab <- str_split(raw_data_research_funding_rates, "\n")
tab <- tab[[1]]
tab %>% head
the_names_1 <-tab[3]
the_names_1
the_names_2 <- tab[5]
the_names_2
#Extracting the data
the_names_1 #remove the leading space and everything following the comma
the_names_1 <- the_names_1 %>%
  str_trim() %>%
  str_replace_all(",\\s.", "") %>%
  str_split("\\s{2,}", simplify = TRUE)
the_names_1

the_names_2
the_names_2 <- the_names_2%>%
  str_trim() %>%
  str_split("\\s+", simplify = TRUE)
the_names_2

tmp_names <- str_c(rep(the_names_1, each =3), the_names_2[-1], sep = "_")

the_names <-c(the_names_2[1], tmp_names) %>%
  str_to_lower() %>%
  str_replace_all("\\s", "_")

the_names
#get the data
new_research_funding_rates <-tab[8:16] %>% 
  str_trim() %>%
  str_split("\\s{2,}", simplify = TRUE) %>%
  data.frame(stringsAsFactors =  FALSE) %>%
  setNames(the_names) %>%
  mutate_at(-1, parse_number)
new_research_funding_rates
identical(research_funding_rates, new_research_funding_rates)

#
#
#recoding the names of categorical variables
library(dslabs)
data("gapminder")
head(gapminder)
gapminder %>%
  filter(region == "Caribbean") %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()

#display long country names
gapminder %>%
  filter(region == "Caribbean") %>%
  filter(str_length(country) >=12) %>%
  distinct(country)

#recode long country names and remake plot
gapminder %>% filter(region == "Caribbean") %>%
  mutate(country = recode(country, 
                          'Antigua and Barbuda' = "Barbuda",
                          'Dominican Republic' = "DR", 
                          'St. Vincent and the Grenadines' = "St. Vincent",
                          'Trinidad and Tobago' = "Trinidad")) %>%
  ggplot(aes(year, life_expectancy, color = country)) + 
  geom_line()
