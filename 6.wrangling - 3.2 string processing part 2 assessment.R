#6. wrangling - string processing part 2

#Q1 the following function identifies values that result in NAs when converted to numeric and values less than 50 inches or greater than 84 inches as NOT being correctly formatted
not_inches <- function(x, smallest = 50, tallest = 84) {
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest 
  ind
}

#Q2 which argument when passed to function not_inches() would return vector FALSE
not_inches(70)
#this is within the range set and entry is correctly formatted

#Q3
#the function not_inches returns a logical vector, True indicates the entry is incorrectly formatted
#false indicates an entry is correctly formatted.  We uset his logical vector to filter the data to 
#only show incorrectly formatted entires.

#Q4 which pattern vector yields the following highlighted (70, 5 ft, 4'11)
s <-c("70", "5 ft", "4'11", "", ".", "six feet")
s
pattern <- "\\d|ft" # pattern searches for at least 1 digit (any numeric characters) or the text  "ft"
str_view_all(s, pattern)

#Q5 what is the printed result of the following?  (str_detect is looking for a lowercase letter)
animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[a-z]"
str_detect(animals, pattern)

#Q6 what is the printed result of the following?  (str_detect is looking for an upper case letter at the end of the string $)
animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[A-Z]$"
str_detect(animals, pattern)

#Q8 which pattern vectors yield the following result str_detect(animals, pattern) = true true true true
animals <- c("moose", "monkey", "meerkat", "mountain lion")
#pattern <- "mo*"
#pattern <- "mo?"
str_detect(animals, pattern)

#Q9 create code that renames everything to full name
schools <- c("U. Kentucky", "Univ New Hampshire", "Univ. of Massachusetts", "University Georgia", "U California", "California State University")
schools
schools %>%
  str_replace("^Univ\\.?\\s|^U\\.?\\s", "University ") %>%
  str_replace("^University of |^University ", "University of ")

#Q10 what is the result of the following code:
problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
problems
pattern_with_groups <- "^([4-7])[,\\.](\\d*)$" #checks for first digit between 4-7, one character, either a comma or period, and last one or two digits
str_replace(problems, pattern_with_groups, "\\1'\\2")

#Q11 
pattern_with_groups <- "([4-7])[,\\.\\s](\\d*)$" #only checks for one character, either a comma, period, or space
str_replace(problems, pattern_with_groups, "\\1'\\2")

#Q14
s <- "19"
pattern <- "[193+]"
pattern
str_detect(s, pattern)
