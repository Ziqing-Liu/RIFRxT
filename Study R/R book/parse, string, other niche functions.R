library(tidyverse)

#read_csv
heights <- read_csv("data/heights.csv")

read_csv("a,b,c 1,2,3 4,5,6")

#Vector
str(parse_logical(c("TRUE", "FALSE", "NA")))

str(parse_integer(c("1", "2", "3")))

str(parse_date(c("2010-01-01", "1979-10-14")))

parse_integer(c("1", "231", ".", "456"), na = ".")

# what an error looks like
x <- parse_integer(c("123", "345", "abc", "123.45"))


# parse function is useful for getting rid of numbers with different symbols in them 
parse_double("1.23")

parse_double("1,23", locale = locale(decimal_mark = ","))

parse_number("$100")

parse_number("20%")

parse_number("It cost $123.45")

parse_number("$123,456,789")

parse_number( "123.456.789", locale = locale(grouping_mark = ".") )

parse_number( "123'456'789", locale = locale(grouping_mark = "'") )

### strings 

charToRaw("Hadley")

### factors 

fruit <- c("apple", "banana") parse_factor(c("apple", "banana", "bananana"), levels = fruit)








