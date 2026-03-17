library(tidyverse)

library(nycflights13)


select(flights, year, month, day)

select(flights, year:day)

select(flights, -(year:day))


### rename used to rename variables or (colum/row names??)
rename(flights, tail_num = tailnum)

### moves these variables to the start of the data frame 
select(flights, time_hour, air_time, everything())


• starts_with("abc") matches names that begin with “abc”.
• ends_with("xyz") matches names that end with “xyz”.
• contains("ijk") matches names that contain “ijk”.
• matches("(.)\\1") selects variables that match a regular expression. 
• num_range("x", 1:3) matches x1, x2, and x3.


### Exercise 
select(flights, year, dep_time, dep_delay, arr_time, arr_delay)

select(flights, year, arr_time, arr_time)

vars <- c(
  "year", "month", "day", "dep_delay", "arr_delay"
)

flights |> select(one_of(vars))


select(flights, contains(yearselect(flights, contains("TIME"), starts_with("month"))





