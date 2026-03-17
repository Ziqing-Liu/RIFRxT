library(tidyverse)

library(nycflights13)

arrange(flights, year, month, day)

arrange(flights, desc(arr_delay))

### missing value always at the end 
### tibble (modern, cleaner version of a data frame)
df <- tibble(x = c(5, 2, NA))
arrange(df, x)
arrange(df, desc(x))

### excerise 

missing <- arrange(flights, is.na(dep_time)) ???



delay <- arrange(flights, arr_delay)

fast <- arrange(flights, air_time)

far <- arrange (flights, distance)

short <- arrange (flights, desc(distance))

                