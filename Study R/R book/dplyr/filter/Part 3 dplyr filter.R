###workflow basics 

library(tidyverse)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

filter(mpg, cyl = 8)
filter(diamond, carat > 3)


###dplyr

library(nycflights13)

library(tidyverse)

nycflights13::flights

flights

### filter (pick observations by their values)
### arrange (reorder the rows)
### select (ick variables by their names)
### mutate (Create new variables with functions of existing variables)
### summarize (Collapse many values down to a single summary)
### group_by (changes the scope of each function from operating on the entire dataset to operating on it group-by-group)


filter(flights, month == 1, day == 1)

jan1 <- filter(flights, month == 1, day == 1)

(jan1 <- filter(flights, month == 1, day == 1))

### == is very important 

### Boolean logic 

#filtered for only december 
filter(flights, month == 11 & !month == 12)

#filter for flights in november and december but not in each invidual month (impossible with the data set but useful in others)
filter(flights, month == 11 & month == 12)

#filters for november 
filter(flights, month == 11)

#filters for flights in both november and december but not in shared (impossible with this data set but useful in others)
filter(flights, month == 11, month == 12)

#filters for every flight
filter(flights, month == "11" | month == "12")

### filter will ignore NA unless i tell it so 
df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)

### is.na tells filter to include NA
filter(df, is.na(x) | x > 1)


### Excerise 
F <- flights

flights

late <- filter(flights, arr_delay >2)

Houston <- filter(flights, dest == "IAH" | dest == "HOU")

### whether to use "" “Am I talking about the thing, or the name of the thing?”
###The thing → no quotes
###The name of the thing → quotes

operated <- filter(flights, carrier == "DL" | carrier == "UA" | carrier == "US")

departed <- filter(flights, month == 7 | month == 8 | month == 9)

arrived <- filter(flights, dep_delay > 2, dep_time == sched_dep_time)

delay <- filter(flights, dep_delay >1, arr_delay<30)

mid <- filter(flights, dep_time<600)

mid2 <- filter(flights, between(dep_time, 1, 600))

### filtering for NA

missing <- filter(flights, is.na(dep_time))

is.na(dep_time)








