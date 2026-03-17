library(tidyverse)

library(nycflights13)

summarize(flights, delay = mean(dep_delay, na.rm = TRUE))

by_day <- group_by(flights, year, month, day) 

sum <- summarize(by_day, delay = mean(dep_delay, na.rm = TRUE))


###Group flights by destination.(On its own, group_by doesn’t change the data you see — it just changes how the next verbs behave.)
by_dest <- group_by(flights, dest) 

###Summarize to compute distance, average delay, and number of  flights.
distvsdelay <- summarize(by_dest, count = n(),
dist = mean(distance, na.rm = TRUE), 
delay = mean(arr_delay, na.rm = TRUE) 
)  

###Filter to remove noisy points and Honolulu airport, which is  almost twice as far away as the next closest airport.
distvsdelay <- filter(delay, count > 20, dest != "HNL")

###aes() tells ggplot which variables in your data control which visual properties of the plot.
ggplot(data = distvsdelay, mapping = aes(x = dist, y = delay)) + 
  geom_point(aes(size = count), alpha = 1/3) + 
  geom_smooth(se = FALSE)

###pipe 
delays <- flights %>% 
  group_by(dest) %>% 
  summarize( count = n(),
             dist = mean(distance, na.rm = TRUE), 
             delay = mean(arr_delay, na.rm = TRUE) 
             ) %>% 
  filter(count > 20, dest != "HNL") 
             
### missing value (na.rm) (remove NA values you’re telling a function to ignore missing values (NA) when doing a calculation)

flights %>% 
  group_by(year, month, day) %>%
  summarize(mean = mean(dep_delay))

flights %>% 
  group_by(year, month, day) %>% 
  summarize(mean = mean(dep_delay, na.rm = TRUE))

###filter out na values in our case cancelled flights 
not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(year, month, day) %>%
  summarize(mean = mean(dep_delay))        

###In R, ! is the logical NOT operator.
###It flips TRUE to FALSE and FALSE to TRUE.

Think: “not”.

Basic example
!TRUE
# FALSE

!FALSE
# TRUE
             
             
             
             
             
             
             
             
             
             
             
             