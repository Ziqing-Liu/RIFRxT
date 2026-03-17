library(tidyverse)

library(nycflights13)

not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarize(mean = mean(dep_delay))

not_cancelled %>%
  group_by(year,month, day) %>%
  summarize(
    # average delay:
    avg_delay1 = mean(arr_delay),
    # average positive delay:
    avg_delay2 = mean(arr_delay[arr_delay > 0])
  )

#Measures of spread standard deviation sd(x), Inter Quartile Range IQR(x), median absolute deviation mad(x)
not_cancelled %>% 
  group_by(dest) %>% 
  summarize(distance_sd = sd(distance)) %>% 
  arrange(desc(distance_sd))

#Measures of rank min(x), quantile(x, 0.25), max(x)  
#Quantiles are a generalization of the median. 
#For example, quan  tile(x, 0.25) will find a value of x that is greater than 25% of  the values, and less than the remaining 75%:

# When do the first and last flights leave each day? 
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarize( first = min(dep_time), last = max(dep_time)
             )

#Measures of position first(x), nth(x, 2), last(x)
#These work similarly to x[1], x[2], and x[length(x)] 
but let  you set a default value if that position does not exist
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarize( first_dep = first(dep_time), 
             last_dep = last(dep_time)
             )
# r here is used as rank 
not_cancelled %>% 
  group_by(year, month, day) %>% 
  mutate(r = min_rank(desc(dep_time))) %>% 
  filter(r %in% range(r))

#To count the number of non-missing values, use sum(!is.na(x)). 
#To count the number of distinct  (unique) values, use n_distinct(x):
not_cancelled %>% 
  group_by(dest) %>% 
  summarize(carriers = n_distinct(carrier)) %>% 
  arrange(desc(carriers))

#more ways to use count
not_cancelled %>% 
  count(dest)

not_cancelled %>% 
  count(tailnum, wt = distance)

#When used with numeric functions, TRUE is converted to 1 and  FALSE to 0. 
#This makes sum() and mean() very useful: 
#sum(x)  gives the number of TRUEs in x, and mean(x) gives the proportion:

# How many flights left before 5am? (these usually indicate delayed flights from the previous day)
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarize(n_early = sum(dep_time < 500))

# What proportion of flights are delayed by more
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarize(hour_perc = mean(arr_delay > 60))

#Grouping by Multiple Variables
daily <- group_by(flights, year, month, day) 
(per_day <- summarize(daily, flights = n()))
(per_month <- summarize(per_day, flights = sum(flights)))
(per_year <- summarize(per_month, flights = sum(flights)))


daily %>%  
  ungroup() %>% # no longer grouped by date 
  summarize(flights = n()) # all flights












