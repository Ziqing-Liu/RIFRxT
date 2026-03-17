library(tidyverse)

library(nycflights13)

#Grouping is most useful in conjunction with summarize(), 
#but you  can also do convenient operations with mutate() and filter():

flights %>% 
  group_by(year, month, day) %>% 
  filter(rank(desc(arr_delay)) < 10)

popular_dests <- flights %>% 
  group_by(dest) %>% 
  filter(n() > 365)

popular_dests %>% 
  filter(arr_delay > 0) %>% 
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)
