library(tidyverse)

diamonds2 <- diamonds %>% 
  filter(between(y, 3, 20))


#ifelse() function to replace unusual values with NA
diamonds2 <- diamonds %>%
  mutate(y = ifelse(y < 3 | y > 20, NA, y))

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point(na.rm = TRUE)

nycflights13::flights %>% 
  mutate( cancelled = is.na(dep_time),
          sched_hour = sched_dep_time %/% 100,
          sched_min = sched_dep_time %% 100,
          sched_dep_time = sched_hour + sched_min / 60 ) %>% 
  ggplot(mapping = aes(sched_dep_time)) + 
  geom_freqpoly( mapping = aes(color = cancelled), 
                 binwidth = 1/4 
                 )

#####

ggplot(data = diamonds, mapping = aes(x = price)) + 
  geom_freqpoly(mapping = aes(color = cut), binwidth = 500)

ggplot(diamonds) + 
  geom_bar(mapping = aes(x = cut))

ggplot( 
  data = diamonds, 
  mapping = aes(x = price, y = ..density..)
  )+ 
  geom_freqpoly(mapping = aes(color = cut), binwidth = 500)

















