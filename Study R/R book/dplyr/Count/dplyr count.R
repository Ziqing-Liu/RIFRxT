library(tidyverse)

library(nycflights13)


# count() is just a convenience wrapper around 
# group_by() + summarise() — with n() 
# for unweighted counts and sum(wt) for weighted counts.

# The count() function is equivalent to grouping by the specified variable(s) 
# and then summarising with either n() or sum() when weights are provided.

not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarize(mean = mean(dep_delay))

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarize( delay = mean(arr_delay) 
             )
ggplot(data = delays, mapping = aes(x = delay)) +
  geom_freqpoly(binwidth = 10)

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarize( delay = mean(arr_delay, na.rm = TRUE), n = n() )

ggplot(data = delays, mapping = aes(x = n, y = delay)) +
  geom_point(alpha = 1/10)

delays %>% 
  filter(n > 25) %>%
  ggplot(mapping = aes(x = n, y = delay)) +
  geom_point(alpha = 1/10)


batting <- as_tibble(Lahman::Batting)

batters <- batting %>% 
  group_by(playerID) %>% 
  summarize( ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE), 
             ab = sum(AB, na.rm = TRUE) 
             )

batters %>% 
  filter(ab > 100) %>% 
  ggplot(mapping = aes(x = ab, y = ba)) +
  geom_point() + geom_smooth(se = FALSE)
