library(tidyverse)

library(nycflights13)

# excerise checking early and late percentages 
flights %>%
  filter(!is.na(arr_delay)) %>%   # remove missing delays
  summarise(
    prop_early_15 = mean(arr_delay <= -15),
    prop_late_15  = mean(arr_delay >= 15)
  )

flights %>%
  filter(!is.na(arr_delay)) %>%   # remove missing delays
  summarise(
    prop_late_10  = mean(arr_delay >= 10)
  )

flights %>%
  filter(!is.na(arr_delay)) %>%   # remove missing delays
  summarise(
    prop_early_30 = mean(arr_delay <= -30),
    prop_late_30 = mean(arr_delay >= 30)
  )

flights %>%
  filter(!is.na(arr_delay)) %>%   # remove missing delays
  summarise(
    prop_on_time = mean(arr_delay <= 0),
    prop_late_2_hours = mean(arr_delay >= 120)
  )

flights %>%
  filter(!is.na(arr_delay)) %>%
  summarise(
    on_time_pct = mean(arr_delay <= 0) * 100,
    late_2hr_pct = mean(arr_delay >= 120) * 100
  )


cancelled <- flights %>%
  filter(!is.na(dep_delay))

cancelled <- flights %>%
  filter(!is.na(arr_delay))

### excerise looking at patterns of days and cancelled flights 
flights %>%
  group_by(year, month, day) %>%
  summarise(
    cancelled = sum(is.na(dep_delay)),
    total_flights = n(),
    .groups = "drop"
  )

letsee <- flights %>%
  group_by(carrier) %>%
  summarise(
    cancelled = sum(is.na(dep_delay)),
    total_flights = n(),
    .groups = "drop"
  )

flights %>%
  filter(!is.na(dep_delay)) %>%
  group_by(carrier) %>%
  summarise(
    avg_dep_delay = mean(dep_delay),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_dep_delay))

