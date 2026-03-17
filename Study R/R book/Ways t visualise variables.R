#ways to visualise variables 

# two category variables 

library(tidyverse)

ggplot(data = diamonds) + 
  geom_count(mapping = aes(x = cut, y = color))

diamonds %>% 
  count(color, cut)

#heat map 
diamonds %>% 
  count(color, cut) %>% 
  ggplot(mapping = aes(x = color, y = cut)) + 
  geom_tile(mapping = aes(fill = n))

#Two Continuous Variables

ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = carat, y = price))

ggplot(data = diamonds) + 
  geom_point( 
    mapping = aes(x = carat, y = price),
    alpha = 1 / 100
    )

ggplot(data = smaller) + 
  geom_bin2d(mapping = aes(x = carat, y = price))

ggplot(data = smaller) + 
  geom_hex(mapping = aes(x = carat, y = price))

ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))

ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_number(carat, 20)))

ggplot(data = smaller) + 
  geom_point(mapping = aes(x = carat, y = price))

#excerise combining cut,carat and price

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~ cut) +
  theme_minimal()














