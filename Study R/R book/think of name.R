library(tidyverse)

# visualising a categorical variable 
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))

diamonds %>% 
  count(cut)

#visualising a continuous variable 
ggplot(data = diamonds) + 
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

diamonds %>% 
  count(cut_width(carat, 0.5))

smaller <- diamonds %>% filter(carat < 3)  

ggplot(data = smaller, mapping = aes(x = carat)) + 
  geom_histogram(binwidth = 0.1)

#the function geom_freqpoly is good fo plot multiple histograms 
ggplot(data = smaller, mapping = aes(x = carat, color = cut)) + 
  geom_freqpoly(binwidth = 0.1)

ggplot(data = smaller, mapping = aes(x = carat)) + 
  geom_histogram(binwidth = 0.01)

ggplot(data = faithful, mapping = aes(x = eruptions)) + 
  geom_histogram(binwidth = 0.25)

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) + 
  coord_cartesian(ylim = c(0, 50))

unusual <- diamonds %>%
  filter(y < 3 | y > 20) %>%
  arrange(y)
unusual


#exerise 

price1 <- diamonds %>% filter(price>0)  

ggplot(data = price1, mapping = aes(x = price)) + 
  geom_histogram(binwidth = 1000)

ggplot(price1, aes(x = carat, y = price)) +
  geom_point()


smaller2 <- diamonds %>% filter(carat < 1.5)  

ggplot(data = smaller2, mapping = aes(x = carat)) + 
  geom_histogram(binwidth = 0.1)

ggplot(data = smaller2) + 
  geom_bar(mapping = aes(x = carat))







