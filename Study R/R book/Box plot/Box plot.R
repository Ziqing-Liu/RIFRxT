library(tidyverse)

ggplot(data = diamonds, mapping = aes(x = cut, y = price)) + 
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()

#reorder makes the plots look much neater 
ggplot(data = mpg) + 
  geom_boxplot( mapping = aes
                ( x = reorder(class, hwy, FUN = median), 
                  y = hwy 
                  )
                )

#cord flip flips the plots sideways 
ggplot(data = mpg) + 
  geom_boxplot( mapping = aes(
    x = reorder(class, hwy, FUN = median), 
    y = hwy ) ) + 
  coord_flip()


# excerise 

nycflights13::flights

ggplot(data = flights, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()

ggplot(data = diamonds2) + 
  geom_boxplot( mapping = aes
                ( x = reorder(cut, price, FUN = median), 
                  y = price 
                )
  )

install.packages(lvplot)

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_violin()

















