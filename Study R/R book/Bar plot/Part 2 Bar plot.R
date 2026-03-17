### Excerise 2
library(tidyverse)

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

demo <- tribble(
  ~a,      
  ~b,
  "bar_1", 20,
  "bar_2", 30,
  "bar_3", 40
)

### displayed by count
ggplot(data = demo) +
  geom_bar(
    mapping = aes(x = a, y = b), stat = "identity"
  )

### displayed by proportion 
ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, y = ..prop.., group = 1)
  )

ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, y = depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, color = cut))

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = cut))

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity))

#dodge
ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, fill = clarity),
    position = "dodge"
  )

###jitter 
ggplot(data = mpg) +
  geom_point(
    mapping = aes(x = displ, y = hwy),
    position = "jitter"
  )

###excerise 
ggplot(data = mpg) +
  geom_jitter(aes(x = cty, y = hwy), width = 0.2, height = 0)

ggplot(data = mpg) +
  geom_bar(
    mapping = aes(x = cty, fill = hwy),
    position = "dodge"
  )

### coordinate system 

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot() +
  coord_flip()

nz <- map_data("nz")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", color = "black")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", color = "black") +
  coord_quickmap()

bar <- ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, fill = cut),
    show.legend = FALSE,
    width = 1
  ) +
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()

###excerise 
example <- ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity))

hope <- example + coord_polar()

hope

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", color = "black") +
  coord_quickmap()

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", color = "black") +
  coord_map()

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() +
  geom_abline() +
  coord_fixed()

### relationship is the cars that are efficient in the city is also efficient on the highway 
### geomabline is imporant because This line represents equal city and highway MPG.
So relative to the line:  Above the line → highway MPG > city MPG  On the line → equal MPG (rare) 
Below the line → highway MPG < city MPG (very rare)  In this plot, most points fall above the line, 
confirming that highway MPG is typically higher than city MPG.
###coord fixed is important is becuase it forces 1 unit on x to be equal to y whihc is imporant to staying consistant
