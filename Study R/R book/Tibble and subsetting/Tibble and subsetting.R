library(tidyverse)

as_tibble(iris)

tibble( x = 1:5, y = 1, z=x^2+y )

tb <- tibble( `:)` = "smile", ` ` = "space", `2000` = "number" )

tibble( a = lubridate::now() + runif(1e3) * 86400, 
        b = lubridate::today() + runif(1e3) * 30, 
        c = 1:1e3, 
        d = runif(1e3), 
        e = sample(letters, 1e3, replace = TRUE) )

nycflights13::flights %>% print(n = 10, width = Inf)

### subsetting 
df <- tibble( x = runif(5), y = rnorm(5) )
df$x
df[["x"]]
df[[1]]

df %>% .$x
df %>% .[["x"]]


# excerise 
mtcars

is_tibble(mtcars)

df <- data.frame(abc = 1, xyz = "a") 
df$x 
df[, "xyz"] 
df[, c("abc", "xyz")]

tibble(abc = 1, xyz = "a")
tibble(df$x)

annoying <- tibble( `1` = 1:10, `2` = `1` * 2 + rnorm(length(`1`)) )

annoying$`1`

ggplot(annoying, aes(x = `1`, y = `2`)) +
  geom_point()





