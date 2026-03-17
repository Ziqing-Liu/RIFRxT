library(tidyverse)

library(nycflights13)

view(flights)

### selected for columns we want
flights_sml <- select(flights,
                      year:day,
                      ends_with("delay"),
                      distance,
                      air_time
)

### added gain and speed column 
mutate(flights_sml,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60
)

### reffering the columns i created 
mutate(flights_sml,
       gain = arr_delay - dep_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours
)

### using transmute to keep the new variables 
transmute(flights,
          gain = arr_delay - dep_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours
)

### (+, -, *, /, ^) R’s basic arithmetic operators

### Arithmetic works element-wise on vectors:
c(1, 2, 3) + c(4, 5, 6)

c(1, 2, 3) * 2

### If vectors are different lengths, R recycles the shorter on

c(1, 2, 3, 4) + c(10, 20)


### (%%) (%/%)
%/% → integer division (quotient)

%% → remainder (modulo)

### uses 

### odd even check
x <- 10
x %% 2 == 0

###grouping 
1:10 %% 3

### time conversion 

seconds <- 367
minutes <- seconds %/% 60
remaining_seconds <- seconds %% 60

transmute(flights,
          dep_time,
          hour = dep_time %/% 100,
          minute = dep_time %% 100
)

### logs (log(), log2(), log10())
### Logarithms are an incredibly useful transformation for dealing with data that ranges across multiple orders of magnitude. They also convert multiplicative relationships to additive

### Offsets (lead(), lag()) 
### lag() looks backward, lead() looks forward, and they’re essential for computing changes in ordered data.

x <- 1:10

lag(x)

lead(x)

### Cumulative and rolling aggregates functions for running sums, products, mins, and maxes
### (cumsum(), cumprod(), cummin(), cummax(), cummean)

cumsum(x)

cumprod(x)

cummin(x)

cummax(x)

cummean(x)

###Logical comparisons <, <=, >, >=, !=

(1<1

1<2

1<=2

1>2

1>=2)

  
  
### Ranking

y <- c(1, 2, 2, NA, 3, 4)

min_rank(y)

min_rank(desc(y))

row_number(y)

dense_rank(y)

percent_rank(y)

cume_dist(y)



### excerise 

time <- select(flights, dep_time, sched_dep_time)

newtime <- mutate (time,
          dep_hour = dep_time %/% 100,
          dep_minute = dep_time %% 100,
          .after = dep_time
          
)

newnewtime <- mutate (newtime,
                      sched_hour = sched_dep_time %/% 100,
                      sched_minute = sched_dep_time %% 100,
                      .after = sched_dep_time
)


compare1 <- select(flights,
                   air_time,
                   arr_time, 
                   dep_time
)

mutate(compare1,
       airtime = arr_time - dep_time,
)


compare2 <- select(flights,
                   dep_time,
                   sched_dep_time, 
                   dep_delay,
)

mutate(compare2,
       realdeptime = sched_dep_time + dep_delay,
)

compare3 <- select(flights,
                   dep_delay)

delay <- arrange(compare3, desc(dep_delay)

test <- delay |> mutate(rank = min_rank(dep_delay))


### warning becuase these two vector length are not compatiable 
1:3 + 1:10 

###R gives you a full standard set of trigonometric functions R uses radians, not degrees

Quick summary table 
Category	Functions
Basic	sin, cos, tan
Inverse	asin, acos, atan, atan2
Hyperbolic	sinh, cosh, tanh
Inverse hyperbolic	asinh, acosh, atanh

