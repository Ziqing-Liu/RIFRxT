### template for most plots 
ggplot(data = <DATA>) +
  <GEOM_FUNCTION>(
    mapping = aes(<MAPPINGS>),
    stat = <STAT>,
    position = <POSITION>
  ) +
  <COORDINATE_FUNCTION> +
  <FACET_FUNCTION>
  
  
###c
In R, adding c() in front of values means “combine these into a vector”.
The c stands for combine (or concatenate).

Basic example
x <- c(1, 2, 3)


Now x is a vector with three elements.
    
  
###aes
In R, aes() (from ggplot2) stands for aesthetic mappings.

It tells ggplot which variables in your data map to visual properties of the plot.

Think:
  
  “Which data goes to which part of the plot?”

Basic structure
ggplot(data = df, aes(x = x_var, y = y_var))


Here you’re saying:
  
  x_var → x-axis

y_var → y-axis

  
###!
In R, ! is the logical NOT operator.
It flips TRUE to FALSE and FALSE to TRUE.

Think: “not”.

Basic example
!TRUE
# FALSE

!FALSE
# TRUE
  
### In R, na.rm stands for “remove NA values”. telling a function to ignore missing values (NA) when doing a calculation.

x <- c(1, 2, NA, 4)

mean(x, na.rm = TRUE)

mean(x)


### pipe (and then...) make data much cleaner 

summarise(
  group_by(
    filter(df, species == "setosa"),
    island
  ),
  mean_bill = mean(bill_length_mm, na.rm = TRUE)
)

df %>%
  filter(species == "setosa") %>%
  group_by(island) %>%
  summarise(mean_bill = mean(bill_length_mm, na.rm = TRUE))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  