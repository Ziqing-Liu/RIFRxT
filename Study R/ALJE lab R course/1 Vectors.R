install.packages("devtools")
devtools::install_github("JanEngelstaedter/boilrdata")

x <- 5

x

print(x)

ls()

rm("x")
ls()

#creating vectors 

v <- c(1, 6, 2)

v <- 1:5

seq(1, 5, by = 0.1)

rep(x, 7)

rep(v, 3)

rep(v, 3, each = 2)

rep(5:7, 3:1)

runif(10, min = 0, max = 3)

sample(c(1, 4, 7, 8, 10), 3)

sample(c(1, 4, 7, 8, 10), 10, replace = TRUE)

x <- 1:5
y <- 6:10
x + y

# functions on vectors 
y <- 10
log(y)

log(y, base = 10)

y <- 1:10
log(y, base = 10)

sum(y)
mean(y)
var(y)
median(y)
length(y)

# strings 

amphibians <- c("frog", "toad", "salamander")
nchar(amphibians)

colours <- c("yellow", "green", "black")
paste(colours, amphibians)

paste(amphibians, colours, sep = ": ")

paste(colours, amphibians, collapse = ", ")

# logical 
x <- 1:10
isFive <- (x == 5)
isFive

isLessThan7 <- (x < 7)
isLessThan7

b1 <- TRUE
b2 <- FALSE

b1 & b2 * the AND operator 

b1 | b2 * the OR operator 

!b1 * negates the logical value

b3 <- c(TRUE, FALSE, TRUE, FALSE)
b4 <- c(FALSE, FALSE, TRUE, TRUE)

b3 & b4

b3 | b4

!b3

# vectors must have the same element
b3 && b4
b3 || b4

any(b3)
all(b4)

# special values and null 
x <- 1:7
is.na(x) <- 4 # sets 4th element to NA
x
is.na(x)

c(NULL, 5)
NULL + NULL
NULL^2

# conversion and coercion 
numbers <- c("3.4", "7.2", "1.9")
is.double(numbers)
is.character(numbers)
typeof(numbers)

convertedNumbers <- as.double(numbers)
is.double(convertedNumbers)
is.character(convertedNumbers)
typeof(convertedNumbers)
convertedNumbers

numbers <- c("3.4", "7.2", "1.9", "I'm not a number")
convertedNumbers <- as.double(numbers)
convertedNumbers

#attributes
coordinates <- c(x=5, y=3, z=7)
coordinates

coordinates <- c(5, 3, 7)
names(coordinates) <- c("x", "y", "z")
coordinates

names(coordinates)

coordinates <- unname(coordinates)
coordinates

weirdMammalWeights <- c(platypus = 1484, giantArmadillo = 45.4, nakedMoleRat = 55)
attr(weirdMammalWeights, "units") <- c("g", "kg", "g")
attr(weirdMammalWeights, "info") <- "This vector contains the weights of some really weird mammals."
attr(weirdMammalWeights, "source") <- "Encyclopedia of Life, at www.eol.org"
weirdMammalWeights

attributes(weirdMammalWeights)

weirdMammalWeights[1]

mean(weirdMammalWeights)

#factors 

genotypes <- factor(c("aa", "aa", "Aa", "aa", "Aa", "aa", "Aa"))
genotypes

genotypes <- factor(c("aa", "aa", "Aa", "aa", "Aa", "aa", "Aa"), 
                    levels = c("aa", "Aa", "AA")
                    )









