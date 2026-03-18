###Data 

library(boilrdata)

#matrices
m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3)
m

m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, byrow = TRUE)
m

nrow(m)

ncol(m)

dim(m)

sum(m)

rownames(m) <- c('a', 'b', 'c')
colnames(m) <- c('C1', 'C2')
m

dimnames(m)

#example
vars <- diag(drosophilaWingG)
vars

GvarsOnly <- diag(vars)
GcovsOnly <- drosophilaWingG - GvarsOnly
GvarsOnly

#Arrays
a <- array(letters[1:24], dim = c(2, 3, 4))
a

aTransposed <- aperm(a, c(3, 2, 1))
dim(aTransposed)

#lists 
myList <- list("abc", 1:5, 1.618, TRUE)
myList

myList <- list(firstLetters = "abc", firstNumbers = 1:5, goldenRatio = 1.618, I_like_it = TRUE)
myList

str(reefFishPhylogeny)

#data frame 
phyla <- data.frame(phylum =  c("Mollusca", "Chordata", "Annelida"),
                    species = c(85000, 65000, 22000),
                    deuterostomes = c(FALSE, TRUE, FALSE))
phyla

phyla <- rbind(phyla, list("Echinodermata", 7000, TRUE))
phyla <- cbind(phyla, example = c("nautilus", "sloth", "leech", "sea lily"))
phyla

str(marsupials)

head(marsupials)
