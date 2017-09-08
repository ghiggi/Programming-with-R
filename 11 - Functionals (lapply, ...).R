#################
## Functionals ##
#################
# Function that take a function as an input and returns a vector as output

##########
# lapply #
##########
# Allows only an input !!!
# Take a function and applies it to :
# - each element of a list
# - to each column a of a dataframe 

lapply1 <- function(x, f, ...) {
  out <- vector("list", length(x)) # allocate memory for the list 
  for (i in seq_along(x)) {
    out[[i]] <- f(x[[i]], ...)
  }
  out
}

# 3 basic way to use lapply for a list or dataframe
xs <- runif(10)
names(xs) <- letters[1:10]
lapply(xs, function(x) sqrt(x))
lapply(seq_along(xs), function(i) sqrt(xs[[i]]))
lapply(names(xs), function(nm) sqrt(xs[[nm]]))

# 2 basic was to use lapply for a list of functions 
xs <- runif(10)
funs <- c(mean, median, sd, mad, IQR) # Create a list of function
lapply(funs, function(f) f(x, na.rm = TRUE))
lapply(funs, function(f,...) { f(...) } , x, na.rm = TRUE)


## Useful for Monte Carlo Simulation ...
params = seq(0,1,0.1)
x = rcauchy(100)
fun <- mean
lapply(params,function(param) fun(x, trim=param))

## Useful to fit statistical models with different predictors 
formulas <- list( mpg ~ disp,
                  mpg ~ I(1 / disp),
                  mpg ~ disp + wt,
                  mpg ~ I(1 / disp) + wt )
la1 <- lapply(formulas, lm, data = mtcars)
la2 <- lapply(formulas, function(x) lm(formula = x, data = mtcars))
 
## Useful for bootstrapping
B = 50 # number of bootstrap replicates 
boot_samples <- lapply(1:B, function(i) {  rows <- sample(1:nrow(mtcars), replace = TRUE)
                                           mtcars[rows, ] 
                                        })
lm_fits <- lapply(boot_samples, lm, formula = mpg ~ disp)

#########
## Map ##  Map(f, ...)
#########
# Allows multiple input !!!
# The input lists must have the same length !!!
# It is an lapply() that iterates over indices 
mtmeans <- lapply(mtcars, mean)
mtmeans[] <- Map(`/`, mtcars, mtmeans) 

mtcars[] <- lapply(mtcars, function(x) x / mean(x)) # this is the lapply() equivalent  

xs <- replicate(5, runif(100000), simplify = FALSE)        # observations
ws <- replicate(5, rpois(100000, 5) + 1, simplify = FALSE) # list of weights
unlist( Map(weighted.mean, x=xs, w=ws, na.rm=TRUE))
unlist( Map(function(x, w) { weighted.mean(x, w, na.rm = TRUE) } , x=xs, w=ws) )
unlist( lapply(seq_along(xs), function(i) { weighted.mean(x=xs[[i]], w=ws[[i]], na.rm=TRUE) }) )

############
## mapply ##
############
# Is equal to Map() when SIMPLIFY=FALSE
mapply(weighted.mean, x=xs, w=ws, MoreArgs=list(na.rm=TRUE), SIMPLIFY=FALSE)

#####################
## sapply & vapply ##
#####################
# - sapply simplify the output "guessing" to produce atomic vector 
# - vapply require addiontal arguments specifying the output type
# - sapply should not be used inside functions !!!

# - vapply checks that all values of FUN are compatible with the FUN.VALUE,
#     in that they must have the same length and type
# - vapply returns a vector or array of type matching the FUN.VALUE
#   If length(FUN.VALUE) == 1 a vector of the same length as X is returned, otherwise an array.

sapply2 <- function(x, f, ...) {
  res <- lapply2(x, f, ...)
  simplify2array(res)
}

vapply2 <- function(x, f, f.value, ...) {
  out <- matrix(rep(f.value, length(x)), nrow = length(f.value))
  for (i in seq_along(x)) {
    res <- f(x[[i]], ...)
    stopifnot(
      length(res) == length(f.value),
      typeof(res) == typeof(f.value)
    )
    out[ ,i] <- res
  }
  out
}

# Given an empty list or dataframe
sapply(list(), is.numeric)             # return list()
vapply(list(), is.numeric, logical(1)) # return logical(0)

# Given a dataframe
df <- mtcars
sapply(df, is.numeric)
vapply(df, is.numeric, logical(1))

sapply(df, class)
vapply(df, class, character(1))

sapply(df, sum)
vapply(df, sum, numeric(1))

vapply(iris[vapply(iris, is.numeric, logical(1))],
       FUN=sd, 
       FUN.VALUE=numeric(1))
vapply(iris[vapply(iris, is.numeric, logical(1))],
       FUN=sd, 
       FUN.VALUE=numeric(1))

# But sapply can also return a list if is not able to simplify it ... 
# --> I.E .. the number of class of an element can vary and output format consequently too
time_seq = Sys.time() + 1:10
class(time_seq)
df <- data.frame(x = 1:10, y = time_seq)
sapply(df, class)
vapply(df, class, character(1))

df <- data.frame(x = time_seq, y = time_seq)
sapply(df, class)
vapply(df, class, character(2))

## sapply and vapply can be much useful to extract data from lists !
trials <- replicate(100, t.test(rpois(10, 10), rpois(7, 10)), simplify = FALSE)
sapply(trials, function(x) x[["p.value"]])
sapply(trials, "[[", "p.value")


###############
## Replicate ##
###############
# - It's a wrapper of sapply() 
# - Return a list with replicate of the evaluated function/expression 
# - Useful for repeated evaluation of an expression 
# - Often used to generate random data (which usually involve sample() or r...()

## Often used to bootstraping or generating random data 
l <- replicate(20, runif(sample(1:10, 1)), simplify = FALSE)

########### 
## apply ##
###########
# - Variant of sapply that works with matrices and arrays
# - MARGIN = 1 : summarize over rows (collapsing rows)       (for each row...)
# - MARGIN = 2 : summarize over columns (collapsing columns) (for each column...)
 
a <- matrix(1:20, nrow = 5 , ncol=4)
apply(a, 1, mean)
apply(a, 2, mean)

# YOU CAN NEVER BE COMPLETELY SURE WHAT TYPE OF OUTPUT YOU WILL GET 
# --> apply is not idempotent 
a1 <- apply(a, 1, identity)
identical(a, a1)    ##  FALSE
identical(a, t(a1)) ##  TRUE
a2 <- apply(a, 2, identity)
identical(a, a2)    ## TRUE

###########
## sweep ##
###########
# Normalize data 
x <- matrix(rnorm(20, 0, 10), nrow = 4)
x1 <- sweep(x, 2, apply(x, 2, min), `-`)
x2 <- sweep(x1, 2, apply(x1, 2, max), `/`)
# Standardize data
x <- matrix(rnorm(20, 0, 10), nrow = 4)
x1 <- sweep(x, 2, apply(x, 2, mean), `-`)
x2 <- sweep(x1, 2, apply(x1, 2, sd), `/`)

########### 
## outer ##
###########
# - Takes multiple vector inputs
# - Creates a matrix or array output where the input function is run over every
#    combination of the inputs 
outer(1:3, 1:10, FUN="*")
outer(month.abb, 1999:2017, FUN = "paste")

############
## tapply ##
############
# - Use the split() function to create a list which groups elements are then passed to lapply
# - Apply a function to different vectors defined by factor variable vector(s)
# - with simplify=FALSE it returns a list ! 

# How split() works (assuming f a single factor variable)
split2 <- function(x, f, drop = FALSE, ...){
  # Check if f is a list of factors
  if(is.list(f)) f <- interaction(f)
  # If drop it set to TRUE, we drop the non occuring levels.
  # --> If f is a character, this has no effect 
  if(drop){f <- f[, drop = TRUE]}
  # Retrieve all unique elements/levels of f
  levs <- if (is.factor(f)) { unique(levels(f)) } else { as.character(unique(f)) }
  # We use these levels to subset x and supply names for the resulting output.
  if (is.data.frame(x))
     setNames(lapply(levs, function(lv) x[f == lv, , drop = FALSE]), levs)
  if (is.vector(x))
     setNames(lapply(levs, function(lv) x[f == lv, drop = FALSE]), levs)
}

tapply2 <- function(x, group, f, ..., simplify = TRUE) {
  pieces <- split(x, group)
  sapply(pieces, f, simplify = simplify)
}
tapply3 <- function(x, group, f, ..., simplify = TRUE) {
  pieces <- split2(x, group)  
  sapply(pieces, f, simplify = simplify)
}

pulse <- round(rnorm(22, 70, 10 / 3)) + rep(c(0, 5), c(10, 12))
group <- rep(c("A", "B"), c(10, 12))
group2 <- rep(c("BB","AA","CC"), c(5,7,10))  
f_list <- list(group,group2)

#Compute the occurence ...
tapply(pulse,  group, length)
tapply2(pulse, group, length)
tapply3(pulse, group, length)

tapply(pulse, group2, length) #group2 is transformed first two factor (alphabetical order!)

tapply(pulse,  f_list, length)
tapply2(pulse, f_list, length)
tapply3(pulse, f_list, length)

# Compute the group mean
tapply(pulse,  group, mean)
tapply2(pulse, group, mean)
tapply3(pulse, group, mean)

# Compute the cell mean
tapply(pulse,  f_list, mean)
tapply2(pulse, f_list, mean)
tapply3(pulse, f_list, mean)

## Compute obs for each cell 
tapply(iris$Sepal.Length, iris$Species, function(x) x)
tapply(warpbreaks$breaks, warpbreaks[,c(2,3)], function(x) x)
tapply(warpbreaks$breaks, warpbreaks[,c(2,3)], length)

########
## by ##
########
# An object-oriented wrapper for 'tapply' applied to data frames (iterate to the other columns)
by(iris[, 1:4], iris$Species, function(x) x)
by(iris[, 1:4], iris$Species, colMeans)
by(iris[, 1:4], iris$Species, summary)

############ 
## Reduce ##
############
# - Reduce a vector x to a single value RECURSIVELY calling a function 
#    two arguments at a time 
# - Useful for implementing recursive operations (i.e. merges and intersections)
# - Additional arguments :
#   - control whether the values are reduced from the left or from the right (right =FALSE)
#   -  an optional initial value (init)
#   - accumulate = FALSE --> whether the successive reduce combinations should be accumulated
Reduce2 <- function(f, x) {
  out <- x[[1]]
  for(i in seq(2, length(x))) {
    out <- f(out, x[[i]])
  }
  out
}

Reduce(`+`, 1:3) # -> ((1 + 2) + 3)
Reduce(sum, 1:3) # -> sum(sum(1, 2), 3)

# Cumulative sum
cumsum(1:3)
Reduce(sum, 1:3, accumulate=TRUE)

# Intersection or union of values 
x <- replicate(5, sample(1:10,10, replace = T), simplify = FALSE)
str(x)
Reduce(intersect, x) 
Reduce(union,x)

# If you given a length one vector to reduce, it simply return the input without
#  calling the function 
Reduce(intersect,NULL)
Reduce(intersect,NA)
Reduce(intersect,NA, init=0)

##################################################
## Predicate functionals for list and dataframe ##
##################################################
# A predicate is a function that returns a single TRUE or FALSE
# --> is.na is not a predicate function
is.na(NULL) # logical(0)

## Filter (require a predicate function !)
# - Select all the elements which match the predicate
# - Example: Select only list component (column of df) that are factors
df <- data.frame(x = 1:3, y = c("a", "b", "c"))
l <- list(x = 1:3, y = factor(c("a", "b", "c")), z = factor(c("d", "e", "f","f")))
str(Filter(is.factor, df))
str(Filter(is.factor, l))

# Function that applies a function only to every numeric column in a data frame.
vapply_num <- function(X, FUN, FUN.VALUE){
  vapply(Filter(is.numeric, X), FUN, FUN.VALUE)
}

## Find
# - Return the first/last list vector with matches the predicate 
str(Find(is.factor, df))
str(Find(is.factor, l))  # starting from top of the list (default)
str(Find(is.factor, l, right = TRUE)) # starting from bottom of the list

## Position
# - Return the position (idx) of the first/last list vector that matches the predicate
# - Position(f, x)  
Position(is.factor, df)
Position(is.factor, l) # starting from top of the list (default)
Position(is.factor, l, right=TRUE) # starting from bottom of the list

## AllPosition 
# - Returns indexes (position) of element that matches the predicate (condition is TRUE)
AllIndex <- function(f, x) {
  logi <- vapply(x, f, logical(1))
  idx <- 1:length(logi)
  idx <- idx[logi]
  return(idx)
}
AllIndex(is.factor, df)
AllIndex(is.factor, l)

# AllLogical 
# - Returns TRUE/FALSE if the condition is TRUE/FALSE
AllLogical  <- function(f, x) {
  vapply(x, f, logical(1))
}
AllLogical(is.factor, df)
AllLogical(is.factor, l)

## Any() : a function that take a list/data.frame and returns TRUE if the predicate
#            function returns TRUE for any of the inputs 
Any <- function(l, f){
  stopifnot(is.list(l)) # if is.data.frame it is also is.list
  for (i in seq_along(l)){
    if (pred(l[[i]])) return(TRUE)
  }
  
  return(FALSE)
}
Any <- function(l, f){
  stopifnot(is.list(l)) # if is.data.frame it is also is.list
  any(vapply(x, f, logical(1)))
}
# All() : a function that take a list/data.frame and returns TRUE if the predicate
#           function returns TRUE for all the inputs 
All <- function(l, pred){
  stopifnot(is.list(l))
  
  for (i in seq_along(l)){
    if (!pred(l[[i]])) return(FALSE)
  }
  
  return(TRUE)
}
All <- function(l, pred){
  stopifnot(is.list(l))
  all(vapply(x, f, logical(1)))
}







