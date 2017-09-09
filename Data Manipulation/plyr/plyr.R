library("plyr")
options(digits = 3)
options(prompt = "R> ")

## For parallel operations 
doSNOW::registerDoSNOW(makeCluster(2, type = "SOCK"))
parallel::makeCluster(2)
doParallel::registerDoParallel(cores = 2)
foreach::getDoParName()

# .paropts in **ply 
# - Important if your code relies on external data or packages
# - use the .export and .packages arguments to supply them so that all cluster 
#   nodes have the correct environment set up for computing.

## Progress bar 
# - Disabled when using parallel operations 
param <- data.frame(n=c(100,50000000,100000),mean=c(5,5,10), sd=c(1,1,2))
l <- mlply(.data = param, .fun = rnorm, .progress ="win") # or "tk"

# NOTE: plyr functions often do NOT preserve row.names !!!! 
# Add always this data as a new columns !

#########
# **ply #
#########
# d*ply(.data, .variables, .fun, ...)
# l*ply(.data, .fun, ..., )
# a*ply(.data, .margins, .fun, ...)
# m*ply(.data, .fun = NULL, ...)
# r*ply(.n, .expr)

#########
# *_ply #
#########
# - Discards the output. 
# - Useful for functions like plot() and write.table() 
# - Useful for function that are called only for their side effects, not their return value.
l_ply(llply(mtcars, round), table, .print = TRUE) # by default is FALSE
r_ply(10, plot(runif(50)))

############################
# as.data.frame.function() #
############################
# Create a new function that returns the existing function wrapped in a data.frame with a single column (named "value")
# Useful when calling *dply functions with a function that returns a vector, 
#   and you want the output in rows, rather than columns. 
# The value column is always created, even for empty inputs.

#############
# colwise() #
#############
# Converts a function that works on vectors, to one that operates column-wise on a data frame, returning a data frame.

# Count number of missing values
nmissing <- function(x) sum(is.na(x))
colwise(nmissing)(baseball)
# To operate only on specified columns, supply them as the second argument. 
ddply(baseball, .(year), colwise(nmissing, .(sb, cs, so)))
ddply(baseball, .(year), colwise(nmissing, c("sb", "cs", "so")))
ddply(baseball, .(year), colwise(nmissing, ~ sb + cs + so))

# You can specify a boolean function that determines whether or not a column should be included
ddply(baseball, .(year), colwise(nmissing, is.character))
ddply(baseball, .(year), colwise(nmissing, is.numeric))
ddply(baseball, .(year), colwise(nmissing, is.discrete))

# These last two cases are particularly common, so some shortcuts are
# provided:
ddply(baseball, .(year), numcolwise(nmissing))
ddply(baseball, .(year), catcolwise(nmissing))

# numcolwise() # apply the function only on numeric variables 
# catcolwise() # apply the function only on categorical variables 
numcolwise(mean)(baseball, na.rm = TRUE)
numcolwise(mean, na.rm = TRUE)(baseball)

###########
# splat() #
###########
# Converts a function that takes multiple arguments to one that take a single list  
# - Useful to provide an entire dataframe to the function (if the column name match function args)
hp_per_cyl <- function(hp, cyl, ...) hp / cyl
hp_per_cyl_d <- function(hp, cyl, ...) data.frame(hp_per_cyl = hp / cyl)
splat(hp_per_cyl)(mtcars[1,])
splat(hp_per_cyl)(mtcars)
splat(hp_per_cyl_d)(mtcars)
dlply(mtcars, .(round(wt)), splat(hp_per_cyl))
dlply(mtcars, .(round(wt)), splat(hp_per_cyl_d))
dlply(mtcars, .(round(wt)),function(df) hp_per_cyl(df$hp, df$cyl))

ddply(mtcars, .(round(wt)), splat(hp_per_cyl_d))
ddply(mtcars, .(round(wt)),function(df) mean(hp_per_cyl(df$hp, df$cyl)))
ddply(mtcars, .(round(wt)),function(df) colwise(mean)(hp_per_cyl_d(df$hp, df$cyl)))
 
##########
# each() #  function(x) c(fun1=fun1(x), fun2=fun2(x))
##########
# Combine multiple functions into a single function returning a named vector of outputs
# - Each function need to produce a single number as output !!!
# - Does not allow to supply additional parameters to the functions (must create your own with default args)
each(min, max)(1:10)
each("min", "max")(1:10)
each(c("min", "max"))(1:10)
each(c(min, max))(1:10)

################
# summarise()  #
################
# Returns the modified/new columns in a new data frame
summarise(baseball, duration = max(year) - min(year),
                    nteams = length(unique(team)))
# Very useful for creating group-wise summaries
ddply(baseball, "id", summarise, duration = max(year) - min(year),
                                 nteams = length(unique(team)))

#############
# mutate()  #
#############
# Add the modified/new columns to the original data frame
# Mutate is faster than transform
# Modify existing variable in the data.frame
mutate(airquality, Ozone = -Ozone)
# Add new variable to the data.frame
mutate(airquality, new = -Ozone, Temp = (Temp - 32) / 1.8)

# Executes the transformations iteratively so that later transformations 
#   can use the columns created by earlier transformations.
mutate(airquality, Temp = (Temp - 32) / 1.8, OzT = Ozone / Temp) # Use the Temp converted previously ! 
 
##############
# failwith() #
##############
# Modify a function so that it returns a default value when there is an error
failwith(NA, f) # Return an NA whenever f throws an error.

################
# Quoted class #	
################
# Expressions for later evaluation.
# Ensure that the values are extracted from the correct frame
# - `.` create "quoted" variables 
# -  as.quoted() convert characters, formulas and calls to "quoted" .variables
# - is.quoted() : check if the variable is "quoted" class 
?`.`
# Create "quoted" object
.(a, b, c)
as.quoted(~ a + b + c)
as.quoted(a ~ b + c)
as.quoted(c("a", "b", "c"))

.(first = a, second = b, third = c)
as.quoted(c(first="a", second="b", third="c"))

 
###############
## arrange() ##
###############
# Reordering a data.frame by its columns
# Do not preserve row.names !!!
mtcars[with(mtcars, order(cyl, disp)), ]
arrange(mtcars, cyl, disp)   
arrange(myCars, cyl, desc(disp))

############
## count() #
############
# Equivalent to as.data.frame(table(x)), but does not include combinations with zero counts.
# Count the occurence (frequency) of "vars"  
count(baseball[1:100,], vars = "id")
count(baseball[1:100,], c("id", "year"))
 
############################
## revalue() & mapvalues() #
############################
library(plyr)
# Replace specified values with new values
# - revalues() : works for factor or character vector
# - mapvalues(): works also for numeric vector 
x <- c("a", "b", "c")
revalue(x, c(a = "A", c = "C"))
mapvalues(x, from=c("a", "c"), to=c("A", "C"))
revalue(x, c("a" = "A", "c" = "C"))
mapvalues(x, from=c("a", "c"), to=c("A", "C"))

y <- factor(c("a", "b", "c", "a"))
revalue(y, c(a = "A", c = "C"))
mapvalues(y, from=c("a", "c"), to=c("A", "C"))
 
# mapvalues() on numeric vectors
z <- c(1, 4, 5, 9)
mapvalues(z, from = c(1, 5, 9), to = c(10, 50, 90))

##########
# Take() # take(x, along, indices, drop = FALSE)
##########
# Take a subset along an arbitrary dimension of matrix or arrays
# - along	: dimension to subset along
# - indices	: the indices to select
x <- array(seq_len(3 * 4 * 5), c(3, 4, 5))
take(x, 3, 1)
take(x, 2, 1)
take(x, 1, 1)
take(x, 3, 1, drop = TRUE)
take(x, 2, 1, drop = TRUE)
take(x, 1, 1, drop = TRUE)

#################
## vaggregate() #
#################
# - Somewhat similar to tapply
# - It only accepts a single grouping vector (use id if you have more) and uses vapply internally 
# - vaggregate is faster than tapply in most situations because it avoids making a copy of the data

# Some examples of use borrowed from ?tapply
n <- 17;
x = 1:n
fac <- factor(rep(1:3, length.out = n), levels = 1:5)
table(fac)
vaggregate(x, .group = fac, sum)
vaggregate(x, .group = fac, sum, .default = NA_integer_)     # default value used for missing groups
vaggregate(x, .group = fac, range)

vaggregate(x, .group = fac, range, .default = c(NA, NA) + 0) # default value used for missing groups
vaggregate(x, .group = fac, quantile)
tapply(x, fac, range)
tapply(x, fac, quantile)

# Unlike tapply, vaggregate does not support multi-group output:
# - use id() if multiple grouping variables 
tapply(warpbreaks$breaks, warpbreaks[,-1], sum)
vaggregate(warpbreaks$breaks, id(warpbreaks[,-1]), sum)

#################
## rbind.fill() #
#################
# - Combine list of data.frames by row, filling missing colmuns with NA
# - Adds in columns that are not present in all inputs
# - Accepts a list of data frames, and operates substantially faster than rbind 
rbind.fill(mtcars[c("mpg", "wt")], mtcars[c("wt", "cyl")])
 
########################
## rbind.fill.matrix() # 
########################
# - Bind matrices by row, and fill missing columns with NA.
# - Matrices are bound together using their column names or the column indices
# - If a matrix doesn't have colnames, the column number is used. 
# Details 
# - Row names are ignored ! 
A <- matrix (1:4, 2)
B <- matrix (6:11, 2)
A
B
rbind.fill.matrix (A, B)
rbind.fill.matrix (A, 99)

colnames(A) <- c(3, 1)
A
rbind.fill.matrix (A, B) # If the second matrix (B) has no colnames, column indices are used ! 
rbind.fill.matrix (B, A) # A column with name "1" is merged with the first column of a matrix without name !

colnames(B) <- c(1,2,3)
B
rbind.fill.matrix (A, B)



 
