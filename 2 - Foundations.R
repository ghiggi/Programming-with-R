#
x <- 1:10
attr(x, "info") <- "my attribute"
is.vector(x) #Returns TRUE only if the object is a vector with no attributes apart from names.
is.atomic(x) || is.list(x) # To test if an object is actually a vector.

NA_real_
NA_integer_
NA_character_

################
## Attributes ##
################
# Allow to associate additional metadata to any object 
y <- 1:10
attr(y, "my_attribute") <- "This is a vector"
attr(y, "my_attribute")
y <- structure(1:10, my_attribute ="This is a vector")
attr(y, "my_attribute")
attributes(y)
y

# Most attributes are lost when modifying a vector 
yy <- y[1:2]
attributes(yy) 

# Some attributes with a given tag are not displayed
structure(1:5, comment  = "my attribute")
structure(1:5, commen  = "my attribute")

## Read data 
# Most data loading functions in R automatically convert character vectors to factors.
z <- read.csv(text = "value\n12\n1\n.\n9", na.strings=".")
z 
z <- read.csv(text = "value\nba\n1\n.\na", na.strings=".") #all is converted to factors
z
class(z[[1]])
#Use the argument stringsAsFactors = FALSE to suppress this behaviour
z <- read.csv(text = "value\nba\n1\n.\na", na.strings=".", stringsAsFactors = FALSE)  
z
class(z[[1]])

########## 
## List ##
##########
# Remove component from a list 
x <- list(a = 1, b = 2)
x[["b"]] <- NULL
str(x)

# To add a literal "NULL" to a list 
y <- list(a = 1)
y["b"] <- list(NULL)
str(y)

###########
## Array ##
###########
# Arrays in R are stored in column-major order

#Use a 2 column matrix to subset a matrix
vals <- outer(1:5, 1:5, FUN = "paste", sep = ",")
vals
select <- matrix(ncol = 2, byrow = TRUE, c(1, 1, 
                                           3, 1, 
                                           2, 4))
select
vals[select]

#Use a 3 column matrix to subset a 3D array 
vals3 <- outer(vals, 1:3, FUN = "paste", sep = ",")
vals3
select <- matrix(ncol = 3, byrow = TRUE, c(1, 1, 1,
                                           3, 1, 2,
                                           3, 2, 3))
vals3[select]

################################
## List-matrix and list-array ##
################################
# Generate a list-matrix
l <- list(1:3, "a", TRUE, 1.0)
dim(l) <- c(2, 2)
l
# Generate a list-array
dim(l) <- c(2, 1, 2)
l

################ 
## Data frame ##
################
# - Is a list of columns with equal number of rows
# -  Is a list of equal-length vectors

## data.frame() turns strings into factors by default. Use stringsAsFactors = FALSE to suppress this behaviour 
df <- data.frame( x = 1:3,
                  y = c("a", "b", "c"),
                  stringsAsFactors = FALSE)
str(df)
names(df)
colnames(df)
typeof(df)
class(df)

## To remove columns from a dataframe
df[setdiff(names(df),"y")]
df$y <- NULL; df
str(df[-2])

# When combining column-wise, the number of rows must match, but row names are ignored.
# When combining row-wise, both the number and names of columns must match.
# Use plyr::rbind.fill() to combine data frames that don't have the same columns.

### Special columns
##  Columns of a dataframe that are list 
df <- data.frame(x = 1:3)
df$y <- list(1:2, 1:3, 1:4)
df
# Using I(), also the following works 
data.frame(x = 1:3, y = I(list(1:2, 1:3, 1:4)))
# The follow does not work because it tries to put each item of the list into its own column
data.frame(x = 1:3, y = list(1:2, 1:3, 1:4)) 

## Colums of a dataframe that are matrix
# --> the number of rows must matches the ones of the data frame
str(data.frame(x = 1:3, y = I(matrix(1:9, nrow = 3))))

 
#########################################
# Simplifying and preserving subsetting #
######################################### 
# - Simplifying subsets returns the simplest possible data structure 
# - Preserving subsetting keeps the structure of the output the same as the input,
#  and is generally better for programming because the result will always be the same type. 
# --> Omitting drop = FALSE when subsetting matrices and data frames is one of 
#     the most common sources of programming errors.

df <- data.frame(x = 1:3)
str(df[["x"]]) # matrix subsetting (simplify by default)
str(df[,"x"])  # matrix subsetting (simplify by default)
str(df["x"])   # list subsetting (returns a dataframe by default)
str(df[,"x", drop=FALSE]) # matrix subsetting Without simplification ! 

mat <- outer(1:5, 1:5, FUN = "paste", sep = ",")
str(mat[,1])
str(mat[,1, drop=FALSE]) # matrix subsetting without simplification !

###########################
# Subsetting with nothing #
###########################
# Subsetting with nothing can be useful in conjunction with assignment 
#  because it will preserve the original object class and structure
mtcars[] <- lapply(mtcars, as.integer)  # is a dataframe 
mtcars <- lapply(mtcars, as.integer)    # is a list

# Replace every element with 0 
x <- matrix(1:10, ncol=2)
x[] <- 0  
x 

#######################
# Logical subsetting  #
#######################
# Use the vector boolean operators & and |  
# X & Y <-> intersect(x, y) # FALSE > TRUE
# X | Y <-> union(x, y)     # TRUE  > FALSE
# X & !Y <-> setdiff(x, y)
# xor(X, Y) <-> setdiff(union(x, y), intersect(x, y))
## De Morgan's laws 
# -->  !(... & ...)  --> ! ... |  !...
# -->  !(... ! ...)  --> ! ... &  !...
# !(X | Y)  <--> !( union(x, y))  <--> !X & !Y
# !(X & Y) <-->  !(intersect(x, y))  !X | !Y
# !(X & !(Y | Z)) <-->  !X | !!(Y|Z)  <-->  !X | Y | Z

##  x[-which(y)] is not equivalent to x[!y]
# If y is all FALSE, which(y) will be integer(0) and -integer(0) is still integer(0)
# --> You'll get no values, instead of all values. 

n <- 10
set.seed(1)
x <- sample(n) < 4
# Convert a boolean representation to an integer representation.
which(x) # index of TRUE in x 
# Convert integer representation to boolean representation  
unwhich <- function(x, n) {
  out <- rep_len(FALSE, n)
  out[x] <- TRUE
  out
}
unwhich(which(x), n)  


#############################
# Out of bound (OOB) values #
#############################
# With atomic vectors
x <- 1:4
str(x[5])
str(x[NA_real_])
str(x[NULL])
str(x[[5]])
str(x[[NA_real_]])
str(x[[NULL]])

# With list and dataframes
ll <- list(x=1:4,y=5:8)
str(ll[3])
str(ll[NA_real_])
str(ll[NULL])
str(ll[[3]])
str(ll[[NA_real_]])
str(ll[[NULL]])

# You can't combine integer indices with NA
x[c(1, NA)] <- c(1, 2) # ERROR

# You can combine logical indices with NA (where they're treated as false).
x[c(T, F, NA, T)] <- 1
x

########################################
## Character matching and lookup table #               
########################################
## Lookup tables (1 character subsetting)
x <- c("m", "f", "u", "f", "f", "m", "m")
lookup <- c(m = "Male", f = "Female", u = NA)
lookup[x]
unname(lookup[x])

## Lookup tables (multiple columns of information)
x <- c(1, 2, 2, 3, 1)
info <- data.frame( grade = 3:1,
                    desc = c("Excellent", "Good", "Poor"),
                    fail = c(F, F, T))
# Using match()
id <- match(x, info$grade)
info[id, ]                      

# Using rownames()
rownames(info) <- info$grade
info[as.character(x), ]

#If you have multiple columns to match on, you'll need to first collapse
# them to a single column (with interaction(), paste(), or plyr::id()). 
# You can also use merge() or plyr::join()

####################################################
# Expanding aggregated counts (integer subsetting) #
####################################################
# Identical rows have been collapsed into one and a count column has been added
# Uncollapse the data by subsetting with a repeated row index
df <- data.frame(x = c(2, 4, 1), y = c(9, 11, 6), n = c(3, 5, 1))
df
rep(1:nrow(df), df$n)
df[rep(1:nrow(df), df$n), ]












