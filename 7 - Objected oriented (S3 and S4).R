

# A class defines the behaviour of objects by describing their attributes and their relationship to other classes.
# Methods are functions  that behave differently depending on the class of their input
#  

# To determine if a function is an S3 generic, you can inspect its source code for a call to UseMethod()
# Functions that do method dispatch in C code are called internal generics (are all primitives???)
?"internal generic"

#Information about the object system
pryr::ftype(mean)
pryr::ftype(sum)

#############
# Base type #
#############
# Base types:
#   - atomic vectors, list, ...
#   - are not really an object system because only the R core team can create new types
#   - encompass functions, environments, and other more exotic objects likes names, calls, and promises

is.object(sum) # return FALSE ! 


########
## S3 ##
########
# - Methods belong to functions (not classes !)
# - S3 objects are usually built on top of lists, or atomic vectors with attribute
# --> A dataframe is a S3 class
# --> A factor  is a S3 class

# Check if it is an S3 object 
is.S3 <- function(x) { (is.object(x) & !isS4(x)) }
pryr::otype
pryr::otype(mean)
is.S3(mean)

.S3PrimitiveGenerics ## base
.knownS3Generics     ## base
utils:::getKnownS3generics()
tools:::.get_S3_primitive_generics()
tools:::.get_internal_S3_generics()  # contains primitives, too

# S3 methods name must be : generic.class()

## List of all methods that belong to a generic function 
methods("mean")      # methods(generic.function="mean") 
.S3methods("mean")   # display only the visible methods

xmet <- methods("[")
xmet <- .S3methods("[")
str(xmet)
attr(xmet,"info") # attributes(xmet)$info

## List all generic functions for a given class
methods(class="ts")

ml <- methods(class = "lm")
str(ml)
class(ml)
attributes(ml)
attr(ml,which="info")
attr(ml,which="byclass")

il <- attr(ml,"info") 
str(il)
il[,"generic"] # functions that can be called
rownames(il)   # real name of the functions !
str(n.l <- il[,"generic"])

## Retrieve class name which have a specific method for a given generic 
xmet <- methods("[")
xmet <- .S3methods("[")
nmet <- as.vector(xmet)
(clss <- sub("\\[\\.", "", nmet))

## Retrieve all functions (methods) related to a given generic 
allS3methods <- function(f) {
  if (!is.character(f)) f <- deparse(substitute(f))
  stopifnot(length(f) == 1)
  ## methods():  some visible, some not
  nmet <- as.vector(.S3methods(f))  ## as.vector: drop all attributes, incl. class
  ## remove function name and "." from beginning : e.g. for  "[."
  cls <- sub(paste0("^", gsub("\\[","\\\\[", f), "\\."),
             "", nmet)
  setNames(lapply(cls, function(cl) getS3method(f, cl)),
           nmet)
}
extract.met <- allS3methods("[")
repl.met <- allS3methods("[<-")

## Retrieve various information of all method related to a generic function 
xmet <- methods("[")
xmet <- .S3methods("[")
nmet <- as.vector(xmet)
metList <- lapply(nmet, function(nm) getAnywhere(nm))
argList <- lapply(nmet, function(nm) argsAnywhere(nm))
class(metList[[1]])
str(metList[1])
metList[1]
metList[[1]]$objs[[1]] # to retrieve the function
str(argList[1])

## Set S3 class
# Create an object, then set class
foo <- list()
class(foo) <- "foo"
# Create and assign class in one step
foo <- structure(list(), class = "foo")
# Check if an object inherits from a specific class 
inherits(foo,"foo")

## Constructor function 
# - Ensure that a class with the correct components is created 
# - Usually constructor functions have the same name as the class
foo <- function(x) {
  if (!is.numeric(x)) stop("X must be numeric")
  structure(list(x), class = "foo")
}

## Construct a new type of R object (bookvec class)
# --> Every time an element is modified, this is count and recorded in wrts 
# Construct a new object of class bookvec 
newbookvec <- function(x) {
  tmp <- list(vec = x,  # the vector itself
              wrts = rep(0,length(x)))  # counts of the writes, one for each element
  class(tmp) <- "bookvec"
  tmp
}

# Function to subset the vector  
`[.bookvec`  <- function(bv,subs) {
  return(bv$vec[subs])
}

# Function to modify a vector element 
`[<-.bookvec` <- function(bv,subs,value) {
  bv$wrts[subs] <- bv$wrts[subs] + 1   
  bv$vec[subs] <- value
  return(bv)
}
# Function to print the bookvec object
print.bookvec <- function(x, ...) {
  cat("vec:\n"); print(x$vec)
  cat("counts:\n"); print(x$wrts)
  invisible(x)
}

bv1 <- newbookvec(5:2)
bv1[2]       # equal:  `[.bookvec`(bv1,2) 
bv1[2] <- 4  # equal:  `[<-.bookvec`(bv1,2,4)
bv1
bv1[] <- bv1[]+ 3
bv1

### Create new methods and generics
# Usemethod() takes :
# - the name of a generic function
# - the argument to use for method dispatch

# Create a generic function  --> Use UseMethod()
f <- function(x) UseMethod("f")

# Create a regular function with the correct (generic.class) name:
f.a <- function(x) "Class a"
a <- structure(list(), class = "a")
class(a)
f(a)

# An object can have multiple classes
b <- structure(list(), class = c("a","b"))

## When a function calling UseMethod("fun") is applied to an object with class 
#   attribute c("first", "second"), the system searches for a function called fun.first 
#   and, if it finds it, applies it to the object.
#   If no such function is found a function called fun.second is tried. 
#   If no class name produces a suitable function, the function fun.default is used.
#   If fun.default does not exists an error results.
f(b) # no method for b ...but use method for a ! 
f(structure(list(), class = "b")) # Return an error

# Is good practice to set up a method for "default" class that is used when  the class is unknown 
f.default <- function(x) "Unknown class"
f(structure(list(), class = "b")) # Now it does not return an error

# Add a method to an existing generic function 
mean.a <- function(x) "a"
mean(a)

######### 
##  S4 ##
#########
# - Methods belong to functions (not classes !)
# - S4 has formal class definitions Which describe the representation (fields)(slots) and inheritance structures
# - Has special helper functions for defining generics and methods.
# - Has multiple dispatch to a generic function, which means that generic functions can pick methods
#    based on the class of any number of arguments, not just one.
# - @ allow to extract slots (fields) from an S4 object 
library(methods) # contains S4 related code
# Load an S4 class 
library(stats4)  # provide some  S4 classes for the examples
y <- c(26, 17, 13, 12, 20, 5, 9, 8, 5, 4, 8)
nLL <- function(lambda) - sum(dpois(y, lambda, log = TRUE))
fit <- mle(nLL, start = list(lambda = 5), nobs = length(y))
mle_nobs <- nobs(fit) 

# Check if it is an S3 object 
isS4(fit)
isS4(fit)
isS4(mle_nobs)
pryr::otype(fit)
pryr::otype(nobs)
pryr::otype(mle_nobs)
pryr::ftype(nobs)

# Get a list of all S4 generics 
getGenerics()
# Get a list of all S4 classes
getClasses()
# Get a list of all S4 methods
showMethods()

 
## Set S4 class
# - name : UpperCamelCase
# - named list of slots (fields) and permitted class
# - a string giving the class it inherits from ("it contains")
# -->  If you modify a class, make sure you also recreate any objects of that class, 
#      otherwise you'll end up with invalid objects !!! 
?setClass
setClass("Person",
         slots = list(name = "character", age = "numeric"))
setClass("Employee",
         slots = list(boss = "Person"),
         contains = "Person")

## Create S4 objects 
alice <- new("Person", name = "Alice", age = 40)
john <- new("Employee", name = "John", age = 20, boss = alice)

alice@age
slot(john, "boss")

# If an S4 object contains (inherits from) an S3 class or a base type, 
# it will have a special .Data slot which contains the underlying base type or S3 object 
setClass("RangedNumeric",
         slots = list(min = "numeric", max = "numeric"),
         contains = "numeric")
rn <- new("RangedNumeric", min = 1, max = 10, 1:10)
 
## Create new S4 method and S4 generic 
# --> If you create a generic function from scratch, you need to supply a function that calls standardGeneric():
setGeneric("myGeneric", function(x) { standardGeneric("myGeneric") })
setGeneric("union") # if the generic function "union" already exists 
setMethod("union", signature = c(x = "data.frame", y = "data.frame"),
          definition = function(x, y) {
                         unique(rbind(x, y))
                       })
   
##########################
# Reference classes (RC) #
##########################
# - Methods belong to objects, not functions !!! 
# - Implements message-passing OO
# - $ is used to separate objects and methods,
# - RC objects are mutable
# - RC objects don't use R's usual copy-on-modify semantics, but are modified in place !
# --> They are a special S4 class (refMethodDef) that wraps around an environment 
# --> Best used to describe object that change over time 

## Create a new RC class
# - Class: class name : UpperCamelCase
# - fields : named list of slots (fields) and permitted class
# --> You access the value of fields with their name, and modify them with <<-
?setRefClass
Account <- setRefClass("Account",
                       fields = list(balance = "numeric"), 
                       methods = list(withdraw = function(x) {
                                                  balance <<- balance - x
                                                 },
                                      deposit = function(x) {
                                                  balance <<- balance + x
                                                 }))
                                   
# Create a RC object
a <- Account$new(balance = 100)
a$balance

## RC objects are mutable (have reference semantics ...pointers)
# a and b are the same object .... 
b <- a
b$balance

a$balance <- 0
b$balance

# RC objects can be copied (so that does not point to the same memory space....)
c <- a$copy()
c$balance
a$balance <- 200
a$balance
b$balance
c$balance

# Use RC methods
a <- Account$new(balance = 100)
a$deposit(100)
a$balance

# Define a parent RC class 
NoOverdraft <- setRefClass("NoOverdraft",
                           contains = "Account",
                           methods = list(withdraw = function(x) {
                                                      if (balance < x) stop("Not enough money")
                                                          balance <<- balance - x
                                                     }))
accountJohn <- NoOverdraft$new(balance = 100)
accountJohn$deposit(50)
accountJohn$balance
accountJohn$withdraw(200)
