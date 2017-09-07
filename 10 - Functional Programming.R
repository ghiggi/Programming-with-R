#########################
## Anonymous functions ##
#########################
# - Functions without a name 
# - Used when is not worth the effort to give it a name
# - A good rule of thumb is that an anonymous function should fit on one line
#   and shouldn't need to use {}
lapply(mtcars, function(x) length(unique(x)))
Filter(function(x) !is.numeric(x), mtcars)
integrate(function(x) sin(x) ^ 2, 0, pi)

# Anonymous function syntax
(function(x) 3) ()
(function(x) x + 3) (10)

##############
## Closures ##
##############
# - Functions written by functions
# - Allows to make functions based on a template 
# - Enclose the environment of the parent function and can access all its variable
# --> The parent environment of a closure is the execution environment of the function
#     that created it 
# --> The closure function capture the execution environment of the function that
#       generated itself (it is its enclosing environment)
# --> Closure maintains access to the environment in which it was created ! 

# Generate a family of power functions in which a parent function (power()) 
#  creates two child functions (square() and cube()).
power <- function(exponent) {
  function(x) {
    x ^ exponent
  }
}

square <- power(2)
cube <- power(3) 
typeof(power)
typeof(square)
typeof(cube)

square(2)
cube(2)

# When you print a closure, you don't see anything terribly useful
# --> The difference is the enclosing environment of the two functions  
square
cube
as.list(environment(square))
as.list(environment(cube))

ls(environment(cube))
ls.str(environment(cube))
env <- environment(cube)
env$exponent # get("exponent", env) # as.list(env)$exponent

# pryr::unenclose() replace variables defined in the enclosing environement with their values
library(pryr)
unenclose(square)
unenclose(cube)

# In R, almost every function is a closure formally. 
# Almost all functions remember the environment in which they were created:
# - the global environment, if it's a function that you've written,
# - a package environment, if it's a function that someone else has written. 
# The only exception are primitive functions, which call to C directly.

#######################
## List of functions ##
#######################
# Create a list of function
funs <- c(mean, median, sd, mad, IQR)

# - Used to summarize an object in multiple way  
x=1:10
lapply(funs, function(f) f(x, na.rm = TRUE))
lapply(funs, function(f,...) f(...), x, na.rm = TRUE)

summary <- function(x) {
  funs <- c(mean, median, sd, mad, IQR)          # create a list by default 
  names(funs) <- c("mean", "median", "sd","mad", "IQR") # funs <- sapply(nms, get)
  lapply(funs, function(f) f(x, na.rm = TRUE))  # lapply(funs, function(f,...) f(...), x, na.rm = TRUE)
}
summary(x)

# - Used to compare performance of different functions
x <- runif(1e5)
compute_mean <- list(
  base = function(x) mean(x),
  sum = function(x) sum(x) / length(x),
  manual = function(x) {
    total <- 0
    n <- length(x)
    for (i in seq_along(x)) {
      total <- total + x[i] / n
    }
    total
  }
)
lapply(compute_mean, function(f) system.time(f(x)))

lapply(compute_mean, function(f) f(x))
call_fun <- function(f, ...) f(...)
lapply(compute_mean, call_fun, x)

##################### 
## Function factory #
#####################
# Create HTML code my mapping each tag to an R function 
simple_tag <- function(tag) {
  force(tag)
  function(...) {
    paste0("<", tag, ">", paste0(...), "</", tag, ">")
  }
}
tags <- c("p", "b", "i")
setNames(tags, tags) 
html <- lapply(setNames(tags, tags), simple_tag)

ls.str(environment(html[[1]]))
ls.str(environment(html[[2]]))

html$p("This is ", html$b("bold"), " text.")
with(html, p("This is ", b("bold"), " text."))
 
#############################
## Mutable state functions ##
#############################
# To manage variables at different levels is necessary 
#  to use the double arrow assignment operator (<<-)
# A static parent environment and <<- make it possible to maintain state across function calls
# If mutable objects are necessary and the code is not very simple, 
#  it's usually better to use reference classes (RC)
new_counter <- function() {
  i <- 0
  function() {
    i <<- i + 1
    i
  }
}

counter_one <- new_counter()
counter_two <- new_counter()

counter_one()
counter_one()
counter_two()
ls.str(environment(counter_one))

counter3 <- new_counter()
environment(counter3)
environment(counter3) <- globalenv() # change the environment 
counter3() # error !
i <- 10 # "fixing" the error (...in some way ...) 
counter3()
counter3()
i




 
 