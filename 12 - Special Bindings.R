library(pryr)
######################
## Deep binding <<- ##
######################
# - Never creates a variable in the current environment
# - Modifies an existing variable found in the chain of parent environments

x <- 0
f <- function() {
  x <<- 1  # assign("x",1,inherits = TRUE)
}
f()
x

# If <<- does not find the variable, it will create it in the global environment
rm(x)
f()


## Create mutable state   
i <- 0
new_counter <- function() {
  i <<- i + 1
  i
}
new_counter()
new_counter()

# Create mutable state using a function factory
library(pryr)
new_counter1 <- function() {
  i <- 0
  function() {
    i <<- i + 1
    i
  }
}

counter_one <- new_counter1()
as.list(environment(counter_one)) # enclosing environment variables
unenclose(counter_one)

counter_one()
unenclose(counter_one)
as.list(environment(counter_one)) # enclosing environment variables

counter_one()
unenclose(counter_one)
as.list(environment(counter_one)) # enclosing environment variables

 
#####################
## Delayed binding ##
#####################
# - Create and stores a promise to evaluate the expression when needed 
library(pryr)
?`%<d-%`
?delayedAssign

system.time( b %<d-% {Sys.sleep(1); 1})
system.time( b)

system.time( delayedAssign("b", {Sys.sleep(1); 1}) )
system.time( b)              

#####################
## Active bindings ##
#####################
# - The binding are recomputed each time they are accessed
# - May be useful for simulation purposes
?`%<a-%`
?makeActiveBinding 

x %<a-% runif(10)
x
a <- x

f <- function() runif(10) 
makeActiveBinding("x", f, env=globalenv())
x




