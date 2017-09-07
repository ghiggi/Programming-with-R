############### 
## Functions ##
###############

`function`
?`function`

is.function(mean)

formals(mean)
body(mean)
environment(mean)

## Finding a functions given its character name
match.fun("sum")

## List of all functions in a given package
pkg = "package:base"
ls(pkg) #all objects name
objs <- mget(ls(pkg), inherits = TRUE) # Retrieve the function of each object
funs <- Filter(is.function, objs)
names(funs)

## Number of arguments of each function in a list 
funs.nargs <- lapply(funs, function(x) {
  if (is.primitive(x)) return(NULL)
  else length(formals(x))
})
head(funs.nargs)

## Functions without arguments
funs.no_args <- lapply(funs, function(x) {
  if (is.primitive(x)) return(NULL)
  else {
    if (length(formals(x))==0) return(x)
    else return(NULL)
  }
})
funs.no_args <- funs.no_args[!sapply(funs.no_args, is.null)]  #Remove null element of a list 
names(funs.no_args)

## To find the name of the functions that called a given function
# --> insert the following code in that function 
message("My parent is ", sys.call(-1))
message("My grandparent is ", sys.call(-2))

########################
#  Primitive functions #
########################
# - Call C code directly with .Primitive()
# - Are only found in the base package
# - Don't have an associated environement (enclosing environement) 
# - Their formals(), body() and environment() are NULL
formals(sum)
body(sum)
environment(sum)

typeof(sum) #builtin

is.function(sum)
is.primitive(sum)

## List of all primitive functions in the base package
pkg = "package:base"
objs <- mget(ls(pkg), inherits = TRUE) # Retrieve the function of each object
funs <- Filter(is.function, objs)
names(Filter(is.primitive, objs))

######################
## Functions  basics #
######################
## Finding functions works as any other R object.
# - Look step by step in all enclosing environemnt
# - Look inside the current function enviroment, then where that function was defined, 
#  and so on, all the way up to the global environment, and then on to other loaded packages
# --> R looks for values when the function is run (called), not when it's created !!!
l <- function(x) x + 1
m <- function() {
  l <- function(x) x * 2
  l(10)
}
m()

# If you are using a name in a context where it's obvious that you want a function 
# R will ignore objects that are not functions while it is searching
n <- function(x) x / 2
o <- function() {
  n <- 10
  n(n)
}
o()

## Once the function is executed, the environment inside which it is is executed
# is eliminated 
j <- function() {
  if (!exists("a")) {
    a <- 1
  } else {
    a <- a + 1
  }
  a
}
j()
j()
######################
# Function arguments #
######################

# Formal arguments are a property of the function
# Actual (calling) arguments can vary each function call

## Arguments are matched 
# - first by exact name (perfect matching), 
# - then by prefix matching
# - finally by position 
# Named arguments should always come after unnamed arguments 

## Calling a function with a list of arguments
args <- list(x=1:10, na.rm=TRUE)
do.call(mean, args)

## Default arguments can even be defined in terms of variables created within the function.
# --> If b not specified, compute d and use it instead of b 
h <- function(a = 1, b = d) {
  d <- (a + 1) ^ 2
  c(a, b)
}
h()

# --> If x is not specified, the default values will be: x=2,y=1  !!!
f1 <- function(x = {y <- 1; 2}, y = 0) {
  x + y
}
f1()
## Determine if an argument was supplied or not with the missing() function.
i <- function(a, b) {
  c(missing(a), missing(b))
}
i()
i(a = 1)

## When want to define a non-trivial default value (maybe function of others arguments), 2 possibilities:
# 1. Set the non trivial default value argument = NULL and then redefine if(is.null(...)) ...
# 2. Does not specify a defalut value in the argument, and then use if(missing(...)) ...

## Default arguments are evaluated inside the function !
f <- function(x = ls()) {
  a <- 1
  x # ls() is executed here 
}

f()     # ls() evaluated inside function f environment 
f(ls()) # ls() evaluated in the global environment 

## ... argument
# -  useful if you want to collect arguments to call another function without specifying possible names
# -  any misspelled argument will not raise an error
# -  any argument must be fully named 

##############
## Laziness ##
##############
## Very useful in if statements !!!
# The second, third.... statements are evaluated only if the previous are TRUE !!!!
x <- NULL
x > 0
if (!is.null(x) && x > 0) 2    # x > 0 not evaluated 
if (is.null(x) && x > 0)  2    # return an error if x=NULL because logical(0) is not a valid input for if()
  
#####################
## Infix operators ##
#####################
## Built-in infix operators that don't need % is: 
#  :, ::, :::, $, @, ^, *, /, +, -, >, >=, <, <=, ==, !=, !, &, &&, |, ||, ~, <-, <<-)
## Built-in infix operators that need % is:  %%, %*%, %/%, %in%, %o%, %x%. 
## Infix operators are composed (evaluated) from left to right

## Create a new operator (that paste together strings)
# --> when creating the function, you have to put the name in backticks because it's a special name ! 
`%+%` <- function(a, b) paste0(a, b)
"new" %+% " string"
`%+%`("new", " string")

## An useful infix operator to provide a default value in case the output of another function is NULL !!!
`%||%` <- function(a, b) if (!is.null(a)) a else b
function_that_might_return_null() %||% default_value 

###########################
## Replacement functions ##
###########################
## Modify their arguments   
# If they are primitive functions, it modifies in place
# If they are not primitive functions, they actually create a modified copy 
# --> Have the special name xxx<-

# The following are 2 different functions
names      # this is a common function 
`names<-`  # this is a replacement function 

x <- c(a = 1, b = 2, c = 3)
names(x)
names(x)[2] <- "two" 
names(x)             

x <- c(a = 1, b = 2, c = 3)
tmp <- names(x)
tmp[2] <- "two"
names(x) <- tmp
names(x)

## Create a replacement function for a vector
`second<-` <- function(x, value) {
  x[2] <- value
  x
}
x <- 1:10
second(x) <- 5L # x <- `second<-`(x, 5L)
x
 
## Create a replacement function for a vector
`modify<-` <- function(x, position, value) {
  x[position] <- value
  x
}
modify(x, 1) <- 10 #x <- `modify<-`(x, 1, 10) 
x

############
## On.exit #
############
# Used as a way to guarantee that changes to the global state are restored when the function exits.
# The code in on.exit() is run regardless of how the function exits

in_dir <- function(dir, code) {
  old <- setwd(dir)
  on.exit(setwd(old))
  code #execute the function code
}
getwd()
in_dir("~", getwd())
in_dir("~", a <- 2)

###################
# Debugging tools #
###################
## List al external dependencies of a function 
f <- function() x + 1
codetools::findGlobals(f)

## List environment occurence of R objects with same name
`[<-`  <- function(){}
find("[<-") # in two places ..
get("[<-",1) # get("[<-",".GlobalEnv")
get("[<-","package:base")

x <- 1:7
# The following doesn't clearly work
try(
  x[1] <- 10
) 
# The following will always work because take the correct function !!!
base::`[<-`(x,1,10)
x

# Remove the bad function from the current session 
rm("[<-") # 
rm("[<-",pos=-1) # -1 ; current environment  (.GlobalEnv)
# PS: It is not allowed to remove variables from the base environment 
#      and base namespace, nor from any environment which is locked 
# --> rm(FUNCTION TO REMOVE, pos=package:NAMEOFTHEPACKAGE)


#############################################
# Formals = argument and body of a function #
#############################################
f <- abline # Choose a function ...lm, plot, ...

## Extract the argument default list
str(args(f))
str(formals(f))

names(formals(f)) # formalArgs(f) does the same job

arguments <- formals(f)
class(formals(f)) #pairlist

if (is.primitive(`+`))  formals(args(`+`)) # otherwise only formals() return NULL

formals(formals)
deff <- formals(formals)[["fun"]]
deff <- formals(formals)$fun 
deff 
class(deff) # in that specific case is a call object ... 

## Extract the body of the function
body(f)
body_fun <- body(f)
class(f)  #{"
typeof(f) #language object

###################################
## Modify the body of a function ##
###################################
f <- function(x) x^5
body(f) <- quote(4^x)
f
f(3)  

e <- expression(y <- x^2, return(y))  
body_modified <- c(as.name("{"), e) # as.name() to generate `{`
body_modified <- as.call(body_modified)
body(f) <- body_modified
f
f(8)

## Overwrite formal arguments of a function (dangerous coding)(i.e: useful to modify default values...)
f <- function(x, a=1, b=2)  a + b + x
f(2)
formals(f) <- alist(x=, a = 2, b = 3) # alist does not evalue the argument (i.e. x) !!!! 
f(2)  

###############
## Missing() ##
###############
## Allows to check if an argument has been specified in the actual call 

f <- function(x, ab=TRUE) {
  cat(" missing(x):", missing(x),"\n",
      "missing(ab):", missing(ab),"\n")
  cat("str( formals() ): "); str(formals())
  cat("Number of args:", nargs(),"\n")
  match.call() # the function *call* (an object of class & type  "call")
}
f(1)
f(1, TRUE)

## Formal and actual arguments
# - When the function is called, formal argument takes the value of the actual argument
y = 1   # actual argument: y
f(x=y)  # formal argument: x 

 





###############################################################################c
add <- function(x) {
  force(x)
  function(y) x + y
}
adders <- lapply(1:10, add)
adders[[1]](10)
adders[[10]](10)
ls(environment(adders[[1]]))
ls(environment(adders[[10]]))
get("x", environment(adders[[1]]))
get("x", environment(adders[[10]]))


g <- function(x) {
  y <- 2
  UseMethod("g")
}
g.numeric <- function(x) y
y <- 1
g(10)
g.numeric(10)

h <- function(x) {
  x <- 10
  UseMethod("h")
}
x <- 3
h.character <- function(x) paste("char", x)
h.numeric <- function(x) paste("num", x)
h(2)
h("a")
