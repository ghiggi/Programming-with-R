#######################
## Expression object ##
#######################
# - Essentially a list of expressions 
# - Can be subsetted by [, [[ or $
(ex1 <- expression(1 + 0:9))
(qu1 <- quote(1 + 0:9))

what <- function(.) c(storage.mode=storage.mode(.), type = typeof(.), class = class(.), mode = mode(.) )
sapply(list(ex1 = ex1, qu1 = qu1, frm = y ~ x), what)

identical(ex1[[1]], qu1) 

## Create an expression object with expression() 
ex1 <- expression(1 + 0:9)        
ex3 <- expression(x+y, x^3, { r <- (x-y)^2 ; r })
length(ex1)  
length(ex3)
ex3[[1]]
ex3[[2]]
ex3[[3]]

as.list(ex3)

## parse() 
# - Allow to create an expression object  
# - Convert a string to an expression object
?parse
ex <- parse(text = c("x <- 4
                     x
                     5"))
length(ex)
typeof(ex)
class(ex)
ex[[1]]
ex[[2]]
ex[[3]]

identical(parse(text = "foo + bar", keep.source=FALSE),
          expression(foo + bar))

# Expression may be useful for performance investigation 
x <- 1:10
Ex  <-  expression(id=x, sq=x^2, x.x= x*x, rt=sqrt(x), p0.5 = x ^ 0.5)
sapply(Ex, class)
lapply(Ex, function(expr) print(expr))
lapply(Ex, function(expr) system.time(for(i in 1:10000) r <- eval(expr)))


#################
## Expressions ##
#################
# - It is also called an abstract syntax tree (AST)
# - It represent the hierarchical tree structure of the code 
# - 4 possible components of an expression 
# -- constant (length one atomic vectors)  
# -- names / symbols 
# -- call 
# -- pairlists

## quote() or substitute() 
# - returns an expression
# - returns an object that represents an action that can be perfomed by R

## bquote() 
# - allows to optionally quote and unquote some parts of an expression
# - everything inside .() is evaluated and thre result inserted in the quote
# - usefult to control what gets evaluated and when 
a <- 1
b <- 3
bquote(a + b)
bquote(a + .(b))
bquote(.(a) + .(b))
bquote(.(a + b))

## deparse()    
# - Convert an expression (not expression object !!!) to a string
z <- quote(y <- x * 10)
deparse(z)

# Check if an element is a valid componenent of an expression 
is.valid4expr <- function(x){
  out <- FALSE
  if(is.atomic(x) & length(x) == 1){out <- TRUE}
  if(is.call(x)){out <- TRUE}
  if(is.name(x)){out <- TRUE}
  if(is.pairlist(x)){out <- TRUE}
  out
}
###############
## Constants ##
###############
# - length one atomic vectors
# --> quoting a constant returns it unchanged 
quote("a")
quote(1)
quote(1L)
quote(TRUE)

# A constants can't contain an atomic vector of length greater than one because
#  vector with greater length require c() to be constructed and thus would be classified
#  as calls

 
###################
## Names/Symbols ##
###################
# - Represent the name of an object (rather its value) 
# - ast() prefixes names with a backtick
library(pryr)
ast(a)
ast(mean)

quote(a)
quote(mean)

class(quote(a))
typeof(quote(a))

class(quote(mean))
typeof(quote(mean))

str(quote(mean))

# Test if an object is a name
is.name(quote(a))

# Convert a string to a name 
as.name("ab")    # quote(ab)     , # as.symbol("ab")
class(as.name("ab"))

as.name("a b")   # quote(`a b`)  , # as.symbol("a b")
class(as.name("a b"))

## The empty name
# - Used to represent missing arguments 
# - It cannot be bind to a variable 
f <- function(x) 10
formals(f) 
is.name(formals(f)$x)      # TRUE
as.character(formals(f)$x) # ""
missing_arg <- formals(f)$x
missing_arg  # Missin argument 
# Create the empty name 
quote(expr=)
z <- quote(expr=)
z

# Create a list of name (symbols) 
al <- alist(x = , y = x)
al[[1]]
al[[2]]

# Use formals() to modify a function arguments and default value of x  
g <- function(x = 20, y) {
  x + y
}
formals(g) <- alist(x = , y = 10)
g

# An equivalent to get() using as.name() and eval()
get2 <- function(x, env) {
  eval(as.name(x), envir = env)
}

x <- 10
get("x", env = .GlobalEnv)
get2("x", env = .GlobalEnv)

# An equivalent to assign() using as.name(),substitute() and eval().
assign2 <- function(x, value, env = parent.frame()) {
  expr <- substitute(x <- value, list(x = as.name(x), value = value))
  print(expr)
  eval( expr, env)
  if (length(x) > 1)
    warning('only the first element is used as variable name')
}
e <- new.env()
assign2('x', 3, e)
e$x

x=4
assign2('x', 3)
x

########### 
## Calls ##
###########
# - Represent the action of calling a function
# - Very similar to a list 
# - It has length , [[ and [ methods
# - The length of the call minus 1 gives the number of arguments of the call (function)
# - Are recursive --> Can contain constants, names, calls and pairlists
# - ast() prints () at the top level of each call 
library(pryr)
ast( a + b)
ast( y <- x * 10)
ast( f(a,b) )
ast( f( g(), h(1,a))  )

z <- quote(a + b)
z
class(z)
length(z)
z[[1]]  # class : name      , typeof : symbol
z[[2]]  # class : name      , typeof : symbol
z[[3]]  # class : name      , typeof : symbol

str(z)

z <- quote(y <- x * 10)
z 
class(z)  # why <- ???????????????????????????????????????????????????????????????????
length(z)
z[[1]]  # class : name , typeof : symbol
z[[2]]  # class : name , typeof : symbol
z[[3]]  # class : call , typeof : language 
str(z)

z <- quote(f( g(), h(1,a) , arg1 = "mydefault" ))
z
class(z)
length(z)
z[[1]]  # class : name , typeof : symbol
z[[2]]  # class : call , typeof : language  --> g()
z[[3]]  # class : call , typeof : language  --> h(1,a)
z[[4]] 
# Test if it is a call
is.call(z)  
is.call(z[[1]]) 
is.call(z[[2]])    

# Add an argument to the function 
z$na.rm <- TRUE
z
# Retrieve default value of an argument
z$arg1
# Modify an argument 
z$na.rm <- FALSE
z
# Delete element of the call (with NULL)
z$arg1 <- NULL
z
# Retrieve a list of the (unevaluated) arguments 
as.list(z[-1])

## Create a call from its components with call()
# - The first argument must be a string with the function name !
# - The other arguments represent the arguments of the call
call(":",1,10)
f1 <- call("mean", quote(1:10), na.rm=TRUE)
f1 <- call("mean", x=quote(1:10), na.rm=TRUE)
f2 <- call("mean", x=1:10, na.rm=TRUE)
f1
f2
as.list(f1)
as.list(f2)
eval(f1)
eval(f2)

h1 <- call("mean", z)        # if z is not in the enclosing environment -> Error
h2 <- call("mean", quote(z)) # z depends on the calling environment
z <- 1:10
eval(h1)
eval(h2)

z <- 1:10
h1 <- call("mean", z)        # z depends on the enclosing environment
h2 <- call("mean", quote(z)) # z depends on the calling environment
z <- 1:20
eval(h1)
eval(h2)

## as.call()
# - Variant of call()
# - Takes a single list as input (first element is the function)
as.call( list(quote(mean),
              quote(1:10),
              na.rm=TRUE))

## Concat()
# - Combine a name and additional arguments to generate a call
concat <- function(f, ...){
  as.call(c(f, list(...)))
}
concat(quote(f), a = 1, b = quote(mean(a)))

# How do.call() works 
do.call2 <- function(fun, args, quote = FALSE, env = parent.frame()){
  if(!is.list(args))
    stop("second argument must be a list")
  if (quote) 
    args <- lapply(args, enquote)
  eval(as.call(c(fun, args)), env)
}
do.call2(median, list(1:10))
do.call2(paste,  list("hi", "hello"))

## make_call()
# - more flexible than concat() ...allow also for a list of arguments)
make_call <- function(x, ...){
  as.call(c(x, ...))
}
make_call(quote(mean), list(quote(x), na.rm = TRUE))
make_call(quote(mean), quote(x), na.rm = TRUE)

## match.call() or standardise_call()
# - returns a call in which all of the specified arguments are specified by their full names.
# - you may provide the method...not the generic functions !
# - match.call() does not work with primitive functions 
z <-  call("mean.default", na.=FALSE, 1:10, 0.3)
match.call(definition = mean.default, 
           call =z,
           expand.dots = TRUE)
pryr::standardise_call(z)

# Modelling functions often use match.call() to capture the call
#  used to create the model. This allow to update() a model 
# - system.call() capture exactly what the user typed !
# --> If you want to re-run code captured with match.call, it is necesssary 
#     to capture the environment in which it was evaluated 
f <- function(abc = 1, def = 2, ghi = 3) {
  list(sys = sys.call(), match = match.call())
}
f(d = 2, 2)
f(d = 2, 2,3)

## Modify a call with pryr::modify_call()
call <- quote(mean(x, na.rm = TRUE))
modify_call(call, list(na.rm = FALSE, trim = 0.1))




###############
## Pairlists ##
###############
# - Only used for the formal arguments of a function 
# - Allows to construct function by hand 
# - Are recursive --> Can contain constants, names and calls
# - ast() prints [] at the top-level of a pairlist
ast(function(x = 1, y) x)

z <- quote(function(x = 1, y) x)
z
length(z)
z[[1]]  # class : name      , typeof : symbol
z[[2]]  # class : pairlist  , typeof : pairlist 
z[[3]]  # class : name      , typeof : symbol
z[[4]]  # class : srcref, 

# Construct a function from its component piecies 
# - as.pairlist() ensure that the function has the pairlist args it needs 
# - alist() doesn't evaluate its arguments <--> alist(x=a) == list(x=quote(a))
# - To have an argument with no default, you need an explicit =
# - To take `...` as an argument put it on the LHS of =
make_function <- function(args, body, env = parent.frame()) {
  args <- as.pairlist(args)
  
  eval(call("function", args, body), env)
}

add <- make_function(alist(a =  , b = 2, ... =), quote(a + b))
add
add(1)
add(1, 2)

adder <- function(x) {
  make_function(alist(y= , ... =), substitute({x + y}), env=parent.frame())
}
adder(10)
adder(5)

## Curve example 
curve(sin(exp(4 * x)), n = 1000)
curve2 <- function(expr, xlim = c(0, 1), n = 100, 
                   env = parent.frame(),...) {
  f <- make_function(alist(x = ), substitute(expr), env)
  # Display the curve 
  x <- seq(xlim[1], xlim[2], length = n)
  y <- f(x)
  plot(x, y, type = "l", ylab = deparse(substitute(expr)),...)
}
curve2(sin(exp(4 * x)), n = 1000)


## partial()
# It takes a function and arguments and then constructs a call of that
# function with those args.
?partial

## unenclose()
# It takes a closure and substitutes the variables in that closure for its values
# found in its environment, which results in the explicit function.
?unenclose

 
