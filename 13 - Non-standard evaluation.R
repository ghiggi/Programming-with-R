# A function is referentially transparent if you can replace its arguments 
#  with their values and its behaviour doesn't change.
# NSE prevents a function from being referentially transparent
# Non-standard evaluation allows to write functions that are extremely powerful.
# However, they are harder to understand and to program with. 

# As well as always providing an escape hatch, carefully consider both the costs 
#  and benefits of NSE before using it in a new domain.

###########################
## Capturing expressions ##
###########################
# - Function arguments are represented by a special type of object called a promise. 
# - A promise captures the expression needed to compute the value and the environment in which to compute it.
#    
## quote()
# - Create an expression 
# - Return the argument without evaluating it or substituing any variable 
# - Doesn't do any of the advanced transformations that can make substitute() confusing.
# - Used in combination with eval() because eval()'s first argument has to be an expression 
?quote
quote(1:10)
quote(x)
quote(x + y^2)
typeof(quote(1:10))

quote(eval(quote(eval(quote(eval(quote(2 + 2)))))))  # an outside quote "always win"

identical(quote(fooBar), as.name("fooBar"))

# substitute()
# - Captures an unevaluated expression such as quote()  
# - Captures the expression needed to compute the value 
# - Can modify the expression within an environment, performing substitutions
# - If substitute() is run from the global environment, it never does substitutions !!! 
#    --> It behaves like quote()
# - If substitute is not run in the global environment, it substitutes any variables bound in the specified
#    environment (by default is the current evaluation environment of the function)
#    --> A function that use substitute() can be difficult to call form another function 
f <- function(x) {
  substitute(x)
}
f(2)
substitute(2)

substitute(1:10)
f(1:10)
typeof(substitute(1:10))
typeof(f(1:10))
 
x <- 10
substitute(x)
f(x)
 
y <- 13
f(x + y^2)
substitute(x + y^2)

a <- 1
b <- 2
substitute(a + b + z)

x <- quote(a + b)
substitute(x, list(a = 1, b = 2))

f <- function() {  
  a <- 1
  b <- 2
  substitute(a + b + z) # it performs substitions because not in global_env
}
f()

## subs()
# - Similar to substitute but perform substitution also in the global_env
library(pryr)
a <- 1
b <- 2
subs(a + b + z)
# The second argument env allow to override the use of the current environment (global_env)
subs(a + b)                       #   1 + 2
subs(a + b, env = parent.frame()) #   1 + 2
subs(a + b, list(a = "y"))        #"y"  + b  <-- b is no more substituded to 2 
subs(a + b, list(a = quote(y)))   # y   + b  <-- b is no more substituded to 2 
subs(a + b, list(a = quote(y()))) # y() + b  <-- b is no more substituded to 2 
subs(a + b, list("+" = quote(f)))
subs(a + b, list("+" = quote(`*`)))
subs(f(g(a, b), c), list(g = quote(`+`),
                         f = quote(`*`)))   
subs(f(a < b, c, d), list(f = quote(`if`)))

subs(a <- a + 1)
subs(a <- a + 1, list( a = quote(a)))

x <- quote(a + b)
substitute(x)
subs(x)

substitute(x, list(a = 1, b = 2))
subs(x, list(a = 1, b = 2))

substitute(x, list(x = 1, b = 2))
subs(x, list(x = 1, b = 2))

x <- quote(mpg)
y <- quote(disp)
subs(x~y)

# --> Application with plot functions
library(lattice)
x <- quote(mpg)
y <- quote(disp)
xyplot(mpg ~ disp, data = mtcars)


xyplot(x ~ y, data = mtcars) # does not work 
subs( xyplot(x ~ y, data = data)  )
     
xyplot2 <- function(x, y, data = data) {
  expres <- substitute(xyplot(x ~ y, data = data))
  # expres <- subs(xyplot(x ~ y, data = data)) # this is equivalent to substitute() in a function
  eval(expres)
}
xyplot2(mpg,disp, data = mtcars)
 

## deparse()
# - Takes an expression (i.e. the result of substitute()), and turns it into a character vector
# - ATTENTION: It can return multiple strings if the input is too long  !!!
# --> width.cutoff = 60 bytes by default 
g <- function(x) deparse(substitute(x))
g(2)
g(1:10)
g(x)
g(x + y^2)
g(f(x))
 
g(a + b + c + d + e + f + g + h + i + j + k + l + m +
    n + o + p + q + r + s + t + u + v + w + x + y + z)

deparse_without_cutoff <- function(x){
  paste0(deparse(substitute(x)), collapse = "")
}
deparse_without_cutoff(a + b + c + d + e + f + g + h + i + j + k + l + m +
                         n + o + p + q + r + s + t + u + v + w + x + y + z)

# If the object inside the brackets is not an expression but other stuff:
X <- 2
deparse(2)    # "2"
deparse(x)    # error
deparse(b)    # error
deparse(f(x)) # error

#  If you call substitute from another function, you may want to set the env argument 
#   of substitute() to parent.frame(), which refers to the calling environment
substitute(x) # is always "x".

f <- function(x) substitute(x, env=environment()) 
g <- function(x) deparse(f(x)) 
g(1:10)                             # -> x
g(x)                                # -> x
g(x + y ^ 2 / z + exp(a * sin(b)))  # -> x

f <- function(x) substitute(x, env = parent.frame()) 
g <- function(x) deparse(f(x)) 
g(1:10)                             # -> 1:10
g(x)                                # -> x
g(x + y ^ 2 / z + exp(a * sin(b)))  # -> x + y ^ 2 / z + exp(a * sin(b))

## eval()
# - Takes an expression and evaluates it in the specified environment.
# - eval()'s first argument is an expression.
#   --> A common mistake when using eval() is to forget to quote the first argument ! 
# - eval()'s second argument:
#   -  may specify the environment in which the code is executed (by default is the environment where eval is called)
#    - may be a list or a data frame 
# - eval()'s third argument allow to specify a parent (enclosing) environment for objects
#         not find in the environment/list/dataframe specifiy in the second argument
eval(quote(x <- 1))
eval(quote(x))

x <- 10
eval(quote(x))
 
e <- new.env()
e$x <- 20
eval(quote(x), e)

eval(quote(x), list(x = 30))
eval(quote(x), data.frame(x = 40))

## evalq(x)
# - Equivalent to eval(quote(x))
eval(quote(x), list(x = 30))
evalq(x, list(x = 30))

## Non-standard evaluation with subset()
# - Expressions a >= 4 or b == c are evaluated in the specified data frame
#   rather than in the current or global environments. 
# - We want x to be interpreted as df$x, not globalenv()$x (for that it uses eval()
df <- data.frame(a = 1:5, b = 5:1, c = c(5, 3, 1, 4, 1))

with(df, a >= 4)
eval(quote(a >= 4), df)

df[ df$a >= 4, ]
df[ with(df, a >= 4), ]
df[ eval(quote(a >= 4), df), ]
subset(df, a >= 4)

subset2 <- function(x, condition) {
  condition_call <- substitute(condition)
  r <- eval(condition_call, envir=x)
  x[r, , drop=FALSE ]
}
subset2(df, a >= 4)

# When working with expressions instead of vlaues, it is necessary to test things
#  extensively and carefully due to possible scoping issues
# --> Check what happen if the expression contains a name of a variable inside the function ! 
y <- 4
x <- 4
condition <- 4
condition_call <- 4
subset2(df, a >= 4)
subset2(df, a >= y)
subset2(df, a >= x)               # error
subset2(df, a >= condition)       # error
subset2(df, a >= condition_call)  # error

# If eval() does not find the variable present in the condition statement inside the 
#  dataframe/list/environment, it looks in the environment of the function 
# --> Need to tell eval where too lok for variable !
# --> Need to search value in the environment where the function was called 
subset.data.frame
subset2 <- function(x, condition) {
  condition_call <- substitute(condition)
  r <- eval(condition_call, envir=x, enclos=parent.frame())
  x[r, drop=FALSE ]
}
x <- 4
subset2(df, a >= x)
subset2(df, a >= condition)       
subset2(df, a >= condition_call) 

# Enclos is a shortcut for converting a list or data.frame to an environment
subset2a <- function(x, condition) {
  condition_call <- substitute(condition)
  env <- list2env(x, parent = parent.frame())
  ls(env) 
}
subset2a(df, a >= 4)

subset2a <- function(x, condition) {
  condition_call <- substitute(condition)
  env <- list2env(x, parent = parent.frame())
  r <- eval(condition_call, envir=env)
  x[r, , drop=FALSE]
}
x <- 4
subset2a(df, a >= 4)

ls(environment(env))

## select() variables from a dataframe using non-standard evaluation 
select <- function(df, vars) {
  vars <- substitute(vars)
  var_pos <- setNames(as.list(seq_along(df)), names(df))  #returns the positions of all the variable names (in a list)
  pos <- eval(vars, var_pos)  #  evaluates "cyl" to its index (2) and the "-" in front of cyl to -2
  df[, pos, drop = FALSE]
}
select(mtcars, -cyl)
select(mtcars, disp:drat)

## arrange ()
# - Works similarly to subset(), but instead of selecting rows, it reorders rows
library(plyr)
arrange1 <- function(df, ...) {
  ord <- eval(substitute(order(...)), df, parent.frame()) #returns the indices neede to rearrange a list
  df[ord, , drop = FALSE]
} 
arrange(mtcars, cyl, disp)  
arrange1(mtcars, cyl, disp)  

## mutate() and transform()
# - transform() does not apply trasformation sequentially 
# - mutate() applies transformations sequentially so that transformation can refer to
#    columns that were just created
df <- data.frame(x = 1:5)
transform(df, x2 = x * x, x3 = x2 * x)
plyr::mutate(df, x2 = x * x, x3 = x2 * x)

## with()
# - Returns the value of an evaluated expression
# - Allows writing an expression (second argument) that refers to variablenames 
#    of data (first argument) as if the corresponding variables were objects themselves
# - It construct a temporary environment, which has the calling frame as a parent,
#    where to evaluates the expression 
# - It is just a wrapper for the standard `eval(substitute())` using data as the eval environment
with1 <- function (data, expr, ...){
  eval(substitute(expr), envir=data, enclos = parent.frame())
}

## within()
# - Returns the modified object
# - Can be used as an alternative to base::transform()
# -->  1. Creates an environment with all the information of `data`   
#      2. Turns is it into a list 
#      3. Use the list to transform data   
#       
within1 <- function(df, expr, ...) {
  # Create an environment with the existing variable of the df
  e <- evalq(environment(), envir=df, enclos=parent.frame())
  # Evaluate the expression in the environment e 
  # --> It changes the variable present in the environment e (based on expr)
  eval(substitute(expr), envir=e)
  # Convert the environment into a list 
  l <- as.list(e, all.names = TRUE)
  l <- l[!vapply(l, is.null, NA, USE.NAMES = FALSE)]
  nl <- names(l)
  # Reconstruct the dataframe
  df[nl] <- l
  df
}
within1(iris, { Sepal.Length <- round(Sepal.Length / 2) } )
transform(iris, Sepal.Length = round(Sepal.Length / 2))
identical(within1(iris, { Sepal.Length <- round(Sepal.Length / 2) } ),
          transform(iris, Sepal.Length = round(Sepal.Length / 2))
          )

###################################
## Calling from another function ##
###################################
# A function that use substitute can be difficult to call from another function 
# --> substitute(expr=condition) must be placed in the first function that receive the condition !

# The following doesn't work (Ex: function that subset column and resample the rows) 
# --> when we evaluate condition_call it also evaluates condition,
#     which has the value a >= 4. However, this can't be computed because there's 
#     no object called "a"  (or not the one we want) in the parent environment 
#     of subset2() which is the global_env
subset2 <- function(x, condition) {
  condition_call <- substitute(condition)
  r <- eval(condition_call, x, parent.frame())
  x[r, ]
}

scramble <- function(x) x[sample(nrow(x)), ]

subscramble <- function(x, condition) {
  scramble(subset2(x, condition))
}
df <- data.frame(a = 1:5, b = 5:1, c = c(5, 3, 1, 4, 1))
subscramble(df, a >= 4)
# Solution 
subset2_q <- function(x, condition) { # condition must be a quoted expression 
  r <- eval(condition, x, parent.frame())
  x[r, ]
}
subscramble <- function(x, condition) {
  condition <- substitute(condition)  # here "a" is found --> condition = quote(a >= 4)
  scramble(subset2_q(x, condition))
}
df <- data.frame(a = 1:5, b = 5:1, c = c(5, 3, 1, 4, 1))
subscramble(df, a >= 4) 

###################################
## Escape hatch for substitute() ##
###################################
# 1. First substitute(substitute(y, env), list(y = x)) is evaluated. 
#    The expression substitute(y, env) is captured and y is replaced by the value of x
# 2. Evaluate that expression: -  eval(substitute(X, env))
#                              -  eval(substitute(X, list(a = 1, b = 2)))
substitute_q <- function(x, env) {
  call <- substitute(substitute(y, env), list(y = x))
  eval(call)
}

x <- quote(a + b)
subs(x)
substitute(x)
substitute(x, list(a = 1, b = 2))
subs(x, list(a = 1, b = 2))

substitute_q(x, list(a = 1, b = 2))




f <- function(x) {
  print(environment())
  print(parent.env(environment()))
  print(parent.frame())
}

