#####################
## Microbenchmark  ##
#####################
# - Measurement of the performance of a very small piece of code, something that
#   might take microseconds (µs) or nanoseconds (ns) to run.
# - Takes multiple expressions as input, and displays summaries of the distribution of times. 
# - By default, microbenchmark() runs each expression 100 times (controlled by the times parameter),
#   randomising the order of the expressions
# - It summarises the results with min, lq, median, uq, and max
# - Focus on the median, and use the upper and lower quartiles (lq and uq) to get a feel for the variability.

# - Is not useful for increasing the speed of real code.
#   --> The observed differences in microbenchmarks will typically be dominated by higher-order effects in real code

# 1 ms, then one thousand calls takes a second
# 1 µs, then one million calls takes a second
# 1 ns, then one billion calls takes a second
# --> Set unit = "eps" to obtain the number of evualiations needed to take 1 second 
library(microbenchmark)
options(digits = 3)
options("microbenchmark.unit" = "ns")
options("microbenchmark.times" = 100L)
microbenchmark:::print.microbenchmark


x <- runif(100)
microbenchmark(sqrt(x),
               x ^ 0.5,
               x ^ (1 / 2),
               exp(log(x) / 2)
              )
x <- seq(1e3)
microbenchmark(sqrt(x),
               x ^ (0.5),
               x ^  0.5,
               x ^ (1 / 2),
               exp(log(x) / 2)
              )

Ex  <-  expression(id=x, sq=x^2, x.x= x*x, rt=sqrt(x), p0.5 = x ^ 0.5)

 
microbenchmark( list = as.list(Ex))
do.call(microbenchmark, as.list(Ex))

mb <- microbenchmark( list = as.list(Ex), times=300)
mb <- do.call(microbenchmark, args=c(list = as.list(Ex), times=300))
plot(mb)
 
# System is not so precise....
microbenchmark( list = as.list(Ex), times=1)
lapply(Ex, system.time) ##  all 0 . `system.time` is not very precise
lapply(Ex, function(expr) system.time(for(i in 1:10000) r <- eval(expr))) 
lapply(Ex, function(expr) system.time(for(i in 1:10000) r <- eval(expr))/10000)  
lapply(Ex, function(expr) mean(replicate(10000, system.time(r <- eval(expr))["elapsed"])))
 



# Basic arithmetic 
# On windows system, short execution times are hard to measure
mb_integer <- microbenchmark(1L + 1L,
                             1L - 1L,
                             1L * 1L,
                             1L / 1L,
                             1L ^ 1L, 
                             times = 1000,
                             control = list(order = "random",
                                            warmup = 2000)
                             )
mb_double <- microbenchmark(1 + 1,
                            1 - 1,
                            1 * 1, 
                            1 / 1,
                            1 ^ 1, 
                            times = 1000,
                            control = list(order = "random",
                                           warmup = 2000),
                            )
mb_integer    
mb_double

#################### 
##   Performance  ##
####################
# - Poor performance due to the language is hard to fix without breaking existing code; 
# - Fixing poor performance due to the implementation is easier.
# - A lot of R code is slow simply because it's poorly written

#  R is an extremely dynamic programming language, and almost anything can be modified after it is created
# Trade-offs that limit the performance of the R-language: 
# - Name lookup with mutable environments, 
# - Extreme dynamism (i.e. method dispatch for S3, S4, and RC)
# - Lazy evaluation of function arguments

## If an interpreter can't predict what's going to happen, it has to look through 
#  many options to find the right one, a slow operation.
## Almost every operation is a lexically scoped function call

## The cost of finding the right method is very high for non-primitive functions
# - + , *, { , ( , are all functions contained in the base package   
# --> R has to look through every environment on the search path

# Compute how much each additional environment between f() and the base environment 
#  makes the function f() slower  
f <- function(x, y) {
  (x + y) ^ 2
}

random_env <- function(parent = globalenv()) { # with some object insides 
  letter_list <- setNames(as.list(runif(26)), LETTERS)
  list2env(letter_list, envir = new.env(parent = parent))
}

set_env <- function(f, env) {
  environment(f) <- env
  f
}
f2 <- set_env(f, env=random_env())
f3 <- set_env(f, env=random_env(environment(f2)))
f4 <- set_env(f, env=random_env(environment(f3)))

ls(environment(f2))
ls(environment(f3))
ls(environment(f4))

microbenchmark(
  f(1, 2),
  f2(1, 2),
  f3(1, 2),
  f4(1, 2),
  times = 1000
)
## Cost of method dispatch for S3, S4, and RC
# S3 and S4 method dispatch is expensive because R must search for the right method 
# every time the generic is called 

rm(list=ls())
f <- function(x) NULL

# S3 
s3 <- function(x) UseMethod("s3")
s3.integer <- f

# S4
A <- setClass("A", representation(a = "integer"))
setGeneric("s4", function(x) standardGeneric("s4"))
setMethod(s4, "A", f)
# RC 
B <- setRefClass("B", methods = list(rc = f))
b <- B$new() # generate data for s4 method

# Generate data for S4 method 
a <- A()      
a@a <- 1L

microbenchmark( fun = f(1L),
                S3  = s3(1L),
                S4  = s4(a),
                RC  = b$rc(1L)
               )

## Lazy evaluation 
# - To implement lazy evaluation, R uses a promise object that contains the expression
#  needed to compute the result, and the environment in which to perform the computation.
# - Every additional argument to an R function slows it down a little.
# - R spent time to create promise objects 
f0 <- function() NULL
f1 <- function(a = 1) NULL
f2 <- function(a = 1, b = 1) NULL
f3 <- function(a = 1, b = 2, c = 3) NULL
f4 <- function(a = 1, b = 2, c = 4, d = 4) NULL
f5 <- function(a = 1, b = 2, c = 4, d = 4, e = 5) NULL
microbenchmark(f0(), f1(), f2(), f3(), f4(), f5(), times = 100000)


## Extracting  asingle value from a data frame 
microbenchmark(
  "[32, 11]"      = mtcars[32, 11],
  "$carb[32]"     = mtcars$carb[32],
  "[[c(11, 32)]]" = mtcars[[c(11, 32)]],
  "[[11]][32]"    = mtcars[[11]][32],
  ".subset2"      = .subset2(mtcars, 11)[32]
)
