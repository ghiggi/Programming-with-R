########################
## Function operators ##
########################
# - A function that takes one (or more) functions as input and returns 
#    a function as output
# - FO_function <- function(args=... , f)
# --> the function f is the last argument in each FO (for clarity purpose !)

#####################
## Behavioural FOs ##
#####################
## chatty() 
# -  Function that print the processing values of another function 
# --> Most useful to inspect functionals
chatty <- function(f) {
  force(f)
  function(x, ...) {
    cat("Processing ", x, "\n", sep = "")
    f(x, ...)
  }
}

f <- function(x) x ^ 2
s <- c(3, 2, 1)

chatty(f)(2)
vapply(s, chatty(f), numeric(1))   

## TODO:  chatty() for dataframe or list 

## An example of behavioural FO, which return the result of it's first run, no matter
# how the input changes 
firs_run <- function(g) {
  force(g)
  result <- NULL
  function(...) {
    if (is.null(result)) { # evaluated only the first time
      result <<- g(...)  # the second time...result will be not NULL
    }
    result
  }
}
runif2 <- first_run(runif)
runif2(5)
 
runif2(10)

## Function that add a small delay between each call 
# --> Useful when downloading file over the internet to avoid hammering the server
delay_by <- function(delay, f) {
  force(f)
  function(...) {
    Sys.sleep(delay)
    f(...)
  }
}
system.time(delay_by(0.2, runif)(n=100,min=0,max=2))

## Function that ensure a certain amount of time has elapsed since the function 
# was last called
ensure_delay <- function(delay, f) {
  force(f)
  # Initialise the timestamp for the last run. 
  # --> Set a specific default value to ensure that the first run of the returned function will never be delayed
  last_runtime <- Sys.time() - (delay + 42)
  function(...) {
    # Continually check if enough time passed with an (empty) while statement.
    while (Sys.time() < (last_runtime + delay))  {
      Sys.sleep(delay/2)
      t_delta <- Sys.time() - last_runtime  
      cat(t_delta , 'more seconds before the function can be called again \n')
    }
    # If enough time is passed, run the function and update last_runtime   
    # after the function is evaluated with on.exit !
    on.exit(last_runtime <<- Sys.time()) 
    return(f(...))
  }
}
system.time(ensure_delay(0.2, runif)(n=1000,min=0,max=2))
 
## Function that wait until a specific time before to rexecute the function 
wait_until <- function(specific_time, f) {
  delay <- as.numeric(specific_time) - as.numeric(Sys.time())
  force(f)
  function(...) {
    if (delay > 0) {
      cat('This function will execute in', delay, 'seconds.\n')
      Sys.sleep(delay)
    }
    f(...)
  }
}
m <- wait_until(Sys.time() + 10, mean) 
m(1:3)
m(1:3) # TODO: modify it ...


## Function that print a dot (to inform about the processing state) when the
#   function f is called n given times 
dot_every <- function(n, f) {
  i <- 1
  function(...) {
    if (i %% n == 0) cat(".")
    i <<- i + 1
    f(...)
  }
}
x <- lapply(1:100, dot_every(10, runif))

## Download many urls siplay a dot every 10 download and delay by 1 s between each download
url1_txt <- "https://github.com/peterhurford/adv-r-book-solutions/blob/master/10_function_operators/01_behavioral/exercise1.r"
url2_txt <- "https://github.com/peterhurford/adv-r-book-solutions/blob/master/10_function_operators/01_behavioral/exercise1.r"
urls = list(url1_txt,url2_txt)
download_file <- function(url, ...) {
  download.file(url, basename(url), ...)
}
lapply(urls, download_file)

download <- dot_every(10, delay_by(1, download_file))
lapply(urls, dot_every(10, delay_by(1, download_file)))
lapply(urls, download)
 
i <- 1
for(url in urls) {
  i <- i + 1
  if (i %% 10 == 0) cat(".")
  Sys.sleep(1)
  download_file(url)
}

## Create a file in the current folder in which :
#  - write the time the function is executed
#  - write an informative message 
 logger <- function(f, filename){
  force(f)
  filename_tmp <- paste(filename, basename(tempfile()), sep = "_")
  write(paste("created at:", Sys.time()), file=filename_tmp, append = TRUE)  
  function(..., message = "you can add a message at each call") {
    write(paste0("used at: ", Sys.time(), ", ", message), filename_tmp, append = TRUE)
    f(...)
  }
}
mean2 <- logger(mean, "mean_log") 
mean2(1:4, message = "first time") 
mean2(1:4, message = "second_time")


####################################
## Capturing function invocations ##
####################################
ignore <- function(...) NULL
tee <- function(f, on_input = ignore, on_output = ignore) {
  force(f)
  function(...) {
    on_input(...)
    output <- f(...)
    on_output(output)
    output
  }
}

## Look inside uniroot functional 
g <- function(x) cos(x) - x
show_x <- function(x, ...) cat(sprintf("%+.08f", x), "\n")
zero <- uniroot(tee(g, on_input = show_x), c(-5, 5)) # The location where the function is evaluated (the final one is the zero)
zero <- uniroot(tee(g, on_output = show_x), c(-5, 5)) # The value of the function 

## Capture the sequence of call 
# Records every argument called   
remember1 <- function() {
  memory <- list()
  f <- function(...) {
    # Append is inefficient because it requires recomputing the list every time it is run.  
    # Faster would be to continue to store the number of items remembered 
    memory <<- append(memory, list(...))
    invisible()
  }
  structure(f, class = "remember")
}
remember <- function() {
  memory <- list()
  number_remembers <- 0
  f <- function(...) {
    ## Faster would be to continue to store the number of items remembered 
    number_remembers <<- number_remembers + 1
    memory[[number_remembers]] <<- list(...)
    invisible()
  }
  structure(f, class = "remember")
}

# Retrieves them when coerced into a list 
as.list.remember <- function(x, ...) {
  environment(x)$memory
}
# Print method for class remember 
print.remember <- function(x, ...) {
  cat("Remembering...\n")
  str(as.list(x))
}

locs <- remember()
vals <- remember()
g <- function(x) cos(x) - x
zero <- uniroot(tee(g, locs, vals), c(-5, 5))

class(locs)
locs
ls(environment(locs))
environment(locs)$memory
environment(locs)$f

x <- unlist(as.list(locs))
error <- unlist(as.list(vals))
plot(x, type = "b"); abline(h = 0.739, col = "grey50")
plot(error, type = "b"); abline(h = 0, col = "grey50")

#################
## Memoisation ##
#################
# - Modify a function to automatically cache it results
# - The function does not have to perform again a calculation that has already done
# - It saves the answers of new invocations, and re-uses the answers of old ones
#  - Under the right circumstances, this can provide a very nice speedup indeed.
#  --> It stores all of the previous inputs and outputs, using more memory !
#  --> Every use of memoise points to a different cache !!!
library(memoise)
slow_function <- function() {
  Sys.sleep(1)
  10
}
system.time(slow_function())
system.time(slow_function())

fast_function <- memoise(slow_function)
system.time(fast_function())
system.time(fast_function())

# Example of Fibonacci series (need previous call outputs...)
fib <- function(n) {
  if (n < 2) return(1)
  fib(n - 2) + fib(n - 1)
}
system.time( fib( n=23) )
system.time( fib( n=24) )

# Memoising in that ways...avoid calculations only if the whole set of function 
#  arguments is identical to a previous one  
fib_mem <- memoise(fib)   
system.time( fib_mem(n=23) )
system.time( fib_mem(n=23) )
system.time( fib_mem(n=24) )
system.time( fib_mem(n=24) )

# Memoising the function definitions that contains recursively call of itself 
#  can speed up computations a lot 
fib2 <- memoise(function(n) {
  if (n < 2) return(1)
  fib2(n - 2) + fib2(n - 1)
})
system.time(fib2(23))
system.time(fib2(24))

# Memoising the download function to avoid dowloand duplicated urls 
# --> Memoising before the delay_by effectively nullifies the utility of 'delaying' 
system.time( lapply(urls, dot_every(10, delay_by(1, download_file))) )
system.time( lapply(urls, dot_every(10, memoise(delay_by(1, download_file)))))

################
## Output FOs ##
################ 

## Negate() takes a function that returns a logical vector and returns the negation
# of that function (same as `!`)
Negate <- function(f) {
  force(f)
  function(...) !f(...)
}
(Negate(is.null))(NULL)

## compact() removes all null elements from a list:
compact <- function(x) Filter(Negate(is.null), x)

## failwith()  turns a function that throws an error into a function that 
# returns a default value when there's an error
failwith1<- function(default = NULL, f, quiet = FALSE) {
  force(f)
  function(...) {
    out <- default
    try(out <- f(...), silent = quiet)
    out
  }
}
library(plyr)
log("a")
failwith(NA, log)("a")
failwith(NA, log, quiet = TRUE)("a")

# --> Useful when try to fit lot of possible models
# TODO : change datasets with formulas !
models <- lapply(datasets, glm, formula = y ~ x1 + x2 * x3) # If any model fails, all models fail to fit:
models <- lapply(datasets, failwith(NULL, glm),             # If a model fails, it will get a NULL value
                 formula = y ~ x1 + x2 * x3)

ok_models <- compact(models) # Remove failed models (NULLs) with compact
# TODO: extract the formula corresponding to failed models
failed_data <- datasets[vapply(models, is.null, logical(1))]

## Capture the function output and convert it to a string 
capture_it <- function(f) {
  force(f)
  function(...) {
    capture.output(f(...))
  }
}
str_out <- capture_it(str)
str(1:10)
str_out(1:10)

# Evaluate the time elapsed to compute some functions 
time_it <- function(f) {
  force(f)
  function(...) {
    system.time(f(...))
  }
}

compute_mean <- list(
    base = function(x) mean(x),
    sum = function(x) sum(x) / length(x)
  )
x <- runif(1e6)
call_fun <- function(f, ...) f(...) 
a <- lapply(compute_mean, time_it(call_fun), x)
 
# Capture all the outputs (results, message, warnings and errors)
capture_all_output <- function(f){
  force(f)
  function(...){
    capture.output(tryCatch(f(...),
                            error = function(e) e,
                            warning = function(w) w,
                            message = function(m) m) )
  }
}
log_t <- capture_all_output(log)
elements <- list(1:10, c(-1, 10), c(TRUE, FALSE), letters)
results <- lapply(elements, function(x) log_t(x))
results

################
## Input  FOs ##
################ 
## Partial()
# Encapsulate the use of an anoymous function to supply default arguments 
# - function(...) f(..., default_arg=val)
# - partial(f, default_arg=val)
# --> Used to simplify code 
library(pryr)
xs <- replicate(5, runif(100000), simplify = FALSE)        # observations
ws <- replicate(5, rpois(100000, 5) + 1, simplify = FALSE) # list of weights
f <- weighted.mean
def_arg = TRUE # default arguments

Map(function(x, y) f(x=x, w=y, na.rm=def_arg), x=xs, y=ws)
Map(partial(f, na.rm=def_arg), x=xs, w=ws)

f <- function(a) g(a, b = 1)
f <- partial(g, b = 1)

compact <- function(x) Filter(Negate(is.null), x)
compact <- partial(Filter, Negate(is.null))
 
funs2 <- list(
  sum = function(...) sum(..., na.rm = TRUE),
  mean = function(...) mean(..., na.rm = TRUE),
  median = function(...) median(..., na.rm = TRUE)
)
funs2 <- list(
  sum = partial(sum, na.rm = TRUE),
  mean = partial(mean, na.rm = TRUE),
  median = partial(median, na.rm = TRUE)
)

## Vectorize()
# - Converts a scalar function to a vector function. 
# - It takes a non-vectorised function and vectorises it with respect
#   to the arguments specified in the vectorize.args argument. 
# - Useful if you want a quick and dirty way of making a vectorised function.
# --> SIMPLIFY = FALSE ensure that the newly vectorised function always returns a list.

# Generate multiple samples in one call.
sample2 <- Vectorize(sample, "size", SIMPLIFY = FALSE)
str(sample2(1:5, size=c(1, 1, 3)))
str(sample2(1:5, size=5:3))

## splat()
# - Converts a function that takes multiple arguments to a function that takes a single list of arguments.
# - Useful if you want to invoke a function with different possible arguments 
library(plyr)
splat1 <- function (f) {
  force(f)
  function(args) {
    do.call(f, args)
  }
}

x <- c(NA, runif(100), 1000)
args <- list(
  list(x),
  list(x, na.rm = TRUE),
  list(x, na.rm = TRUE, trim = 0.1)
)
lapply(args, splat(mean))

## colwise() and rowise()
# - Converts a vector function to one that works with data frames 
library(plyr)
colwise(median)(mtcars)
# TODO: rowwise() 
colwise1 <- function (.fun, .cols = true, ...)  {
  # We check if .cols is not a function, since it is possible to supply a
  # predicate function.
  # if so, the .cols arguments will be "quoted", and filter() will 
  # be a function that checks and evaluates these .cols within its other argument
  if (!is.function(.cols)) {
    .cols <- as.quoted(.cols)
    filter <- function(df) eval.quoted(.cols, df)
  }
  # otherwise, filter will be be Filter(), which applies the function 
  # in .cols to every element of its other argument
  else {
    filter <- function(df) Filter(.cols, df)
  }
  # the ... arguments are caught in the list dots
  dots <- list(...)
  # a function is created, which will also be the return value.
  # it checks if its input is a data frame
  function(df, ...) {
    stopifnot(is.data.frame(df))
    # if df is split (in "plyr" speaking), this will be taken into account...
    df <- strip_splits(df)
    # now the columns of the data frame are chosen, depending on the input of .cols
    # this can chosen directly, via a predicate function, or all columns (default)
    filtered <- filter(df)
    # if this means, that no columns are selected, an empty data frame will be returned
    if (length(filtered) == 0) 
      return(data.frame())
    # otherwise lapply will be called on all filtered columns, with 
    # the .fun argument, which has to be provided by the user, and some other
    # arguments provided by the user, when calling the function (...) and
    # when defining the function (dots)
    out <- do.call("lapply", c(list(filtered, .fun, ...), 
                           dots))
    # the output will be named and converted from list into a data frame again
    names(out) <- names(filtered)
    quickdf(out)
  }
}

 # FOs that convert a function to return a matrix instead of a data frame
as.matrix.function <- function(f){
  force(f)
  function(...){
    as.matrix(f(...))
  }
}

as.data.frame.function <- function(f){
  force(f)
  function(...){
    as.data.frame(f(...))
  }
}
as.data.frame(identity)(mat)
as.matrix(identity)(iris)

###################
## Combining FOs ##
###################
# FO can take multiple functions as input 

## each()
# - Takes a list of vectorised functions and combines them into a single functions 
library(plyr)
x=1:10
summaries <- each(mean, sd, median)
summaries(x)

x=1:10
funs <- list(mean,sd,median)
sapply(funs,function(f) f(x))

## compose()
# Function composition : f(g(x))
library(pryr)
compose1 <- function(f, g) {
  function(...) f(g(...))
}

sapply(mtcars, function(x) length(unique(x)))
sapply(mtcars, compose(length, unique))

sqrt(1 + 8)
compose(sqrt, `+`)(1, 8)

Negate
Negate <- partial(compose, `!`)

## Logical predicates ,boolean algebra and elementary algebra
and <- function(f1, f2) {
  force(f1); force(f2)
  function(...) {
    f1(...) && f2(...)
  }
}

or <- function(f1, f2) {
  force(f1); force(f2)
  function(...) {
    f1(...) || f2(...)
  }
}

not <- function(f) {
  force(f)
  function(...) {
    !f(...)
  }
}
Filter(or(is.character, is.factor), iris)
Filter(not(is.numeric), iris)

plus <- function(f1, f2) {
  force(f1); force(f2)
  function(...) {
    f1(...) + f2(...)
  }
}

minus <- function(f1, f2) {
  force(f1); force(f2)
  function(...) {
    f1(...) - f2(...)
  }
}

multiply <- function(f1, f2) {
  force(f1); force(f2)
  function(...) {
    f1(...) * f2(...)
  }
}

divide <- function(f1, f2) {
  force(f1); force(f2)
  function(...) {
    f1(...) / f2(...)
  }
}

exponentiate <- function(f1, f2) {
  force(f1); force(f2)
  function(...) {
    f1(...) ^ f2(...)
  }
}

mns <- minus(mean, function(x) x^2)
mns(1:5)
mean(1:5)-(1:5)^2








 