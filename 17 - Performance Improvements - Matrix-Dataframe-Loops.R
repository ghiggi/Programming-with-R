## Flexible function to assess performance of different algorithm/functions
plot_performance <- function(x, fun=NULL, fun_list=NULL, arg_list=NULL,
                             n_select="columns", microbenchmark=FALSE,...) {
  # - x must be a list of input of different size , 
  # - n_select : "columns","all","rows"... only input in list x is matrix or dataframe
  n_x <- length(x)
  if (!is.null(fun_list)) {
    n_f <- length(fun_list)
    perf <- vector("list", n_f) 
    for (i in 1:n_f){
        perf[[i]] <- vapply(x, function(x) system.time(do.call(fun_list[[i]], args=c(list(x),arg_list)))[1:3],
                           numeric(3))
    alg_names <- unlist(fun_list)
    names(perf) <- alg_names 
    }
  }   
  if(!is.null(fun) & !is.null(arg_list)) {
    n_f <- length(arg_list)
    perf <- vector("list", n_f) 
    for (i in 1:n_f){
      perf[[i]] <- vapply(x, 
                          function(x) system.time(do.call(fun, 
                                                          args=c(x, arg_list[[i]])))[1:3],
                          numeric(3))
    }
    alg_names <- names(arg_list)
    names(perf) <- alg_names 
  }
  # Compute dimensions 
  if (is.vector(x[[1]]) & (length(x[[3]])==1) ) { 
    n <- unlist(x) # for vectors containg already number n of ... 
    n_select <- "elements"
  }
  else if (is.vector(x[[1]]) & !(length(x[[3]])==1) ) {
    n <- vapply(x,length,numeric(1))
    n_select <- "elements"
  }
  else if (is.matrix(x[[1]] | is.data.frame(x[[1]]))) {
    if (identical(n_select,"rows")) n <- vapply(x, nrow, numeric(1))
    else if  (identical(n_select,"columns")) n <- vapply(x, ncolumns, numeric(1)) 
    if (identical(n_select,"all")) n <- vapply(x, function(x) nrow(x)*ncol(x) , numeric(1))
  }
  else if (is.array(x[[1]]) & !is.matrix(x[[1]])) {
    n <- vapply(x, function(x) rev(cumprod(dim(x)))[1], numeric(1)) 
    n_select <- "array cells"
  }
  # Plot performance evolution with increasing n 
  plot_table <- vapply(perf, function(x) x[1,], numeric(n_x))
  matplot(n, plot_table, type="l", col=1:n_f, ylab="Time",xlab=paste("Number of", n_select),...)
  legend("topleft",legend=alg_names , text.col = 1:n_f)
  perf
}

################################################################################
## Vector memory allocation
n= 1e5
system.time({ 
  a <- NULL
  for(i in 1:n)a[i] <- i
})

system.time({
  a <- rep(1, times=n)  # preallocate memory
  for(i in 1:n)a[i] <- i
})
system.time({
  a <- numeric(n)       # preallocate memory
  for(i in 1:n)a[i] <- i
})
system.time({
  a <- vector("numeric",n)       # preallocate memory
  for(i in 1:n)a[i] <- i
})
system.time(a <- 1:n)

################################################################################
### Substitute row elements of a dataframe with element of the previous rows
# 3 possible ways of doing it : 
# "vector": extract the 'x' vector, operate on that vector in a loop,
#            then insert the resulting vector into the data.frame.  
# "matrix" : convert the dataframe to a matrix, operate on the matrix in a loop
#            then insert the column of the matrix into the dataframe
# "d1,df2,df3":  modify the row element of the dataframe directly 
# --> Vector indexing and substitution is faster than matrix 
# --> Matrices are faster than data frames
# --> Data frames can have many types of data so R must determine
#     which type is handling on every access  
dumkoll <- function(n = 1000, df = "df1"){
  dfr <- data.frame(x = rnorm(n), y = rnorm(n))
  if (identical(df, "vector")) {
    x <- dfr$x
    for(i in 2:length(x)) {
      # if (!(i %% 100)) cat("i = ", i, "\n") # advancement status 
      x[i] <- x[i-1]
    }
    dfr$x <- x
  }
  else if (identical(df, "df1")) {
    for (i in 2:NROW(dfr)){
      # if (!(i %% 100)) cat("i = ", i, "\n") # advancement status 
      dfr$x[i] <- dfr$x[i-1]
    }
  }
  else if (identical(df, "df2")) {
    for (i in 2:NROW(dfr)){
      # if (!(i %% 100)) cat("i = ", i, "\n") # advancement status 
      dfr[i,"x"] <- dfr[i-1,"x"]
    }
  }
  else if (identical(df, "df3")) {
    for (i in 2:NROW(dfr)){
      # if (!(i %% 100)) cat("i = ", i, "\n") # advancement status 
      dfr[i,1] <- dfr[i-1,1]
    }
  }
  else if (identical(df, "matrix")) {
    dm <- as.matrix(dfr)
    for (i in 2:NROW(dm)) {
      # if (!(i %% 100)) cat("i = ", i, "\n") # advancement status
      dm[i, 1] <- dm[i-1, 1]
    }
    dfr$x <- dm[, 1]
  }
  
  dfr
}

# Check that the results from each method are identical:
identical(dumkoll(100,df="df1"), dumkoll(100,df="matrix"))
identical(dumkoll(100,df="df1"), dumkoll(100,df="quicker"))

n <- c("10k"=1e4, "20k"=2e4, "50k"=5e4, "100k"=1e5, "200k"=2e5)
sapply(n, function(n)system.time(dumkoll(n, df="df1"))[1:3])
sapply(n, function(n)system.time(dumkoll(n, df="df2"))[1:3])
sapply(n, function(n)system.time(dumkoll(n, df="df3"))[1:3])
sapply(n, function(n)system.time(dumkoll(n, df="matrix"))[1:3])
a <- sapply(n, function(n)system.time(dumkoll(n, df="vector"))[1:3])
 
n_list <- list("1k"=1e3, "10k"=1e4, "50k"=5e4,"100k"=1e5)
arg_list <- list(df1 = list(df = "df1"),
                 df2 = list(df = "df2"),
                 df3 = list(df = "df3"),
                 matrix = list(df= "matrix"),
                 vector = list(df= "vector"))
plot_performance(n_list,fun="dumkoll",arg_list=arg_list)
#perf <- plot_performance(n_list,fun=dumkoll,arg_list=arg_list)

## Prons and cons 
# - For "df*" methods
#   dfr$x[i] <- dfr$x[i-1]  will be executed n times.  It does the following:
#   1.  Get a pointer to the x element of dfr.  This requires R to look 
#       through all the names of dfr to figure out which one is "x".
#   2.  Extract the i-1 element from it.  Not particularly slow.
#   3.  Get a pointer to the x element of dfr again.  (R doesn't cache these 
#       things.)
#   4.  Set the i element of it to a new value.  This could require the 
#       entire column or even the entire dataframe to be copied, if R hasn't 
#       kept track of the fact that it is really being changed in place. 
# - For "matrix" methods
#   If the data.frame has columns of various types, then as.matrix will
#   coerce them all to a common type (often character), so it may give
#   the wrong result in addition to being unnecessarily slow.

# --> Simpler data structures that only store one type of data can be manipulated much faster.
# --> If you can represent data in a matrix instead of a data frame you can speed things up considerably.

###############
## Compiling ##
###############
## cmpfun()
# A "free" way to increase performance of R functions: R's byte compiler 
# works well in cases  
# - when defining a new function that is mostly numerical manipulation.
# Doesn't work well  
# - with functions that call a lot of other R functions
# - with function that involve manipulating and translating between data types. 
# - on already-defined R functions, which have been pre-compiled 
# - on already-defined R functions  sometimes written in compiled languages like C
## enableJIT()
# - enables or disables just-in-time (JIT) compilation. 
# - JIT is disabled if the argument is 0.
# - level=1: closures are compiled before their first use
# --> Automatically compile EVERY function the first time that it is run.
#     This will slow down R quite a bit at first, as each function must be 
#     compiled before it is run the first time, but but then speed it up later.
# - level=3:  in addition all loops are compiled before they are executed
#    -->  JIT level 3 requires the compiler option optimize to be 2 or 3. 
library(compiler)
?cmpfun 
dumkoll2 <- cmpfun(dumkoll)

# Use microbenchmark to compare for given n more precisely 
library(microbenchmark)
n <- 2000
mbd <- microbenchmark(dumkoll(n ,df="df1")   , dumkoll2(n, df="df1"),
                      dumkoll(n, df="matrix"), dumkoll2(n, df="matrix"),
                      dumkoll(n, df="vector"), dumkoll2(n, df="vector"), times = 25)
mbd
plot(mbd, log="y")
ggplot2::autoplot(mbd)

## Profiling
library("proftools")
n=10000
pd <- profileExpr(dumkoll(n, df="df1") ,srclines=TRUE,GC=TRUE)
funSummary(pd)
hotPaths(pd, total.pct = 10.0) 

pd <- profileExpr(dumkoll(n, df="matrix") ,srclines=TRUE,GC=TRUE)
funSummary(pd)
hotPaths(pd, total.pct = 10.0) 

pd <- profileExpr(dumkoll(n, df="vector") ,srclines=TRUE,GC=TRUE)
funSummary(pd)
hotPaths(pd, total.pct = 10.0) 

#####################################################################################
## Avoid indexing data.frame && Power of vectorization
# vectorization is a key to better performance. 
# - Move outside the loop all what is possible 
# - Initialize vectors/matrix/arrays/lists     
# - Compute all conditioning outside the loops if possible
# - It may be possible to not loop over all i, but only for the ones that fit condition.
# - Avoid indexing of dataframe --> Create vectors....
#   --> Indexing of vectors is much faster than indexing dataframes 
loopA <- function(dfr){
  res <- numeric(nrow(dfr))
  for (i in 1:nrow(dfr)){    
    res[i] <- i
    if (i > 1) {             
      if ((dfr[i,6] == dfr[i-1,6]) & (dfr[i,3] == dfr[i-1,3])) { 
        res[i] <- dfr[i,9] + res[i-1]                   
      } else {
        res[i] <- dfr[i,9]                                    
      }
    } else {
      res[i] <- dfr[i,9]
    }
  }
  dfr$`Kumm.` <- res
  return(dfr)
}

loopB <- function(dfr){
  cond <- c(FALSE, (dfr[-1,6] == dfr[-nrow(dfr),6] ) & (dfr[-1,3]) == dfr[-nrow(dfr),3])
  res <- dfr[,9]
  for (i in 1:nrow(dfr)) {
    if (cond[i]) res[i] <- dfr[i,9] + res[i-1] 
  }
  dfr$`Kumm.` <- res
  return(dfr)
}

loopC <- function(dfr){
  cond <- c(FALSE, (dfr[-1,6] == dfr[-nrow(dfr),6] ) & (dfr[-1,3]) == dfr[-nrow(dfr),3])
  res <- dfr[,9]
  for (i in (1:nrow(dfr))[cond]) { #decrease the time the loop is executed 
    res[i] <- dfr[i,9] + res[i-1]
  }
  dfr$`Kumm.` <- res
  return(dfr)
}

loopD <- function(dfr){
  cond <- c(FALSE, (dfr[-1,6] == dfr[-nrow(dfr),6] ) & (dfr[-1,3]) == dfr[-nrow(dfr),3])
  res <- dfr[,9]
  for (i in (1:nrow(dfr))[cond]) {
      res[i] <- res[i] + res[i-1]
  }
  dfr$`Kumm.` <- res
  return(dfr)
}

 
n <- c(100,1e3,5e3, 1e4,5e4)
dfr_list <- list(as.data.frame(matrix(sample(1:10, n[1]*10, TRUE), n[1], 10)),
                 as.data.frame(matrix(sample(1:10, n[2]*10, TRUE), n[2], 10)),
                 as.data.frame(matrix(sample(1:10, n[3]*10, TRUE), n[3], 10)),
                 as.data.frame(matrix(sample(1:10, n[4]*10, TRUE), n[4], 10)),
                 as.data.frame(matrix(sample(1:10, n[5]*10, TRUE), n[5], 10)))
                 
fun_list <- list("loopA",
                 "loopB",
                 "loopC",
                 "loopD")
perf1 <- plot_performance(dfr_list,fun_list=fun_list, n_select = "rows")
################################################################################
##  Mean performance 
x <- sample(1:100, 100, replace = TRUE)
comp <- microbenchmark(mean(x), sum(x)/length(x), .Internal(mean(x)), times = 1e+05)
comp
ggplot2::autoplot(comp)


### Quick way to transform a list in a dataframe (if vectors are of equal length)
quick_as_df <- function(l) {
  class(l) <- "data.frame"
  attr(l, "row.names") <- .set_row_names(length(l[[1]]))
  l
}
l <- lapply(1:26, function(i) runif(1e3))
names(l) <- letters
library(microbenchmark)
microbenchmark(quick_as_df = quick_as_df(l),
                as.data.frame = as.data.frame(l))



# Most of base R functions are written for flexibility and functionality, not performance 
 
# Every time you redefine the size of an object in R, you are also redefining 
#  the allotted memory - and this takes some time
 
## Extra parentheses to help make the code more readable. This has a small performance cost,

## Code that uses parentheses is actually slower than the same code with curly brackets:
# - R treats curly brackets as a "special" operator, whose arguments are not automatically evaluated
# - R treats parentheses as a "built in" operator, whose arguments (just one for parentheses) are evaluated automatically


# Formula interfaces is considerably slower 


# read.csv(): specify known column types with colClasses.
# factor(): specify known levels with levels
# unlist(x, use.names = FALSE) is much faster than unlist(x).
# interaction(): if you only need combinations that exist in the data, use drop = TRUE
# cut(): don't generate labels with labels = FALSE if you don't need them, or, even better, use findInterval()

# For S3, you can do this by calling generic.class() instead of generic().
# For S4, you can do this by using getMethod() to find the method, saving it to a variable, and then calling that function.


# Vectorised subsetting can lead to big improvement in speed
# --> Subset matrix or dataframe with integer matrix 
# --> Show TRUE-FALSE vs. index 


## Modifying an object in a loop (x[i] <- y) can also create a copy at each loop 
#  iteration depending on the class of x

#*apply functions are inherently faster than for loops
 