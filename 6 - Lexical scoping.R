#####################
## Lexical scoping ##
#####################

## h() finds 'd' inside f()  and finds 'w' in  globalenv()
f <- function(y) {
  #f's environment is the global environment 
  h <- function() {
    ## h's environment is the local environment() of f() 
    # It contains variable that are declared before h() is called (such as d <- 8...see below)
    cat("environment inside h:                    "); print(ls(environment())) # this is printed only when h() is called 
    tmp <- d*(w+y)
    cat("environment inside h:                    "); print(ls(environment())) # this is printed only when h() is called 
    return(tmp)
  }
  # Inspect R objects in the global environment 
  cat("environment(f) = Global_Env :            "); print(ls(environment(f)))  
  # Inspect R objects in the enclosing enviornment of h (--> f) (***)
  cat("environment(h):                          "); print(ls(environment(h)))   
  d <- 8 
  # Inspect R objects in the enclosing enviornment of h (--> f) (**)
  cat("environment(h):                          "); print(ls(environment(h)))
  cat("environment inside f - the same as '(h)':"); print(ls(environment())) #(**)

  return(h()) # It is only here that h() is called !!!!
}
w <- 10
w <- f(3)

# 1. Notice that we first call (***), d is not reported because still not declared !
# 2. When (**) is called, d appears because has been declared 
environment(f)  # environment(f) is the global environment 
########################################
## R don't use **dynamical scoping**  ##
########################################
rm(w)
# The following does not work !!!

g <- function(a) {
  w <- 10
  f(a) # f() is defined in the global environment and it searches the required variable  
       #     in its local environment and its parents if some variable are not found ...
}
g(3) ## --> error:
ls(environment(g))
# Because w is neither defined in :
# - environment(f) = global environment 
# - environment(h)

# Defininf f inside the g environment ...then it works 
rm(w)
g <- function(a) {
  f <- function(y) {
    d <- 5 # this does not matter 
    h <- function() return(d*(w+y))
    d <- 8  #because when h() is called ...d is already redefined to 8
    return(h()) # It is only here that h() is called !!!!
  }
  w <- 10
  f(a)  
}
g(3)
##################################
## Functions returning Functions # 
################################## 
## - The result functions are **closures**   
## - Closure: functions with non-trivial environment 
##
##  Closure :=   function + data = function + non-trivial environment

## Useful practical example 
?splinefun
?approxfun
?stepfun 
?ecdf

# Example with splinefun() 
x <- 1:9
y <- sin((x-0.5)*pi)
f <- splinefun(x, y)
plot(x,y, ylim=c(-3,3))
curve(f(x), 1, 10, col = "green", lwd = 1.5,add=TRUE)

typeof(f) # f it's a closure 
class(f)

ls(envir = environment(f))       # get the environment where f is declared 
get("z", envir = environment(f)) # get data present in the "non-trivial environment of f"


 
 