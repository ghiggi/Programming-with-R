# An  Environment is an object.
# Environments are specialized, they can only hold two things:
# - A frame : just a collection of named objects --> the place where an environment holds objects (name-onject binding)
# - The enclosing environment (calso called parent environment or "the environment's owner")
# --> The job of an environemnt is to associate (bind) a set of names to a set of values.
#     Each name points to an object stored elsewhere in memory 

# Every environment has a pointer to another environement 
# Every environment has a parent environment (execept the emptyenv)
# Every parent environment has a child 

## Every environment (except R_EmptyEnv) has an enclosure (enclosing=parent environment)
# - The empty environment has no parent environment 
parent.env( emptyenv())

## The globalenv() (global environment) is the interactive workspace (where usually we work)
# The parent environment of the global environment is the last "attached" package -->  i.e. at position [2] or [3] in search()
parent.env( globalenv())  
parent.env( .GlobalEnv )
search()
head(search())
 
## Each time you load a new package, it is inserted between the global environment and the package that
#  was previously at the top of the search path
search()
library(gstat)
search()
parent.env(globalenv())

# The chain of enclosing environments stops at a special environment called the Empty Environment
basePackageEnv = as.environment( "package:base" ) 
basePackageEnv
baseenv()
parent.env(basePackageEnv)
emptyenv()
print(ls(emptyenv()))
print(ls(emptyenv(), all.names = )) # print also names that start with .a_name

# However, the enclosing environment of the namespace of package:base is the globalEnv
baseNamespaceEnv <- asNamespace("base")
baseNamespaceEnv
parent.env(baseNamespaceEnv)

# The environment Autoloads is used to save memory by only loading
#  package objects (like big datasets) when needed
 
# Retrieve environment of a package and its namespace   
statsPackageEnv <- as.environment( "package:stats" ) 
statsNamespaceEnv <-  asNamespace( "stats" ) 
statsNamespaceEnv <- getNamespace( "stats")
sd
nsSd = get( "sd" , envir = statsPackageEnv , inherits = FALSE )
pkgSd = get( "sd" , envir = statsNamespaceEnv , inherits = FALSE )
identical( nsSd , pkgSd ) #nsSd and pkgSd are pointers to the same object

## When you create an object it is automatically placed in the
## "current" or "local" environment, accessible using environment()
environment()
myValue <- 5
ls( envir = environment() )

## Create a new environment 
# - Set parent to emptyenv to not accidentaly inherit object from somewhere else 
myEnvironment  <- new.env(parent=emptyenv())
myEnvironment
m  <- myEnvironment
m # ...and myEnvironment are the same environment !!!
 
## Specify a name to an environement  
# - Both m and myEnvironment are modified !!
attr( myEnvironment, "name") <- "Cool Name"
myEnvironment
m
environmentName( myEnvironment )
environmentName( m)

attributes(myEnvironment)
attr(myEnvironment,"name")

## Create an object in an environment other than the local (current) environment.
# - Attach object to a given environment
myEnvironment$abc <- LETTERS
assign(x="myLogical", value=c(FALSE, TRUE), envir = myEnvironment )
ls(myEnvironment)
## Objects don't live in the enviroment  
# - so different names can point to objects that have the same value 
# - so multiple names can point to the same object (in the memory)
myEnvironment$ABC <- LETTERS
myEnvironment$lette <- myEnvironment$abc 
myEnvironment$abc
myEnvironment$lette
pryr::address(abc)    # place in memory
pryr::address(lette)  # place in memory
pryr::address(ABC)    # place in memory
# If two names point to the same object, modify one equals to create a copy of the objects and modify the copy 
myEnvironment$abc <- "only one letter"
myEnvironment$abc
myEnvironment$lette
 
# Retrieve any named object from any given environment using the get() function
myEnvironment$abc
myEnvironment$myLogical
myEnvironment[["myLogical"]]
get( "myLogical", envir = myEnvironment)
get( "myLogical", envir = m)
 
# Determine if an object (a binding) exist in an environment and its parent
# --> if you want to check only in a specific environment : inherits = FALSE
exists("ABC",envir=myEnvironment, inherits=FALSE)
# Remove an object from an environment
rm("ABC",envir=myEnvironment)
rm("ABsdddC",envir=myEnvironment) #display a warning if the object is not inside the environment 

## Query the environment for all objects in the frame using ls().
ls( myEnvironment)
ls( m)

# View content of an environment
str(myEnvironment)
ls.str(myEnvironment)

## The default enclosing environment of myEnvironment is the local environment
parent.env(myEnvironment)

## Modify the enclosing environment using the replacement form of parent.env().
myEnvironment2 <- new.env()
parent.env( myEnvironment2 )
parent.env( myEnvironment2 ) <- myEnvironment
parent.env( myEnvironment2 )

## When R executes an expression, there is always one local or current environment
## When R executes a function it automatically creates a new environment for that function.
# This is very useful! Variables/objects created inside the function will live in the new local environment.
# When the function completes executing, the local environment of the function dies.
# -  We create a function that calls environment() to query for the local environment.
# -  We can see that Test() does NOT print R_GlobalEnv !!!
Test <- function() { print( environment() ) }
Test # if the environment is not displayed, it means that the object is created in the global environment !
environment(Test)
Test() # each time the local enviroment of the function changes 
 
# FromLocal's enclosing environment is the MyFunction environment --> age=22
# FromGlobal's enclosing environment is R_GlobalEnv --> Age = 32
# The environment of NoSearch() already has the age object --> Does not need to search in its enclosing environment(s).
age = 32 
MyFunction = function()  { 
  age = 22 
  FromLocal = function() { print( age + 1 )          ; print(environment()) } 
  FromGlobal = function() { print( age + 1 )         ; print(environment()) } 
  NoSearch =  function() { age = 11; print( age + 1 ); print(environment()) } 
  print(environment(FromLocal))
  print(environment(FromGlobal))
  print(environment(NoSearch))
  environment( FromGlobal ) = .GlobalEnv # modifiy enclosing environment of the function !
  print(environment(FromGlobal))
  FromLocal() 
  FromGlobal() 
  NoSearch() 
} 
MyFunction() 

## The purpose of environment() is not to tell you an object's owner/environment.   
# Environments() returns the enclosing environment of the function specified 
# - By default, R sets a function's environment property equal to the environment
#    where the function was created (the environment that owns the function). 
# - However, its not necessary that a function's executing environment and the
#    environment that owns the function are one and the same. 
environment( plot)
environment( rnorm)

MyFunction = function() { } 
MyFunction 
environment( MyFunction ) 
newEnvironment = new.env() 
environment( MyFunction ) = newEnvironment 
environment( MyFunction ) 
MyFunction

# More informations about function environments ...
# - parent.env(environment()) inside a function returns its enclosing enviroment() 
fstr1 <- function(name) {
  list( str = capture.output(str(name)),
        calling = parent.frame(),
        parent = parent.env(environment())
  )
}
fstr2 <- function(object){
  if(!is.function(object)){stop("fstr works only for functions")}
  
  object_str <- as.character(substitute(object))
  
  flist <- list(ftype = pryr::ftype(object),
                where = pryr::where(object_str),
                enclosing_env = pryr::enclosing_env(object),
                args = pryr::fun_args(object)
  )
  
  return(flist)
} 
fstr1(var)
fstr2(var)

# Evaluate an R expression in an environment constructed from data with with()
f <- mean
x <- list(f=f,  z=1:10)
rm(f)
with(x, f(z))
x$f(x$z)

# From A List, Build or add R object (and functions) to an Environment
# --> similar to attach() detach()
list2env(x, environment())
rm(list = names(x), envir = environment())

############################################
## Environments have reference semantics  ##
############################################
modify <- function(x) {
  x$a <- 2
  invisible()
}
# If applied to a list, the original x is not changed (the action of modifying creates and modifies a copy)
x_l <- list()
x_l$a <- 1
modify(x_l)  # x_e is a list (R object)
x_l$a
 
# If applied to an environment, the original environment is modified
x_e <- new.env()
x_e$a <- 1
modify(x_e)  # x_e is an environment 
x_e$a 



###########################
## Namespaces & Packages ##
###########################
## Every package has 3 enviroments 
# 1. "package environment" 
#    - This is where a package's exported objects go. 
#    - Contains the objects that the package author wants you to see. These are most likely functions.
# 2. "namespace environment" 
#    - This is where all objects in a package go.
#    - It includes all the objects of the package enviroment 
#    - It also includes objects that are not meant to be accessed by the end-user ("hidden" objects) 
# 3  "imports environment"
#    - This environment contains all objects from other packages that are explicitly 
#       stated requirements for a package to work properly.
# The package environment is just a pass-thru to the namespace environment. 
# R executes the function always within the namespace environment 
# In the namespace, the call of a functions create a new environment whose enclosure is the namespace environment 

## Retrieve the environment of a namespace
pkgName <- "stats"
nsName <- "stats" 
nsName <- pkgName
ns <- asNamespace(nsName)

## Load the name space of package 
loadNamespace(pkgName)

##  All what it is in package, it also present in the namespace
l1 <- ls("package:stats")
l2 <- ls(envir= asNamespace("stats"))
all(l1 %in% l2) ## -> TRUE

## Retrieve a list of hidden R Objects (which are present only in the namespace environment)
setdiff(l2, l1)  

# Hidden object (non-exported objected) require the ::: operator to be called 
primes.(10)
sfsmisc:::primes.(10) # allow to call the intern variable (the hidden object)  

## Often, functions name are similar but not the same !  
?sfsmisc::primes
getAnywhere("primes")  
getAnywhere("primes.") #this is an hidden object !

# Retrieve a function from a namespace environment 
nsName <- "stats"         # namespace name
ns <- asNamespace(nsName) # namespace environment
stats::median
stats:::median
getExportedValue(ns,"median") 
x = c(1,2,3)
stats::median(x)
getExportedValue(ns,"median")(x)

# Retrieve a function from the namespace of a package (giving only the names)
getFromNamespace("median", ns="stats")
microbenchmark:::print.microbenchmark

# Substitute a function in the namespace of a package 
assignInNamespace("median.myclass", "median_myfunctionforthatclass",
                  ns="stats")

# Look at the objects in the "Imports environment"
importEnv <- parent.env( asNamespace("MASS"))
importEnv
ls( importEnv)  
getNamespaceImports(asNamespace("MASS"))

## Import vs Depends 
# If a package is specified:
# - in Depends, then the package is loaded as it would be called  by library() or require()
# - in Import, all the objects of that package go into the "imports environment"
#   - More specifically the imports environement and namespace environment of 
#      the imported package are the enclosure of the imports:package environment   
# Depends makes a package vulnerable to whatever other packages are loaded by the user !!!

## All "imports environments" have namespace:base as their enclosure.
# If R will find the base functions immediately after Imports, there is no chance of corruption ! 
# Since the base functions are used frequently, they are most likely a dependency 
#  for any package (or a package's imports).
# Without namespace:base as enclosure, R would have to go hunting quite far to find package:base. 
#  There would be a big risk that another package has a function of the same name as a base function

# When R goes searching for the names in that expression, it first looks at the objects within the local environment. 
# If the object is not found by name in that environment, then R searches the enclosing environment of the local environment.
# If the object is not in the enclosure, then R searches the enclosure's enclosure, and so on. 
# R traverses the enclosing environments and stops at the first environment that contains the named object.
search() # moving to the right are the enclosing enviroments (parent environments)

## It's never possible to make a function completely self-contained, because you must always rely on
#   functions defined in base R or other packages .
#  R rely on lexical scoping to find everything !!!
f <- function() x + 1
x=1
f()
environment(f) <- emptyenv()
f()

## List of all enclosing environments of a function we  may pass through when called
allParents <- function(e) {
  r <- e # collection of environments we encounter} 
  EE <- emptyenv()
  while(!identical(EE, p <- parent.env(e))) {
    e <- p       # e := (new) current env.
    r <- c(r, e) # add to the collection
  }
  r <- c(r, EE)
  setNames(r, vapply(r, environmentName, ""))
}
myfun <- plot
ap1 <- allParents(environment(myfun))
str(ap1, give.attr=FALSE)

## Finds the "closest" environment where the object (the name) is defined
where1 <- function (name, env = parent.frame()) {
  stopifnot(is.character(name), length(name) == 1)
  if (identical(env, emptyenv())) {
    stop("Can't find ", name, call. = FALSE)
  }
  if (exists(name, env, inherits = FALSE)) { # Success case
    env
  }
  else { #Inspect parents (recursively)
    where1(name, env= parent.env(env)) # reuse the function that is defining !
  }
}
where2 <- function(name, env = parent.frame()) {
  is_empty <- function(x) identical(x, emptyenv())
  while(!is_empty(env)) {
    if (exists(name, env, inherits = FALSE)) {   # Success case
      return(env)
    }
    #Else inspect the following parent environment iteratively
    env <- parent.env(env)
  }
}
x <- 10
pryr::where("x")
where1("x")
where2("x")

## Finds all the parent environments where the object (the name) is defined
all_where <- function(name, env = parent.frame()) {
  is_empty <- function(x) identical(x, emptyenv())
  list_env <- list(NA)
  i=1L
  while(!is_empty(env)) {
    if (exists(name, env, inherits = FALSE)) {   # Success case
      list_env[[i]] <- env
      i <- i + 1L
    }
    #And inspect the following parent environment until the baseenv()
    env <- parent.env(env)
  }
  list_env
}
x <- 10
all_where("x")  # find()
 
## Skip the search-and-find
?`::`
`::`
`:::`
# - If the object is exported --> Reference it directly using the :: operator
# --> Look for function in the package environment (only exported objects)
sd 
stats::sd 
# - If the object is not exported or you are unsure --> Use the ::: operator
# --> Look for function in the namespace environment 
Wilks 
stats:::Wilks

###################################
## Search functions for R objects #
###################################
# List of R objects for each package  
ls.srch <- sapply(grep("package:", search(), value=TRUE), # retrieve the package names  
                  ls, all.names = TRUE)                   # and all objects in each of them 
# List of R functions for each package   
fn.srch <- sapply(ls.srch, function(nm) { # for each package 
  nm[sapply( lapply(nm,get)   # generate a list with each R object 
             , is.function) ] # test if the R object is a function and return it 
})        
# Proportion of objects and functions in each package 
rbind(cbind(ls   = (N1 <- sapply(ls.srch, length)),
            funs = (N2 <- sapply(fn.srch, length))),
      TOTAL = c(sum(N1), sum(N2))) -> res
res

## Find a function with specified name
fget1 <- function (name, env = parent.frame()) {
  if (identical(env, emptyenv())) {
    stop("Could not find function called ", name, call. = FALSE)
  }
  if (exists(name, env, inherits = FALSE) && is.function(env[[name]])) {
    env[[name]]
  }
  else {
    fget1(name, parent.env(env))
  }
}
pryr::fget("mean")
fget1("mean")
# Retrieve an R object (also from a Namespace)
getAnywhere("mean")  
# Retrieve the arguments of any R object that is a functions (also from Namespace)
argsAnywhere("mean")   

##########################################################################################
# Check for conflicts or double objects #
#########################################
mydnorm <- function(z)  1/sqrt(2 * pi) * exp(- z^2 / 2)
mydnorm(-2:2) 
x <- seq(-5,5, length=100)
all.equal(dnorm(x), mydnorm(x))

# The function mydnorm is not so safe...
# - pi is searched first in the global environment ...
pi <- 3
mydnorm(-2:2) # output change 
find("pi") # where "pi" variables are defined 

#Should define the function in this way
mydnorm <- function(z) {
  pi <- base::pi
  1/sqrt(2 * pi) * exp(- z^2 / 2)
}
pi <- 3
mydnorm(-2:2)

## Look for conflicts
search() # list of packages loaded
conflicts() # reports on objects that exist with the same name in two or more places 
find("pi") # return a character vector giving the names of all objects in search() matching "what"pi"
## Why to build a package ??
# - Complete protection of the functions  
# - Avoid re-definitions of objects in "globalenv" or other packages !!!

########################################################################################3
## Check if two objects refer to the same place in memory ##
############################################################
is.sameobject <- function(x, y)
{
  f <- function(x) capture.output(.Internal(inspect(x)))
  all(f(x) == f(y))
}

# Objects in namespace and package environments are pointers to the same object 
statsPackageEnv <- as.environment( "package:stats" ) 
statsNamespaceEnv <-  asNamespace( "stats" ) 
nsSd = get( "sd" , envir = statsPackageEnv , inherits = FALSE )
pkgSd = get( "sd" , envir = statsNamespaceEnv , inherits = FALSE )
is.sameobject(nsSd, pkgSd) #TRUE --> nsSd and pkgSd are pointers to the same object
# These are not the same object ! 
is.sameobject(1:5, 1:5)    # FALSE
# But j and k are the same object ! (pointer to the same memory location)
j <- rnorm(1e7); 
k <- j
is.sameobject(j,k) 
# But when I modify k, the whole vector k needs to get copied over to a new location
# PS: k <- j operation is much faster than modifying a single element of k !
k[1] <- 1  






########################################################################################### ??????
?Reduce
Funcall <- function(f, ...) f(...)
## n-fold iterate of a function, functional style:
Iterate <- function(f, n = 1) {
        function(x) {
          Reduce(Funcall, rep.int(list(f), n), x, right = TRUE)
        }
}
lapply(seq_along(search()),  function(n) Iterate(parent.env, n=n) (globalenv()))


nth.G.parent <- function(n, env) Iterate(parent.env, n=n) (env)
lapply(seq_along(search()), nth.G.parent, env = globalenv())

dropP <- function(x) { 
    attr(x, "path") <- NULL 
    x 
}
nth.G.parent <- function(n, env) dropP(Iterate(parent.env, n=n) (env))

lapply(seq_along(search()), nth.G.parent, env = globalenv())

 
