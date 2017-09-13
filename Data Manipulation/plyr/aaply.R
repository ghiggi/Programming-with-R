library(plyr)
# Function to return shape of an object: Dimensions, or length
shape <- function(x) {
  if (is.vector(x)) length(x) 
  else dim(x)
}
shape(x)

#########
# a*ply #   a*ply(.data, .margins, .fun, ..., .progress = "none")
#########
## aaply()
## Array output dimension:
# - The dimensions are included in the output array after the split dimensions.
# - If the object returned by the processing function
#   - is an array, its dimensions are included in the output array after the split dimensions.
#   - is a list, the output will be a list-array 

# For 2D arrays, matrix and dataframe 
# .margin = 1  : apply the function rowise
# .margin = 2  : apply the function colwise
# .margin = c(1,2) : apply the function at each cell    
x <-  matrix(0, nrow=2, ncol=3)
shape(x)
# - Processing function output is 1 value 
shape(aaply(x, 1, function(y) 0))   # dim(1)
shape(aaply(x, 2, function(y) 0))   # dim(2)
shape(aaply(x, c(1,2), function(y) 0))   # dim(1) x dim(2)
# - Processing function output is a vector 1xp   
p = 5
shape(aaply(x, 1, function(y) rep(1,p))) # dim(1) x dim(p) 
shape(aaply(x, 2, function(y) rep(1,p))) # dim(2) x dim(p) 
shape(aaply(x, c(1,2), function(y) rep(1,p))) # dim(1) x dim(2) x dim(p)
# - Processing function output is an matrix nxp  
n = 5
p = 6
shape(aaply(x, 1, function(y) matrix(0,nrow=n,ncol=p))) # dim(1) x dim(n) x dim(p)
shape(aaply(x, 2, function(y) matrix(0,nrow=n,ncol=p))) # dim(2) x dim(n) x dim(p) 
shape(aaply(x, c(1,2), function(y) matrix(0,nrow=n,ncol=p))) # dim(1) x dim(2) x dim(n) x dim(p)
# - Processing function output is an array nxpxt 
n = 5
p = 6
t = 7
shape(aaply(x, 1, function(y) array(0,5:7))) # dim(1) x dim(n) x dim(p) x dim(t) 
shape(aaply(x, 2, function(y) array(0,5:7))) # dim(2) x dim(n) x dim(p) x dim(t)
shape(aaply(x, c(1,2), function(y) array(0,5:7))) # dim(1) x dim(2) x dim(n) x dim(p) x dim(t)
# - Processing function output is a list 
shape(aaply(x, 1, function(y) list(y=mean(y))))  # dim(1)               class --> list 
shape(aaply(x, 2, function(y) list(y=mean(y))))  # dim(2)               class --> list 
shape(aaply(x, 1, function(y) list(y=mean(y)),.drop=FALSE))# dim(1)x1   class --> matrix (each cell is a list)
shape(aaply(x, c(1,2), function(y) list(y=mean(y)))) # dim(1) x dim(2)  class --> matrix 

# For 3D arrays 
# - "The additional output dimensions are added at the end" 
# - .drop = FALSE :  add a x1 dimension  at the end. If TRUE it simplify the output
# .margin = 1  : extract row 2-D vertical slice
# .margin = 2  : extract column 2-D vertical slice 
# .margin = 3  : extract horizontal 2-D slice 
# .margin = c(1,2) : apply the function to each vertical slice
# .margin = c(2,3) : apply the function to each horizontal column slice
# .margin = c(1,3) : apply the function to each horizontal rows slice
# .margin = c(1,2,3) : apply the function to each array cell 
x <-  array(1:24, 2:4)
shape(x)
# - Processing function output is 1 value 
shape(aaply(x, 1, function(y) 0))   # dim(1)  --> class numeric
shape(aaply(x, 2, function(y) 0))   # dim(2)
shape(aaply(x, 3, function(y) 0))   # dim(3)
shape(aaply(x, 1, function(y) 0, .drop=FALSE))   # dim(1)x1 -->> class matrix 
shape(aaply(x, c(1,2), function(y) 0)) # dim(1) x dim(2)  
shape(aaply(x, c(1,3), function(y) 0)) # dim(1) x dim(3)
shape(aaply(x, c(2,3), function(y) 0)) # dim(2) x dim(3)  
shape(aaply(x, c(3,2), function(y) 0)) # dim(3) x dim(2)  
shape(aaply(x, c(1,2,3), function(y) 0)) # dim(1) x dim(2) x dim(3)
shape(aaply(x, c(1,3,2), function(y) 0)) # dim(1) x dim(3) x dim(2)
# - Processing function output is a vector 1xp   
p = 5
shape(aaply(x, 1, function(y) rep(1,p))) # dim(1) x dim(p) 
shape(aaply(x, 2, function(y) rep(1,p))) # dim(2) x dim(p) 
shape(aaply(x, 3, function(y) rep(1,p))) # dim(2) x dim(p)
shape(aaply(x, c(1,2), function(y) rep(1,p))) # dim(3) x dim(2) x dim(p) 
shape(aaply(x, c(1,3), function(y) rep(1,p))) # dim(1) x dim(3) x dim(p) 
shape(aaply(x, c(2,3), function(y) rep(1,p))) # dim(2) x dim(3) x dim(p) 
shape(aaply(x, c(1,2,3), function(y) rep(1,p))) # dim(1) x dim(2) x  dim(3) x dim(p) 
# - Processing function output is an matrix nxp  
n = 5
p = 6
shape(aaply(x, 1, function(y) matrix(0,nrow=n,ncol=p))) # dim(1) x dim(n) x dim(p)
shape(aaply(x, 2, function(y) matrix(0,nrow=n,ncol=p))) # dim(2) x dim(n) x dim(p) 
shape(aaply(x, 3, function(y) matrix(0,nrow=n,ncol=p))) # dim(3) x dim(n) x dim(p)

shape(aaply(x, c(1,2), function(y) matrix(0,nrow=n,ncol=p))) # dim(1) x dim(2) x dim(n) x dim(p)
shape(aaply(x, c(2,3), function(y) matrix(0,nrow=n,ncol=p))) # dim(2) x dim(3) x dim(n) x dim(p)
shape(aaply(x, c(1,3), function(y) matrix(0,nrow=n,ncol=p))) # dim(1) x dim(3) x dim(n) x dim(p) 
shape(aaply(x, c(1,2,3), function(y) matrix(0,nrow=n,ncol=p))) # dim(1) x dim(2) x dim(3) x dim(n) x dim(p)
# - Processing function output is an array nxpxt 
n = 5
p = 6
t = 7
shape(aaply(x, 1, function(y) array(0,5:7))) # dim(1) x dim(n) x dim(p) x dim(t) 
shape(aaply(x, c(1,2), function(y) array(0,5:7))) # dim(1) x dim(2) x dim(n) x dim(p) x dim(t)
shape(aaply(x, c(1,2,3), function(y) array(0,5:7))) # dim(1) x dim(2) x dim(3) x dim(n) x dim(p) x dim(t)
# - Processing function output is a list  
shape(aaply(x, 1, function(y) list(y=mean(y))))  # dim(1)                 class --> list 
shape(aaply(x, 1, function(y) list(y=mean(y)),.drop=FALSE)) # dim(1)x 1   class --> matrix (each cell is a list)
shape(aaply(x, c(1,2), function(y) list(y=mean(y))))   # dim(1) x dim(2)  each array cell contains a list 
shape(aaply(x, c(1,2,3), function(y) list(y=mean(y)))) # dim(1) x dim(2) x dim(3) 

# With spatio-temporal data (lat x long x time)
# - 1 : latitudinal statistics 
# - 2 : longitudinal statistics
# - 3 : global (2D space) statisics 
# - c(1,2) : temporal statistics (at each location)
# - c(1,3) : latitudinal statistics (at each time step)
# - c(2,3) : longitudinal statistics (at each time step)

# TODO
# - aaply() with cell neighborhood 
# with 4 dimension 
# - spatio temporal ensembles 
# - multivariate spatio-temporal ensembles 
# 

# Examples 
aaply(ozone, 1, each(min, max)) # Compute latitudinal statistics
aaply(ozone, 3, each(min, max)) # Compute global statistics 

standardize <- function(x) (x - mean(x)) / sd(x)
normalize <- function(x) (x - min(x)) / (max(x) - min(x))
aaply(ozone, 1:2, standardize) # standardize each time series
aaply(ozone, 1:2, normalize)   # standardize each time series
aaply(ozone, 1:2, diff)        # Differencing each time series

## alply()
#  - To use when error (or no output) can occur in the processing functions 
alply(ozone, 3, function(x) table(round(x))) # Occurence of values at given time (at all locations )

## adply() 

