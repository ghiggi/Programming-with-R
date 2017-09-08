library(mply)
###########
## m*ply ##   m*ply(.data, .fun = NULL, ...)
###########
## mlply 
# - Call a multi-argument function with values taken from columns of a data frame or array
# - Each row of the data.frame or array is provided to the function 
# - Arguments of the function that does not varies can be provided to ... 
args <- data.frame(n=c(10,50,100),
                   mean=c(5,5,10),
                   sd=c(1,1,2))
args1 <-  expand.grid(mean = 1:5, sd = 1:5)
args2 <-  expand.grid(n=seq(10,20,2),mean = 1:5, sd = 1:5)


l <- mlply(args,rnorm)
l1 <- mlply(args1,rnorm, n=10)
l2 <- mlply(args2,rnorm)

mlply(cbind(x=1:4, times = 4:1), rep)
mlply(cbind(from=1:4, to=4:1), seq)
mlply(cbind(from=1:4, length = 4:1), seq)
mlply(cbind(from=1:4, by = 4:1), seq, to = 20)

## maply()
## The output dimension of the function must be constant 
a1 <- maply(args1,rnorm, n=10)
dim(a1)

## mdply()
mdply(args1, rnorm, n = 2)
mdply(args1, as.data.frame(rnorm), n = 5)

