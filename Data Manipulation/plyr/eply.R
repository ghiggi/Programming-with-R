library(plyr)
############
## eply() ##
############
# - Combining plyr and expand.grid.
# - Simon Barthelmé, University of Geneva
elply <- function(vars, fun, ..., .progress="none", .parallel=FALSE) {
  df <- do.call("expand.grid",vars)
  if (all(names(vars) %in% names(formals(fun))))  {
    #We assume that fun takes the variables in vars as named arguments
    funt <- function(v,...)    {
      do.call(fun,c(v,list(...)))
    }
    res <- alply(df,1,funt,...,.progress=.progress,.parallel=.parallel)
  }
  else  {
    #We assume that fun takes a named list as first argument
    res <- alply(df,1,fun,...,.progress=.progress,.parallel=.parallel)
  }
  res
}

edply <- function(...){
  res <- elply(...)
  plyr:::list_to_dataframe(res,attr(res, "split_labels"))
}

# Examples 1
library(ggplot2)
fun <- function(x, freq,phase) {
  data.frame( x=x, value=sin(freq*x-phase))
}
x <- seq(0,2*pi,l=100);
d <- edply(list(freq=c(1,2,4,8),phase=c(0,1)),fun, x=x)
ggplot(d,aes(x,value,col=as.factor(phase)))+facet_wrap( ~ freq)+geom_path()
# Examples 2 
fun <- function(x,y) dnorm(x)*dnorm(y)*sin(x)
d <- edply(list(x=seq(-3,3,l=40),y=seq(-3,3,l=40)),fun)
ggplot(d,aes(x,y))+geom_raster(aes(fill=V1))
