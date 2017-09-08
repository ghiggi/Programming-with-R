#########
# r*ply # rlply(.n, .expr)
#########
# - Corresponds to replicate() in base
# - .expr is revaluated each time !!! 
# - Useful for drawing distributions of random numbers
# TODO : - https://github.com/hadley/plyr/pull/278/commits for parallel r*ply 
## rlply()
expr <- quote(lm(y ~ x, data=data.frame(x=rnorm(100), y=rnorm(100))))
mods <- rlply(100, eval(expr))
mods <- rlply(100, lm(y ~ x, data=data.frame(x=rnorm(100), y=rnorm(100)))) # Alterantive (all-in-one)

## raply()
# The array dimension is always n x .....
raply(100, mean(runif(100)))
raply(100, each(mean=mean, var=var)(runif(100)))
raply(10, runif(4))
hist(raply(1000, mean(rexp(1000)))) # central limit theorem  

raply(10, matrix(runif(10), nrow=2,ncol=5)) #dim(n) x dim(nrow) x dim(ncol) 

## rdply()
rdply(20, mean(runif(100)))
rdply(20, each(mean, var)(runif(100)))

rdply(20, data.frame(x = runif(2))) # nrow=20*2, ncol=2 ... "x" and ".n" 