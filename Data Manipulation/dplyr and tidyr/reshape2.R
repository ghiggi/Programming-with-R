library(tidyr)
library(dplyr)
library(reshape2)
# Create data
set.seed(10)
dat <- data.frame(id = 1:4,
                    trt = sample(rep(c('control', 'treatment'), each = 2)),
                    work.T1 = runif(4),
                    home.T1 = runif(4),
                    work.T2 = runif(4),
                    home.T2 = runif(4))
###########
## melt() #
###########
# - Convert from wide to long format 
# - measure.vars : specifies the variables that should be gathered together to make a new variable
# - value.name : name of the new variable
# - variable.name : name of the factor variable 
# - id.vars : name of the ID variables (that are recycled) 
# - It is necessary to specify either id.vars or measure.vars 
# - By default, factors and character variables are automatically considered id variables
# - By default, all columns with numeric values are assumed variables with values.

gather(dat, key, value, -id, -trt) # - (select ID variables)
gather(dat, key, value, work.T1, home.T1,work.T2, home.T2)  
gather(dat, key, value, contains("T1"), contains("T2"))

melt(dat, variable.name = "key", value.name = "value",  id.vars = c("id", "trt"))
long_dat <- melt(dat, variable.name = "key", value.name = 'value',  measure.vars = c("work.T1", "home.T1","work.T2", "home.T2"))
 
# melt() can handle matrices or arrays 
# - Convert matrix/arrays in a data.frame with [row_idx, col_idx,(t_idx), value]
# - varnames : variable names to use in the molten data.frame (variable.name...) 
set.seed(3)
M <- matrix(rnorm(6), ncol = 3)
dimnames(M) <- list(letters[1:2], letters[1:3])
M
melt(M)
melt(M, varnames=c("Var1","Var2"), value.name = "value")

set.seed(3)
M <- array(rnorm(27), c(3,3,3))
melt(M)

################
## colsplit() ##
################
# - Separate characters into multiple variables 
separate(long_dat, col=key, into = c("location", "time"), sep = "\\.") 
tidy_dat <- cbind(long_dat[1:2], 
                  colsplit(string=long[, 3], pattern="\\.", names=c("location", "time")), 
                  long_dat[4])

############# 
## dcast() ##
#############
# Cast a molten data.frame into a wide-format dataframe 
# ID variables must all be declared in the formula (with +)
spread(long_dat, key=key, value=value)
dcast(long_dat, formula = id + trt ~ key, value.var = "value")
 
###########
# acast() #
###########
# Cast a molten data.frame into a 2D array
acast(long_dat, formula = id + trt ~ key, value.var = "value")
# Cast a molten data.frame into a 3D array
acast(long_dat, formula = id ~ trt ~ key, value.var = "value") 
acast(long_dat, formula = key ~ trt ~ id, value.var = "value") 

#################################
## Data aggregatopm with dcast ##
#################################
head(tips)
m.tips <- melt(tips)
head(m.tips)
dcast(m.tips, day+time ~ variable, mean)  
dcast(m.tips, smoker ~ variable, mean)
