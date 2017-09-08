library(plyr)
#########
# l*ply #  l*ply(.data, .fun, ...)
#########
## laply 
# - Similar to daply but function apply to each list element indipendently from the other list elements
# - The array dimension depends on the output dimension of the function 
# - .drop_o	= TRUE by default : Extra dimensions of length 1 are dropped, simplifying the output.  
laply(baseball, is.factor) # Logical
laply(baseball, function(x) 1)  # Numeric
laply(baseball, function(x) c(mean=mean(x), sd=sd(x))) # Matrix

# Create an array 
laply(seq_len(10), identity)   
laply(seq_len(10), rep, times = 4)
laply(seq_len(10), matrix, nrow = 2, ncol = 2)

## ldply  
# - Recreate a data.frame
l <- as.list(baseball)
a <- ldply(baseball, function(x) identity(x))
a <- as.data.frame(l)  
# - Apply function to list elements (must return same length vector for each list element!)
ldply(baseball[,2:3], function(x) mean(x))
ldply(baseball, is.factor)

## llply
# - Occurence of each factor / continous variable (rounded)
llply(llply(mtcars, round), table)
# - Summary for each list element
llply(baseball, summary)
