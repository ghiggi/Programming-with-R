library(abind)
 

## abind()
# - Combine multi-dimensional arrays.
# - Generalization of rbind() and cbind()

## acorn() or ahead()
# - Return a small corner of an array object 
# - Take a few slice on each dimension 
x<- array(1:24,dim=c(4,3,2),dimnames=rev(list(letters[1:2],LETTERS[1:3],letters[23:26])))
acorn(x)
acorn(x, n=6, m=5,r=2)

## adrop()
# - Drop dimensions of an array object

## afill()
# - Fill an array with subarrays 
 
## asub()
# - Allow arbitrary subsetting of array-like objects at specified indices
 
## aperm()
# - Transpose an array by permuting its dimensions 
