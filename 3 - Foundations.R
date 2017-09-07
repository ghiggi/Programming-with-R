## In R,
## Everything that **exists** is an object;
## Everything that **happens** is a function call  
 
#######
# c() #
#######
## Combine Values into a Vector or List
# - The output type is determined from the highest type of the components in the
#   hierarchy: NULL < raw < logical < integer < double < complex < character < list < expression. 
# - Pairlists are treated as lists
# - Non-vector components (such names and calls) are treated as one-element lists 
#    which cannot be unlisted even if recursive = TRUE 
c
?c
class(c(NULL,TRUE))
typeof(c(NULL,TRUE))
class(c(TRUE,3L))
typeof(c(TRUE,3L))
class(c(as.integer(3),5))
typeof(c(as.integer(3),5))
class(c(2,"a"))
typeof(c(2,"a"))

## Combine two lists together
ll <- list(A = 1, c = "C")
c(ll, d = list(1:3))  # c() combining two lists

# The following does not works correctly
# c(ll, d = 1:3) # c(ll, as.list(d = 1:3))
 
# Use "recursive = TRUE"
str(list(L1 = c(B = 1), L2 = c(D=1,E=2)))
str(c(list(L1 = c(B = 1), L2 = c(D=1,E=2)), recursive = TRUE))

L4 <- list(subList1 = c(3,4), subList2 = "a")
str(list(L1 = 200 , L2 = c(D=1,E=2), L3=L4))
str(c(list(L1 = 200, L2 = c(D=1,E=2), L3=L4), recursive = TRUE))   
 
str(options())
str(c(options(), recursive = TRUE))

###################
# [] [[ ]]  and $ #
###################
## Extract Parts of an Object
# [ can select more than one element 
# [[ and $ can select and return only a single element/value
`[`
`[[` 
`$`  # shorthand of `[[`
get("[")  #To avoid to use ``
get("[[") #To avoid to use ``
get("$")  #To avoid to use ``
?`[`
?`[[`
?`$`

methods("[")
methods("[[")
methods("$")

class(`[`)
class("[") #this is a character element
typeof(`[`)
typeof(`[[`)
typeof(`$`)

x <- 10:20
x[2]    
`[`(x, 2)
getElement(x, 2)

x[[2]]

?`[.data.frame` #method to handle indexing of specific classes of objects
?`[.factor`     #method to handle indexing of specific classes of objects
`[.factor`      #method to handle indexing of specific classes of objects
`[.data.frame`  #method to handle indexing of specific classes of objects

## Replace Parts of an Object
`[<-`
`[[<-` 
`$<-`  
 ?`[<-`      

methods("[<-")
methods("[[<-")
methods("$<-")

x <- 10:20; x[2]
 
`[<-`(x,2,13)  
x[2] <- 13    

## Difference between [[ and $
# `$`  does partial matching
# `[[` require exact matching (by default) !!!
var <- "cyl"
mtcars$cyl
mtcars[[var]]
mtcars$var # doesn't work - mtcars$var translated to mtcars[["var"]]
var <- "cy"
mtcars$cy
mtcars[[var]]
mtcars[[var, exact=FALSE]]

###############################
## Internal Generic function ##
############################### 
# -->  function calls of .Primitive("....")
?InternalMethods       
names
`names<-`
get("names<-")  # alternative to avoid to write ``
dim
`dim<-`

rm(x)
x <- 1:10
x
get("names<-")(x,letters[1:10] ) # Do the same
setNames(x,letters[1:10])        # Do the same
`names<-`(x,letters[1:10] )      # The names of x are not modified 
x  

names(x) <- letters[1:10]   # It modifies the names of x
x <- `names<-`(x,letters[1:10] ) 
x <- setNames(x,letters[1:10])
x

rm(D)
D  <- diag(4)
D
`dim<-`(D, NULL)     # The dimension of D doesn't change
D 
dim(D)


dim(D) <- NULL          # It modifies also the real dimension of D !!!
D <- `dim<-`(D, NULL)   # The same ...
D
dim(D)

`dim<-`(D, c(4,4))   # The dimension of D doesn't change
D 
dim(D) <- c(4,4)        # It changes the real dimension of D !!!
D <- `dim<-`(D, c(4,4)) # The same ... 
D
 
 






 

 
