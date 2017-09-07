## Memory profiling - tracking how your code uses memory.
# These include Rprofmem(), tracemem(), and Rprof(memory.profiling=TRUE)

# Accidental copies are a major cause of slow code
# Understand when object are copied is very important for writing efficient code

## object_size()
# - allow to see how much memory an object occupies
library(pryr)
object_size(1:10)
object_size(mean)
object_size(mtcars)

## mem_used()
# - shows the total size of all objects in memory 
library(pryr)
mem_used()

## mem_change()
# - informs about how memory changes during code execution 
library(pryr)
mem_change( x <- 1:1e6)
mem_change( rm(x)) # get the memory back ... 
# Even operations that don't do anything use up a little memory 
# --> R is tracking the history of everything you do
library(pryr)
mem_change(NULL)  

## address()
# - tell the variable's location in memory
library(pryr)
x <- 1:10
address(x)
## refs()
# - tell how many names points to that location 
# - in RStudio, refs() may wrongly return  +1
#   --> The environement browser make a reference to every object create on the command line
library(pryr)
x <- 1:10
refs(x)
y <- x
refs(x)
rm(y)
refs(x)# should return 1 because y has been removed 
############################# 
## Memory usage of vectors ##
#############################
object_size(list())
object_size(logical())
object_size(numeric())
object_size(raw())
# Every vector has at least the following components :
# - object metadata (4 byte)  
# - 2 pointers (2*8 bytes) on next and previous object in memory
# - 1 pointer to attributes (8 bytes)
# - length of the vector (4 bytes)
# - the "true" length of the vector (4 bytes) 
# Every length 0 vector occupies 40 bytes memory
#   --> 4 remaining bytes are used for padding so that each component start on a 
#       8 byte (64-bit) boundary

# Integer vectors occupy  4 bytes for every element
# Numeric vectors occupy  8 bytes for every element 
# Complex vectors occupy 16 bytes for every element 
sizes <- sapply(0:50, function(n) object_size(seq_len(n)))
plot(0:50, sizes, xlab = "Length", ylab = "Size (bytes)", 
     type = "s")

plot(0:50, sizes - 40, xlab = "Length", 
     ylab = "Bytes excluding overhead", type = "n")
abline(h = 0, col = "grey80")
abline(h = c(8, 16, 32, 48, 64, 128), col = "grey80")
abline(a = 0, b = 4, col = "grey90", lwd = 4)
lines(sizes - 40, type = "s")
# Having to request memory every timea small vector is created  would slow down R considerably 
# --> R ask big block of memory and then manages that block itself --> small vector pool 
# --> For vectors less than 128 bytes, the small vector pool is used and R allocates 
#      vectors that are 8,16,32,48,64 or 128 bytes long
# For vectors larger than 128 bytes, R ask for memory in multiples of 8 bytes 


# Components can be shared across multiple object 
# - R is smart enough to not copy x, but pointing to existing x 
x <- 1:1e6
object_size(x)
y <- list(x, x, x)
object_size(y)
object_size(x, y) # space that x and y occupy together
# - R is not smart enough to not copy the element 
x1 <- 1:1e6
y1 <- list(1:1e6, 1:1e6, 1:1e6)
object_size(x1)
object_size(y1)
object_size(x1, y1)
object_size(x1) + object_size(y1) == object_size(x1, y1)

# With strings, R takes up less memory than you might expect becose
# it tends to store unique strings in a "global string pool"
object_size("banana")
object_size(rep("banana", 10))

######################
# Garbage collection #
######################
## A memory leak occurs when you keep pointing to an object without realising it
# --> Can happen when using formulas and closures because they both capture the 
#     enclosing environment 

# Create a big object
mem_change(x <- 1:1e6)
mem_change(y <- x) # - 4MB  ???????? whyyy ????????????????????????????????????????????????????????
# Remove x does not free the memory because y is still pointing to it 
mem_change(rm(x))
# If all objects that points to x are removed, memory is freed
mem_change(rm(y))

###########################
## Modification in place ##
###########################
# - Any primitive replacement function modify in place provided that the object
#   is not referred to elsewhere 
library(pryr)

x <- 1:10
c(address(x), refs(x))
x[5] <- 10    # copy x, modifies the copied x and the name x to point to the new location 
c(address(x), refs(x))
y <- x
c(address(y), refs(y))

x <- list(a=1:10, b=1:2) # With list, modification in place !!!
c(address(x), refs(x))
x$a <- 1:10       
c(address(x), refs(x))
x[[2]] <- 3:4        
c(address(x), refs(x))
x[[2]][1:2] <- 1:2   
c(address(x), refs(x))

# tracemem() 
# - print a message every time the traced object is copied
# - allow to understand when R modifies in place and when R modifies a copy 
library(pryr)
x <- 1:10
tracemem(x)
x[5] <- 10    # is not modified in place ! A copy is created 
 
x <- matrix(1:9,3,3)
tracemem(x)
x[2,2] <- 10  # is not modified in place ! A copy is created 
 
# Modifying a list uses primitive functions, so all modifications occur in place !!!
y <- as.list(x)
tracemem(y)
for(i in 1:5) {
  y[[i]] <- y[[i]] - 2
  print(c(address(y), refs(y)))
}

x <- list(a=1:10, b= matrix(1:9,3,3))
tracemem(x)
x$a <- 1:10 
x[[1]] <- 2:4
x$b[1,2] <- 1  # also modification of a matrix inside a list does not lead to copies !!! ?????
 
zz <- vector("list",3)
tracemem(zz)
zz[[1]] <- 5
zz[[2]] <- "a"
zz[3] <- list("a") 

# Modifying a dataframe don't use a primitive functions --> every time a copy 
x <- data.frame(matrix(runif(100 * 1e4), ncol = 100))
tracemem(x)
x[1,3] <- 2  
x[2,2] <- 2
medians <- vapply(x, median, numeric(1))
for(i in 1:5) {
    x[, i] <- x[, i] - medians[i]
    print(c(address(x), refs(x)))
}

## Improve dataframe subsetting 
x <- data.frame(matrix(runif(1000 * 1e4), ncol = 1000))

sub1 <- function(x) {
  # tracemem(x)
  medians <- vapply(x, median, numeric(1))
  for(i in 1:5) {
    x[, i] <- x[, i] - medians[i]
    # print(c(address(x), refs(x)))
  }
  x
}
sub2 <- function(x) {
  l <- as.list(x)
  #tracemem(l)
  medians <- vapply(l, median, numeric(1))
  for(i in 1:5) {
    l[[i]] <- l[[i]] - medians[i]
    #print(c(address(l), refs(l)))
  }

}
 
library(microbenchmark)
microbenchmark(sub1(x),
               sub2(x))
