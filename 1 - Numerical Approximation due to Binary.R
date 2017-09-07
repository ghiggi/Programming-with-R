###########################
# 10 * 0.1 is rarely 1.0  #
###########################
# The only numbers that can be represented exactly in R's numeric type are : 
# - integers 
# - fractions whose denominator is a power of 2
# --> All other numbers are internally rounded to 23 or 52 binary digits accuracy.

10 * 1/10 == 1.0     # this is rather lucky coincidence
1/10 + 2/10 == 3/10  # here we are less lucky
1/10  + 2/10  - 3/10 
.3 + .6 == .9
.3 + .6 - .9
# x1 and x2 are not always identical  
x1 <- seq(0, 1, by = 0.1)
x2 <- 0:10 / 10 

# ...it seems, that x2 is exact, but x1 is not 
n <- 0:10
rbind(x1 * 10 == n,
      x2 * 10 == n) 

# ... however, using x1 or x2 and subtracting consecutive numbers: 
d1  <- diff(x1)
d2  <- diff(x2)
unique(d1)
unique(d2) # ... there are 0.1 that differs from other 0.1. Why all that?
# Looking better
print(d1, digits=17)
print(d2, digits=17)

# The computer can:
# - only compute with finite precision,
# - uses binary representations of all the "numeric" (double) numbers.
print(x1, digits=17)
print(x2, digits=17)
 
# To be exact in usual computer arithmetic, the denominator must be 2^k (2,4,8,16,...)
n <- 20 # 500
ii <- 1:n
cbind(n=ii, "1/n + 2/n - 3/n" = sapply(1:20, function(n) 1/n + 2/n - 3/n))

n <- 500
ii <- 1:n
not.eq <- sapply(ii, function(n) (1/n + 2/n - 3/n) != 0) #TRUE ... if not equal 
are.eq <- sapply(ii, function(n) (1/n + 2/n - 3/n) == 0) #TRUE ... if equal
ii[are.eq]  # Select denominator that does not lead to numerical approximation
ii[not.eq]  # Select denominator that lead to numerical approximation

#############
# Take away #
#############
# Do not use '==' for numbers unless they are integer 
# Compare (vectors of) numbers with all.equal()
all.equal(x1, x2)
all.equal(x1, x2, tol = 1e-10)
all.equal(x1, x2, tol = 1e-15)
all.equal(x1, x2, tol = 0) ## -> shows the *relative* difference
tol = 1e-10
abs(x1 - x2) <= tol * abs((x1 + x2)/2)


 
 
 