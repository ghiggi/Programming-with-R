#Typical bottlenecks that C++ can help with are:
#  
# #  Loops that can't easily be vectorised because each iteration depends on the previous. 
# C++ modifies objects in place, so there is little overhead when modifying a data structure many times.
# 
# Recursive functions, or problems which involve calling functions millions of times. 
# The overhead of calling a function in C++ is much lower than the overhead of calling a function in R.

inline for automated wrapping of simple expression
Rcpp for easing the interface between R and C++
  
  
  
  
  R offers several functions to access compiled code: .C and
.Fortran as well as .Call and .External

conv <- function(a, b) .Call("convolve2", a, b)




## A simple Fortran example
# inline is a package by Oleg Sklyar et al that provides the function
# cfunction that can wrap Fortran, C or C++ code
# cfunction takes care of compiling, linking, loading, . . . by placing
# the resulting dynamically-loadable object code in the per-session
# temporary directory used by R
 code <??? " integer i
           do 1 i =1, n (1)
            1 x ( i ) = x ( i )??????3
         "
cubefn <??? cfunction ( signature (n=" integer " , x=" numeric " ) ,
                         code , convention=" . Fortran " )
x <??? as . numeric (1:10)
n <??? as . integer (10)
 cubefn (n , x ) $x
 
 
 # 
 # using .Call is that vectors (or matrices)
 # can be passed directly between R and C++ without the need for
 # explicit passing of dimension arguments. And by using the C++ class
 # layers, we do not need to directly manipulate the SEXP objects
 # 
 # Rcpp eases data transfer from R to C++, and back. We always
 # convert to and from SEXP, and return a SEXP to R
 # 
 # We pass data from R via named lists that may contain different types
 # by initialising a RcppParams object and extracting as in
list(intnb=42, fltnb=6.78, date=Sys.Date(),  txt="some thing", bool=FALSE)
RcppParams param(inputsexp);
int nmb = param.getIntValue("intnb");
double dbl = param.getIntValue("fltnb");
string txt = param.getStringValue("txt");
bool flg = param.getBoolValue("bool";
RcppDate dt = param.getDateValue("date");

# we never have to deal with dimensions and / or memory
# allocations - all this is shielded by C++ classes.
# 
# Similarly, for the return, we declare an object of type
# RcppResultSet and use the add methods to insert named
# elements before coverting this into a list that is assigned to the
# returned SEXP.
# Back in R, we access them as elements of a standard R list by
# position or name










 