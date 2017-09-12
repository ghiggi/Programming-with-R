## funs()  
# - Create a list of functions calls.
# - Generate a named list of functions for input to other functions
funs(mean, "mean", mean(., na.rm = TRUE))
funs(m1 = mean, m2 = "mean", m3 = mean(., na.rm = TRUE)) # Override default names
# - If function names  are in a vector, use funs_
fs <- c("min", "max")
funs_(fs)


#  Quasiquotation. You can unquote raw expressions or quosures:
var <- quo(mean(cyl))
summarise(mtcars, !! var)