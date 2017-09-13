library(rlang)
library(dplyr)
# - Most dplyr functions use non-standard evaluation (NSE)
# - Most dplyr arguments are not referentially transparent 
# - dplyr function inputs are not evaluated, but quoted !  (by default)
df <- tibble(
  g1 = c(1, 1, 2, 2, 2),
  g2 = c(1, 2, 1, 2, 1),
  a = sample(5), 
  b = sample(5))

## Quoting
# - The action of capturing an expression instead of evaluating it.
# - quote() in base R capture only the expression (not the environment!)
# - quo() in rlang package capture the expression and its current environment 
# - quo() generate quosures, which capture the expression and track the environments

## Evaluate expressions
# - eval() in base R
# - eval_tidy() in rlang package
# - eval_tidy() know how to evaluate quosures 

f <- function(x) {
  quo(x)
}
x1 <- f(10)   # is a quosure 
x2 <- f(100)  # is a quosure 
get_env(x1)
get_env(x2)
eval_tidy(x1)
eval_tidy(x2)

f1 <- function(x) {
  quote(x)
}
x1 <- f1(10)  # is a name (symbol)
x2 <- f1(100) # is a name (symbol)
eval(x1)
eval(x2)

# Symbols does not keep track of an enviroment ! 
# Quosures keep track of the environment (are closure)! 
expr <- quo(x) 
get_expr(expr) # extract the expression (the symbol/name)
get_env(expr)  # extract the environment

## quo() around a function call is a very useful tool for debugging.
my_var = quo(a)
quo(summarise(df, mean = mean(!!my_var),
              sum = sum(!!my_var),
              n = n()))

################
## Unquoting  ##
################
# -  UQ() or !!
# - the variable is evaluated 
quo(toupper(letters[1:5]))
quo(toupper(!!letters[1:5])) # capture the value of `letters[1:5]`
quo(toupper(UQ(letters[1:5])))

var1 <- quo(letters[1:5])
quo(toupper(!!var1))
 
my_var <- quo(x)
quo(filter(df, !!my_var == 1))
quo(filter(df, UQ(my_var) == 1))
quo(filter(df, UQE(my_var) == 1)) # UQE ignore the environment (useful to define new variable name...) 

## UQS or !!! (unquote-splicing)
# - Takes a vector and inserts each element of the vector in the surrounding function call:
x <- letters[1:5]
quo(list(!! x))
quo(list(!!! x))
# - Takes a list of elements and splices them into to the current call 
x <- list(foo = 1L, bar = quo(baz))
quo(list(!!! x))  # vector name become argument names 
 
args <- list(na.rm = TRUE, trim = 0.25)
quo(mean(x, !!! args))

args <- list(quo(x), na.rm = TRUE, trim = 0.25)
quo(mean(!!! args))

args <- list(mean = quo(mean(cyl)), count = quo(n())) 
mtcars %>%  group_by(am) %>%
            summarise(!!! args)

my_summarise <- function(df, group_var) {
  df %>%
    group_by(!! group_var) %>%     # evaluate the input with `!!``
    summarise(a = mean(a))
}
my_summarise(df, quo(g1))         

# enquo() 
# - Evaluate the function argument and quote it (creating a quosure)
# - To be used inside functions
# - Works similarly to substitute() in base R 
# 1. Takes a symbol referring to a function argument, 
# 2. Quotes the R code that was supplied to this argument
# 3. Captures the environment where the function was called  
# 4. Bundles that environment in a quosure.
 
my_summarise <- function(df, group_var) {
  group_var <- enquo(group_var)    # evaluate group_var and then quote (with enquo())
  df %>%
    group_by(!! group_var) %>%
    summarise(a = mean(a))
}
my_summarise(df, g1)

my_summarise1 <- function(df, expr) {
  expr <- enquo(expr)    # evaluate group_var and then quote (with enquo())
  summarise(df, 
            mean = mean(!!expr),
            sum = sum(!!expr),
            n = n()
  )
}
my_summarise1(df, a*b)

my_mutate <- function(x) {
  x <- enquo(x)
  mtcars %>%
    select(cyl) %>%
    slice(1:4) %>%
    mutate(cyl2 = cyl + (!! x))
}
my_mutate(100)

## `:=`
# - Allow to set variable names 
mean_nm <- "mean"
count_nm <- "count"
mtcars %>% group_by(am) %>%
           summarise(!!mean_nm := mean(cyl),
                     !!count_nm := n())
       
## quo_name() 
# - Convert an expression to a string 
my_mutate <- function(df, expr) {
  expr <- enquo(expr)
  mean_name <- paste0("mean_", quo_name(expr))
  sum_name <- paste0("sum_", quo_name(expr))
  mutate(df,  !!mean_name := mean(!!expr), 
              !!sum_name := sum(!!expr)
  )
}
my_mutate(df, a) 

## .data() avoid that a is searched in another environment 
mutate_y <- function(df) {
  mutate(df, y = .data$a + .data$x)
}
## quos() 
# -  Accept any number of expression and ...
# - Returns a list of quosures
# - The environments bundled to quosures are the ones where the code was supplied as arguments

## Capturing multiple variables using quos() and !!!
my_summarise <- function(df, ...) {
  group_var <- quos(...)
  
  df %>%
    group_by(!!!group_var) %>%
    summarise(a = mean(a))
}

my_summarise(df, g1, g2)






 
 

 





 
