library(magrittr)
################
# Pipes # %>%  #
################
# Help to write code in a way that easier to read and understand
# Avoid function arguments to be spread far apart
# Most useful for rewriting a fairly short linear sequence of operations
# - The magrittr R package contain the pipe operator %>%
# - %>%  is used to connect one command to another. 
# - The output of one command becomes the input for the next command
# - Allow to don't save the output at each intermediate step
# - Avoid the creation of many intermediate variables 
# - Order of operations  might be important 
# Rule 1
f(xs)
xs %>% f
# Rule 2
f(xs, y=5)
xs %>% f(y=5)
# Rule 3
g(f(xs, y=5), n=2)
xs %>% f(y=5) %>% g(n=2)
# Rule 4
# - If input is not the first argument,  .  is used as an argument placeholder.
# - Placeholder says where the piped input should land. 
f(y, x)
x %>% f(y, .)
# Rule 5
f(y, z = x)
x %>% f(y, z = .)

# Subsetting dataframe with pipes
df <- data.frame(x=1:5,y=1:5)
df %>% .$x
df %>% .[["x"]]

# Example with with dyplr manipulation verbs
library(nycflights13)
flights %>%
  group_by(year, month, day) %>%
  select(arr_delay, dep_delay) %>%
  summarise(arr = mean(arr_delay, na.rm = TRUE),
            dep = mean(dep_delay, na.rm = TRUE)) %>%
  filter(arr > 30 | dep > 30)
  print(n = 10, width = Inf)

##############################################
# Create a function chain that can be reused #
##############################################
# The following pipeline describes a function chain that can be saved and re-used
num_unique <- . %>% unique %>% length
num_unique 
 
############################
## When pipes don't work ! #
############################
## Pipe won't work for 2 classes of functions !!!!!
# - Functions that use the current environment (i.e assign, get, load)
x <-10
x
"x" %>% assign(100) # assign work in the temporary environment of %>%
x
env <- environment()
"x" %>% assign(100, envir = env) # now works because we provide env explicitly
x
# - Functions that use lazy evaluation (i.e. try(), suppressMessages(), and suppressWarnings())
tryCatch(stop("!"), error = function(e) "An error")
stop("!") %>%   tryCatch(error = function(e) "An error")