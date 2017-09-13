library(dplyr)
library(nycflights13)
#########################
# dplyr's print options #
#########################
options(width = 45,
        dplyr.width = 44,
        dplyr.print_min = 4,
        dplyr.print_max = 4)

######################
# Manipulation verbs #
######################
# - arrange() to reorder the rows by one or more columns in ascending or descending order
# - rename() to rename a variable  
# - select() to select variables based on their names (return a subset of columns)
# - filter() to select cases based on their values/character/level (logical condition)
# - mutate() to add new (or transform existing) variables that are functions of existing variables.

# - summarise() to condense multiple values to a single value.

#########  
# Joins #
#########
# - inner_join(x,y) includes only  observations that match in both x and y 
# - full_join(x, y) includes all observations from x and y
# - left_join(x, y) includes all observations in x, regardless of whether they match or not in y 
# - right_join(x,y) includes all observations in y, regardless of whether they match or not in x
# - semi_join(x, y) keeps all observations in x that have a match in y.
# - anti_join(x, y) drops all observations in x that have a match in y.

##################
# dbplyr package #
##################
# - Allows to use the same verbs with a remote database. 
# - It takes care of generating the SQL for you so that you can avoid the cognitive
#   challenge of constantly switching between language

##################################
## Useful functions for vectors ##
##################################
### Vector subsetting 
# first() extract the first, last or nth value from a vector
# last()  extract the last value from a vector
# nth()	  extract the th value from a vector
x <- 1:10
first(x)
nth(x, 1)
last(x)
nth(x, -1) # last element
nth(x, -2) # second-last element
 
## n_distinct(x)
# - Count the number of unique values in a set of vector
# - Return the number of unique values in x
# - A faster and more concise equivalent of length(unique(x))
x <- 1:10
n_distinct(x)

#################################
## Useful functions for tibbles #
#################################
df <- tibble( x = sample(10, 100, rep = TRUE),
              y = sample(10, 100, rep = TRUE))
## all_equal()
# - Check if two dataframe are equal
# - Ignores ordering of rows and columns  
scramble <- function(x) x[sample(nrow(x)), sample(ncol(x))]
all_equal(mtcars, scramble(mtcars))

##  distinct()
# - Select distinct/unique rows
# - Similar to unique.data.frame(), but considerably faster
# - Variable to use when determining uniqueness in ...
# - Only the first row per combination are preserved 
nrow(df)
distinct(df, x) # Uniqueness in x
distinct(df, y) # Uniqueness in y 
nrow(distinct(df))       # Uniqueness in the combination of x and y
nrow(distinct(df, x, y)) # Uniqueness in the combination of x and y
# - To keep the other variables as well : keep_all = TRUE 
distinct(df, x, .keep_all = TRUE)
distinct(df, y, .keep_all = TRUE)

## sample_n() 
sample_n(df, 10)
sample_n(df, 50, replace = TRUE) # perform bootstrap sample 
sample_frac(mtcars, 0.1)
sample_frac(mtcars, 1.5, replace = TRUE)

## tally()
#  - count the number of occurences in a group 
#  - must be applied to already grouped data
#  - Is a wrapper for summarise that will either call n()  
flights %>%  group_by(dest, month) %>%
             tally
flights %>%  group_by(dest, month) %>%
             summarise(n())
## add_tally	
#  - Adds a column "n" to a table based on the number of items within each existing group
#  - Must be applied to already grouped data
flights %>%  group_by(dest, month) %>%
             add_tally

## count()
# - Count the number of occurences in a group 
# - short-hand for  group_by() + tally()
flights %>% count(dest, month)

## add_count()
#  - Adds a column "n" to a table based on the number of items within each existing group
#  - Does not require grouped data 
# - short-hand for group_by() + add_tally()
flights %>% add_count(dest, month) 

# top_n	
# - Wrapper that uses filter() and min_rank() to select the top or bottom entries
# - Select top (or bottom) n rows (by value)
# - If n is positive --> top n rows (by values) 
# - If n is negative --> bottom n rowes (byvalues) 
df <- data.frame(x = c(10, 4, 1, 6, 3, 1, 1))
df %>% top_n(2)
df %>% top_n(-2)

flights %>%  group_by(dest) %>%
  tally %>%
  top_n(10)

flights %>%  group_by(dest) %>%
  tally %>%
  top_n(-10)

################################################################################
## if_else()
# - if_else(condition, true, false, missing = NULL)
# - It checks that true and false are the same type.
x <- c(-5:5, NA)
if_else(x < 0, NA_integer_, x)
if_else(x < 0, "negative", "positive", NULL)
if_else(x < 0, "negative", "positive", "missing")
# - Unlike ifelse(), if_else() preserves types
x <- factor(sample(letters[1:5], 10, replace = TRUE))
ifelse(x %in% c("a", "b", "c"), x, factor(NA))
if_else(x %in% c("a", "b", "c"), x, factor(NA))
ifelse(x %in% c("a", "b", "c"), x, "2") # convert automatically to character vector
if_else(x %in% c("a", "b", "c"), x, 2)  # error : not same type (and/or length)

## case_when()
# - Vectorise multiple if and else if statements
# - Useful to create a new variable that relies on a complex combination of existing variables
x <- 1:50
case_when( x %% 35 == 0 ~ "fizz buzz",
           x %% 5 == 0  ~ "fizz",
           x %% 7 == 0  ~ "buzz",
           TRUE ~ as.character(x))

## na_if()
# - Convert a specific value to NA
y <- c("abc", "def", "", "ghi")
na_if(y, "")
x <- c(1, -1, 0, 10)
100 / x
100 / na_if(x, 0)

## coalesce() 
# - Given a set of vecotrs, finds the first non-missing value at each position
# - Use a single value to replace all missing values
x <- sample(c(1:5, NA, NA, NA))
coalesce(x, 0L)
# - Match together a complete vector from missing pieces
y <- c(1, 2, NA, NA, 5)
z <- c(NA, NA, 3, 4, 4) # substitute NA with values of this vector (at the same position) 
coalesce(y, z)

## recode()

################################################################################
