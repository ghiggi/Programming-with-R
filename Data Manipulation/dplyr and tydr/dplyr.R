library(dplyr)

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
n_distinct(x)


# distinct(.data, ...)
# - Select distinct/unique rows
# - Similar to unique.data.frame(), but considerably faster
# - Variable to use when determining uniqueness in ...
# - Only the first row per combination are preserved 
df <- tibble(
  x = sample(10, 100, rep = TRUE),
  y = sample(10, 100, rep = TRUE)
)
nrow(df)

distinct(df, x) # Uniqueness in x
distinct(df, y) # Uniqueness in y 
nrow(distinct(df))       # Uniqueness in the combination of x and y
nrow(distinct(df, x, y)) # Uniqueness in the combination of x and y

# To keep the other variables as well : keep_all = TRUE 
distinct(df, x, .keep_all = TRUE)
distinct(df, y, .keep_all = TRUE)

## funs()  
# - Create a list of functions calls.
# - Generate a named list of functions for input to other functions
funs(mean, "mean", mean(., na.rm = TRUE))
funs(m1 = mean, m2 = "mean", m3 = mean(., na.rm = TRUE)) # Override default names
# - If function names  are in a vector, use funs_
fs <- c("min", "max")
funs_(fs)

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




if_else(), recode(), case_when()
na_if(), coalesce()
#---------------------------------------------------------------------------

######################
# Manipulation verbs #
######################
# - arrange() to reorder the rows by one or more columns in ascending or descending order
# - rename() to rename a variable  
# - select() to select variables based on their names (return a subset of columns)
# - filter() to select cases based on their values/character/level (logical condition)
# - mutate() to add new (or transform existing) variables that are functions of existing variables.

# - summarise() to condense multiple values to a single value.




#dpylr's print method
options(
  width = 45,
  dplyr.width = 44,
  dplyr.print_min = 4,
  dplyr.print_max = 4)



# sample_n() and sample_frac() to take random samples.
# sample	Sample n rows from a table
# sample_frac	Sample n rows from a table
# sample_n	Sample n rows from a table
sample_n() for a fixed number and sample_frac() for a fixed fraction.
replace = TRUE to perform a bootstrap sample
 
 

# slice	Select rows by position
# top_n	Select top (or bottom) n rows (by value)

# all_equal	Flexible equality comparison for data frames
# all_vars	Apply predicate to all variables
# any_vars	Apply predicate to all variables

# add_count	Count/tally observations by group
# add_tally	Count/tally observations by group
# tally	Count/tally observations by group
# count	Count/tally observations by group
# count(x, group_by sort=TRUE) # count the number of occurences 
#tally is a shortcut for counting number of items per group. 
flights %>%
  group_by(dest, month) %>%
  tally


# order_by	A helper function for ordering window function output
# pull	Pull out a single variable



##########
## do() ##
##########
# do() is equivalent to dlply())
# rowwise() # group by row with rowwise.
do: arbitrary code on each chunk





 


################################
# 'grouped_df', 'tbl_df', 'tbl'#
################################

# - A grouped_df can be thought of as a list where each item in the list is a data.frame 
#   which contains only the rows that correspond to the level of the factor variable in group_by
 

# tbl_cube	A data cube tbl
# as.table  Coerce a 'tbl_cube' to other data structures
# as.tbl	Create a table from a data source
# as.tbl_cube	Coerce an existing data structure into a 'tbl_cube'


 



##########
# dbplyr #
##########
# dplyr also allows you to use the same verbs with a remote database. 
# It takes care of generating the SQL for you so that you can avoid the cognitive
#  challenge of constantly switching between language


#---------------------------------------------------------------------------
join tables together: left_join, right_join, inner_join, full_join
filtering joins: semi_join, anti_join
# join	Join two tbls together
# anti_join	Join two tbls together
# full_join	Join two tbls together
# inner_join	Join two tbls together
# intersect	Set operations
# left_join	Join two tbls together
# right_join	Join two tbls together
# semi_join	Join two tbls together
 
#---------------------------------------------------------------------------
# union	    Set operations
# union_all	Set operations
# setdiff	Set operations
# setequal	Set operations
# setops	Set operations

#---------------------------------------------------------------------------


assembly: bind_rows, bind_cols

 
 



