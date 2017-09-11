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



assembly: bind_rows, bind_cols
column-wise operations: mutate_each, summarise_each
join tables together: left_join, right_join, inner_join, full_join
filtering joins: semi_join, anti_join
do: arbitrary code on each chun




#dpylr's print method
options(
  width = 45,
  dplyr.width = 44,
  dplyr.print_min = 4,
  dplyr.print_max = 4)

#tally is a shortcut for counting number of items per group. 
flights %>%
  group_by(dest, month) %>%
  tally


##########
## do() ##
##########
# sample_n() and sample_frac() to take random samples.

 
# n()                          # count number of observations
# count(x, group_by sort=TRUE) # count the number of occurences 
# mutate()   add new variable to the df 



  %>%
  print(n = Inf)   # to print all ! 

################################
# 'grouped_df', 'tbl_df', 'tbl'#
################################

str(gapminder %>% group_by(continent))
A grouped_df can be thought of as a list where each item in the list is a data.frame 
which contains only the rows that correspond to the level of the factor variable in group_by









?scoped
# arrange	Arrange rows by variables
# arrange.grouped_df	Arrange rows by variables
# arrange_all	Arrange rows by a selection of variables
# arrange_at	Arrange rows by a selection of variables
# arrange_if	Arrange rows by a selection of variables
.by_group = TRUE, in which case it orders first by the grouping variables


# rename	Select/rename variables by name
# rename_all	Select and rename a selection of variables
# rename_at	Select and rename a selection of variables
# rename_if	Select and rename a selection of variables

# filter	Return rows with matching conditions
# filter_all	Filter within a selection of variables
# filter_at	Filter within a selection of variables
# filter_if	Filter within a selection of variables

# mutate	Add new variables
# mutate_all	Summarise and mutate multiple columns.
# mutate_at	Summarise and mutate multiple columns.
# mutate_if	Summarise and mutate multiple columns.
# - allows you to refer to columns that you've just created

# transmute	
# - works as mutate but create a new tibble dataframe
# transmute_all	Summarise and mutate multiple columns.
# transmute_at	Summarise and mutate multiple columns.
# transmute_if	Summarise and mutate multiple columns.

# summarise	Reduces multiple values down to a single value
# summarise_all	Summarise and mutate multiple columns.
# summarise_at	Summarise and mutate multiple columns.
# summarise_if	Summarise and mutate multiple columns.
# summarize	Reduces multiple values down to a single value
# summarize_all	Summarise and mutate multiple columns.
# summarize_at	Summarise and mutate multiple columns.
# summarize_if	Summarise and mutate multiple columns.

# groups	Return grouping variables
# group_by	Group by one or more variables
# group_by_all	Group by a selection of variables
# group_by_at	Group by a selection of variables
# group_by_if	Group by a selection of variables
# group_vars	Return grouping variables
# rowwise	Group input by rows

# sample	Sample n rows from a table
# sample_frac	Sample n rows from a table
# sample_n	Sample n rows from a table
sample_n() for a fixed number and sample_frac() for a fixed fraction.
replace = TRUE to perform a bootstrap sample



# most of dplyr is equivalent to ddply() + various functions, 
# do() is equivalent to dlply())

# slice	Select rows by position
# top_n	Select top (or bottom) n rows (by value)

## n_distinct(x)
# - Return the number of unique values in x
# - A faster and more concise equivalent of length(unique(x))

# all_equal	Flexible equality comparison for data frames
# all_vars	Apply predicate to all variables
# any_vars	Apply predicate to all variables

# lag	Lead and lag.
# lead	Lead and lag.
# lead-lag	Lead and lag.

# add_count	Count/tally observations by group
# add_tally	Count/tally observations by group
# tally	Count/tally observations by group
# count	Count/tally observations by group

# case_when	A general vectorised if
# vars	Select variables
# ungroup	Group by one or more variables
# desc	Descending order
# distinct	Select distinct/unique rows

# between	Do values in a numeric vector fall in specified range?
# coalesce	Find first non-missing element
# n	The number of observations in the current group.
# na_if	Convert values to NA
# near	Compare two numeric vectors
# n_distinct	Efficiently count the number of unique values in a set of vector
# order_by	A helper function for ordering window function output
# pull	Pull out a single variable
# scoped	Operate on a selection of variables
# recode	Recode values
# recode_factor	Recode values


##########
# dbplyr #
##########
# dplyr also allows you to use the same verbs with a remote database. 
# It takes care of generating the SQL for you so that you can avoid the cognitive
#  challenge of constantly switching between language
#---------------------------------------------------------------------------


# tbl_cube	A data cube tbl
# as.table  Coerce a 'tbl_cube' to other data structures
# as.tbl	Create a table from a data source
# as.tbl_cube	Coerce an existing data structure into a 'tbl_cube'

#---------------------------------------------------------------------------

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


 
 



