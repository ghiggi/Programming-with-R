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
# Unlike ifelse(), if_else() preserves types
x <- factor(sample(letters[1:5], 10, replace = TRUE))
ifelse(x %in% c("a", "b", "c"), x, factor(NA))
if_else(x %in% c("a", "b", "c"), x, factor(NA))
ifelse(x %in% c("a", "b", "c"), x, "2") # convert automatically to character vector
if_else(x %in% c("a", "b", "c"), x, 2)  # error : not same type (and/or length)

#---------------------------------------------------------------------------


year_country_gdp <- select(gapminder,year,country,gdpPercap)
year_country_gdp <- gapminder %>% filter(continent=="Europe")  %>%  select(year,country,gdpPercap) 

gdp_bycontinents <- gapminder %>%
  group_by(continent) %>%
  summarize(mean_gdpPercap=mean(gdpPercap)
            .... = mean(...))

# arrange() to reorder the rows 
# rename() to rename a variable  
# filter() to select cases based on their values/character/level
# select() to select variables based on their names.
# mutate() to add new variables that are functions of existing variables.
# summarise() to condense multiple values to a single value.
# sample_n() and sample_frac() to take random samples.

transmute()
# n()                          # count number of observations
# count(x, group_by sort=TRUE) # count the number of occurences 
# mutate()   add new variable to the df 

# combination of mutate() and ifelse() facilitates filtering right where it is needed: 
# Fast and powerful way of discarding certain data 
# Useful for updating values depending on this given condition.
mutate(gdp_billion = ifelse(lifeExp > 25, gdpPercap * pop / 10^9, NA)) 

 

################################
# 'grouped_df', 'tbl_df', 'tbl'#
################################

str(gapminder %>% group_by(continent))
A grouped_df can be thought of as a list where each item in the list is a data.frame 
which contains only the rows that correspond to the level of the factor variable in group_by







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

# select	Select/rename variables by name
# select_all	Select and rename a selection of variables
# select_at	Select and rename a selection of variables
# select_if	Select and rename a selection of variables
all columns between year and day year:day
# Execept: -(....)

# sample	Sample n rows from a table
# sample_frac	Sample n rows from a table
# sample_n	Sample n rows from a table
sample_n() for a fixed number and sample_frac() for a fixed fraction.
replace = TRUE to perform a bootstrap sample

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





# starts_with	Select helpers
# ends_with	Select helpers
# everything	Select helpers
# matches	Select helpers
# contains	Select helpers
# current_vars	Select helpers
# one_of	Select helpers
# select_helpers	Select helpers
# num_range	Select helpers



# When you group by multiple variables, each summary peels off one level of the grouping.
# That makes it easy to progressively roll-up a dataset:
# Very useful form for sums and counts,
# Require think about weighting for means and variances 
library(nycflights13)
flights
daily <- group_by(flights, year, month, day)
(per_day   <- summarise(daily, flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
(per_year  <- summarise(per_month, flights = sum(flights)))

# select() expects column names or positions
# `year` represents the integer 1
select(flights, year)
select(flights, 1)
year <- 5
select(flights, year) #still represent 1 
select(flights, year:2) #still represent 1 

# Refer to contextual (surronding environment) variables with selection helpers:
year <- "dep"
select(flights, starts_with(year))
select(flights, identity(year)) #dep

vars <- c("year", "month")
select(flights, vars, "day")

year <- c("year", "month")
select(flights, year, "day") # unsafe --> wrap the external variables with identity() 

unquote the variable with the !! operator. This tells dplyr to bypass the data frame and to directly look in the context
flights$vars <- flights$year
# The new column won't be an issue if you evaluate `vars` in the context with the `!!` operator:
# Quasiquotation of an expression
vars <- c("year", "month", "day")
select(flights, !! vars)



mutate() expects column vectors.
df <- select(flights, year:dep_time)
mutate(df, "year", 2)
# Must provide column symbols
# length-1 vectors are interpreted as new columns in the data frame.
# These vectors are recycled so they match the number of rows
mutate(df, "year" + 10) # ? error

# Create a new vector that we add to the data frame (must be same rows)
var <- seq(1, nrow(df))
mutate(df, new = var)  #  better to use cbind 


group_by()
has mutate semantics
it allows to group by a modified column
# first create (or substitute existing) variable and then group by 
group_by(df, month)
group_by(df, month = as.factor(month))
group_by(df, day_binned = cut(day, 3))

group_by_at() 
# select semantics
wrap the selection with vars()
group_by_at(df, vars(year:day))
group_by_if


?scoped


################
# Pipes # %>%  #
################
# - Replaces the first argument in the following function
# - The following function takes the object before the pipe
# - Order of operations  might be important 
# - %>% operator from magrittr. 
# - x %>% f(y) turns into f(x, y) 
# - Avoid the creation of many intermediate variables 

# Example 1
flights %>%
  group_by(year, month, day) %>%
  select(arr_delay, dep_delay) %>%
  summarise(
    arr = mean(arr_delay, na.rm = TRUE),
    dep = mean(dep_delay, na.rm = TRUE)
  ) %>%
  filter(arr > 30 | dep > 30)


# most of dplyr is equivalent to ddply() + various functions, 
# do() is equivalent to dlply())

#############
## Tibbles ##
#############
# Tibbles are the tidyverse implementation of dataframes. 
library(tibble)
as_tibble(iris)
tibble(x = 1:5, y = 1, z = x ^ 2 + y)
# Define a tibble row-by-row with tribble():
tribble(~x, ~y,  ~z,
        "a", 2,  3.6,
        "b", 1,  8.5
        )

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
# slice	Select rows by position
# top_n	Select top (or bottom) n rows (by value)
#---------------------------------------------------------------------------
## n_distinct(x)
# - Return the number of unique values in x
# - A faster and more concise equivalent of length(unique(x))


# all.equal.tbl_df	Flexible equality comparison for data frames
# all_equal	Flexible equality comparison for data frames
# all_vars	Apply predicate to all variables
# any_vars	Apply predicate to all variables

# cumall	Cumulativate versions of any, all, and mean
# cumany	Cumulativate versions of any, all, and mean
# cume_dist	Windowed rank functions.
# cummean	Cumulativate versions of any, all, and mean

# join	Join two tbls together
# anti_join	Join two tbls together
# full_join	Join two tbls together
# inner_join	Join two tbls together
# intersect	Set operations
# left_join	Join two tbls together
# right_join	Join two tbls together
# right_join.tbl_df	Join data frame tbls
# semi_join	Join two tbls together
# semi_join.tbl_df	Join data frame tbls
#---------------------------------------------------------------------------
# union	    Set operations
# union_all	Set operations
# setdiff	Set operations
# setequal	Set operations
# setops	Set operations

# lag	Lead and lag.
# lead	Lead and lag.
# lead-lag	Lead and lag.


 




# add_count	Count/tally observations by group
# add_tally	Count/tally observations by group
# tally	Count/tally observations by group
# count	Count/tally observations by group

# min_rank	Windowed rank functions.
# percent_rank	Windowed rank functions.
# dense_rank	Windowed rank functions.
# ntile	Windowed rank functions.
# ranking	Windowed rank functions.
# row_number	Windowed rank functions.


# case_when	A general vectorised if
# vars	Select variables
# ungroup	Group by one or more variables
# desc	Descending order
# distinct	Select distinct/unique rows
#---------------------------------------------------------------------------
# auto_copy	Copy tables to same source, if necessary
# copy_to	Copy a local data frame to a remote src

# between	Do values in a numeric vector fall in specified range?

# coalesce	Find first non-missing element

# do	Do anything

# is.tbl	Create a table from a data source
# n	The number of observations in the current group.
# na_if	Convert values to NA
# near	Compare two numeric vectors

# n_distinct	Efficiently count the number of unique values in a set of vector

# order_by	A helper function for ordering window function output

# pull	Pull out a single variable

# scoped	Operate on a selection of variables

# recode	Recode values
# recode_factor	Recode values

# explain	Explain details of a tbl
# show_query	Explain details of a tbl

# sql	SQL escaping.
# ident	Flag a character vector as SQL identifiers

# band_instruments	Band membership
# band_instruments2	Band membership
# band_members	Band membership

# collapse	Force computation of a database query
# collect	Force computation of a database query
# compute	Force computation of a database query

# src_dbi	Source for database backends
# src_mysql	Source for database backends
# src_postgres	Source for database backends
# src_sqlite	Source for database backends



