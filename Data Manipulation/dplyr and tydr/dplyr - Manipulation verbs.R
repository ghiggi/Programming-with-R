library(dplyr)
library(nycflights13)
##############
## Arrange() #
##############
# - Reeorder the rows by one or more columns in ascending or descending order
# - If more than one column name is provided, each additional column is used 
#   to break ties in the values of preceding columns
# - Missing values are always sorted at the end
arrange(flights, year, desc(month))

############
# rename() #
###########
# - Used to rename some variables 
# - All variables are kept
#. # * rename() keeps all variables
rename(iris, petal_length = Petal.Length)
 
############ 
# select() #
############ 
# - Select variables based on their names (return a subset of columns)
# - Expects column names or positions
select(flights, year) # column name
select(flights, 1)    # position
select(flights, c("year","month")) # column name (characters should not be used)
select(flights, year:day) # select all columns between year and day
select(flights, year:3)

# Drop/Omit multiple variables with -c(...,...)
select(flights, -c(1:3))
select(flights, -c(year,month,day))
select(flights, -c("year","month","day")) # This does not work !!!!!

# Change the order of some column and bring them to the front of the tibble
select(flights, time_hour, air_time, everything())

# Use selection helpers to find the variables
select(flights, starts_with("dep")) # select columns which name begin with "dep".
select(flights, ends_with("delay")) # select columns which name end with "delay".
select(flights, contains("dep"))    # select columns which name contain "dep".

select(flights, matches("(.)\\1")) # selects variables that match a regular expression.
select(flights, matches(".de."))   # selects variables that match a regular expression.
# Select variables that matches V1, V2 and V3.  
df <- flights
names(df) <- paste0("V",1:length(names(df)))
names(df) 
select(df, num_range("V", 1:3)) 
# Select variables contained in the character vector
vars <- c("year","month","day")
select(flights, one_of(vars))
# Select() without selection helpers do not look for variable in the surrounding environments        
year <- 5
select(flights, year)   # year still refers to a column name 
select(flights, year:3) # year still refers to a column name 
# To use contextual (surronding environment) variables, selection helpers are required
# R use lazy evaluation, so you need to force evaluation using :
#  - selection helpers 
year <- "dep"
select(flights, starts_with(year))
#  - the identity() function
year <- 5
select(flights, identity(year)) # year refers to position 5
vars <- c("year", "month")
select(flights, vars, "day") # it works...because vars is not a column name ...but is unsafe !!!
year <- c("year", "month")
select(flights, year, "day") # it selects the column "year"
select(flights, identity(year), "day")
# - the `!!` operator tells dplyr to bypass the data frame and to directly look in the context
select(flights, !! year)

##############
## filter() ##
##############
# - Select rows based on their values/character/level (logical condition)
# - Can deal with any number of filtering condition.
# - Only select rows which condition is TRUE. It excludes both FALSE and NA values !!!
filter(air_quality, Day <5 & Solar.R >= 200)
filter(air_quality, Day %in% c(1,2))  
filter(air_quality, Month==8 | Wind < 5)
# To preserve missing values, ask for them explicitly
filter(air_quality, Day < 5 | !is.na(Day)) 
    
# Filter operators 
# -  >, >=, <, <=, != , == .
# - When comparing numbers, use near() instead of == (due to numerical approximations)
# - between(x, left , right) can also be useful to check if values fall in a specified range 
# Boolean operators  
# -  & , | , ! 
# -  x %in% y  # Select every row where x is one of the values in y
# De Morgan's law: 
# -  !(x & y) is the same as   !x | !y   
#  - !(x | y) is the same as   !x & !y
# && and || must not be used in filter (). They must be used only in conditional execution !!!

##############
## mutate() ##
##############
# - Allow to add new variables that are functions of existing variables
# - Allow also to transform existing variables  
# - All original variables are kept (execept if NULL ...)
# - Adds new columns at the end of your dataset
# - The processing function must return a vector with the same number of rows of the original df.
# - Length-1 vectors are recycled to match the number of rows
df <- select(flights, year:dep_time)
mutate(df, "year", 2)   #  "year" and 2 are recycled as a character and numeric vector (respectively)

# Create a new vector that we add to the data frame (must be same rows)
# - However is better to use cbind()
var <- seq(1, nrow(df))
mutate(df, new = var)   

# Combination of mutate() and ifelse() facilitates filtering right where it is needed 
# - Fast and powerful way of discarding certain data 
# - Useful for updating values depending on this given condition.
df <- mutate(df, dep_morning = ifelse(dep_time < 1200, "Yes", "No"), dep_morning=factor(dep_morning)) # Use variable just created
mutate(df, dep_lag = dep_time - lag(dep_time),  new_lag = dep_lag) # Use variable just created

# Drop variables by setting them to NULL
df
df %>% mutate(dep_morning = NULL)

# Quasiquotation.
# - Unquote quosures, which can refer to both contextual variables and variable names:
year <- 100
day= 5
mutate(flights, day = day * year)
mutate(flights, day = !! (day * year)) # use contextual variables 

###############
# transmute() #
###############
# Works as mutate but keep only the new variables created
transmute(flights,
          gain = arr_delay - dep_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours)
  
################
## group_by() ##
################
# - Takes an existing tibble or data.frame and converts it into a grouped tbl 
# - Following operations are performed "by group".
# - Grouping doesn't change how data looks 
# - group_by() overrides existing grouping

# First create (or select existing) variable and then group by 
df <- flights
group_by(df, month)
group_by(df, month = as.factor(month))
group_by(df, day_binned = cut(day, 3))

# Retrieve the grouping variables 
df_g <- group_by(df, year, month, day)
group_vars(df_g) # returns a character vector 
groups(df_g)     # return a list of symbols 

# Subsequent group_by() calls overrides existing grouping
df_g <- group_by(df_g, year, day)
group_vars(df_g)

## Removes grouping with ungroup() 
group_vars(ungroup(df_g))
 
# Each call to summarise() remove a layer of grouping 
# - When you group by multiple variables, each summary peels off one level of the grouping.
# - The summarise function applies within each level of the last grouping variable 
# - This makes it easy to progressively roll-up a dataset 
flights
daily <- group_by(flights, year, month, day)
(per_day   <- summarise(daily, flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
(per_year  <- summarise(per_month, flights = sum(flights)))

## Whenever you do any aggregation, it's always a good idea to include
# - count of all observation (NA included) in each group : n() 
# - count of non-missing values :   sum(!is.na(x))
# - count the number of distinct (unique) values/characters: n_distinct(x)
# --> In that way it is possible to check that conclusions are not based on very small amounts of data

#################
## Summarise() ##
#################
# - Condense multiple values to a single value.
# - Usually applied after that data that has been grouped by one or more variable
# - Provides aggregation to summarise each group.

# A summary applied to ungrouped tbl returns a single row
mtcars %>%  summarise(mean = mean(disp), n = n())

# Usually, you'll want to group first
mtcars %>%  group_by(cyl) %>% summarise(mean = mean(disp), n = n())

# Each summary call removes one grouping level (since that group become a single row)
mtcars %>%  group_by(cyl, vs) %>%
            summarise(cyl_n = n()) %>%
             group_vars()

# summarise(n=n()) perform same work as table 
mtcars %>%  group_by(cyl) %>%
            summarize(n = n())
table(mtcars$cyl)

####################
## Scoped variants #
####################
?scoped
group_by_at(df, vars(year:day))
group_by_if



 
# arrange_all	Arrange rows by a selection of variables
# arrange_at	Arrange rows by a selection of variables
# arrange_if	Arrange rows by a selection of variables
.by_group = TRUE, in which case it orders first by the grouping variables

 
# rename_all	Select and rename a selection of variables
# rename_at	Select and rename a selection of variables
# rename_if	Select and rename a selection of variables

 
# filter_all	Filter within a selection of variables
# filter_at	Filter within a selection of variables
# filter_if	Filter within a selection of variables

 
# mutate_all	Summarise and mutate multiple columns.
# mutate_at	Summarise and mutate multiple columns.
# mutate_if	Summarise and mutate multiple columns.
# - allows you to refer to columns that you've just created

 
# transmute_all	Summarise and mutate multiple columns.
# transmute_at	Summarise and mutate multiple columns.
# transmute_if	Summarise and mutate multiple columns.

 
# summarise_all	Summarise and mutate multiple columns.
# summarise_at	Summarise and mutate multiple columns.
# summarise_if	Summarise and mutate multiple columns.
# summarize	Reduces multiple values down to a single value
# summarize_all	Summarise and mutate multiple columns.
# summarize_at	Summarise and mutate multiple columns.
# summarize_if	Summarise and mutate multiple columns.

 
# group_by	Group by one or more variables
# group_by_all	Group by a selection of variables
# group_by_at	Group by a selection of variables
# group_by_if	Group by a selection of variables

column-wise operations: mutate_each, summarise_each

summarize_each() applies the same summary function(s) to multiple variables.
my_gap %>%
  filter(year %in% c(1952, 2007)) %>%
  group_by(continent, year) %>%
  summarise_each(funs(mean, median), lifeExp, gdpPercap)

# vars	Select variables
#  Quasiquotation. You can unquote raw expressions or quosures:
var <- quo(mean(cyl))
summarise(mtcars, !! var)

