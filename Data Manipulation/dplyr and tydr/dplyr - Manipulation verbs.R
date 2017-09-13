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
# - Takes an existing tibble or data.frame and converts it into a grouped_df 
# - A grouped_df can be thought of as a list where each item in the list is a data.frame 
#   which contains only the rows that correspond to the level of the factor variable in group_by
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

# summarise(n=n()) perform same work as table()
mtcars %>%  group_by(cyl) %>%
            summarize(n = n())
table(mtcars$cyl)

mtcars %>% group_by(cyl) %>%
           select(mpg) %>%
           table()  

####################
## Scoped variants #
####################
# The variants suffixed with _if, _at or _all apply an expression (sometimes several)
#  to all variables within a specified subset.  
# The subset can contain :
# - all variables : _all variants , 
# - a vars() selection : _at variants
# - variables selected with a predicate : _if variants 

## Manipulation verbs suffixed with _all()
# - apply an operation on all variables

## Manipulation verbs suffixed with _at()
# - apply an operation on a subset of variables 
# - variables can be specified with 
#    * the quoting function vars() --> list of column symbols
#    * a integer vector of column position 
#    * a character vector of column names 

## Manipulation verbs suffixed with _if()
# - apply an operation on the subset of variables for which 
#   * a predicate function returns TRUE 
#   * a logical vector indicate which variables to select 

## select_  and rename_ 
rename_all(mtcars, toupper)
rename_all(mtcars, toupper)
rename_all(mtcars, "toupper")
rename_all(mtcars, funs(toupper(.)))
select_at(mtcars, vars(starts_with("d")))  
select_at(mtcars, vars(starts_with("d")), toupper)
rename_at(mtcars, vars(starts_with("d")), toupper)
is_whole <- function(x) all(floor(x) == x)
select_if(mtcars, is_whole)           # the renaming function is optional for select() 
select_if(mtcars, is_whole, toupper)  # drop unselected variables 
rename_if(mtcars, is_whole, toupper)  # maintain unselected variables 

## arrange_
arrange_all(mtcars, desc)
arrange_all(mtcars, funs(desc(.)))
arrange_at(mtcars, vars(starts_with("d")), desc)
arrange_at(mtcars, c("disp", "drat"), desc)
arrange_at(mtcars, c(4,5), desc)
arrange_if(mtcars, is_whole, desc)
arrange_if(mtcars, ~ all(floor(.) == .), desc)

## filter_ 
#  Scoped filter verbs require an expression with the pronoun `.` and replicate it over all selected variables. 
#  The predicate expression should be quoted with all_vars() or any_vars() 
all_vars(is.na(.))
any_vars(is.na(.))
# Take the intersection of the replicated expressions 
filter_all(mtcars, all_vars(. > 150))
# Take the union of the replicated expressions 
filter_all(mtcars, any_vars(. > 150))
# Select the columns on which to apply the predicate by their name (or position) 
filter_at(mtcars, vars(starts_with("d")), any_vars((. %% 2) == 0))
# Select variables with a predicate function:
filter_if(mtcars, ~ all(floor(.) == .), all_vars(. != 0))

## mutate_     
by_species <- iris %>% group_by(Species)
by_species %>% mutate_all(funs(. / 2.54))       # replace existing variables 
by_species %>% mutate_all(funs(cm = . / 2.54))  # add as new variables 
iris %>% mutate_at(vars(contains("Width"), contains("Length")), funs(. / 2.54))       # replace existing variables 
iris %>% mutate_at(vars(contains("Width"), contains("Length")), funs(cm = . / 2.54))  # add as new variables 
iris %>% mutate_if(is.numeric, funs(. / 2.54))       # replace existing variables 
iris %>% mutate_if(is.numeric, funs(cm = . / 2.54))  # add as new variables 
# mutate_if is particularly useful for transforming variables from one type to another
iris %>% as_tibble() %>% mutate_if(is.factor, as.character)
iris %>% as_tibble() %>% mutate_if(is.double, as.integer)
 
## summarise_   
by_species <- iris %>% group_by(Species)
by_species %>% summarise_all( funs(min, max))
by_species %>% summarise_all( c("min", "max"))
by_species %>% summarise_all( funs(med = median))
by_species %>% summarise_all( funs(Q3 = quantile), probs = 0.75)
by_species %>% summarise_at( vars(starts_with("Petal")), funs(med = median))
by_species %>% summarise_at( vars(Sepal.Length:Petal.Width), funs(med = median))
iris %>% summarise_if(is.numeric, mean, na.rm = TRUE)

## funs()  
# - Create a list of functions calls.
# - Generate a named list of functions for input to other functions
funs(mean, "mean", mean(., na.rm = TRUE))
funs(m1 = mean, m2 = "mean", m3 = mean(., na.rm = TRUE)) # Override default names
# - If function names  are in a vector, use funs_
fs <- c("min", "max")
funs_(fs)

## group_by_ 
# - have optional mutate semantics (shortcut for group_by() + mutate())
group_by_all(mtcars) # group a data frame by all variables
group_by_all(mtcars, as.factor) # convert all variable as factors and then group_by
group_by_at(mtcars, vars(vs, am))
group_by_if(iris, is.factor) # group a data frame by all factor variables 
group_by_if(iris, is.factor, as.character) # convert factor variable to character and then group_by

##########
## do() ##
##########
# - do() is similar to dlply())
# - create a tibble with a column-list 
mtcars %>% group_by(cyl) %>% do(head(.,2))
models <- mtcars %>% group_by(cyl) %>%
  do(lm = lm(mpg ~ wt, data = .))
models %>%  summarise(rsq = summary(lm)$r.squared)

models %>% do( data.frame(var = names(coef(.$lm)),
                          coefs= coef(summary(.$lm))))

## Create a tibble with a column-list of model fits 
library(mgcv)
by_dest <- flights %>% group_by(dest) %>% filter(n() > 100)
by_dest %>% do(smooth = gam(arr_delay ~ s(dep_time) + month, data = .))

# rowwise() 
# - Is used for the results of do() when you create list-variables
# - Group input by rows

