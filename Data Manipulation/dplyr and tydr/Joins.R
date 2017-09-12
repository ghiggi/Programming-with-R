library(dplyr)
library(nycflights13)
weather
airlines 
airports
planes
flights
flights2 <- flights %>% select(year:day, hour, origin, dest, tailnum, carrier)

#####################
## Relational data ##
#####################
# Verify that the primary keys uniquely identify each observation.
planes %>% 
  count(tailnum) %>% 
  filter(n > 1)
weather %>% 
  count(year, month, day, hour, origin) %>% 
  filter(n > 1)
# Check that none of the variables in the primary key are missing

# If a table lacks a primary key, it's sometimes useful to add one ("surrogate key")
# - mutate()
# - row_number()

## Coercition rule with factors 
# - Factors with different levels are coerced to character with a warning
# - Factors with the same levels in a different order are coerced to character with a warning
# - A factor and a character are coerced to character with a warning:
# - Factors are preserved only if the levels match exactly:

###################
## Mutating joins #
###################
# - Add new variables to one table from matching rows in another.
# - Each mutating join takes an argument by that controls which variables are 
#   used to match observations in the two tables
# * by = NULL (default): use all common variables that appear in both tables (natural join) 
# * by = "primary_key":  use the primary key (or surrogate key) to merge the tables 
# * by = c(x_name=y_name"):  match variable x_name in table x with with variable y_name in table y 

## inner_join(x, y)
# - Only includes observations that match in both x and y 
# - Return rows from x that have matching values in y, and all columns from x and y. 
# - If there are multiple matches between x and y, all combination of the matches are returned. 
df1 %>% inner_join(df2, by = "x") 

### Outer joins 
## full_join(x, y)
# - Includes all observations from x and y
# - Return all rows and all columns from both x and y.
# - Where there are not matching values, returns NA for the one missing. 
 
## left_join(x, y)
# - Includes all observations in x, regardless of whether they match or not in y 
# - Return all rows from x, and all columns from x and y. 
flights2 %>% left_join(airlines)
flights2 %>% left_join(weather)
flights2 %>% left_join(planes, by="tailnum") # year columns in the output are disambiguated with a suffix ( x and y)
flights2 %>% left_join(airports, c("dest" = "faa"))  # match variable a in table x with variable b in table y
flights2 %>% left_join(airports, c("origin" = "faa"))  # match variable a in table x with variable b in table y
# - If there are multiple matches between x and y, all possible combination of the matches are returned. 
df1 <- data_frame(x = c(1, 1, 3, 4), y = 1:4)
df2 <- data_frame(x = c(1, 1, 2), z = c("a", "b", "a"))
df1 %>% left_join(df2)

## right_join(x, y)
# - Includes all observations in y, regardless of whether they match or not in x
# - Return all rows from y, and all columns from x and y. 
# - If there are multiple matches between x and y, all combination of the matches are returned. 
df1 %>% right_join(df2)

## merge()
# - base implementation for mutating joins...but much slower !!!!

###################
# Filtering joins #
################### 
# - Filter observations from one table based on whether or not they match an observation in the other table.


## semi_join(x, y)
# - Keeps all observations in x that have a match in y.
# - Return rows from x that have matching values in y,, keeping just columns from x. 
df1 %>% semi_join(df2, by = "x")

## anti_join(x, y)
# - Drops all observations in x that have a match in y.
# - Return rows from x that do not have matching values in y, keeping just columns from x. 

# Diagnosing join mismatches
flights %>% 
  anti_join(planes, by = "tailnum") %>%   # flights which doesn't have tailnum in planes 
  count(tailnum, sort = TRUE)             # count for each tailnum the number of flights 
####################
## Set operations ##
####################
# Combine the observations in the data sets as if they were set elements.
# - intersect(x, y): return only observations in both x and y
# - union(x, y):     return all (unique) observations in x and y
# - setdiff(x, y):   return observations in x, but not in y 

df1 <- data_frame(x = 1:2, y = c(1L, 1L))
df2 <- data_frame(x = 1:2, y = 1:2)
df1
df2

intersect(df1, df2)
union(df1, df2)
setdiff(df1, df2) 
setdiff(df2, df1)

####################################
## Bind rows, columns, and vectors #
####################################
library(dplyr)
## bind_rows()
one <- mtcars[1:4, ]
two <- mtcars[11:14, ]
bind_rows(one, two)
bind_rows(list(one, two), list(two, one)) #content of lists is automatically spliced 
# - Allow mixing of vectors and dataframes
bind_rows(c(a = 1, b = 2),                     
          data_frame(a = 3:4, b = 5:6),
          c(a = 7, b = 8))
# - Columns don't need to match when row-binding
bind_rows(data.frame(x = 1:3), data.frame(y = 1:4))

## bind_cols()
bind_cols(one, two)
bind_cols(list(one, two))

##  combine() 
# - Acts like c() or unlist() but uses consistent dplyr coercion rules.
f1 <- factor("a")
f2 <- factor("b")
c(f1, f2)
unlist(list(f1, f2))
combine(f1, f2)       # warning because levels are not the same 
combine(list(f1, f2)) # warning because levels are not the same 

