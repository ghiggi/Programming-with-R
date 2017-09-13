library(tidyr)
library(dplyr)
## Tidy data 
# - Data are in "long format"
# - Each column is a variable
# - Each row is an observation
# - Data is generally taller and narrower. 
# - Data are in a compact form  
# - Are key enabler for data aggregations and visualization.
# - No conceptual variable is spread across multiple variables 
#   --> Create a factor variable and the fundamental key variable with gather()
vignette("tidy-data")

##  Untidy formats 
# - Data are in the "wide format"
# - Each row is often a site/subject/patient and you have multiple observation variables containing the same type of data
# - Typical for repeated observations over time, observation of multiple variables, or a mix of both
# - More attractive (intuitive) for data entry or examination
# - Storage is less efficient 

## Common problems 
# - Column names are not names of variables, but values of a variable 
#   If a column name is a number, it must be surrounded by backticks `..`
#   --> Use gather()
# -  Observations are scattered across multiple rows
#   --> Use spread() 

## gather() 
gap_wide <- read.csv("gapminder_wide.csv", stringsAsFactors = FALSE)
str(gap_wide)
gap_long <- gap_wide %>% gather(key=obstype_year, value=obs_values, 
                                 starts_with('pop'), starts_with('lifeExp'), starts_with('gdpPercap'))
gap_long <- gap_wide %>% gather(key=obstype_year, value=obs_values, -continent, -country)
str(gap_long)

## separate() 
# - Split a character string into multiple columns  
# - By defalut it split values wherever it sees a non-alphanumeric character (i.e. a character that isn't a number or letter)
# - sep : can be a regular expression or also a vector of integers which indicate the position to split at 
gap_long1 <- gap_long %>% separate(col=obstype_year, into=c('obs_type', 'year'), sep="_")
str(gap_long1)
gap_long1$year <- as.integer(gap_long1$year)
gap_long1 <- gap_long %>% separate(col=obstype_year, into=c('obs_type', 'year'), sep="_",convert=TRUE)
str(gap_long1)
## spread()
# - Spread a key-value pair across multiple columns
# - Spread tidy data to intermediate or wide format 
# - fill : allow to specify the value of missing value  
gap_intermediate <- gap_long1 %>% spread(key=obs_type, value=obs_values)
str(gap_intermediate)

## unite() 
# - Paste together multiple columns into one 
# - col : name of the new column 
gap_temp <- gap_long %>% unite(col=var_ID, continent, country, sep="_")
str(gap_temp)
gap_temp <- gap_long %>% unite(col=ID_var, continent, country, sep="_") %>%
                         unite(col=var_names, obs_type, year, sep="_")
str(gap_temp)

## Transform long format to wide format 
gap_wide_new <- gap_temp %>% spread(key=var_names, value=obs_values)
str(gap_wide_new)
gap_wide_betterID <- gap_wide_new %>% separate(col=ID_var, c("continent", "country"), sep="_")
str(gap_wide_betterID)

##############################
## Deal with missing values ##
##############################
# Explicit :  flagged with NA 
# Implicit :  simply not present in the data 
stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)
# Make implict missing values explicit
stocks %>% spread(key=year, value=return)
stocks %>% complete(year, qtr) # columns to expand
 
# Make explicit missing values implicit 
stocks %>%  spread(key=year, value=return) %>% 
            gather(key=year, value=return, `2015`:`2016`, na.rm = TRUE)

## complete()
# - Takes a set of columns, and finds all unique combinations. 
# - Ensures that the original dataset contains all values, filling in explicit NAs where necessary.
stocks %>% complete(year, qtr) # columns to expand

## Figure out which observations are missing 
experiment <- tibble( name = rep(c("Alex", "Robert", "Sam"), c(3, 2, 1)),
                      trt  = rep(c("a", "b", "a"), c(3, 2, 1)),
                      rep = c(1, 2, 3, 1, 2, 1),
                      measurment_1 = runif(6),
                      measurment_2 = runif(6))
# - Retrieve all possible combinations 
all <- experiment %>% expand(nesting(name, trt), rep)
all
# - We can use anti_join to figure out which observations are missing
all %>% anti_join(experiment)
# - Use right_join to add in the appropriate missing values to the original data
experiment %>% right_join(all)
# - Shorthand with complete()
experiment %>% complete(nesting(name, trt), rep)

## fill()
# - NA values are replaced by the most recent non-missing values (by row order) 
treatment <- tribble(
  ~ person,           ~ treatment, ~response,
  "Derrick Whitmore", 1,           7,
  NA,                 2,           10,
  NA,                 3,           9,
  "Katherine Burke",  1,           4
)
treatment %>% fill(person)

## drop_na()
# - Drop rows containing missing values
df <- tibble(x = c(1, 2, NA), y = c("a", NA, "b"))
df %>% drop_na()
df %>% drop_na(x)

## replace_na
# - Replace missing values 
df <- tibble(x = c(1, 2, NA), y = c("a", NA, "b"))
df %>% replace_na(list(x = 0, y = "unknown"))

## coalesce() 
# - Given a set of vecotrs, finds the first non-missing value at each position
# - Use a single value to replace all missing values
library(dplyr)
x <- sample(c(1:5, NA, NA, NA))
coalesce(x, 0L)
# - Match together a complete vector from missing pieces
library(dplyr)
y <- c(1, 2, NA, NA, 5)
z <- c(NA, NA, 3, 4, 4) # substitute NA with values of this vector (at the same position) 
coalesce(y, z)


## na_if()
# - Convert a specific value to NA
library(dplyr)
y <- c("abc", "def", "", "ghi")
na_if(y, "")
x <- c(1, -1, 0, 10)
100 / x
100 / na_if(x, 0)

################################################################################
## Other useful functions ##
############################
## separate_rows()
# - Separate a collapsed column into multiple rows
df <- data.frame( x = 1:3,
                  y = c("a", "d,e,f", "g,h"),
                  z = c("1", "2,3,4", "5,6"),
                  stringsAsFactors = FALSE)
separate_rows(df, z, convert = TRUE)
separate_rows(df, y, z, convert = TRUE)

## expand()
# - Return all possible combinations (also those not present in the data) 
# - Provide the complete set of data 
expand(mtcars, vs, cyl)

## nesting() 
# - It only keeps combinations of all variables that appear in the data.
expand(mtcars, nesting(vs, cyl))

## crossing()
# - Similar to expand.grid() 
# - Never converts strings to factors
 
## full_seq
# - Create the full sequence of values in a vector.
full_seq(c(1, 2, 4, 5, 10), period=1)

################################################################################
# nest	Nest repeated values in a list-variable.
# unnest	Unnest a list column.
# extract() 	Extract one column into multiple columns.
# readr::parse_number()







