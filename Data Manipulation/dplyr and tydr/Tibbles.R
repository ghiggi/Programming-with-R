library(tibble)
##*********************************************************************************
# * Some functions don't work with tibble because of the `[` function 
# * When subsetting a data.frame with `[`, it can return a vector, while tibble always return a tibble
# *******************************************************************************
## Tibbles
# - are augmented lists
# - have class "tbl_df" + "tbl" + "data.frame", 
# - have names (column) and row.names attributes  
# - include class "data.frame" which means tibbles inherit the regular data frame behaviour by default.
# - never change an input's type (i.e., no more stringsAsFactors = FALSE!).
tb <- tibble::tibble(x = 1:5, y = 5:1)
typeof(tb)
attributes(tb)

# - It never adjusts the names of variables 
names(data.frame(`crazy name` = 1))
names(tibble(`crazy name` = 1))

# - It is possible for a tibble to have column names that are not valid R variable names.
#   For example, they might not start with a letter, or they might contain unusual characters like a space.
#   To refer to these variables, you need to surround them with backticks  
#   Backticks are also required when working with these variables on dplyr and tidyr
tb <- tibble(  `:)` = "smile", 
               ` ` = "space",
               `2000` = "number")
 
# - Never uses row.names(). Variable are not saved as a special attribute.
# - rownames_to_column() converts rownames to an explicit column variable

# - Evaluates arguments lazily and sequentially 
tibble(x = 1:5, y = x ^ 2)
 
# - When a tibble is printed, it shows the first ten rows and only the columns that fit on one screen.
# - Prints an abbreviated description of the column type
# * int stands for integers
# * dbl stands for doubles, or real numbers
# * chr stands for character vectors, or strings
# * lgl stands for logical, vectors that contain only TRUE or FALSE
# * fctr stands for factors, which R uses to represent categorical variables with fixed possible values
# * date stands for dates
# * dttm stands for date-times (a date + a time)

# - Options for printing  
n = 40
m = 20
options(tibble.print_max = n, tibble.print_min = m)  # If there are more than n rows, print only the first m rows. 
options(tibble.print_max = Inf) # show all rows
options(tibble.width = Inf)     # print all columns 

# - Subsetting tibbles always return tibbles (and not vectors !)
tibbles ignore the drop argument 
data.frame(a = 1:3)[, "a"]  
data.frame(a = 1:3)[, "a", drop=FALSE]   
tibble(a = 1:3)[, "a", drop = TRUE]

# - Tibbles never do partial matching
df <- data.frame(abc = 1)
df$a
df2 <- tibble(abc = 1)
df2$a

# - Only vector of length 1 (values) are recycled.
#   The first column with length different to one determines the number of rows in the tibble
tibble(a = 1:3, b = 1)
tibble(a = 1:3, c = 1:2)

# - Create tibbles with zero rows
tibble(a = integer(), b = 1)

# - Tibbles allow to have list-columns:
tibble(x = 1:3, y = list(1:5, 1:10, 1:20))

###############
# as_tibble() #
###############
# - Convert a data.frame to a tibbles object 
as_tibble(iris)  

# - Convert vectors or matrix much faster than as.data.frame
as_tibble(matrix(1:4, 2,2))

#############
# tribble() #
############# 
# Makes it possible to lay out small amounts of data in easy to read form.
# - Column headings (names) are defined by formulas (i.e. they start with ~)
# - Entries are separated by commas. This 
tribble(
  ~x, ~y, ~z,
  #--|--|----
  "a", 2, 3.6,
  "b", 1, 8.5
)