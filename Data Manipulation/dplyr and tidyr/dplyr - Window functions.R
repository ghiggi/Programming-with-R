####################
# Window functions #
####################
# - Take n inputs and returns n outputs. 
# - The output depends on all the input values
x = 1:20
x[16] <- NA
x <- x[sample(length(x))]
x
df <- data.frame(x) 

## Lag and lead functions 
lag(x)
lead(x)
##  Ranking and ordering functions 
min_rank(desc(x))
min_rank(x)
dense_rank(x)   # like min_rank(), but with no gaps between ranks
row_number(x)   # equivalent to rank(ties.method = "first")
percent_rank(x) # a number between 0 and 1 computed by rescaling min_rank to [0, 1]
cume_dist(x)    # cumulative distribution function (proportion of values less than or equal to the current value)
ntile(x, 5)     # rough rank, which breaks the input vector into 5 buckets.
filter(df,min_rank(desc(x)) < 2 | min_rank(x) < 2)  # to obtain extreme values 
filter(df, ntile(x, 2) == 2)

y <- c(1, 2, 1, 2, 2)
row_number(y)
min_rank(y)
dense_rank(y)

## Cumulative aggregates
# - Functions such : cumsum, cummin, cummax, cummean  
df <- data.frame(year = 2000:2005, value = (0:5) ^ 2)
scrambled <- df[sample(nrow(df)), ]
wrong <- mutate(scrambled, running = cumsum(value))
arrange(wrong, year)
right <- mutate(scrambled, running = order_by(year, cumsum(value)))
arrange(right, year)
# - cumall :  cumulative versions of &&
# - cumany :  cumulative versions of || 
df <- data.frame(x=c(1,1,1,rep(11,7)))
df1 <- data.frame(x=rev(c(1,1,1,rep(11,7))))
df
df1
filter(df, cumany(x>10))   # FALSE | TRUE --> TRUE 
filter(df, cumall(x>10))   # FALSE & TRUE --> FALSE
filter(df1, cumany(x>10))   # FALSE | TRUE --> TRUE 
filter(df1, cumall(x>10))   # FALSE & TRUE --> FALSE

## Rolling aggregates
# - windowed mean, median, sum, product, minimum, maximum, standard deviation, and variance
library(RcppRoll)
