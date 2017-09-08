library(plyr)
##########
#  d*ply #   d*ply(.data, .variables, .fun, ..., .progress = "none")
##########
# - If single variable, the df is split into groups defined by the levels of that variable.
# - If multiple variables, the groups will be formed by the interaction of the factor variables
#   Output will be labelled with all three variables.
# .variable : .(fac1, fac2)  
#             = c("fac1", "fac2")
#             ~ fac1 + fac2
#             .(round(var1)) # considered as factor
#          
# Create a data.frame 
df <- data.frame( group1 = c(rep('A', 8), rep('B', 15), rep('C', 6)),
                  group2 = sample(c("M", "F"), size = 29, replace = TRUE),
                  var1 = runif(n = 29, min = 18, max = 54),
                  year = 100 +1:29,
                  var3 = rnorm(n = 29))
# ddply()
# - Similar to aggregate()  
# - Grouping by factor variables 
ddply(df, .(group1, group2), summarize,  mean = round(mean(var3), 2),
      sd = round(sd(var3), 2))
ddply(df, ~ group1 + group2, summarize,  mean = round(mean(var3), 2),
      sd = round(sd(var3), 2))
ddply(df, ~ group1 * group2, summarize,  mean = round(mean(var3), 2),
      sd = round(sd(var3), 2))
ddply(df, c("group1","group2"), summarize,  mean = round(mean(var3), 2),
      sd = round(sd(var3), 2))
# - Grouping using a continous variable (converted to factor)
ddply(df, .(round(var2)), summarize,  mean = round(mean(var3), 2))
ddply(df, .(round(var2)), numcolwise(mean))

# Examples 
ddply(df, .(group1), c("nrow", "ncol"))
ddply(df, .(group1), mutate, var4 = year - min(year)+1) 
ddply(df, .(group1), transform, var4 = year - min(year)+1)  

# Modify values in a dataframe based on groups
df <- melt(ozone) 
ddply(df, .(lat, long), transform, time = sample(time))
ddply(df, .(lat, long), transform, value = scale(value))
# Group-wise summaries  
ddply(df, .(lat, long), summarise, ozone_min = min(value), ozone_max = max(ozone)) # create a new df
ddply(df, .(lat, long), mutate, ozone_min = min(value), ozone_max = max(ozone))    # add column to existing df 
# Group-wise subsetting  
ddply(df, .(lat, long), subset, value == min(value))



# daply()  
# - Similar to aggregate() when the function operate column-wise 
# - The output array gets a dimension for each split .variable, labelled by levels of those variables

# - 1 grouping and 1 output value  -->   1xnlevels(group1) 
daply(baseball, .(year), nrow)

# - 1 grouping and p output values -->   nxp 
daply(baseball[, c(2, 6:9)], .(year), colwise(mean))    # year included in .data
daply(baseball[, 6:9], .(baseball$year), colwise(mean)) # year not included in .data
daply(baseball, .(year), function(df) colwise(mean)(df[, 6:9]))
# - 2 grouping and p output values -->  nlevels(group1) x nlevels(group2) x p
baseball$year1 <- baseball$year
a <- daply(baseball[, c(2, 6:9,23)], .(year,year1), colwise(mean)) 
dim(a)
# - 3 grouping and p output values -->  nlevels(group1) x nlevels(group2) x nlevels(group3) x p
baseball$year2 <- baseball$year
a <- daply(baseball[, c(2, 6:9,23,24)], .(year,year1,year2), colwise(mean)) 
dim(a) 

## Application for spatial data  
ozone_df <- reshape2::melt(ozone)
# - Compute mean spatial field
ozone_mean_a <-  daply(ozone_df, .(lat,long), function(df) mean(df[, "value"]))
# - Reshape to spatio-temporal array 
ozone_a <-  daply(ozone_df, .(lat,long), function(df)  df[,"value"])
dim(ozone_a)
dimnames(ozone_a)

## dlply()
# - List element names are the levels of the .variables 

# - 1 grouping variable
linmod <- function(df) {
  lm(rbi ~ year, data = mutate(df, year = year - min(year)))
}
models <- dlply(baseball, .(id), linmod)
names(models) # "id"
str(attributes(models))
attr(models_l, "split_labels")

# - 2 grouping variable 
ozone_df <- reshape2::melt(ozone) # Load ozone data
month.abbr <- month.name          # Create month name
month <- ordered(rep(month.abbr, length = 72), levels = month.abbr) # Create factor variable for months 
rlm_df <- function(df)   MASS::rlm(value ~ month - 1, data = df, maxit = 100) # Function for linear model fit 
models_l <- dlply(ozone_df, .(lat, long), rlm_df)   
names(models_l) # "lat.long"
attr(models_l, "split_labels")