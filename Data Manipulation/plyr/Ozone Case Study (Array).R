# ----------------------------
library(MASS)    # rlm()  
library(reshape) # melt()
library(plyr)
library(ggplot2)
## Load ozone data 
str(ozone)
ozone_df <- melt(ozone) # Create ozone df
time <- attributes(ozone)$dimnames$time # Extract time series
lat <- attributes(ozone)$dimnames$lat   # Extract latitude coordinates
long <- attributes(ozone)$dimnames$long # Extract longitude coordinates 
month.abbr <- month.name
month <- ordered(rep(month.abbr, length = 72), levels = month.abbr) # Create factor variable for months 
year <- ordered(rep(1:6, each = 12))

## Analysis at one location 
# - Extract time series at one location 
ts_point <- ozone[1,1,] 
# - Plot annual variability of ozone at that location 
qplot(month, ts_point, geom="line", group = year)
# - Estimate the seasonal component (-1 to remove the intercept...)
model <- rlm(ts_point ~ month - 1)  
# - Extract the deseasonalized time series 
res <- resid(model)  
# - Seasonal coefficient 
month_coef <-  coef(model)
# - Plot deaseasonalized time series 
plot(time, res,type="l")
# - Plot of seasonal effect  
plot(1:12, month_coef,type="l")

## Analysis at all locations 
# - Fit a rlm model at all locations 
deseasf_a <- function(value) rlm(value ~ month - 1, maxit = 100)
models_a <- aaply(ozone, 1:2, deseasf_a) # - If rlm fit crash --> 2d-list array is not created --> error 
models_l <- alply(ozone, 1:2, deseasf_a) # - Create a list instead of an array  !! 

deseasf_df <- function(df)   rlm(value ~ month - 1, data = df, maxit = 100) # Alternative
models_l <- dlply(ozonedf, .(lat, long), deseasf_df)  # Alternative

# - Create a 2D array which locations at which model fit crashed 
failed <- laply(models_l, function(x) !x$converged)
# - Create a 3D array with deaseasonalized time series at each location 
res_a <- laply(models_l, resid)
dimnames(res_a)[[3]] <- time
names(dimnames(res_a))[3] <- "time"
dim(res_a)
# - Create a 3D array with seasonal effect coefficents  
month_coef_a <- laply(models_l, coef)
dimnames(month_coef_a)[[3]] <- month.abbr
names(dimnames(month_coef_a))[3] <- "month"
dim(month_coef_a)
# - Convert the array to a dataframe 
month_coefs_df <- melt(month_coef_a)
res_df <- melt(res_a)
head(month_coefs_df)
head(res_df)
# - Add  statistics to the dataframe at each location 
month_coefs_df <- ddply(month_coefs_df, .(lat, long), transform, 
                        avg = mean(value),
                        std = value/ max(value) )
str(month_coefs_df)
head(month_coefs_df)

###### Visualization ####
library(maps)
library(ggplot2)
make_stars <- function(data, time, value) {
  # Data must contain long and lat columns
  data[, c(time, value)] <- lapply(data[, c(time, value)], function(x) {
    x <- as.numeric(x)
    (x - min(x)) / diff(range(x))
  })
  
  ddply(data, .(lat, long), function(df) {
    df$x <- df[, value] * cos(df[, time] * 2 * pi + pi / 2)
    df$y <- df[, value] * sin(df[, time] * 2 * pi + pi / 2)
    df[order(df[, time]), ]
  })
}
# Select area of data 
outlines <- as.data.frame(map("world",xlim=-c(113.8, 56.2), ylim=c(-21.2, 36.2), plot=FALSE)[c("x","y")])
map <- c(geom_path(aes(x=x, y=y, fill=NULL, group=NULL, order=NULL, size=NULL),
                   data = outlines, colour = alpha("grey20", 0.2)),
         scale_x_continuous("", limits = c(-114.8, -55.2), breaks=c(-110, -85, -60)),
         scale_y_continuous("", limits = c(-22.2, 37.2)))

## Star glyphs showing seasonal variation and deseasonalized time series over the years 
stars_month_coef <- make_stars(month_coefs_df, "month", "std")
stars_res <- make_stars(res_df, "time", "value")
res <- 1.2 # Resolution 
ggplot(stars_month_coef , aes(x = long + res * x, y = lat + res * y, fill=avg)) +
  geom_polygon(aes(group = interaction(long, lat)), colour="grey50") +
  scale_fill_gradient(low = "blue", high = "yellow")
ggplot(stars_res , aes(x = long + res * x, y = lat + res * y)) +
  map + 
  geom_path(aes(group = interaction(long, lat)), colour="grey50", fill=NA) +

##
# Map of coefficient values 
coef_limits <- range(month_coefs_df$value)
monthsurface <- function(mon) {
  df <- subset(month_coefs_df, month == mon)
  qplot(long, lat, data = df, fill = value, geom="tile") + 
        scale_fill_gradient(limits = coef_limits, low = "blue", high = "yellow") + 
        map  
}
monthsurface("January")
monthsurface("July")

