library("ggplot2")
library("plyr")
#########################
# Base ball case study  #
#########################
str(baseball)
# ----------------------------
# Extract data for 1 person 
baberuth <- subset(baseball, id == "ruthba01")
# Number of years since the player start playing 
baberuth <- transform(baberuth, cyear = year - min(year) + 1) 
# Runs per bat for Babe Ruth.
qplot(cyear, rbi / ab, data = baberuth, geom = "line")

# Apply the same to all players 
baseball <- ddply(baseball, .(id), transform, cyear = year - min(year) + 1)
baseball <- ddply(baseball, .(id), transform, mean_rbiab = rbi/ab)
# Apply to all baseball players with ab > 25 
baseball <- subset(baseball, ab >= 25)
# Create a plot for each player 
xlim <- range(baseball$cyear, na.rm=TRUE)
ylim <- range(baseball$rbi / baseball$ab, na.rm=TRUE)
plotpattern <- function(df) {
  qplot(cyear,rbi/ab, data = df, geom = "line")
}
pdf("path.pdf")
d_ply(baseball, .(id), failwith(NA, plotpattern), 
      .print = TRUE)
dev.off()
# Fit a linear model to each player 
model <- function(df) {
  lm(rbi / ab ~ cyear, data=df)
}
bmodels <- dlply(baseball, .(id), model)
# Extract model info for each player and put in a dataframe
rsq <- function(x) summary(x)$r.squared
bcoefs <- ldply(bmodels, function(x) c(coef(x), rsquare = rsq(x)))
names(bcoefs)[2:3] <- c("intercept", "slope")
bcoefs
# Merge bcoefs and baseball
baseballcoef <- merge(baseball, bcoefs, by = "id")
subset(baseballcoef, rsquare > 0.999)$id
# Histogram of model R-squared 
hist(bcoefs$rsquare)
qplot(rsquare, data=bcoefs, geom="histogram", binwidth=0.01)
# A scatterplot of model intercept and slope
# The size of the points is proportional to the R-square of the model. 
# Vertical and horizontal lines emphasise the x # and y origins.
ggplot(bcoefs, aes(slope, intercept)) + 
  geom_point(aes(size = rsquare), alpha = 0.5) +
  geom_vline(xintercept = 0, size=0.5, colour="grey50") + 
  geom_hline(yintercept = 0, size = 0.5, colour="grey50") + 
  scale_area(to = c(0.5, 3), breaks = c(0, 0.25, 0.5, 0.75, 1))
last_plot() + xlim(-0.01, 0.01) + ylim(-0.1, 0.25)


 