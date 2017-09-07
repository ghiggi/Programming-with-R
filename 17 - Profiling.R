################# 
##  Profiling  ##
#################
# - Method for determining where a program run spends most of its execution time (code bottlenecks)
# - Examines  the code to determine what parts of it are running slow.
# - Can be very helpful in guiding programmer effort for improving program performance
# - R's code profiler samples the "call stack" at regular intervals
# - The call stack is the list of the current function running, the function that called it, and all the functions that those. 

# Profiling can at best capture memory usage every 1 ms
# Path of the file to profile 
srcfile <- system.file("samples", "bootlmEx.R", package = "proftools")

###########################
## Profile with lineprof ##
###########################
# - The vertical direction represents the call stack
# - White blocks represent code where profvis doesn't have the source code 
# - The width of the block is proportional to the amount of time spent in that function
library(profvis)
profvis({
  library(ggplot2)
  g <- ggplot(diamonds, aes(carat, price)) + geom_point(size = 1, alpha = 0.2)
  print(g)
})

profvis({			
  data(diamonds, package = "ggplot2")			
  plot(price ~ carat, data = diamonds)			
  m <- lm(price ~ carat, data = diamonds)			
  abline(m, col = "red")			
})
################################################################################
##  Profile with proftools ##
#############################
# - Return profile data collected while evaluating an expression.
# - Uses Rprof to profile execution of expr and returns the profile data 
#   read in using readProfileData. 
# - By default GC and source information are included in the profile data.
library(proftools)
# Path of the file to profile 
srcfile <- system.file("samples", "bootlmEx.R", package = "proftools")
# Profile source file 
pd <- profileExpr(source(srcfile) ,srclines=TRUE,GC=TRUE)
pd1 <- readProfileData(profout) # if using Rprof()

## Functions to summarize profile data.
# funSummary() 
# - Summarize profile results at the function level
# - Returns a summary of the time spent:
#    - in each call if source information is available and requested (srclines = TRUE by default)
#    - in each function (if srcleines=FALSE) ("to avoid call/function replicates")
head(funSummary(pd, srclines = TRUE), 10)
head(funSummary(pd, srclines = FALSE), 10)
# callSummary()
# - summarize profile by call
head(callSummary(pd), 10)
# srcSummary()
# - can be used When source information is available in the profile data 
# - summarize at the source line level
# - only lines appearing in the sample are included:
srcSummary(pd)

# annotateSource()
# - Show the full files with profiling annotations
annotateSource(pd)

# hotPaths()
# - Look for hot execution paths.
# - useful way to explore where the computational effort is concentrated
# - Sorts functions called at top level by the total amount of time spent in their top level calls
# - The total.pct argument causes leaf functions in stack traces to be pruned back
#   until the execution time percentage for each stack trace is at least total.pct
hotPaths(pd, total.pct = 10.0)

# filterProfileData()
# - Allow filter profile data 
# - Used to select or omit certain functions
# - Used to drop functions with small self or total times 
# --> Focus on the work done in the sourced file (skip the first 4 leading calls  
filteredPD <- filterProfileData(pd, skip = 4)
hotPaths(filteredPD, total.pct = 10.0) 
# --> Narrow the examination to stack frames containing calls to a given function (here glm)
glmPD <- filterProfileData(filteredPD, focus = "glm")
hotPaths(glmPD, total.pct = 5.0)

## Plot a call graph for the full profile data 
plotProfileCallGraph(pd)
# Examine the glm.fit call
plotProfileCallGraph(filterProfileData(pd, focus = "glm.fit"))
printProfileCallGraph(filterProfileData(pd, focus = "glm.fit"))

# Show a flame graph 
# The order argument determines the ordering of call
# - order = "hot" (by default) : it uses the hot path ordering with the call with the largest amount of time first. 
# - order = "alpha" : orders the calls alphabetically 
# - order = "time" : order the calls in the order in which they occur 
flameGraph(pd, order="hot")
flameGraph(filteredPD, order="hot")
flameGraph(filteredPD, order = "time")
flameGraph(filteredPD, order = "alpha")

# Display a TreeMap
calleeTreeMap(pd)

# Identify the individual calls when they are not visible.
fg <- flameGraph(filteredPD, order="hot")
identify(fg)
ct <- calleeTreeMap(filteredPD)
identify(ct)

################################################################################
######################
## Profile manually ## with Rprof()
######################
# Path of the file to profile 
srcfile <- system.file("samples", "bootlmEx.R", package = "proftools")
# Enable or disable profiling of the execution of R expressions
profout <- tempfile() # Create a temporary file
Rprof(file = profout, gc.profiling = TRUE, line.profiling = TRUE)
# Profile the code 
source(srcfile)
# Finish profiling
Rprof(NULL)
# Summarise Output of R Sampling Profiler
# -$by.self component lists the time spent by functions alone --> bottom-level function that takes most time.
# -$by.total table lists the time spent by functions and all the functions they call --> parent.function that takes most time
summaryRprof(profout)
# Look which function takes more time...
proftable(profout)
# Delete the temporary file 
unlink(profout)

##
#' An alternative to \code{summaryRprof()}
#' 
#' \code{proftools} parses a profiling file and prints an easy-to-understand
#' table showing the most time-intensive function calls. 
#' 
#' Line numbers are included if \code{Rprof()} was run with 
#' \code{line.numbering=TRUE}. If it was run with \code{memory.profiling=TRUE},
#' this function will probably break.
#' 
#' Below the table are printed any files identified if line numbering is true,
#' the total time recorded by \code{Rprof()}, and the "parent call".  The
#' parent call consists of the parent call stack of all the call stacks in the\
#' table. Note that this is the parent call stack of only the printed lines,
#' not of all stacks recorded by \code{Rprof()}. This makes the table easier to read and fit into the console. 
#' 
#' @export
#' @param file A profiling file generated by \code{Rprof()}
#' @param lines The number of lines (call stacks) you want returned. Lines are
#' printed from most time-intensive to least.
proftable <- function(file, lines = 10) {
  profdata <- readLines(file)
  interval <- as.numeric(strsplit(profdata[1L], "=")[[1L]][2L]) / 1e+06
  filelines <- grep("#File", profdata)
  files <- profdata[filelines]
  profdata <- profdata[-c(1, filelines)]
  total.time <- interval * length(profdata)
  ncalls <- length(profdata)
  profdata <- gsub("\\\"| $", "", profdata)
  calls <- lapply(profdata, function(x) rev(unlist(strsplit(x, " "))))
  stacktable <- as.data.frame(table(sapply(calls, function(x) paste(x, collapse = " > "))) / ncalls * 100, stringsAsFactors = FALSE)
  stacktable <- stacktable[order(stacktable$Freq[], decreasing = TRUE), 2:1]
  colnames(stacktable) <- c("PctTime", "Call")
  stacktable <- head(stacktable, lines)
  shortcalls = strsplit(stacktable$Call, " > ")
  shortcalls.len <- range(sapply(shortcalls, length))
  parent.call <- unlist(lapply(seq(shortcalls.len[1]), function(i) Reduce(intersect, lapply(shortcalls,"[[", i))))
  shortcalls <- lapply(shortcalls, function(x) setdiff(x, parent.call))
  stacktable$Call = sapply(shortcalls, function(x) paste(x, collapse = " > "))
  if (length(parent.call) > 0) {
    parent.call <- paste(paste(parent.call, collapse = " > "), "> ...")
  } else {
    parent.call <- "None"
  }
  frac <- sum(stacktable$PctTime)
  attr(stacktable, "total.time") <- total.time
  attr(stacktable, "parent.call") <- parent.call
  attr(stacktable, "files") <- files
  attr(stacktable, "total.pct.time") <- frac
  print(stacktable, row.names=FALSE, right=FALSE, digits=3)
  if(length(files) > 0) {
    cat("\n")
    cat(paste(files, collapse="\n"))
    cat("\n")
  }
  cat(paste("\nParent Call:", parent.call))
  cat(paste("\n\nTotal Time:", total.time, "seconds\n"))
  cat(paste0("Percent of run time represented: ", format(frac, digits=3)), "%")
  
  invisible(stacktable)
}

################################################################################

library(profr)




