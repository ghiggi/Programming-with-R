# Parallelisation using multiple cores does not reduce the total computation time, but allows
# to better exploit the computer resources
# 
# The dispatching would probably take more time than the actual calculations. 
# Instead, organize the tasks into blocks, and dispatch each block of tasks, for instance
# 
# multicore, snow,snowfall, doParallel,foreach, plyr,



      
#provide an option to run it not in parallel.

#This script uses parallel processing if p.flag=TRUE.  Set up a parallel 
#cluster as appropriate for your machine as appropriate. (the commented code 
#below will use 2 cores on a multicore computer)
#library(doParallel)
#cl <- makeCluster(2)  # Use 2 cores
#registerDoParallel(cl) # register these 2 cores with the "foreach" package
library(plyr)
p.flag=FALSE  # Change to TRUE if using parallel processing
aaply(1:10000, 1, function(x) rnorm(1, mean=x), .parallel=TRUE)
aaply(seq(1,10000,100), function(x) rnorm(1, mean=x+(1:100)), .parallel=p.flag))    
      
library(parallel)
cores <- detectCores()
cores
# Set up a local cluster and then use parLapply():
cluster <- makePSOCKcluster(cores)
system.time(parLapply(cluster, 1:10, function(i) Sys.sleep(i)))
#Use clusterEvalQ() to run arbitrary code on each cluster and load needed packages
x <- 10
psock <- parallel::makePSOCKcluster(1L)
clusterEvalQ(psock, x)
#Use clusterExport() to copy objects in the current session to the remote sessions.
clusterExport(psock, "x")
clusterEvalQ(psock, x)