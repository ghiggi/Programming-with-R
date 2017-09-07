########################################
# Retrieve information about packages  #
########################################
library(sfsmisc)
sessionInfo() 
capture.and.write(sessionInfo() , first = 1, last = 12)# for "better visualization"

## Package information
pkgName <- "stats"
ns <- asNamespace(pkgName)
ns <- getNamespace(pkgName)

packageDescription( pkgName) 
getNamespaceInfo(ns, which="spec")
getNamespaceInfo(ns, which="spec")[["version"]] # return the version of the package 

environmentName(ns)  # return the name of the environment 

## Information about a package 
lsNamespaceInfo <- function(ns, ...) {
  ns <- asNamespace(ns, base.OK = FALSE)
  ls(..., envir = get(".__NAMESPACE__.", envir = ns, inherits = FALSE)) ### what is doing here ????????
}
allinfoNS <- function(ns) sapply(lsNamespaceInfo(ns), getNamespaceInfo, ns=ns)
lsNamespaceInfo(pkgName)
str(ns.stats <- allinfoNS(pkgName))
str(ns.MASS <- allinfoNS("MASS")) 

# Lazy data (datasets included in the package) (it's an environment !!!)
ls(ns.stats$lazydata) # no lazy data
ls(ns.MASS$lazydata)  # data(package="MASS")

# Path to the package 
ns.MASS$path 

# S3 method of a package 
ns.MASS$S3methods  #[generic, class , generic.class] 
sort(ns.MASS$S3methods)

## Imports of the namespace  
getNamespaceImports("MASS")
ns.MASS$imports

## Exports of a namespace   
getNamespaceExports("stats")
 

loadNamespace() # functions to load namespaces 
unloadNamespace(ns) # function to unload namespaces
 