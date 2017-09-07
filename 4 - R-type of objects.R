l.ex <- list(one=1, s1= 1:2, pi=pi, I=1i, let= c("A", "a", "b", ":::"),
             fn = mean, fn2 = c, fnSpec = `function`,
             n = as.name("Mä"), n2 = quote(x), ex = expression(1+1),
             cl = call("round",10), cl2 = quote(sin(x)), formals = formals(lm), arg1 = formals(lm)[[1]])
str(l.ex, vec.len = 16, max.level = 1)

myShow <- function(x, max.length= 1000L) {
  # Takes one element of a list at a time (look sapply below...)
  r <- tryCatch(  format(x), error=function(e)e)
  r <- if(inherits(r, "error")) {
    tryCatch(as.character(x), error=function(e)e)
  } else {
    paste(r, collapse = " ")
  }
  r <- if(inherits(r, "error")) {
    tryCatch(capture.output(x), error=function(e)e)
  } else {
    paste(r, collapse = " ")
  }
  substr(r, start=1L, stop=max.length)
}

cbind(show    = sapply(l.ex, myShow, max.length = 16),
      class   = sapply(l.ex, class),
      typeof  = sapply(l.ex, typeof),
      st.mode = sapply(l.ex, storage.mode),
      mode    = sapply(l.ex, mode)) -> tab.ex

knitr::kable(tab.ex, format="pandoc") #use a knitr package feature to print a nice table