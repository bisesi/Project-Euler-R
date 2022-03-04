#ATB
#Project Euler Problem #65
#Convergents of e
#Find the sum of digits in the numerator of the nth convergent of the continued fraction


eConvergents <- function(x){
  library("gmp")
  eExpansion <- function(digits){
    start <- 2
    e <- c(1,2,1)
    add <- c(0,2,0)
    length <- floor(digits/3)-1
    expand <- sapply(0:length, function(z) add * z) + e
    expand <- as.vector(expand)
    expansion <- c(start, expand)
    return(expansion)
  }
  getFractions <- function(e){
    n <- length(e)
    a <- e[n-1] * e[n-2] + 1
    b <- e[n-2]
    for (i in (n-2):1) {
      count <- a;
      a <- e[i] * a + a
      b <- count
    }
    fractions <- list(numerator=a, denumerator=b)
    return(fractions)
  }
  digs <- eExpansion(x)
  results <- getFractions(digs)
  num <- as.character(as.bigz(results$numerator))
  sum <- sum(as.numeric(unlist(strsplit(num, ""))))
  return(sum)
}
