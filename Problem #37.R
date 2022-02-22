#ATB
#Project Euler Problem #37
#Truncatable primes
#Find the sum of the n primes that are both truncatable from left to right and right to left

truncatablePrimes <- function(x){
  library("numbers")
  library("purrr")
  getPrimes <- function(x) {
    if (x >= 2) {
      integers = seq(2, x)
      primes = c()
      for (i in seq(2, x)) {
        if (any(integers == i)) {
          primes = c(primes, i)
          integers = c(integers[(integers %% i) != 0], i)
        }
      }
      return(primes)
    }
  }
  truncateleft <- function(x){
    trunc <- TRUE
    while(x>9){
      x <- x %/% 10
      trunc <- isPrime(x)*trunc
    }
    return(as.logical(trunc))
  }
  truncateright <- function(x){
    trunc <- TRUE
    while(x>9){
      x <- x - (x %/% 10**floor(log10(x)))*(10**floor(log10(x)))
      trunc <- isPrime(x)*trunc
    }
    return(as.logical(trunc))
  }
  answer <- getPrimes(1000000) %>%
    keep(truncateleft) %>%
    keep(truncateright)
  answer <- as.data.frame(answer)
  cleaned <- answer %>%
    filter(answer > 9) %>%
    head(n = x) %>%
    sum()
  return(cleaned)
}
