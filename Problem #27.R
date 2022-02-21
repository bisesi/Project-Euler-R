#ATB
#Project Euler Problem #27
#Quadratic Primes
#Get the product of the numbers a and b that maximize the number of primes returned by the quadratic form n^2 + an +b
#a and b must both be odd, b must be positive prime

quadraticPrimes <- function(x){
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
  is.prime <- function(x){
    if (x <= 1) {
      return(FALSE)
    } 
    if (x == 2){
      return(TRUE)
    }
    primes <- getPrimes(ceiling(sqrt(x)))
    return(prod(x %% primes != 0) == 1)
  }
  max <- 0
  if (x %% 2 == 0){
    seq.a <- seq(-x+1, x+1, 2)
  }
  if (x %% 2 != 0){
    seq.a <- seq(-x, x, 2)
  }
  seq.b <- getPrimes(x)
  for (a in seq.a){
    for (b in seq.b){
      x <- 0
      while (is.prime(x^2 + (a*x) + b)){
        x <- x + 1
      }
      if (x > max){
        max <- x
        max.a <- a
        max.b <- b
      }
    }
  }
  product <- max.a*max.b
  return(product)
}