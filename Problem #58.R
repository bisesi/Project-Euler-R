#ATB
#Project Euler Problem #58
#Spiral primes
#What is the side length of the square spiral for which percent of primes along both diagonals first falls below percent

spiralPrimes <- function(x){
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
  side <- 3
  prime.ratio <- 1
  prime.count <- 0
  while (prime.ratio > x/100){
    corners <- seq(from = (side * (side - 3) + 3), by = side - 1, length.out = 4)
    prime.count <- prime.count + sum(sapply(corners, is.prime))
    prime.ratio <- prime.count / (2 * side - 1)
    side <- side + 2
  }
  return(side - 2)
}
