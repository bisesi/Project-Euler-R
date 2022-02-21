#ATB
#Project Euler Problem #7
#10001st Prime
#Return the nth prime

nthPrime <- function(x){
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
    else 
    {
      stop("That is not a valid input.")
    }
  }
  finder <- x*25
  primes <- getPrimes(finder)
  getnth <- nth(primes, x)
  return(getnth)
}