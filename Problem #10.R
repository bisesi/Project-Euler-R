#ATB
#Project Euler Problem #10
#Summation of Primes
#Find the sum of all the primes below n

primeSummation <- function(x) {
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
  to_sum <- getPrimes(x)
  if (any(to_sum == x)){
    to_sum <- to_sum[to_sum < max(to_sum)]
  }
  return(sum(to_sum))
}
