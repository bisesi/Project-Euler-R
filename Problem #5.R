#ATB
#Project Euler Problem #5

#Smallest multiple
#Find the smallest positive number evenly divisible by all numbers from one to n
smallestMultiple <- function(x){
  getPrimes <- function(x) { #find prime numbers between 1 and n
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
  primes <- data.frame(numbers = getPrimes(x))
  for (i in 1:x){ #get powers of all primes between 1 and n
    primes[,i] <- primes$numbers^i
  }
  reduced <- primes %>% #filter results less than n
    select_if(function(p) any(p <= x))
  reduced[reduced > x] <- 0
  maxval <- reduced %>%
    dplyr::rowwise() %>%
    dplyr::mutate(maxval = max(across())) %>%
    dplyr::select(maxval)
  return(prod(maxval))
}