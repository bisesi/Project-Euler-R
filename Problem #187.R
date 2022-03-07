#ATB
#Project Euler Problem #187
#Semiprimes
#How many composite integers have two prime factors?

semiPrimes <- function(x){
  library("gmp")
  limit <- x
  n <- 1:floor((limit - 1) / 2)
  primes <- 1:10000
  primes <- primes[as.logical(isprime(primes))]
  for (i in primes){
    n <- n[as.logical(n %% i)]
  }
  id <- as.logical(isprime(n))
  allprimes <- c(primes, n[id])
  count <- 0
  for (i in allprimes[allprimes < sqrt(limit)]){
    count <- count + length(allprimes[allprimes < limit / i])
    allprimes <- allprimes[-1]
  }
  print(count)
}
