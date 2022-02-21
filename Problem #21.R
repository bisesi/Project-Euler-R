#ATB
#Project Euler Problem #21
#Amicable numbers
#Find the sum of all amicable numbers under n

sumAmicable <- function(x){
  findDivisors <- function(x){
    divisors <- vector()
    d <- 1
    for (i in 1:floor(sqrt(x))){
      if (x %% i == 0){
        divisors[d] <- i
        if (i != x/i){
          d <- d + 1
          divisors[d] <- x / i
        }
        d <- d + 1
      }
    }
    return(divisors)
  }
  result <- 0
  count <- 2
  while (count <= x){
    divisorsums <- sum(findDivisors(count)) - count
    if (count == sum(findDivisors(divisorsums)) - divisorsums & count != divisorsums){
      result <- result + count + divisorsums
      count <- divisorsums
    }
    count <- count + 1
  }
  return(result)
}
