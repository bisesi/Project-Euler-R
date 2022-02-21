#ATB
#Project Euler Problem #23
#Non abundant sums
#Find the sum of all integers up to n that cannot be written as the sum of two abundant numbers

nonabundantSums <- function(x){
  properDivisors <- function(x){
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
  getabundants <- function(x){
    abundants <- c()
    a <- 1
    for (i in 1:x){
      numbers <- sum(properDivisors(i))
      if (numbers > 2 * i){
        abundants[a] <- i
        a <- a + 1
      }
    }
    return(abundants)
  }  
  uppermax <- getabundants(28123)
  allnumbers <- c(1:x)
  for (i in 1:length(uppermax)){
    for (j in i:length(uppermax)){
      if (uppermax[i] + uppermax[j] <= x){
        allnumbers[uppermax[i] + uppermax[j]] <- NA
      }
    }
  }
  nums <- allnumbers[!is.na(allnumbers)]
  answer <- sum(nums)
  return(answer)
}