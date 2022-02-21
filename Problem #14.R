#ATB
#Project Euler Problem #14
#Longest Collatz sequence

collatzSequence <- function(x){
  library("tidyverse")
  collatz.length <- vector(length = x)
  collatz.length[1] <- 0
  for (i in 2:x){
    n <- i
    count <- 0 
    while (n != 1 & n >= i){
      if (n %% 2 == 0){
        n <- n / 2
        count <- count + 1
      } else {
        n <- (3 * n + 1) / 2
        count <- count + 2
      }
    }
    count <- count + collatz.length[n]
    collatz.length[i] <- count
  }
  understart <- as.data.frame(cbind(collatz.length, numbers = 1:x))
  longest <- understart %>%
    dplyr::filter(collatz.length == max(collatz.length)) %>%
    dplyr::select(numbers)
  return(longest)
}