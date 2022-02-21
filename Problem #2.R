#ATB
#Project Euler Problem #2
#Even Fibonacci Numbers
#Find the sum of even-valued Fibonacci numbers 

evenFibs <- function(x){
  library(tidyverse)
  #throws an error with rename when reshape package is installed
  a = 0
  b = 1
  count = 2
  fibs = data.frame()
  evens = data.frame()
  if (x <= 0){
    stop("Please enter a number greater than 0!")
  } else {
    if (x ==1) {
      stop(paste0("The sum of the even Fibonacci numbers below ", x, " is: ", 0))
    } else {
      while(count < x){
        nth = a + b
        fibs <- rbind(fibs, nth)
        a = b
        b = nth
        count = count + 1
      }
    }
    evens <- fibs %>%
      dplyr::rename(number = X1) %>%
      dplyr::filter(number <= x & number %% 2 == 0) %>%
      dplyr::summarise(evenfib = sum(number))
    print(paste0("The sum of the even Fibonacci numbers below ", x, " is: ", evens))
  }
}