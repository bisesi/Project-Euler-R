#ATB
#Project Euler Problem #1
#Multiples of 3 and 5
#Find the sum of all multiples of 3 or 5 below a provided parameter

multiplesThreeFive <- function(x){
  if (length(x) > 0 & is.numeric(x) & x > 0){
    multiples <- data.frame()
    sums <- data.frame()
    for (i in 0:(x-1)){
      if (i%%5 == 0 | i%%3 == 0){
        multiples <- rbind(multiples, i)
        sums <- sum(multiples)
      } 
    }
    print(paste0("The sum of the multiples of 3 and 5 below ", x, " is: ", sums))
  }else {
    stop("That is not a valid input!")
  }
}