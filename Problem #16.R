#ATB
#Project Euler Problem #16
#Power digit sum
#Return the sum of the digits of the number = 2^n

powerDigitSum <- function(x){
  library("gmp")
  power <- as.bigz(2^x)
  sum <- sum(as.numeric(strsplit(as.character(power),"")[[1]]))
  return(sum)
}
