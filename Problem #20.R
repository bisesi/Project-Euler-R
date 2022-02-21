#ATB
#Project Euler Problem #20
#Factorial digits
#Sum the digits of a factorial n!

sumFactorial <- function(x){
  library("gmp")
  factorials <- factorialZ(x)
  sum <- sum(as.numeric(strsplit(as.character(factorials),"")[[1]]))
  return(sum)
}