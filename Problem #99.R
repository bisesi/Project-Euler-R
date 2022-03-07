#ATB
#Project Euler Problem #99
#Largest exponential
#Using an array of base and exponent pairs, determine pair with greatest numerical value

largestExponential <- function(array){
  array$power <- array$exponent * log(array$base)
  print(which.max(array$power))
}
