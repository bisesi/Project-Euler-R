#ATB
#Project Euler Problem #97
#Large non-Mersenne prime 
#Find the last ten digits of that non-Mersenne prime in the form of multiplier x 2^power + 1

nonMersennePrime <- function(multiplier, power){
  library("gmp")
  two <- as.bigz(2)
  string <- multiplier * two^power + 1
  characters <- as.character(string)
  separated <- strsplit(characters, "")
  digits <- as.numeric(unlist(separated))
  return(tail(digits, n = 10))
}
