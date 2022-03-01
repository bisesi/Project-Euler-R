#ATB
#Project Euler Problem #57
#Square root convergents
#How fractions contain a numerator with more digits than denominator?

squareRootConvergents <- function(x){
  library("gmp")
  numerator <- as.vector(as.bigz(3))
  denominator <- as.vector(as.bigz(2))
  for (i in 2:x) {
    numerator[i] <- numerator[i - 1] + 2 * denominator[i - 1]
    denominator[i] <- numerator[i - 1] + denominator[i - 1]
  }
  answer <- sum(floor(log10.bigz(numerator)) > floor(log10.bigz(denominator)))
  return(answer)
}

