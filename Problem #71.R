#ATB
#Project Euler Problem #71
#Ordered fractions
#List reduced proper fractions with d less than limit, find the numerator to the left of 3/7

orderedFractions <- function(x){
  bound <- x
  start_num <- 3
  start_den <- 7
  numerator <- 0
  denominator <- 1
  for (i in bound:2){
    j <- floor((start_num * i - 1) / start_den)
    if ((j * denominator) > (numerator * i)){
      denominator <- i
      numerator <- j
    }
  }
  print(numerator)
}
