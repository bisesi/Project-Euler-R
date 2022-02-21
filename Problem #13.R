#ATB
#Project Euler Problem #13
#Large sum
#Get the first ten digits of the sum of a many digit number

largeSum <- function(digitlist){
  library("gmp")
  nums <- as.bigz(digitlist)
  sum <- sum(unlist(nums))
  splitnums <- as.numeric(strsplit(as.character(sum),"")[[1]])
  topten <- splitnums[1:10]
  return(topten)
}