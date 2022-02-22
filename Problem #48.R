#ATB
#Project Euler Problem #48
#Self Powers
#Find the last ten difits of the series 

selfPowers <- function(powers, finaldigits){
  library("gmp")
  selfs <- c(1:powers)
  selfpowers <- as.bigz(1:powers)
  for (i in 1:length(selfs)){
    selfpowers[i] <- i^i
  }
  sumpowers <- sum(selfpowers)
  string <- data.frame(strsplit(as.character(sumpowers), ""))
  answer <- as.list(tail(string, n = finaldigits))[[1]]
  return(answer)
}
