#ATB
#Project Euler Problem #56
#Powerfil digit sum
#What is the maximum digital sum of the form a^b

powerfulDigitSum <- function(n) {
  library("gmp")
  a <- c(1:n)
  b <- combn(a, 2)
  powers <- list()
  for (i in 1:ncol(b)){
    powers[i] <- b[1,i] ^ b[2,i]
  }
  big <- lapply(powers, as.bigz)
  strings <- lapply(big, function(x) strsplit(as.character(x), ""))
  integers <- lapply(strings, function(x) as.numeric(unlist(x)))
  sums <- lapply(integers, sum)
  return(max(unlist(sums)))
}
