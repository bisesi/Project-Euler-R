#ATB
#Project Euler Problem #36
#Double-base palindromes
#Get the numbers between 1000 and 1000000 which are palindromic in base 10 and base 2

doublePalindromes <- function(x){
  convertbase2 <- function(x){
    newinteger <- paste(as.integer(rev(intToBits(x))), collapse = "")
    answer <- stringr::str_remove(newinteger, "^0+")
    return(answer)
  }
  getPalindromes <- function(x) {
    x <- as.character(x)
    forward <- unlist(strsplit(x, split = ""))
    reverse <- rev(forward)
    pal <- all(forward == reverse)
    return(pal)
  }
  binary <- list()
  for (i in 1:x){
    binary[i] <- convertbase2(i)
  }
  decimals <- c(1:x)
  lookup <- as.data.frame(cbind(bits = unlist(binary), decimals))
  palbits <- c()
  for (i in 1:length(lookup$bits)){
    palbits[i] <- getPalindromes(lookup$bits[i])
  }
  paldecimals <- c()
  for (i in 1:length(lookup$decimals)){
    paldecimals[i] <- getPalindromes(lookup$decimals[i])
  }
  lookupall <- cbind(lookup, paldecimals, palbits)
  answer <- lookupall[lookupall$palbits == TRUE & lookupall$paldecimals == TRUE,]
  sums <- sum(as.numeric(answer$decimals))
  return(sums)
}
