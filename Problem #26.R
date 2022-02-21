#ATB
#Project Euler Problem #26
#Reciprocal cycles
#Get the number with the longest repeating diigt for range < 1/n

reciprocalCycles <- function(x){
  findDecimals <- function(x, output = "") {
    x <- floor(abs(x))
    decimals <- c()
    remainders <- c()
    i <- 1
    r <- 10
    remainders <- r
    repeat {
      decimals[i] <- floor(r / x)
      r <- 10 * (r %% x)
      if (r == 0 | r %in% remainders) {break}
      remainders[i + 1] <- r
      i <- i + 1 
    }
    recurring <- ifelse(r != 0, length(remainders) - which(r == remainders) + 1, 0)
    if (output == "len")
      return(recurring)
    else {
      if (recurring != 0) {
        if (recurring == length(decimals)) 
          l <- "("
        else
          l <- c(decimals[1:(length(decimals) - recurring)], "(")
        dec <- c(l, decimals[(length(decimals) - recurring + 1):length(decimals)], ")")
      }
      return(paste0("0.", paste0(decimals, collapse = "", sep = "")))
    }
  }
  dataset <- c(1:x)
  lookup <- sapply(dataset, findDecimals)
  return(max(nchar(lookup)) - 1)
}