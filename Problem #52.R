#ATB
#Project Euler Problem #52
#PPermuted multiples
#Find the smallest positive integer such that multipled by 2..n it contains the same digits

permutedMultiples <- function(n){
  checkPermutations <- function(number, multipliers){
    characters <- function(x) {
      d <- as.character(number * x)
      d <- unlist(strsplit(d, ""))
      d <- d[order(d)]
      paste(d, collapse = "")
    }
    multiplied <- 1:multipliers
    ordered <- sapply(multiplied, characters)
    all(ordered %in% ordered[1])
  }
  answer <- 0
  i <- 2
  while (answer == 0){
    if (checkPermutations(i, n) == TRUE) answer <- i
    i <- i + 1
  }
  return(answer)
}


