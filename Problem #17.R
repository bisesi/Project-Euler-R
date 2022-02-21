#ATB
#Project Euler Problem #17
#Number letter counts
#Count total letters used in list of numbers

countLetters <- function (x){
  library("xfun")
  written <- c()
  for (i in 1:x){
    written[i] <- n2w(i, hyphen = FALSE, and = TRUE)
  }
  nospaces <- gsub("\\s+","",written)
  return(sum(nchar(nospaces)))
}