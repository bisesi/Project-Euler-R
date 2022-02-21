#ATB
#Project Euler Problem #24
#Lexographic permutations
#Find the nth permutation of digits 1-9

getPermutations <- function(x){
  library("combinat")
  library("purrr")
  library("dplyr")
  z <- c(0:9)
  permutes <- permn(z)
  df <- map_df(permutes, ~as.data.frame(t(.)))
  df <- df %>%
    arrange()
  return(df[x,])
}