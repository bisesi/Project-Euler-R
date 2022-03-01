#ATB
#Project Euler Problem #53
#Combinator selections
#How many combinatoric selections for n <= 100 are greater than 1 million

combinatoricSelections <- function(x, limit){
  count = 0
  for (i in seq(23, x)){
    for (j in seq(i)){
      if (ncol(combn(i,j)) > limit){
        middle = floor(i/2)
        if (i %% 2 == 0){
          count = count + (2 * (middle - j) + 1)
        }
        else {
          count = count + (2 * (middle - j) + 2)
        }
        break
      }
    }
  }
  return(count)
}
