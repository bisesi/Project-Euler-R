#ATB
#Project Euler Problem #31
#Coin sums
#How many different ways can n pence be made using any number of coins

coinSums <- function(x){
  coins <- c(1,2,5,10,20,50,100,200)
  numberofcoins <- length(coins)
  findSums <- function(x, y) {
    if (y ==1)
      return(1)
    combos <- 0
    for (i in 1:y) {
      remaining <- x - coins[i]
      if(remaining == 0)
        combos <- combos + 1
      if(remaining > 0)
        combos <- combos + findSums(remaining,i)
    }
    return(combos)
  }
  print(findSums(x, numberofcoins))
}
