#ATB
#Project Euler Problem #92
#Square digit chains
#How many starting numbers below limit arrive at 89

squareDigitChains <- function(max){
  limit <- max
  end <- vector(length=limit)
  for (i in 1:limit) {
    y <- i
    while (y != 1 & y != 89) {
      z <- 0
      while (y > 0) {
        n <- y %% 10
        z <- z + n * n
        y <- floor(y/10)
      }
      y <- z
    }
    end[i] <- y
  }
  print(sum(end == 89))
}
