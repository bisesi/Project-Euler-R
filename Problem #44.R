#ATB
#Project Euler Problem #44
#Pentagon numbers
#Find the pair of pentagonal numbers with pentagonal sums and differences

pentagonNumbers <- function(){
  getnthPentagon <- function(x){
    x * (3*x - 1)/2
  }
  isPentagonal <- function(x){
    ((sqrt(24 * x + 1) + 1) %% 6) == 0
  }
  n <- 2
  nottheanswer <- TRUE
  while(nottheanswer) {
    first = getnthPentagon(n)
    for (i in seq(n-1, 1)) {
      second = getnthPentagon(i)
      if(isPentagonal(first + second) && isPentagonal(first - second)) {
        print("The two pentagonal numbers are: ")
        nottheanswer = FALSE
        print(c(n, i))
        print("The minimized distance between them is: ")
        print(first - second)
        break
      }
    }
    n = n + 1
  }
}

