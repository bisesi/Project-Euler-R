#ATB
#Project Euler Problem #25
#1000-digit Fibonacci
#Find the index of first Fib number to contain n digits

digitFibonacci <- function(x){
  a = 0
  b = 1
  count = 2
  fibs = data.frame()
  while (count < x*5){
    nth = a + b
    fibs <- rbind(fibs, nth)
    a = b
    b = nth
    count = count + 1
  }
  allfibs <- rbind(1,fibs)
  allfibs$digits <- floor(log10(allfibs)) + 1
  digits <- head(which(allfibs$digits == x, arr.ind = TRUE), n = 1)
  return(as.numeric(digits[,1]))
}
