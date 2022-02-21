#ATB
#Project Euler Problem #3
#Largest Prime Factor
#Return the largest prime factor of a given number

largePrime <- function(d) {
  factors = c()
  i = 2
  x <- d
  if (length(x) > 0 & is.numeric(x) & x > 0){
    while (x >= i){
      if (!x %% i){
        factors <- c(factors,i)
        x <- x/i
        i <- i - 1
      }
      i <- i + 1
    } 
    print(paste0("The largest prime factor of ", d, " is: ", max(factors)))
  } else {
    stop("That is not a valid input!")
  }
}