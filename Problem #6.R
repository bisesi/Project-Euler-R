#ATB
#Project Euler Problem #6
#Sum Square Difference
#Find the difference between the sum of the squares of the first n natural numbers
#and the square of the sum

sumSquareDifference <- function(x){
  square_of_sum <- (sum(1:x))^2
  sum_of_squares <- data.frame()
  for (i in 1:x){
    square <- i^2
    sum_of_squares <- rbind(sum_of_squares, square)
  } 
  difference <- square_of_sum - sum(sum_of_squares)
  return(difference)
}