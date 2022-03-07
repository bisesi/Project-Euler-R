#ATB
#Project Euler Problem #11
#Largest product in a grid
#What is the greatest product of four adjacent numbers in the same direction in a grid?

largestGridProduct <- function(array){
  rows <- nrow(array)
  cols <- ncol(array)
  if (rows != cols){
    print("That matrix won't work for this function!")
  }
  vertical <- array[1:(rows - 3), ] * array[2:(rows - 2), ] * array[3:(rows - 1), ] * array[4:rows, ] 
  horizontal <- array[ , 1:(rows - 3)] * array[ , 2:(rows - 2)] * array[, 3:(rows - 1)] * array[, 4:rows]
  diag1 <- array[1:(rows - 3), 1:(rows - 3)] * array[2:(rows - 2), 2:(rows - 2)] * array[3:(rows - 1), 3:(rows - 1)] * array[4:rows, 4:rows]
  diag2 <- array[4:rows, 1:(rows - 3)] * array[3:(rows - 1), 2:(rows - 2)] * array[2:(rows - 2), 3:(rows - 1)] * array[1:(rows - 3), 4:rows]
  return(max(vertical, horizontal, diag1, diag2))
}
