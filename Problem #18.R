#ATB
#Project Euler Problem #18
#Maximum path sum
#Maximum total from top to bottom of the triangle

maximumPathSum <- function(inputtriangle) {
  for (row in nrow(inputtriangle):2) {
    for (col in 1:(ncol(inputtriangle) - 1)) {
      inputtriangle[row - 1, col] <- max(inputtriangle[row, col:(col + 1)]) + inputtriangle[row - 1, col]
    }
    inputtriangle[row, ] <- NA
  }
  return(max(inputtriangle, na.rm = TRUE))
}
