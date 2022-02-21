#ATB
#Project Euler Problem #12
#Highly divisible triangular number
#Find the value of the first triangle number to have >n divisors

divisibleTriangle <- function(x){
  computeTriangles <- c()
  triangles <- c()
  for (i in 1:(x*x*x)){
    computeTriangles[[i]] <- c(1:i)
    triangles[i] <- sum(computeTriangles[[i]])
  }
  factors <- data.frame(triangles)
  for (i in 1:max(triangles)){
    factors[,i+1] <- factors$triangles %% i 
  } 
  zeros <- apply(factors == 0, 1, sum)
  countzeros <- data.frame(cbind(triangles, zeros))
  maxrow <- countzeros[countzeros$zeros > x, ]
  filterfirst <- head(maxrow, n = 1)
  maxtriangle <- filterfirst$triangles
  return(maxtriangle)
}