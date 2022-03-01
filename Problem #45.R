#ATB
#Project Euler Problem #45
#Triangular, pentagonal and hexagonal
#Find triangle numbers that are also pentagonal and hexagonal

triPentHex <- function(x){
  getTris <- function(x){
    triangles <- c()
    for (i in 1:x){
    triangles[i] <- (i*(i+1))/2
    }
    return(triangles)
  }
  getPent <- function(x){
    pentagonals <- c()
    for (i in 1:x){
      pentagonals[i] <- (i*((3*i)-1))/2
    }
    return(pentagonals)
  }
  getHex <- function(x){
    hexagonal <- c()
    for (i in 1:x){
      hexagonal[i] <- i*((2*i)-1)
    }
    return(hexagonal)
  }
  tris <- getTris(x)
  pents <- getPent(x)
  hex <- getHex(x)
  overlap <- intersect(intersect(tris, pents), hex)
  return(max(overlap))
}
