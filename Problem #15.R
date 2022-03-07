#ATB
#Project Euler Problem #15
#Lattice paths
#How many routes exist in a grid moving to the right and down

latticePaths <- function(x){
  latticesize <- x
  lattice <- matrix(ncol = latticesize + 1, nrow = latticesize + 1)
  lattice[latticesize + 1, -(latticesize + 1)] <- 1
  lattice[-(latticesize + 1), latticesize + 1] <- 1
  for (i in latticesize:1){
    for (j in latticesize:1){
      lattice[i,j] <- lattice[i + 1, j] + lattice[i, j + 1]
    }
  }
  return(lattice[1,1])
}
