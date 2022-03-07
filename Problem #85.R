#ATB
#Project Euler Problem #85
#Counting rectangles
#Find the area of the grid with the nearest solution for n rectangles

countingRectangles <- function(x){
  maximumrects <- x
  difference <- x
  for (side1 in 1:(sqrt(x))){
    for (side2 in side1:sqrt(x)){
      rectangles <- (side1 * (side1 + 1) * side2 * (side2 + 1)) / 4
      if (abs(maximumrects - rectangles) < difference) {
        difference <- abs(maximumrects - rectangles)
        area <- side1 * side2
      }
    }
  }
  return(area)
}
