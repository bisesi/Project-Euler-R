#ATB
#Project Euler Problem #66
#Diophantine equation
#Find value of D for which largest value of x is obtained

diophantineEquation <- function(n){
  library("tidyverse")
  D <- n
  x <- c()
  y <- c()
  position <- c()
  for (i in c(1:1000)){
    for (j in c(1:1000)){
      for (d in 1:D){
        if (d == (i^2 - 1)/j^2){
          x[i] <- j
          y <- 1:length(x)
          position[i] <- d
          pairs <- cbind(x, y, position)
        }
      }
    }
  }
  cleaned <- pairs %>%
    as.data.frame() %>%
    drop_na(x) %>%
    arrange(position) %>%
    group_by(position) %>%
    filter(row_number() == 1)
  max <- cleaned %>%
    ungroup() %>%
    filter(y == max(y))
  return(max$position)
}
