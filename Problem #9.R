#ATB
#Project Euler Probelm #9
#Special Pythagorean triplet
#Find product abc where a + b +  = n

pythagoreanTriplet <- function(x){
  library("tidyverse")
  values <- expand.grid(a = 1:1000 , b = 1:1000)
  values$c <- sqrt(values$a^2 + values$b^2)
  actuals <- subset(values, (a < b) & (b < c)) 
  actuals$sum <- rowSums(actuals)
  actuals <- actuals %>%
    dplyr::filter(sum == x) %>%
    dplyr::select(-sum) %>%
    dplyr::mutate(products = a * b * c) %>%
    dplyr::select(products)
  return(actuals)
}