#ATB
#Project Euler Problem #30
#Digit n powers
#Find the sum of all numbers that can be written as the sum of n powers of their digits

digitnPowers <- function(x){
  power <- x
  numbers <- as.list(1:x^8)
  digits <- lapply(numbers, function(x) {as.numeric(unlist(strsplit(as.character(x), "")))})
  sums <- list()
  for (i in 1:length(numbers)){
    sums[i] <- sum(digits[[i]]^x)
  }
  lookup <- as.data.frame(cbind(numbers = numbers, sums = sums))
  lookup$numbers <- as.numeric(lookup$numbers)
  lookup$sums <- as.numeric(lookup$sums)
  same <- lookup[which(lookup$numbers == lookup$sums),]
  dropone <- same %>% dplyr::filter(numbers != 1)
  return(sum(dropone$numbers))
}
