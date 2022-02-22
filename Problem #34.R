#ATB
#Project Euler Problem #34
#Digit factorials
#Find the numbers and the sum of the numbers equal to the sum of the factorial of their digits

digitFactorials <- function(){
  numbers <- c(1:1000000)
  digits <- list()
  for (i in 1:length(numbers)){
    digits[i] <- strsplit(as.character(numbers[i]), "")
  }
  factorialsum <- list()
  for (i in 1:length(digits)){
    factorialsum[i] <- sum(factorial(as.numeric(digits[[i]])))
  }
  lookup <- as.data.frame(cbind(sums = unlist(factorialsum), numbers))
  answer <- which(lookup$sums == lookup$numbers)
  answer <- answer[-c(1:2)]
  print(paste0("The sum of these factorials is: ", sum(answer)))
  print(paste0("The first digit factorial is: ", answer[1]))
  print(paste0("The first second digit factorial is: ", answer[2]))
}
