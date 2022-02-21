#ATB
#Project Euler Problem #4
#Largest palindrome product
#Find the largest palindrome made from the product of two n-digit numbers (n < 7)

largestPalindrome <- function(x){
  is.palindrome <- function(x) { #check if a number is a palindrome
    digits <- strsplit(as.character(x), "")[[1]]
    all(digits == rev(digits))
  }
  if (round(x) < 5){
    if (round(x) == 0){
      nums = 0
    } else if (round(x) == 1){
      nums = 1:9
    } else if (round(x) ==2){
      nums = 1:99
    } else if (round(x) == 3){
      nums = 1:999
    } else if (round(x) == 4){
      nums = 1:9999
    } else if (round(x) == 5){
      nums = 1:99999
    } 
  }else {
    stop("This generator cannot accommodate such a large input. Please choose a smaller number.")
  }
  products <- outer(nums, nums)
  pal_list <- sapply(products, is.palindrome)
  max_pal <- products == max(products[!!pal_list])
  result <- col(products)[max_pal]
  return(prod(result))
}