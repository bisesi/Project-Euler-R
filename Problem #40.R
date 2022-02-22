#ATB
#Project Euler Problem #40
#Champernowne's constant
#Return the product of the nth digits of the fractional part

champernownesConstant <- function(x){
  string <- as.character(c(1:1000000))
  constant <- paste(string, collapse = "")
  d1 <- stringr::str_sub(constant, 1,1)
  d10 <- stringr::str_sub(constant, 10,10)
  d100 <- stringr::str_sub(constant, 100,100)
  d1000 <- stringr::str_sub(constant, 1000,1000)
  d10000 <- stringr::str_sub(constant, 10000,10000)
  d100000 <- stringr::str_sub(constant, 100000,100000)
  d1000000 <- stringr::str_sub(constant, 1000000,1000000)
  if (x == 1){
    return(as.numeric(d1))
  }
  if (x == 10){
    return(as.numeric(d1)*as.numeric(d10))
  }
  if (x == 100){
    return(as.numeric(d1)*as.numeric(d10)*as.numeric(d100))
  }
  if (x == 1000){
    return(as.numeric(d1)*as.numeric(d10)*as.numeric(d100)*as.numeric(d1000))
  }
  if (x == 10000){
    return(as.numeric(d1)*as.numeric(d10)*as.numeric(d100)*as.numeric(d1000)*as.numeric(d10000))
  }
  if (x == 100000){
    return(as.numeric(d1)*as.numeric(d10)*as.numeric(d100)*as.numeric(d1000)*as.numeric(d10000)*as.numeric(d100000))
  }
  if (x == 1000000){
    return(as.numeric(d1)*as.numeric(d10)*as.numeric(d100)*as.numeric(d1000)*as.numeric(d10000)*as.numeric(d100000)*as.numeric(d1000000))
  }
}
