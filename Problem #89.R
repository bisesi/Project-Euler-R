#ATB
#Project Euler Problem #89
#Roman numerals
#Find characters saved by writing number in minimal roman form

romanNumerals <- function(x){
  numerals <- data.frame(x)
  numerals_split <- sapply(x, function(x) strsplit(x, ""))
  numerals2sums <- lapply(numerals_split, function(x) sum(as.numeric(str_replace_all(x, c("M" = "1000",
                                                  "C" = "100",
                                                  "D" = "500",
                                                  "L" = "50",
                                                  "X" = "10",
                                                  "V" = "5",
                                                  "I" = "1")))))
  convertRoman <- function(x){
    M <- paste0(rep("M" , floor(x/1000)), collapse="")
    x <- x%%1000
    if (x>=500) D <- "D" else D <- ""
    x <- x%%500
    C <- paste0(rep("C" , floor(x/100)), collapse="")
    if (nchar(C)==4) {
      C <- ""
      ifelse(D=="D", D <- "CM", D <- "CD")
    }
    x<- x%%100
    if (x>=50) L <- "L" else L <- ""
    x <- x%%50
    X <- paste0(rep("X" , floor(x/10)), collapse="")
    if (nchar(X)==4) {
      X <- ""
      ifelse(L=="L", L <- "XC", L <- "XL")
    }
    x <- x%%10
    if (x>=5) V <- "V" else V <- ""
    x <- x%%5
    I <- paste0(rep("I" , x), collapse="")
    if (nchar(I)==4) {
      I <- ""
      ifelse(V=="V", V <- "IX", V <- "IV")
    }
    return(paste0(M, D, C, L, X, V, I))
  }
  converted <- lapply(numerals2sums, convertRoman)
  count <- c()
  for (i in 1:length(converted)){
    count[i] <- nchar(converted[[i]])
  }
  count_original <- c()
  for (i in 1:length(numerals)){
    count_original[i] <- nchar(numerals[i])
  }
  compare <- as.data.frame(cbind(count, count_original))
  saved <- compare %>%
    mutate(saved = count_original - count) 
  return(sum(saved$saved))
}

