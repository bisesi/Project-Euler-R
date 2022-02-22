#ATB
#Project Euler Problem #33
#Digit cancelling fractions
#Find the denominator of the product of four fractions that cancel 

cancellingFractions <- function(){
  getnumbers <- expand.grid(10:99, 10:99)
  getnumbers$fractions <- getnumbers$Var1 / getnumbers$Var2
  lessthanone <- getnumbers %>%
    dplyr::filter(fractions < 1)
  singledigits <- expand.grid(1:9, 1:9)
  singledigits$fractions <- singledigits$Var1 / singledigits$Var2
  singleslessthanone <- singledigits %>%
    dplyr::filter(fractions < 1)
  lookup <- merge(lessthanone, singleslessthanone, by = c("fractions"))
  lookup2 <- lookup %>%
    dplyr::rename(Big1 = Var1.x, Big2 = Var2.x, Small1 = Var1.y, Small2 = Var2.y) %>%
    dplyr::filter(Big1 %% 10 != 0 & Big2 %% 10 != 0) %>%
    dplyr::filter(Big1 %% 11 != 0 & Big2 %% 11 != 0)
  Big1 <- as.list(lookup2$Big1)
  split1 <- lapply(Big1, function(x) {as.numeric(unlist(strsplit(as.character(x), "")))})
  col1 <- c()
  col2 <- c()
  bound1 <- c()
  for (i in 1:length(split1)){
    col1[i] <- split1[[i]][1]
    col2[i] <- split1[[i]][[2]]
    bound1 <- as.data.frame(cbind(col1, col2))
  }
  Big2 <- as.list(lookup2$Big2)
  split2 <- lapply(Big2, function(x) {as.numeric(unlist(strsplit(as.character(x), "")))})
  col1 <- c()
  col2 <- c()
  bound2 <- c()
  for (i in 1:length(split2)){
    col1[i] <- split2[[i]][1]
    col2[i] <- split2[[i]][[2]]
    bound2 <- as.data.frame(cbind(col1, col2))
  }
  compare <- cbind(bound1, bound2)
  colnames(compare) <- c("a","b", "c", "d")
  reciprocal <- compare %>%
    dplyr::filter(b == c & a !=d) %>%
    dplyr::distinct(.keep_all = TRUE) %>%
    tidyr::unite(., num, c(a,b), sep = "", remove = FALSE) %>%
    tidyr::unite(., dem, c(c,d), sep = "", remove = FALSE) 
  reciprocal$num <- as.numeric(reciprocal$num)
  reciprocal$dem <- as.numeric(reciprocal$dem)
  reciprocal$a <- as.numeric(reciprocal$a)
  reciprocal$b <- as.numeric(reciprocal$b)
  reciprocal$c <- as.numeric(reciprocal$c)
  reciprocal$d <- as.numeric(reciprocal$d)
  reciprocal$fractions1 <- reciprocal$num / reciprocal$dem
  reciprocal$fractions2 <- reciprocal$a / reciprocal$d
  matched <- reciprocal %>%
    dplyr::filter(fractions1 == fractions2) %>%
    dplyr::select(a, d)
  value <- prod(matched$a) / prod(matched$d)
  return(value)
}

