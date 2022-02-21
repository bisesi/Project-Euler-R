#ATB
#Project Euler Problem #22
#Names scores
#Sort alphabetical list and find score

namesScore <- function(x) {
  alphabetical <- sort(x)
  ordered <- as.data.frame(cbind(alphabetical, rank = rank(alphabetical)))
  ordered$rank <- as.numeric(ordered$rank)
  strings <- strsplit(alphabetical, "")
  scores <- c()
  for (i in 1:length(strings)){
    scores[[i]] <- sum(as.numeric(factor(strings[[i]], levels = letters)))
  }
  scoredrank <- cbind(ordered, scores = do.call(rbind, scores))
  scoredrank$product <- scoredrank$rank * scoredrank$scores
  return(sum(scoredrank$product))
}