#ATB
#Project Euler Problem #19
#Counting Sundays
#How many Sundays between two years?

countSundays <- function(x,y){
  date1 <- paste0(x,"-01-01")
  date2 <- paste0(y,"-01-01")
  dates <- seq.Date(as.Date(date1), as.Date(date2), by = "days")
  days <- rep(1:7, length.out = length(dates))
  answer <- sum(days[substr(dates, 9,10) == "01"] == 1)
  return(answer)
}

