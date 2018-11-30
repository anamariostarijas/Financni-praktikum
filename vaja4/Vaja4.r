# 4. vaje

# 1. naloga
library(readr)
library(graphics)
library(stats)
srebro <- read_csv('srebro.csv')
#srebro[order(as.Date(srebro$Date, format="%m %d,%Y", decreasing = FALSE)),]
srebro <- srebro[,c(0,1,5)]
srebro <- srebro[123:1,]
srebro$Close <- as.numeric(gsub("\\$", "", srebro$Close))
timeseries <- ts(data=srebro$Close)
graf <- ts.plot(timeseries, xlab="Time", ylab="$", main="Vrednost zlata" )
points(srebro$Close, pch=20)

# 2. naloga
#a)
G <- function(vrsta, k) {
  zglajena_vrsta <- c()
  j <- 0
  for (i in (k+1):length(vrsta)){
    zglajena_vrsta[i] <- sum(vrsta[(i-k):(i-1)])/k
  }
  ts <- ts(zglajena_vrsta)
  return(ts)
}

#b)
zglajena <- G(timeseries, 5)
zglajena <- ts(c(zglajena, rep(tail(zglajena, n=1), times=10)))
napoved <- ts.plot(timeseries,zglajena, col=c("black", "red"), main="Drseče povprečje",
                   ylab="$", xlab="Time", lwd=c(1,2))
points(srebro$Close, pch=20)

#c)
SKN <- function(vrsta, glajena_vrsta, k){
  napaka <- 0
  for (i in k:length(vrsta)){
    napaka <- napaka + (vrsta[i+1]  }
}
