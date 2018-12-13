# 4. vaje

# 1. naloga
library(readr)
library(graphics)
library(stats)
#a)
#srebro <- read_csv('srebro.csv')
srebro <- srebro[,c(0,1,5)]
srebro <- srebro[123:1,]
srebro$Close <- as.numeric(gsub("\\$", "", srebro$Close))
#b)
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

#b), #c)
napoved_vrsta <- function(vrsta,k){
  zglajena_vrsta <- G(vrsta,k)
  napoved <- rep(tail(zglajena_vrsta, n = 1),10)
  napoved_vrsta <- ts(c(zglajena_vrsta,napoved))
  return(napoved_vrsta)
}
napoved5 <- napoved_vrsta(timeseries,5)
graf1 <- ts.plot(timeseries,napoved5,
        main ="Drseče povprečje reda 5",
        xlab = "Čas",
        ylab = "Vrednost v dolarjih",
        col = c("black", "red"),
        lwd = c(1,2))
points(srebro$Close, pch = 20)

#d)
SKN <- function(vrsta, k){
  glajena <- G(vrsta,k)
  napaka <- 0
  for (i in k:(length(vrsta)-1)){
    napaka <- napaka + (vrsta[i+1] - glajena[i+1])^2
  }
  return (napaka/(length(vrsta)-k))
}

skn5 <- SKN(timeseries, 5)
print(skn5)

#e)
napoved15 <- napoved_vrsta(timeseries,15)
graf_napovedana15 <- ts.plot(timeseries,napoved15,
                             main ="Drseče povprečje reda 15",
                             xlab = "Time",
                             ylab = "$",
                             col = c("black", "red"),
                             lwd = c(1,2))
points(srebro$Close, pch = 20)
skn15 <- SKN(timeseries, 15)
print(skn15)

napoved30 <- napoved_vrsta(timeseries,30)
graf_napovedana30 <- ts.plot(timeseries,napoved30,
                             main ="Drseče povprečje reda 30",
                             xlab = "Time",
                             ylab = "$",
                             col = c("black", "red"),
                             lwd = c(1,2))
points(srebro$Close, pch = 20)
skn30 <- SKN(timeseries, 30)
print(skn30)


# 3. naloga
# a)

EG <- function(vrsta, alpha){
  dolzina <- length(vrsta)
  glajene_vrednosti <- vrsta[1]
  for (i in 2:dolzina){
    glajene_vrednosti[i] <- alpha*vrsta[i] + (1-alpha)*glajene_vrednosti[i-1]
  }
  zglajena_vrsta <- ts(glajene_vrednosti)
  return(zglajena_vrsta)
}

# b)
# alpha = 0,15
eks_glajena <- EG(timeseries,0.15)
napoved_eks <- tail(eks_glajena, n=1) 
print(napoved_eks)
eks_glajena_napoved <- ts(c(eks_glajena, rep(napoved_eks, 10)))
graf_eksponentno <- ts.plot(timeseries,eks_glajena_napoved,
                            main ="Eksponentno glajenje alpha = 0.15",
                            xlab = "Time",
                            ylab = "$",
                            col = c("black", "red"),
                            lwd = c(1,2))
points(srebro$Close, pch = 20)


# c)

SKN_E <-function(vrsta, alpha){
  dolzina <- length(vrsta)
  napaka <- 0
  glajena_vrsta <- EG(vrsta, alpha)
  for (i in 1:(dolzina-1)){
    napaka <- napaka + (vrsta[i+1] - glajena_vrsta[i+1])^2
  }
  return(napaka/(dolzina-1))
}

opt_alpha <- as.numeric(optimize(SKN_E, c(0,1), vrsta = timeseries)[1])
print(opt_alpha)

# d) 
# alpha = 0.9999315
eks_glajena_OPT <- EG(timeseries,0.9999315)

napoved_eks_OPT<- tail(eks_glajena_OPT, n=1)  
print(napoved_eks_OPT)
eks_glajena_OPT_napoved <- ts(c(eks_glajena_OPT, rep(napoved_eks_OPT, 10)))

graf_eks_OPT <- ts.plot(timeseries,eks_glajena_OPT_napoved,
                               main ="Eksponentno glajenje optimalna alpha",
                               xlab = "Time",
                               ylab = "$",
                               col = c("black", "red"),
                               lwd = c(1,2))
points(srebro$Close, pch = 20)
