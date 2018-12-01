# 3. VAJE FP
library(combinat)
# 1. Naloga
# b)
izplacilo <- function(vrsta, W, type) {
  K <-(sum(vrsta*W))/sum(W)
  if (type == "call") {
  return (max(vrsta[length(vrsta)] - K, 0))
  } else {
    return (max(K - vrsta[length(vrsta)] , 0))
  }
}

#a) 
W <- c(1, 2, 3, 4, 5, 6)
S0 <- 50 
u <- 1.05 
d <- 0.95 
T <- 5
R <- 0.03
vrs <- c(50.00, 52.50, 49.88, 47.38, 45.01, 47.26)
izplacilo(vrs,W,"put")

#2.Naloga
# a)

binomski <- function(S0,u,d,R,T,W,type){
  matrika <- cbind(S0, u^(hcube(rep(2,T))-1) * d^(2-hcube(rep(2,T))))
  com_prod_vrstic <- t(apply(matrika, 1, cumprod))
  izplacila_vrstic <- t(apply(com_prod_vrstic, 1, izplacilo, W=W, type=type))
  q <- (1+R-d)/(u-d)
  st_u <- rowSums(hcube(rep(2,T))-1)
  st_d <- T - st_u
  return (sum(izplacila_vrstic * (q^st_u*((1-q)^st_d)))/(1+R)^T)
}
binomski(S0,u,d,R,T,W,"put")
#b)

monte <- function(S0,u,d,R,T,W,type,N){
  p <- (1+R-d)/(u-d)
  drevo <- matrix(rbinom(T*N,1,p),N,T)
  drevo[drevo == 1] <- u
  drevo[drevo == 0] <- d
  
  drevo <- t(apply(drevo,1, cumprod))
  
  vrednosti <- cbind(S0, S0*drevo)
  izplacila <- apply(vrednosti,1, function(x) izplacilo(x,W,type))
  premija_opcije <- mean(izplacila)/(1+R)^T
  
  return(premija_opcije)
}

W1 = rep(1, 16)

ocena1 <- monte(60,1.05,0.95,0.01,15,W1,"put",10) 
ocena2 <- monte(60,1.05,0.95,0.01,15,W1,"put",100) 
ocena3 <- monte(60,1.05,0.95,0.01,15,W1,"put",1000) 

#3.naloga

#a
N1 <- c()
N2 <- c()
N3 <- c()
M <- 100

for (k in c(1:M)){
  N1 <- c(N1,monte(60,1.05,0.95,0.01,15,W1,"put",10))
  N2 <- c(N2,monte(60,1.05,0.95,0.01,15,W1,"put",100))
  N3 <- c(N3,monte(60,1.05,0.95,0.01,15,W1,"put",1000))
}

premija_binomske <- binomski(60,1.05,0.95,0.01,15, W1,"put")

#N = 10
povprecje1 <- mean(N1)

odklon1 <- sqrt(var(N1))

h1 <- hist(N1,
           xlim = c(0,5),
           main = "Monte Carlo: N = 10",
           xlab = "Premija",
           ylab = "Frekvenca",
           col = "yellow"
)
abline(v = povprecje1,col= "green",lwd = 2)
abline(v = premija_binomske, col = "red", lty = 2)
arrows(x0 = povprecje1,y0 = 0, x1 = povprecje1 + odklon1, col = "green",length = 0.1,lwd = 2)
arrows(x0 = povprecje1,y0 = 0, x1 = povprecje1 - odklon1, col = "green",length = 0.1,lwd = 2)

legend("topright",legend = c("Monte Carlo", "Analiza modela"),
       col = c("green","red"),lty = c("solid","dashed"), cex = 0.8)


#N = 100
povprecje2 <- mean(N2)

odklon2 <- sqrt(var(N2))

h2 <- hist(N2,
           xlim = c(0,5),
           main = "Monte Carlo: N = 100",
           xlab = "Premija",
           ylab = "Frekvenca",
           col = "yellow"
)
abline(v = povprecje2,col= "green",lwd = 2)
abline(v = premija_binomske, col = "red", lty = 2)
arrows(x0 = povprecje2,y0 = 0, x1 = povprecje2 + odklon2, col = "green",length = 0.1,lwd = 2)
arrows(x0 = povprecje2,y0 = 0, x1 = povprecje2 - odklon2, col = "green",length = 0.1,lwd = 2)

legend("topright",
       legend = c("Monte Carlo", "Analiza modela"),
       col = c("green","red"),lty = c("solid","dashed"),cex = 0.8)


#N = 1000
povprecje3 <- mean(N3)
odklon3 <- sqrt(var(N3))

h3 <- hist(N3,
           xlim = c(0,5),
           main = "Monte Carlo: N = 1000",
           xlab = "Premija",
           ylab = "Frekvenca",
           col = "yellow"
)
abline(v = povprecje3,col= "green",lwd = 2)
abline(v = premija_binomske, col = "red", lty = 2)
arrows(x0 = povprecje3,y0 = 0, x1 = povprecje3 + odklon3, col = "green",length = 0.1,lwd = 2)
arrows(x0 = povprecje3,y0 = 0, x1 = povprecje3 - odklon3, col = "green",length = 0.1,lwd = 2)

legend("topright",
       legend = c("Monte Carlo", "Analiza modela"),
       col = c("green","red"),lty = c("solid","dashed"),cex = 0.8)