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
#b)

monte <- function(S0,u,d,R,T,W,type,N){
  
}