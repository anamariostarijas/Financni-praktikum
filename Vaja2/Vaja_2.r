# FINANČNI PRAKTIKUM, 2.VAJE

# 1. NALOGA
# a)
# uvoz knjižnic
library(actuar)
library(readr)
library(ggplot2)

# uvoz podatkov, izbran vzorec3
vzorec3 <- read_csv("Financni-praktikum\\Vaja2\\vzorec3.txt", col_names="odskodnina")

#vzorčna porazdelitev individualnih škodnih zahtevkov
histogram1 <- hist(vzorec3$odskodnina, 
                   col=c(blues9), 
                   xlab="Visina odskodnine", ylab="Frekvenca",
                   main="Histogram odskodnin")
print(histogram1)

# b) modeliranje višine škodnega zahtevka
parametri <- mde(vzorec3$odskodnina, dexp, start=list("rate"=1), measure="CvM")
rate <- parametri$estimate

# c)
histogram2 <- hist(vzorec3$odskodnina, col=c("cadetblue2"), xlab="Visina odskodnine", ylab="Density",
                   main="Histogram odskodnin", probability = TRUE)
curve(dexp(x, rate), add=TRUE, col="blue")
legend("topright", "eksponentna porazdelitev", col="blue", lty=1)

# d) upanje in disperzija kolektivne škode S
binomska <- rbinom(n=20, size=20, prob=1/2)
povpr_bin = 10 
povp_exp = 1/rate
var_bin = 5
var_exp = 1/(rate^2)
upanje = povpr_bin * povp_exp
print(upanje)
varianca = povpr_bin * var_exp +(povp_exp - var_exp) * var_bin
print(varianca)

# 2. NALOGA

# a) diskretiziranje porazdelitev spremenljivke Y
h = 0.25
n=100
disc <- discretise(pexp(x, rate), from=0, to=n*h, step=h)

# b) 

g <- plot(stepfun(seq(0,24.9,by=h),diffinv(disc)), main = "Eksponentna porazdelitev",
             ylab = "Porazdelitvena funkcija")
curve(pexp(x,rate), add = TRUE, col = "red")
legend("bottomright", legend=c('diskretizacija', 'Eksponentna porazdelitev'), col=c('black','red'),lty=1:1, cex=0.8)

# c)

disc_1 <- discretize(pexp(x, rate), step=h, from=0, to=10000)
por_S <- aggregateDist(method="recursive", 
                              model.freq = "poisson",
                              model.sev = disc_1, 
                              lambda = 15, x.scale=h, tol=0.001, convolve = 0)
plot(por_S,  main='Porazdelitvena funkcija odskodnin', xlab='Višina odskodnine')

# d)

upanje_por_S <- mean(por_S)
print(upanje_por_S)
varianca_por_S <- sum(knots(por_S)^2 * diff(por_S)) - upanje_por_S^2
print(varianca_por_S)

# 3. NALOGA

# a)

sim_N <- rpois(10000,15)
print(sim_N)
sim_S <- vector(mode="numeric", length=10000)
k <- 1
for (i in sim_N){
  sim_S[k] <- sum(rexp(i, rate))
  k <- k + 1
}
print(sim_S)

# b)

upanje_S <- mean(sim_S)
print(upanje_S)
disperzija_S <- var(sim_S)
print(disperzija_S)
# Vrednosti se precej ujemata, upanje je bilo ocenjeno bolj natančno kot disperzija.


# d)
plot(por_S)
plot(ecdf(sim_S), col = "green", lwd = 3, add = TRUE)
legend('bottomright', 
       legend = c('Panjerjev algoritem', 'Monte Carlo simulacija'),
       col = c('black', 'green'),
       lty=1:1, cex=0.75,
       bty = "n")
