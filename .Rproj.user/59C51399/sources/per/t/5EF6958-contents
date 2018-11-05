# FINANČNI PRAKTIKUM, 2.VAJE

# 1. NALOGA
# a)
# uvoz podatkov, izbran vzorec3
library(actuar)
library(readr)
library(ggplot2)
vzorec3 <- read_csv("Vaja2//vzorec3.txt", col_names="odskodnina")

#vzorčna porazdelitev individualnih škodnih zahtevkov

histogram1 <- hist(vzorec3$odskodnina, col=c(blues9), xlab="Visina odskodnine", ylab="Frekvenca",
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
