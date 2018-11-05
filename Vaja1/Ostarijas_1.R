# FINANČNI PRAKTIKUM
# 1. VAJE
# 1. Naloga
# importing the data for selected time period 
library(readr)
# deleting unnecessery columns
rates2013 <- rates_2013[,c(1,2,24,44,64,85,107,127,150,172,193,216,237)] 
rates2012 <- hist_EURIBOR_2012[,c(1,2,24,45,67,86,108,129,151,174,194,217,239)]
rates2011 <- hist_EURIBOR_2011[,c(1,2,23,43,66,85,107,129,150,173,195,216,238)]
# editing tables 
rates2011 <- t(rates2011)
rates2012 <- t(rates2012)
rates2013 <- t(rates2013)
colnames(rates2011) <- rates2011[1, ] # the first row will be the header
rates2011 <- rates2011[-1, ] 
colnames(rates2012) <- rates2012[1, ] # the first row will be the header
rates2012 <- rates2012[-1, ] 
colnames(rates2013) <- rates2013[1, ] # the first row will be the header
rates2013 <- rates2013[-1, ] 
#joining tables
rates <- rbind(rates2011, rates2012, rates2013)
# plotting 6x12 time
rates_6_12 <- rates[,c(9,15)]
timeseries <- ts(data=rates_6_12[,1], start=c(2011, 1), frequency=12)
timeseries2 <- ts(data=rates_6_12[,2], start=c(2011, 1), frequency=12)
plot_euribor <- ts.plot(timeseries2, timeseries, main = "EURIBOR", col = c("red", "blue"), ylab="%")
legend("topright", legend = c("6m", "12m"), col=c("blue", "red"), lty=1)
