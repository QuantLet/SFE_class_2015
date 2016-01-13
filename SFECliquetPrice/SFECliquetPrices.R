#------------------------------------------------------------------------
# Book:         SFE4
# -----------------------------------------------------------------------
# Quantlet:     SFECliquetPrices
# -----------------------------------------------------------------------
# Description:  SFEcliquetprices calculates and plots BS price of a 
#               cliquet call option as a function of S, r, D, sigma and 
#               points of reset.
#               The cliquet option is priced as sum of aligned start 
#               forward options with strike price equal to the underlying
#               at the reset points.
#------------------------------------------------------------------------
# Usage:        -
#------------------------------------------------------------------------
# See also:     SFECliquetPayoff
#------------------------------------------------------------------------
# Keywords:     black-scholes, plot, option, exotic-option, option-price
#------------------------------------------------------------------------
# Inputs:       S0, r, D, sigma, reset
#------------------------------------------------------------------------
# Output:       A plot of the underlying and a plot of the price of a 
#               cliquet call option with one pay-off at the final 
#               maturity. 
# -----------------------------------------------------------------------
# Example:      plots are generated for the following parameter values:
#               S0 = 100, r = 0.3, D = 0.01, sigma = 0.5, 
#               reset = c(0.3, 0.7)
#------------------------------------------------------------------------
# Author:       Weongi Woo, Thorsten Disser, 2015/12/20
#------------------------------------------------------------------------
#########################################################################
# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

#Parameter Setting
S0 	  = 100               # stock price at t = 0
r 	  = 0.03              # risk free interest rate
D     = 0.01              # continuous dividend payment
b     = r - D             # cost of carry
sigma = 0.5               # volatiliy
reset = c(0.3, 0.7)       # reset points


#########################################################################
#Simulating Underlying Price
Tn 	= 1000
t 	= (1:Tn)/Tn
dt 	= t[2] - t[1]
Wt1 = rnorm(length(t), mean = 0, sd = 1)
Wt 	= cumsum(Wt1)  # cumulative sum
St 	= S0 * exp((r - 0.5 * sigma) * dt + sigma * sqrt(dt) * Wt)

#########################################################################
#adding time 0 and 1
if(reset[1] != 0){
  reset = c(0, reset)
}

if(reset[length(reset)] != 1) {
  reset[length(reset) + 1] = 1
}

#Generating Strike Prices
K1 = S0
for(i in 2:length(reset)) {
  assign(paste("K", i, sep=""), St[(reset[i]) * 1000])
}

#########################################################################
#Cliquet Call Pricing model
d1        = 0
d2        = 0
bscall    = 0
callprice = 0
m         = reset[2]

vanilla = function(s, m){
# Computes the price of a European Call Option for given period
#
# Args:
#     s: the time point where the European Call Option begins 
#     m: the maturity
#
# Returns:
#     A vector of European Call Option price for given life of the option
#
  if(s == 0) {
    for(i in 1:((m * 1000) - 1)){ 
      d1[i] = (log(St[i]/K1) + (b - (sigma^2/2)) * (m - i/1000)) / (sigma * sqrt((m - i/1000)))
      d2[i] = d1[i] - (sigma * sqrt((m - i/1000)))
      bscall[i] = exp((b - r) * (m - i/1000)) * St[i] * pnorm(d1[i]) - exp(-r * (m - i/1000)) * K1 * pnorm(d2[i])
    }
    bscall[(m*1000)] = max(St[(m*1000)] - K1, 0) #Option Price at Boundary is Intrinsic Value
  } else {
    for(i in ((s*1000)+1):((m*1000)-1)){
      d1[i] = (log(St[i]/get(paste("K", match(s, reset), sep=""))) + (b - (sigma^2/2)) * (m - i/1000)) / (sigma * sqrt((m - i/1000)))
      d2[i] = d1[i] - (sigma * sqrt((m - i/1000)))
      bscall[i] = exp((b - r) * (m - i/1000)) * St[i] * pnorm(d1[i]) - exp(-r * (m - i/1000)) * get(paste("K", match(s, reset), sep="")) * pnorm(d2[i])
    }
    bscall[(m * 1000)] = max(0, (St[(m * 1000)] - get(paste("K", (match(s, reset)), sep="")))) #Call Price at Boundary is Intrinsic Value
  }
  assign("callprice", bscall, envir = .GlobalEnv)
}

forward = function(sp, s, m){
# Computes the price of a Forward Start Call Option for given life of the option
#
# Args:
#     sp: the time point where the price of the Forward Start Call Option will be evaluated
#     s : the time point where the Forward Start Option begins
#     m : the maturity
#
# Returns:
#     A vector of Forward Start Call Option price for given life of the option
#
    for(i in ((sp + 0.001) * 1000):(reset[(match(sp, reset) + 1)] * 1000)){
    d1[i] = ((b + sigma^2/2) * (m - s)) / (sigma * sqrt(m - s))
    d2[i] = d1[i] - (sigma * sqrt(m - s))
    bscall[i] = St[i] * exp((b - r) * (s - sp - i/1000 + 0.001)) * ((exp((b - r) * (m - s)) * pnorm(d1[i])) - (exp(-r * (m - s)) * pnorm(d2[i])))
  }
  assign("forwardcallprice", bscall, envir = .GlobalEnv)
}

#summing up vanilla options
vanres = rep(0, 1000)
forres = rep(0, 1000)
for(i in 1:(length(reset) - 1)){
  vanilla(s = reset[i], m = reset[i+1])
  callprice[is.na(callprice)] = 0
  vanres = vanres + c(callprice, rep(callprice[length(callprice)], 1000 - length(callprice)))
}

#summing up forward start options
for(i in 1:(length(reset) - 2)) {
  for(j in 1:(length(reset) - 1 - i)) {
    forward(sp = reset[i], s = reset[i + j], m=reset[i + j + 1])
    forwardcallprice[is.na(forwardcallprice)] = 0
    forres = forres + c(forwardcallprice, rep(0, 1000 - length(forwardcallprice)))  
  }  
}

#summing up vanilla options and forward start options 
cliquet = vanres + forres

#########################################################################
#Generating Plots
par(mfrow = c(2,1), mar = c(2,4,3,3))
plot(t, St, type="l", col="blue3", xlab="", ylab="S_t", xaxt="n", lwd=2)
title("Cliquet Call Option Price")
axis(1, at=c(reset), label=c(reset))
par(mar=c(4.5,4,2,3))
plot(t, cliquet, type="l", xlab="Time", ylab="Cliquet", col="red3", xaxt="n", lwd=2)
axis(1, at=c(reset), label=c(reset))
