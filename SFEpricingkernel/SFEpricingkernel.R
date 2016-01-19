# Clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# Set working directory
# setwd('~/...')    		# linux/mac os
# setwd('/Users/...') 		# windows

## Install packages
# install.packages(KernSmooth)
# install.packages(rugarch)
# install.packages(fGarch)
# install.packages(forecast)
# install.packages(TSA)
# install.packages(ks)
# install.packages(matlab)

# Load packages
library(KernSmooth)
library(rugarch)
library(fGarch)
library(forecast)
library(TSA)
library(ks)
library(matlab)

#--------------------------------------------------------#
# (0) Adjust the following Parameters for different data #
#--------------------------------------------------------#

# File Names 
Odata		= "odata20142212.txt"	# option data
Idata		= "DAX.txt"		# dax index data

# Reading Option Data
OdataAll 	= read.table(Odata, header=TRUE)
Odata1		= subset(OdataAll,OdataAll[,7]<1 & OdataAll[,4]<0.9 
		& OdataAll[,6]==1)

# Reading Index Data
dax1		= read.table(Idata, header=TRUE)[,2]
periods		= length(dax1)
dax		= c(1:periods)
# Apply the foolowing loop if the data are given in reversed order
# if not then set dax=dax1 
dax		= flipud(dax1)
dax.ts 		= ts(dax)
dax.ret		= log(dax.ts)[2:periods]-log(dax.ts)[1:(periods-1)]
dax.retts	= ts(dax.ret)

# Parameters for calculation/simulation
numbapprox  	= 2000			# fineness of the grid
N		= 5000			# No. of Simulations
# Check return series for ARMA effects, e.g. with the following function
# auto.arima(dax.retts, max.p=10, max.q=10, max.P=5, max.Q=5, 
# start.p=1, start.q=1,start.P=1, start.Q=1, stationary=T, seasonal=F)
p		= 0
q		= 0
arma		= c(p,q)
# specify garch order (need to be checked)
m		= 1
s		= 1
garch		= c(m,s)
# Specify GARCH model (default is standard GARCH)
# for changing GARCH-model + submodel, please refer to 
# rugarch package for further information
garchmodel	= "fGARCH"
submodel	= "GARCH"
# underlying distribution (default: "sstd" - skewed stundent t's)
# (alternatives: "norm" - normal, "ghyp"- generalized hyperbolic)
udist		= "sstd"
# set archm=T for ARCH in mean model (archpow specifies the power)
archm		= F
archpow		= 1
# set include.mean = F if you don't want to include a mean in the mean model
include.mean 	= T  

#--------------------#
# (1) SPD Estimation #
#--------------------#

#---------------------------------------------------------------------------#
# Function for estimation of state price densities (risk neutral densities) #
#---------------------------------------------------------------------------#

spdbl	= function(m, sigma, sigma1, sigma2, s, r, tau){

rm	= length(m)
st	= sqrt(tau)
ert	= exp(r*tau)
rt	= r*tau

# Modified Black-Scholes scaled by S-div instead of F
d1	= (log(m)+tau*(r+0.5*(sigma^2)))/(sigma*st)
d2	= d1-sigma*st

f	= pnorm(d1,mean=0,sd=1)-pnorm(d2,mean=0,sd=1)/(ert*m)

# First derivative of d1 term
d11	= (1/(m*sigma*st))-(1/(st*(sigma^2)))*((log(m)+tau*r)*sigma1)+
	0.5*st*sigma1

# First derivative of d2 term
d21	= d11-st*sigma1

# Second derivative of d1 term
d12	= -(1/(st*(m^2)*sigma))-sigma1/(st*m*(sigma^2))+sigma2*
	(0.5*st-(log(m)+rt)/(st*(sigma^2)))+
	sigma1*(2*sigma1*(log(m)+rt)/(st*sigma^3)-1/(st*m*sigma^2))

# Second derivative of d2 term
d22	= d12-st*sigma2

# Please refer to either Rookley (1997) or 
# the XploRe Finance Guide for derivations
f1	= dnorm(d1,mean=0,sd=1)*d11+(1/ert)*((-dnorm(d2,mean=0,sd=1)*d21)/m+
	pnorm(d2,mean=0,sd=1)/(m^2))

f2	= dnorm(d1,mean=0,sd=1)*d12-d1*dnorm(d1,mean=0,sd=1)*(d11^2)-
	(1/(ert*m)*dnorm(d2,mean=0,sd=1)*d22)+
	((dnorm(d2,mean=0,sd=1)*d21)/(ert*m^2))+
	(1/(ert*m)*d2*dnorm(d2,mean=0,sd=1)*(d21^2))-
	(2*pnorm(d2,mean=0,sd=1)/(ert*(m^3)))+
	(1/(ert*(m^2))*dnorm(d2,mean=0,sd=1)*d21)
                                                                                
# Recover strike price
x	= s/m
c1	= -(m^2)*f1
c2	= s*((1/x^2)*((m^2)*f2+2*m*f1))

# Calculate the quantities of interest
cdf	= ert*c1+1
fstar	= ert*c2

return(fstar)
}

#--------------------------#
# Nonparametric Regression #
#--------------------------#
 
SPDlpe 		= function(RawData1, gx, numbapprox, SpotPrice, Strike, maturity){

Moneyness     	= as.matrix(SpotPrice/Strike)
lm		= length(Moneyness)
Y	        = as.matrix(RawData1[,7]) 	# implied volatility
interestrate	= unique(RawData1[,3])
 
beta		= matrix(0, nrow = numbapprox, ncol = 3)

j		= 1

while(j < numbapprox + 1){
	i 	= 1
	X 	= matrix(0, nrow=lm, ncol=3)
	
	while(i < lm+1){
		X[i,]	= t(c(1,(Moneyness[i]-gx[j]),(Moneyness[i]-gx[j])^2))
		i	= i+1	
}

#Choosing bandwith
	h 	= (nrow(RawData1))^(-1/9)
		
	W 	= matrix(0, nrow=lm, ncol=lm)
	i 	= 1
	while(i < lm+1){
		u	= (Moneyness[i]-gx[j])/h
		W[i,i]	= (15/16)*(1-u^2)^2*(abs(u) <= 1)/h
		i	= i+1
}

	beta[j,]	= t(solve(t(X)%*%W%*%X)%*%t(X)%*%W%*%Y)
	j 		= j+1
}

G			= cbind(gx,beta)
	
RNDens			= spdbl(G[,1], G[,2], G[,3], G[,4], SpotPrice, 
			interestrate, maturity)
	
#Recover Grid/evaluation points
temp			= (1/(G[,1]/SpotPrice))
return(list(RNDens=RNDens, temp=temp))
}

#-----------------------------------#
# (2) Historical Density Estimation #
#-----------------------------------#

#-----------------------------------------#
# Simulation via Time Series: GARCH model #
#-----------------------------------------#

HDEgarch 		= function(garchfit, data, grid, maturity, N, start){
# Simulation
garchsim 		= ugarchsim(garchfit, n.sim = round(maturity*365), 
			n.start = 0, m.sim=N, startMethod=("sample"), 
			mexsimdata=TRUE)

# head(g11sim@simulation)[2]= extracting simulated return data
returnsim		= as.vector(head(garchsim@simulation)[2])

# Calculating spot prices from return data
for(i in 1:N) {
	value[,i]  	= start*exp(sum(returnsim$seriesSim[,i]))		
}

# Computing density on given grid: Either by using build in function (kde(...)):
# kde() computes bandwith h wih function hpi() which uses Wand&Jones (1994) estimator
ValDens			= kde(value[1,],eval.points=grid, gridsize=length(grid),
					xmin=min(grid), xmax=max(grid))
return(ValDens$estimate)
}

#-----------------#
# (3) Calculation #
#-----------------#

# Capture different maturities
RawData    		= Odata1
maturities 		= sort(unique(RawData[,4]),decreasing=F)
SpotPrice		= matrix(0, ncol = length(maturities))

# Needed specifications for function SPDlpe()
minK			= as.numeric(quantile(RawData[,2], probs = 0.15, type = 6))
maxK			= as.numeric(quantile(RawData[,2], probs = 0.99, type = 6))
gx			= matrix(0, nrow = numbapprox, ncol = length(maturities))

# Needed specifications for function HDEtss()
spec			= ugarchspec(variance.model = list(model = garchmodel, 
			garchOrder = garch, submodel = submodel), mean.model = 
			list(armaOrder = arma, archm=archm,archpow=archpow,
			include.mean=include.mean), distribution.model = udist)
					
# for faster computation change type of solver
garchfit 		= ugarchfit(data=dax.retts, spec=spec, solver = "hybrid")
value			= matrix(0,ncol=N)

#------------------------------------------------------#
# Estimation of SPD's / Historical Densities and EPK's #
#------------------------------------------------------#

temp			= matrix(0, nrow = numbapprox, ncol = length(maturities))
RND			= matrix(0, nrow = numbapprox, ncol = length(maturities))
HDE			= matrix(0, nrow= numbapprox, ncol=length(maturities))  
EPK			= matrix(0, nrow= numbapprox, ncol=length(maturities))  

for(t in 1:length(maturities)){
	RawData1	= subset(RawData, RawData[,4] == maturities[t])
	Strike		= RawData1[,2]
	SpotPrice[,t] 	= unique(RawData1[,1])
	gx[,t]		= seq(SpotPrice[,t]/maxK, SpotPrice[,t]/minK, 
			length.out = numbapprox)
	spdlpe		= SPDlpe(RawData1, gx[,t], numbapprox, 
			SpotPrice[,t], Strike, maturities[t])
	RND[,t]		= spdlpe$RNDens
	temp[,t]	= spdlpe$temp
	HDE[,t]		= HDEgarch(garchfit, dax.retts, temp[,t], 
			maturities[t], N, SpotPrice[,t])
	EPK[,t]		= RND[,t] / HDE[,t]
}

#-----------------------------------#
# (4) Plot EPK's for all maturities #
#-----------------------------------#

#Capture days to maturity/date/index price
days			= rep(round(maturities*365))
date			= as.character(RawData[1,8])		
indexprice 		= as.character(SpotPrice[,1])

# Plot EPK's for all maturities
dev.new()
par(mar=c(5, 5, 5, 5))
title			= paste("EPKs on",date, "for index price", indexprice)
plot(temp[,1],EPK[,1],col=1,type="l", xlab=~S[T], ylab="EPK", cex=1.5,
xlim=c(min(temp[,1]),max(temp[,1])),ylim=c(0,4), lwd=3, cex.lab=1, main=title)	

for(t in 2:length(maturities)){
	lines(temp[,t], EPK[,t],type="l",col=t, lwd=3)
}
legend("topright",legend=days,col=c(1:length(maturities)) ,
title="Days to maturity", lwd=2, cex=.75,xpd=T,inset=c(-0.165,0))
	
# Uncomment to plot EPK's next to corresponding SPD and historical density
# for(t in 1:length(maturities)){
	# dev.new()
	# par(mfrow=c(1,2),mar=c(10, 5, 5, 5))
	# plot(temp[,t],RND[,t],col="blue",type="l", xlab=~S[T], ylab="Density", 
	# xlim=c(min(temp[,1]),max(temp[,1])), main="SPD (blue) and \n historical density (red)", lwd=3)		
	# lines(temp[,t],HDE[,t],type="l", col="red", lwd=3)
	# title	= cbind(paste("Empirical pricing kernel on", date), 
	# paste("with tau=",days[t],"and index price=", indexprice))
	# plot(temp[,t],EPK[,t], lwd=3,col="black",type="l", xlab=~S[T], ylab="EPK", 
	# xlim=c(min(temp[,1]),max(temp[,1])),ylim=c(0,5), main=title, cex=1)	
# }
