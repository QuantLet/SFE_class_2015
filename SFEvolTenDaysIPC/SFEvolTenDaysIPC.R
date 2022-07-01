# Install Performance Analytics package version 1.4.3541 and highfrequency package version 0.4. Zoo and xts package needs to be installed
# Clean up our space
graphics.off()
rm(list = ls())

# Load Packages
library(xts)
library(PerformanceAnalytics)
library(highfrequency)

# Download File
ipc = read.csv(file = "IPC (5 MIN) 26_11-10_12.csv", sep = ",", head = TRUE)

# Download data and transform it to a data frame
ipc = as.data.frame(ipc)

# Convert the date strings to charachter vectors in order to transform
# it to Date format.
ipc[, 1] = as.character(ipc[, 1])
ipc[, 2] = as.character(ipc[, 2])

# Separate the Price vector from the data frame .
price = ipc[, 5]
price = as.data.frame(price)

# Transform the time and date charachter vectors to a valid R format
time = as.POSIXct(paste(ipc$Date, ipc$Time), format = "%Y-%m-%d %H:%M")

# Set the price vector as an extensible time series object in order to apply function harMode
price = xts(price, order.by = time)

# Compute the logaritmic returns of our transformed price vector
pricer = makeReturns(price)

# Set log returns vector in a data frame format
price.ch = ts(price)
price.ch = price.ch/lag(price.ch, -1) - 1

# Apply harModel function which returns the estemites of an heterogeneous autoregressive model For realized volatility. 
# The model is mainly used to forecast next day volatility based on the high frequency returns of the past.
rv = harModel(pricer, periods = c(1, 1, 1), periodsJ = c(1, 1, 1), RVest = c("rCov", 
                                                                             "rBPCov"), type = "HARRVCJ", transform = "sqrt")

# Grafical representation of our results
pdf(file = "IPCA1.pdf")

par(mfrow = c(2, 1))

plot(ts(price), col = "blue", main = "IPC 26/11/2015-10/12/2015")
plot(price.ch, main = "Percentage change in stock prices nov 26 to dec 10")

dev.off()

# Plot individual daily returns for the days sample
pdf(file = "IPCA2_5.pdf", width = 12, height = 4)

#Choose one of the days in sample
#plot(pricer[159:237, 1], main = "Returns on Nov 30")
#plot(pricer[238:316, 1], main = "Returns on Dec 01")
#plot(pricer[317:395, 1], main = "Returns on Dec 02")
#plot(pricer[396:474, 1], main = "Returns on Dec 03")
#plot(pricer[475:553, 1], main = "Returns on Dec 04")

chart.TimeSeries(
  pricer[475:553, 1],
  type = "l", 
  main = "Returns on Dec 04", 
  ylab = "",
  colorset = "blue", 
  auto.grid = F,
  lwd = 1.5,
  date.format = "%H:%M",
  xaxis = T,
  element.color ="black",
  minor.ticks = F,
)

dev.off()

# Plot the outcome of our harModel
pdf(file = "IPCA3.pdf")

chart.TimeSeries(
  cbind(rv$residuals + rv$fitted.values, rv$fitted.values)[-1,],
  type = "l", 
  main = NA,
  ylab = "RV",
  col = c("blue","red"), 
  auto.grid = F,
  date.format = "%d/%m",
  lwd = 2,
  element.color ="black",
  minor.ticks = FALSE
)

dev.off()
