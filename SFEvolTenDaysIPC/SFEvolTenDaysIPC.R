# Clean up our space
graphics.off()
rm(list = ls())
path = "../../Project/project/AlejandroSarmiento&Dimovic/"

# Load Packages
library(xts)
library(highfrequency)
library(PerformanceAnalytics)

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

# price.ch = ts(price)
# price.ch = price.ch/lag(price.ch, -1) - 1
price.ch = exp(diff(log(price)))-1

# Apply harModel function which returns the estemites of an heterogeneous autoregressive model For realized volatility. 
# The model is mainly used to forecast next day volatility based on the high frequency returns of the past.
rv = harModel(pricer, periods = c(1, 1, 1), periodsJ = c(1, 1, 1), 
              RVest = c("rCov", "rBPCov"), type = "HARRVCJ", transform = "sqrt")

# Grafical representation of our results
# pdf(file = "IPCA1.pdf",width = 8, height = 4)
png(file = "IPCA1.png", width = 8, height = 3, units = "in", res = 300)
  chart.TimeSeries(
    price.ch,
    type = "l", 
    main = NA, # "Observed and forecasted RV based on HAR Model: HARRV", 
    ylab = NA, # "Realized Volatility",
    col = c("blue"), 
    auto.grid = F,
    date.format = "%d/%m",
    lwd = 2,
    element.color ="black",
    minor.ticks = FALSE
  )
dev.off()


# data preparation
dates = unique(format(time(pricer), "%F"))
dat = pricer[dates[1]]
for (i in dates[-1])
  dat = cbind(dat, as.double(pricer[i]))

# Plot individual daily returns for our 10 days sample
# pdf(file = "IPCA2.pdf", width = 8, height = 5)
png(file = "IPCA2.png", width = 8, height = 5, units = "in", res = 300)
  col <- function(a, b, n){
    if(is.character(a))
      a = col2rgb(a)
    if(is.character(b))
      b = col2rgb(b)
    res = t(sapply(0:(n - 1)/(n - 1), function(x)(b - a) * x + a))
    return(rgb(res, maxColorValue = 255))
  }
  chart.TimeSeries(
    dat,
    auto.grid = F,
    date.format = "%R",
    col = col("red", "blue", dim(dat)[2]),
    lwd = 1,
    type = "l", 
    main = NA, # "5 minutes realized volatility",
    element.color = "black",
    ylab = "%-change in returns",
    major.ticks=F,
    minor.ticks=F
  )
dev.off()

# Plot the outcome of our harModel
# pdf(file = "IPCA3.pdf", width = 8, height = 4)
png(file = "IPCA3.png", width = 8, height = 4, units = "in", res = 300)

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