
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **SFErealizedVolatilityDAX** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : SFErealizedVolatilityDAX

Published in : Statistics of Financial Markets I

Description : 'Realized variance analysis and graphical representation of 5 seconds intraday
highfrequency realized variance from the German stock market DAX and comparison with it's lagged
subsample'

Keywords : Volatility, graphical representation, time-series, log returns, variance

Author : Luis Alejandro Sarmiento Abogado

Submitted : Tue, December 28 2015 by Luis Alejandro Sarmiento Abogado

Datafile : Dax Data current.csv

```

![Picture1](DAX1_1.png)

![Picture1](DAX1_2.png)

![Picture2](DAX2.png)

![Picture3](DAX3.png)

![Picture4](DAX4.png)


### R Code:
```r
# Install Performance Analytics package version 1.4.3541 and highfrequency version 0.4
# Dax realized variance, observed and predicted volatility and price movements

# Clear enviorenment
graphics.off()
rm(list = ls())

# Load packages
library(highfrequency)
library(PerformanceAnalytics)

# Download File
dax = read.csv(file = "Dax Data current.csv", sep = ",", head = TRUE, na.strings = c("", 
                                                                                     "NA"))

# Download data and transform it to a data frame
dax = as.data.frame(dax)

# Select my working vextors for 5 minutes daily realized variance
daxrv = dax$Realized.Variance..5.minute.

# Convert the date strings to charachter vectors in order to transform
# it to Date format.
dax[, 1] = as.character(dax[, 1])
daxt     = dax$DateID

# Transform the time and date charachter vectors to a valid R format
time = as.POSIXct(paste(daxt), format = "%Y-%m-%d")

# set our xts elements Realized volatility 5 min
daxrv = xts(daxrv, order.by = time)

# Returns 5 min
returns = xts(dax[, 4], order.by = time)

# 5 minutes with one minute subsampling
daxrvs = xts(dax[, 5], order.by = time)

# Eliminate NA's
daxrv  = daxrv[complete.cases(daxrv)]
daxrvs = daxrvs[complete.cases(daxrvs)]

# Apply har model
daxm = harModel(data = daxrv, periods = c(1, 5, 22), RVest = c("rCov"), 
                type = "HARRV", h = 1, transform = NULL)

daxm = cbind(daxm$fitted.values,daxrv)

# Plot observed and forecasted volatility
pdf(file = "DAX1.pdf", width = 12, height = 4)

chart.TimeSeries(
  daxm,
  type = "l", 
  main = "Observed and forecasted RV based on HAR Model: HARRV", 
  ylab = "",
  colorset = c("red","blue"), 
  auto.grid = F,
  lwd = 1.5,
  date.format = "%b",
  xaxis = T,
  element.color ="black",
  minor.ticks = F,
)
# Plot realized variance
cycles.dates = list(c("2000-01-03", "2002-01-03"), c("2008-09-01", "2010-09-01"), 
                    c("2015-06-05", "2015-12-15"))
risk.dates = c("2000-01-04", "2008-09-01", "2015-06-03")
risk.labels = c("Dot-com bubble", "Global Crisis", "Commodities volatility")

chart.TimeSeries(daxrv, type = "l", main = "Dax Volatility", ylab = "", period.areas = cycles.dates, period.color = "#0000FF22", 
                 event.lines = risk.dates, event.labels = risk.labels, event.color = "red", 
                 lwd = 1, grid.color = NA)


dev.off()

# plot returns
pdf(file = "DAX2.pdf", width = 12, height = 6)

chart.TimeSeries(returns, type = "l", main = "DAX returns", ylab = "Return", 
                 period.areas = cycles.dates, period.color = "#0000FF22", event.lines = risk.dates, 
                 event.labels = risk.labels, event.color = "red", lwd = 1)

dev.off()

# 2008 Volatility
daxrv8 = daxrv["2008"]

# 2008 one minute lagged variance
daxrvs8 = daxrvs["2008"]

# apply har model
daxm8 = harModel(data = daxrv8, periods = c(1, 5, 22), RVest = c("rCov"), 
                 type = "HARRV", h = 1, transform = NULL)

daxm8 = cbind(daxm8$fitted.values,daxrv8)

# Plot harModel 2008
pdf(file = "DAX3.pdf", width = 12, height = 4)

chart.TimeSeries(
  daxm8,
  type = "l", 
  main = "Observed and forecasted RV based on HAR Model: HARRV", 
  ylab = "Realized Volatility",
  colorset = c("red","blue"), 
  auto.grid = F,
  lwd = 1.5,
  date.format = "%b",
  xaxis = T,
  element.color ="black",
  minor.ticks = F,
)

dev.off()

# Plot realized variance and a volatility approx.
pdf(file = "DAX4.pdf", width = 10, height = 12)

par(mfrow = c(3, 1))

chart.TimeSeries(daxrv8, colorset = "red", type = "h", main = "5 minutes realized volatility", 
                 ylab = "RV", xlab = "Time", cex.main = 2)

# Plot realized volatility with subsampling
chart.TimeSeries(daxrvs8, colorset = "blue", type = "h", main = " 5 minutes realized volatility with subsampling", 
                 ylab = "RV", xlab = "Time", cex.main = 2)

# set difference between volatilities with and wothout subsampling.
diff = daxrv8 - daxrvs8

# Plot the volatility differences
chart.TimeSeries(diff, colorset = "purple", type = "h", main = " Difference in lagged and normal volatility", 
                 ylab = "RV", xlab = "Time", cex.main = 2)

dev.off()




```
