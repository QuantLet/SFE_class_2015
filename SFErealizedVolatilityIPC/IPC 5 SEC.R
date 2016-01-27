# ipc realized variance, observed and predicted volatility and price
# movements

# Clear enviorenment
graphics.off()
rm(list = ls())

# Load packages
library(highfrequency)
library(PerformanceAnalytics)

# Download file
ipc = read.csv(file = "IPC RV Data.csv", sep = ",", head = TRUE, na.strings = c("", 
                                                                                "NA"))

# Download data and transform it to a data frame
ipc = as.data.frame(ipc)

# Select my working vextors for 5 minutes daily realized variance
ipcrv = ipc$Realized.Variance..5.minute.

# Convert the date strings to charachter vectors in order to transform
# it to Date format.
ipc[, 1] = as.character(ipc[, 1])
ipct     = ipc$DateID

# Transform the time and date charachter vectors to a valid R format
time = as.POSIXct(paste(ipct), format = "%Y-%m-%d")

# set our xts elements Realized volatility 5 min
ipcrv = xts(ipcrv, order.by = time)

# Returns 5 min
returns = xts(ipc[, 4], order.by = time)

# 5 minutes with one minute subsampling
ipcrvs = xts(ipc[, 5], order.by = time)

# Eliminate NA's
ipcrv  = ipcrv[complete.cases(ipcrv)]
ipcrvs = ipcrvs[complete.cases(ipcrvs)]

# Apply har model
ipcm = harModel(data = ipcrv, periods = c(1, 5, 22), RVest = c("rCov"), 
                type = "HARRV", h = 1, transform = NULL)

# Plot observed and forecasted volatility for the given time frame
png(filename = "IPC1.png")

par(mfrow = c(2, 1))

plot(ipcm)

# Plot realized variance
cycles.dates = list(c("2000-01-03", "2002-01-03"), c("2008-09-01", "2010-09-01"), 
                    c("2015-06-05", "2015-12-15"))
risk.dates = c("2000-01-04", "2008-09-01", "2015-06-03")
risk.labels = c("Dot-com bubble", "Global Crisis", "Commodities volatility")

chart.TimeSeries(ipcrv, type = "l", main = "IPC volatility", ylab = "Return", 
                 col = "black", grid.color = "yellow", period.areas = cycles.dates, 
                 period.color = "lightgoldenrod3", event.lines = risk.dates, event.labels = risk.labels, 
                 event.color = "red", lwd = 1)

dev.off()

# plot returns
png(filename = "IPC2.png")

chart.TimeSeries(returns, type = "l", main = "IPC returns", ylab = "Return", 
                 col = "black", grid.color = "yellow", period.areas = cycles.dates, 
                 period.color = " lightcoral", event.lines = risk.dates, event.labels = risk.labels, 
                 event.color = "blue", lwd = 1)

dev.off()

# 2008 Volatility
ipcrv8 = ipcrv["2008"]

# 2008 one minute lagged variance
ipcrvs8 = ipcrvs["2008"]

# apply har model
ipcm8 = harModel(data = ipcrv8, periods = c(1, 5, 22), RVest = c("rCov"), 
                 type = "HARRV", h = 1, transform = NULL)

# Plot harModel 2008
png(filename = "IPC3.png")

plot(ipcm8, cwd = 0.5, bg = "pch")

dev.off()
# Plot realized variance and a volatility approx.
png(filename = "IPC4.png")

par(mfrow = c(3, 1))

chart.TimeSeries(ipcrv8, col = "red", type = "h", main = "5 minutes realized volatility", 
                 ylab = "RV", xlab = "Time")

# Plot realized volatility with subsampling
chart.TimeSeries(ipcrvs8, col = "blue", type = "h", main = " 5 minutes realized volatility with subsampling", 
                 ylab = "RV", xlab = "Time")

# set difference between volatilities with and wothout subsampling.
diff = ipcrv8 - ipcrvs8

# Plot the volatility differences
chart.TimeSeries(diff, col = "purple", type = "h", main = " Difference in lagged and normal volatility measures", 
                 ylab = "RV", xlab = "Time")

dev.off()





