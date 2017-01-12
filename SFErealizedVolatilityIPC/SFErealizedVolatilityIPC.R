# ipc realized variance, observed and predicted volatility and price
# movements

# Clear enviorenment
graphics.off()
rm(list = ls())

# changing local coordinates, s.t. abbreviations of months are in English
Sys.setlocale("LC_ALL","English")
# resolution of png images
resolution = 300

# Load packages
library(highfrequency)
library(PerformanceAnalytics)

# Download file
ipc = read.csv(file = "IPC RV Data.csv", sep = ",", head = TRUE, na.strings = c("","NA"))

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
# combining data
dat = cbind(ipcm$fitted.values,ipcrv)
dat = na.omit(dat)
# Plot observed and forecasted volatility for the given time frame
# pdf(file = "IPC1.pdf",width = 8, height = 4)
png(file = "IPC1.png",width = 8, height = 4,units = "in", res = resolution)
  par(mar = c(2,4,1,1))
  cycles.dates = list(c("2000-02-03", "2002-01-03"), 
                      c("2008-09-01", "2010-09-01"), 
                      c("2015-06-05", "2015-12-15"))
  #   risk.labels = c("Dot-com bubble", "Global Crisis", "Commodities volatility")
  chart.TimeSeries(
    dat,
    type = "l", 
    main = NA, #"Observed and forecasted RV based on HAR Model: HARRV", 
    ylab = "Realized Volatility",
    col = c("red","blue"), 
    auto.grid = F,
    period.areas = cycles.dates,
    period.color = "gray", 
    lwd = 1.5,
    date.format = "%Y",
    xaxis = F,
    element.color ="black",
    minor.ticks = FALSE
  )
  # function chart.TimeSeries plots last Ticks label wrong
  # problem was fixed by drawing axis with function axis()
  sub = 1:17 %in% seq(1,17,2)
  axTicks = c(1,which(diff(as.numeric(format(time(dat),"%Y")))!=0),dim(dat)[1])
  axis(1,axTicks,ifelse(sub,2000:2016,""),cex.axis=0.8)

dev.off()

# plot returns
# pdf(file = "IPC2.pdf",width = 8, height = 4)
png(file = "IPC2.png",width = 8, height = 4,units = "in", res = resolution)
  par(mar=c(3,4,3,1))
  cycles.dates = list(c("2000-01-03", "2002-01-03"), 
                      c("2008-09-01", "2010-09-01"), 
                      c("2015-06-05", "2015-12-15"))
  chart.TimeSeries(
    returns, 
    type = "l", 
    col = "blue",
    main = NA,#"DAX returns", 
    ylab = "Returns",
    auto.grid = F,
    period.areas = cycles.dates, 
    period.color = " gray", 
    lwd = 1,
    #     date.format = "%Y",
    xaxis = F,
    element.color ="black",
    minor.ticks = FALSE
  )
  sub = 1:17 %in% seq(1,17,2)
  axTicks = c(1,which(diff(as.numeric(format(time(returns),"%Y")))!=0),length(returns))
  axis(1,axTicks,ifelse(sub,2000:2016,""),cex.axis=0.8)

dev.off()

# 2008 Volatility
ipcrv8 = ipcrv["2008"]

# 2008 one minute lagged variance
ipcrvs8 = ipcrvs["2008"]

# apply har model
ipcm8 = harModel(data = ipcrv8, periods = c(1, 5, 22), RVest = c("rCov"), 
                 type = "HARRV", h = 1, transform = NULL)
# combining data
dat = cbind(ipcrvs8,ipcm8$fitted.values)
dat = na.omit(dat)

# Plot harModel 2008
# pdf(file = "IPC3.pdf", width = 8, height = 4)
png(file = "IPC3.png",width = 8, height = 4,units = "in", res = resolution)
  par(mar = c(2,4,1,1))
  chart.TimeSeries(
    dat,
    auto.grid = F,
    date.format = "%b",
    col = c("blue","red"),
    lwd = 2,
    type = "l", 
    main = NA,#"5 minutes realized volatility",
    element.color = "black",
    ylab = "RV",
    major.ticks=F,
    xaxis=F
  )
  axTicks = c(1,which(diff(as.numeric(format(time(dat),"%m")))!=0)+1)
  axis(1,c(axTicks,dim(dat)[1]),c(format(time(dat)[c(axTicks)],format="%b"),"Jan"),cex.axis=0.8)

dev.off()


# Plot realized variance and a volatility approx.
# pdf(file = "IPC4.pdf", width = 8, height = 4)
png(file = "IPC4.png",width = 8, height = 4,units = "in", res = resolution)

  par(mar = c(2,4,1,1))
  
  chart.TimeSeries(
    cbind(ipcrvs8,ipcrv8), 
    auto.grid = F,
    date.format = "%b",
    col = c("red","blue"),
    lwd = 2,
    type = "l", 
    main = NA,#"5 minutes realized volatility",
    element.color = "black",
    ylab = "RV",
    major.ticks=F,
    xaxis=F
  )
  axTicks = c(1,which(diff(as.numeric(format(time(ipcrv8),"%m")))!=0)+1)
  axis(1,c(axTicks,length(ipcrv8)),format(time(ipcrv8)[c(axTicks,1)],format="%b"),cex.axis=0.8)

dev.off()