
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **SFErealizedVolatilityVarious** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : SFErealizedVolatilityVarious

Published in : Statistics of Financial Markets I

Description : 'Realized volatility analysis using harModel of Dow Jones, CAC 50, FTSE 100 and
Euro-USD exchange market'

Keywords : Volatility, graphical representation, time-series, log returns, variance

Author : Luis Alejandro Sarmiento Abogado

Submitted : Tue, December 28 2015 by Luis Alejandro Sarmiento Abogado

```

![Picture1](CAC.png)

![Picture2](DJ.png)

![Picture3](FTSE.png)

![Picture3](UE.png)


### R Code:
```r
# Install highfrequency version 0.2 and PerformanceAnnalytics version 1.4.3541
# Clear environment
graphics.off()
rm(list = ls())

# Load packages
library(highfrequency)

# Load data from R enviorenment
data(realized_library)

# Select Dow Jones, CAC 40, FTSE 100 and USD-Euro returns Realized
# Variance vectors
DJI_RV  = realized_library$Dow.Jones.Industrials.Realized.Variance
CAC_RV  = realized_library$CAC.40.Realized.Variance
FTSE_RV = realized_library$FTSE.100.Realized.Variance
UE_RV   = realized_library$USD.Euro.Realized.Variance

# Remove NA's
DJI_RV  = DJI_RV[!is.na(DJI_RV)]
CAC_RV  = CAC_RV[!is.na(CAC_RV)]
FTSE_RV = FTSE_RV[!is.na(FTSE_RV)]
UE_RV   = UE_RV[!is.na(UE_RV)]

# Select Year 2008
DJI_RV  = DJI_RV["2008"]
CAC_RV  = CAC_RV["2008"]
FTSE_RV = FTSE_RV["2008"]
UE_RV   = UE_RV["2008"]
head(DJI_RV)
class(DJI_RV)
# Apply the har Model for observed and forecasted realized variance in
# a HAR model

DJ   = harModel(data = DJI_RV, periods = c(1, 5, 22), RVest = c("rCov"), 
                type = "HARRV", h = 1, transform = NULL)

CAC  = harModel(data = CAC_RV, periods = c(1, 5, 22), RVest = c("rCov"), 
                type = "HARRV", h = 1, transform = NULL)

FTSE = harModel(data = FTSE_RV, periods = c(1, 5, 22), RVest = c("rCov"), 
                type = "HARRV", h = 1, transform = NULL)

UE   = harModel(data = UE_RV, periods = c(1, 5, 22), RVest = c("rCov"), type = "HARRV", 
                h = 1, transform = NULL)

# results summary
summary(DJ)
summary(CAC)
summary(FTSE)
summary(UE)

DJ = cbind(DJ$fitted.values,DJI_RV)
CAC = cbind(CAC$fitted.values,CAC_RV)
FTSE = cbind(FTSE$fitted.values,FTSE_RV)
UE = cbind(UE$fitted.values,UE_RV)

# PLOTS
png(file = "Various1.png")
par(mfrow = c(2, 1))

chart.TimeSeries(
  DJ,
  type = "l", 
  main = "DJ", #"Observed and forecasted RV based on HAR Model: HARRV", 
  ylab = "Realized Volatility",
  col = c("red","blue"), 
  auto.grid = F,
  lwd = 1.5,
  date.format = "%b",
  xaxis = T,
  element.color ="black",
  minor.ticks = F,
)

chart.TimeSeries(
  CAC,
  type = "l", 
  main = "CAC",
  ylab = "Realized Volatility",
  col = c("red","blue"), 
  auto.grid = F,
  lwd = 1.5,
  date.format = "%b",
  xaxis = T,
  element.color ="black",
  minor.ticks = F,
)


dev.off()

png(file = "Various2.png")
par(mfrow = c(2, 1))

chart.TimeSeries(
  FTSE,
  type = "l", 
  main = "FTSE", #"Observed and forecasted RV based on HAR Model: HARRV", 
  ylab = "Realized Volatility",
  col = c("red","blue"), 
  auto.grid = F,
  lwd = 1.5,
  date.format = "%b",
  xaxis = T,
  element.color ="black",
  minor.ticks = F,
)

chart.TimeSeries(
  UE,
  type = "l", 
  main = "UE", #"Observed and forecasted RV based on HAR Model: HARRV", 
  ylab = "Realized Volatility",
  col = c("red","blue"), 
  auto.grid = F,
  lwd = 1.5,
  date.format = "%b",
  xaxis = T,
  element.color ="black",
  minor.ticks = F,
)

dev.off()

```
