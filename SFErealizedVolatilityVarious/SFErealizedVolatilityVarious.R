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

# PLOTS. Insert one of the indices above to plot the HAR graph
png(file = "DJ.png", width = 12, height = 4, units = "in", res = 300)

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
