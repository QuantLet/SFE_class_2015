# Clear enviorenment
graphics.off()
rm(list = ls())

# Load packages
library(PerformanceAnalytics)

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
DJ_RV  = DJI_RV["2008"]
CAC_RV  = CAC_RV["2008"]
FTSE_RV = FTSE_RV["2008"]
UE_RV   = UE_RV["2008"]
# due to time delay in timeseries of UE, its time has to be shifted by 1 day
time(UE_RV) = time(UE_RV)+60*60*24
# Apply the har Model for observed and forecasted realized variance in
# a HAR model
DJ   = harModel(data = DJ_RV, periods = c(1, 5, 22), RVest = c("rCov"), 
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

class(UE_RV)
nam = c("DJ","CAC","FTSE","UE")
# constructing timeseries
for (i in nam){
  rv = get(paste0(i,"_RV"))
  fit = get(i)$fitted.values
  assign(x = paste0(i,"ts"),xts(fit,order.by = time(rv)[-c(1:22)]))
}
# combining time series
dat = cbind(DJ_RV, CAC_RV, FTSE_RV, UE_RV, DJts, CACts, FTSEts, UEts)
dat = na.omit(dat[format(time(dat),"%B")!="Januar",])
colnames(dat) = c(nam,paste0(nam,"ts"))
# PLOTS
for (i in nam){
  pdf(file = paste0(i,".pdf"), width = 8, height = 1.5)
  
    par(mar = c(2,4,1.5,1))
    
    chart.TimeSeries(
      dat[,sort(grep(i,colnames(dat)),decreasing = T)],
      auto.grid = F,
      date.format = "%b",
      col = c("red","blue"),
      lwd = 1,
      type = "l", 
      main = i,#"5 minutes realized volatility",
      element.color = "black",
      ylab = "RV",
      major.ticks=F,
      xaxis=F
    )
    axTicks = c(1,which(diff(as.numeric(format(time(dat),"%m")))!=0)+1)
    axis(1,c(axTicks,dim(dat)[1]),c(format(time(dat)[axTicks],format="%b"),"Jan"),cex.axis=0.8)
  dev.off()
}