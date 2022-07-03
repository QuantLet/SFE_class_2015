
set.seed(3)

print("Please input Stock's Initial Price as S0, Interest Rate per Year as r,")
print("Volatility per year as vol , Value of the barrier as b and Type of Barrier as c")
print("Choose c = 1: Down and Out, 2: Up and Out, 3: Down and In, 4: Up and In")
print("Notice that Down and Out option: S0 > b, Up and Out option: S0 < b")
print("Down and In option: S0 > b, Up and In option: S0 < b")
print("For instance for Down and Out option = 200 0.05 0.03 100 1")


barrier_option <- function(S0, r, vol, b, c) {

N <- 1000
t <- (1:N)/N
volatility <- vol * vol
dt <- 1
randomWt1 <- rnorm(N, 0, 1)
randomWt2 <- rnorm(N, 0, 1)
Wtsum1 <- cumsum(t(randomWt1))
Wtsum2 <- cumsum(t(randomWt2))
Path1 <- exp((r - 0.5 * volatility) * dt + vol * sqrt(dt) * Wtsum1)
Path2 <- exp((r - 0.5 * volatility) * dt + vol * sqrt(dt) * Wtsum2)
StockPath1 <- matrix(0, N, 1)
StockPath2 <- matrix(0, N, 1)
StockPath1[1] <- S0
StockPath2[1] <- S0

#plot different type of Barrier Option base on selection
if (c == 1) {
  # down and out graph
  for (i in 2:N) {
    StockPath1[i] <- S0 * Path1[i]
    StockPath2[i] <- S0 * Path2[i]
  }
  
  plot(t, rep(b, length(t)), col = "black", lwd = 2, ylab = "Asset Price", main = "Down and Out Option", type = "l",
    ylim = c(min(c(StockPath1, StockPath2, b)), max(c(StockPath1, StockPath2, 
      b))))
  
  lines(t, StockPath1, col = "blue", lwd = 2)
  lines(t, StockPath2, col = "blue", lwd = 2)
  end <- length(t)
  
  h <- which(StockPath1 < b)[1]
  if (is.na(h) == F) {
    lines(t[h:end], StockPath1[h:end], col = "red", lwd = 2)
  }
  hh <- which(StockPath2 < b)[1]
  if (is.na(hh) == F) {
    lines(t[hh:end], StockPath2[hh:end], col = "red", lwd = 2)
  }
} else if (c == 2) {
  # up and out graph
  for (i in 2:N) {
    StockPath1[i] <- S0 * Path1[i]
    StockPath2[i] <- S0 * Path2[i]
  }
  
  plot(t, rep(b, length(t)), col = "black", lwd = 2, ylab = "Asset Price", main = "Up and Out Option", type = "l",
    ylim = c(min(c(StockPath1, StockPath2, b)), max(c(StockPath1, StockPath2, 
      b))))
  
  lines(t, StockPath1, col = "blue", lwd = 2)
  lines(t, StockPath2, col = "blue", lwd = 2)
  end <- length(t)
  
  h <- which(StockPath1 > b)[1]
  if (is.na(h) == F) {
    lines(t[h:end], StockPath1[h:end], col = "red", lwd = 2)
  }
  hh <- which(StockPath2 > b)[1]
  if (is.na(hh) == F) {
    lines(t[hh:end], StockPath2[hh:end], col = "red", lwd = 2)
  }
} else if (c == 3) {
  # down and in graph
  for (i in 2:N) {
    StockPath1[i] <- S0 * Path1[i]
    StockPath2[i] <- S0 * Path2[i]
  }
  
  plot(t, rep(b, length(t)), col = "black", lwd = 2, ylab = "Asset Price", main = "Down and In Option", type = "l",
    ylim = c(min(c(StockPath1, StockPath2, b)), max(c(StockPath1, StockPath2, 
      b))))
  
  lines(t, StockPath1, col = "red", lwd = 2)
  lines(t, StockPath2, col = "red", lwd = 2)
  end <- length(t)
  
  h <- which(StockPath1 < b)[1]
  if (is.na(h) == F) {
    lines(t[h:end], StockPath1[h:end], col = "blue", lwd = 2)
  }
  hh <- which(StockPath2 < b)[1]
  if (is.na(hh) == F) {
    lines(t[hh:end], StockPath2[hh:end], col = "blue", lwd = 2)
  }
} else {
  # up and in graph
  for (i in 2:N) {
    StockPath1[i] <- S0 * Path1[i]
    StockPath2[i] <- S0 * Path2[i]
  }
  
  plot(t, rep(b, length(t)), col = "black", lwd = 2, ylab = "Asset Price", main = "Up and In Option", type = "l",
    ylim = c(min(c(StockPath1, StockPath2, b)), max(c(StockPath1, StockPath2, 
      b))))
  
  lines(t, StockPath1, col = "red", lwd = 2)
  lines(t, StockPath2, col = "red", lwd = 2)
  end <- length(t)
  
  h <- which(StockPath1 > b)[1]
  if (is.na(h) == F) {
    lines(t[h:end], StockPath1[h:end], col = "blue", lwd = 2)
  }
  hh <- which(StockPath2 > b)[1]
  if (is.na(hh) == F) {
    lines(t[hh:end], StockPath2[hh:end], col = "blue", lwd = 2)
  }
}

}

# Down and out Option
barrier_option(150, 0, 0.005, 140, 1)
# Up and Out Option
barrier_option(140, 0, 0.005, 150, 2)
# Down and In Option
barrier_option(150, 0, 0.005, 140, 3)
# Up and In Option
barrier_option(140, 0, 0.005, 150, 4)
