garmanklassTA <- function (open, high, low, close) {
  TS = is.ts(open)
  if (TS) {
    x = open
    open = as.vector(open)
    high = as.vector(high)
    low = as.vector(low)
    close = as.vector(close)
  }
  
  prices = log(cbind(open, high, low, close))
  n = nrow(prices)
  alpha = 0.12
  f = 0.192
  u = high - open
  d = low - open
  cc = close - open
  oc = (prices[2:n, 1] - prices[1:(n - 1), 4])^2
  garmanklass = 0.511 * (u - d)^2 - 0.019 * (cc * (u + d) - 2 * u * d) - 0.383 * cc^2
  garmanklass = sqrt(((1 - alpha) * garmanklass[2:n])/(1 - f) + (alpha * oc)/f)
  garmanklass = c(NA, garmanklass)
  
  if (TS) {
    garmanklass = matrix(garmanklass)
    colnames(garmanklass) = "GK"
    rownames(garmanklass) = rownames(x@Data)
    x@Data = garmanklass
  } else {
    x = garmanklass
  }
  x <- x[-1]
  x <- ts(x, start = c(2015,1,2), frequency = 251.4)
  x
}