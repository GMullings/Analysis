require(tseries)
require(zoo)
require(fBasics)

# Retrieving the Market Factor.
sp <- get.hist.quote("^GSPC", start='1989-12-01', quote="AdjClose", compression="m", quiet=TRUE)

mytick = c('ATI','TSN','GME','LB','PAYX') # Storing all of my stocks in a variable for looping.

data <- get.hist.quote(mytick[1], quote="AdjClose", start="1989-12-01", compression="m", quiet=TRUE, retclass="zoo")

for (i in 2:length(mytick))
  
{
  
  temp = get.hist.quote(mytick[i], quote="AdjClose", start="1989-12-01", compression="m", quiet=TRUE, retclass="zoo")
                         
                         data = cbind(data, temp)
                         
}

colnames(data) = mytick

# Logarithmic returns
ret = 100 * diff(log(data))
sp = 100 * diff(log(sp))
colnames(sp) = "SP.500"

# Analyzing the descriptive statistics of the returns

StocksMarketRet = cbind(ret,sp)
basicStats(StocksMarketRet)

# Lots of downside risk on ATI based on the mean, median, and high variation. The high number of NAs may bias these results,
# as well as those of GME. All but ATI and PAYX seem negatively skewed based on mean and median with larger tails present on GME 
# (which also happens to be the second-highest stock in variance). All stocks seem to have higher variances, accompanied by 
# higher mean returns except for ATI.

sum(is.na(StocksMarketRet)) # Any missing return values?

StocksMarketRetAdj = StocksMarketRet[151:318,] # Eliminating the first 149 observations from all assets to reduce bias.

plot(as.zoo(StocksMarketRetAdj), main="Portfolio Stock Market Returns")
plot(as.data.frame(StocksMarketRetAdj), main="Portfolio Stock Market Returns Plotted Against Each Other")

# There's definetly evidence of comovement between all the stocks and the market, albeit to varying degrees.

cor(StocksMarketRetAdj) # Would have included use="pairwise.complete" if I hadn't eliminated the missing observations already.

fit = lm(StocksMarketRetAdj[,mytick] ~ StocksMarketRetAdj$SP.500)
coef(summary(fit))
