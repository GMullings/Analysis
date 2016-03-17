install.packages("fImport")
install.packages("fBasics")
require(fImport)
require(fBasics)

qqew = yahooSeries("QQEW", from="1970-01-01", frequency="monthly")
psce = yahooSeries("PSCE", from="1970-01-01", frequency="monthly")

length(qqew)
length(psce)
#QQEW is longer in history.

qqewadj = qqew[48:119,] #We're subsetting QQEW's last 72 rows before classifying the data as a time series, because plotting for QQEW later will be difficult otherwise.

LogPqqew = log(qqew)
LogPqqewAdj = LogPqqew[48:119,] #This variable will hold the last 72 observations of the Log prices of QQEW, making it easier to reference for the rest of our analysis.

qqewadj = ts(qqewadj, start=c(2010,4), frequency=12)
psceTs = ts(psce, start=c(2010,4), frequency=12) #We're making the time series classification of this data its own variable because transforming the original variable to a time series doesn't allow us to manipulate it as a data frame later on in the analysis.
LogPqqewAdj = ts(LogPqqewAdj, start=c(2010,4), frequency=12)

LogPpsce = log(psceTs) #Log returns are easier to compare over time, and accurate as long as returns aren't too large.

par(mfrow=c(2,2))
plot(qqewadj[,6], ylab="Adj. Closing Price", main="QQEW's Adj. Closing Price")
plot(psceTs[,6], ylab="Adj. Closing Price", main="PSCE's Adj. Closing Price")
plot(LogPqqewAdj[,6], ylab="Log of Adj. Closing Price", main="Log of QQEW's Adj. Closing Price")
plot(LogPpsce[,6], ylab="Log of Adj. Closing Price", main="Log of PSCE's Adj. Closing Price")

LogRqqew = 100 * diff(log(qqew))
LogRpsce = 100 * diff(log(psce))
LogRqqewAdj = LogRqqew[48:119,] #To eliminate bias we again only use the last 72 rows.
basicStats(LogRqqewAdj$QQEW.Adj.Close)
basicStats(LogRpsce$PSCE.Adj.Close)

LogRqqewAdjTs = ts(LogRqqewAdj, start=c(2010,4), frequency=12) #Make a separate variable just for the time series classification because "LogRqqewAdj" may be needed as a data frame later.

par(mfrow=c(1,2))
plot(LogRpsce[,6], ylab="Log of Adj. Closing Price Returns", main="Log of PSCE's Adj. Closing Price Returns")
plot(LogRqqewAdjTs[,6], ylab="Log of Adj. Closing Price Returns", main="Log of QQEW's Adj. Closing Price Returns")

par(mfrow=c(2,1))
histPlot(LogRpsce[,6], col="steelblue", fit=TRUE, main="Log Returns of PSCE")
histPlot(LogRqqewAdj[,6], col="steelblue", main="Log Returns of QQEW")

fit = lm(LogRpsce$PSCE.Adj.Close ~ LogRqqewAdj$QQEW.Adj.Close)
plot(LogRpsce$PSCE.Adj.Close ~ LogRqqewAdj$QQEW.Adj.Close, xy.lines=FALSE, main="PSCE Log Returns vs. QQEW's", xlab="QQEW's Log Returns", ylab="PSCE's Log Returns")
abline(a=fit)

Ret = cbind(LogRpsce$PSCE.Adj.Close, LogRqqewAdj$QQEW.Adj.Close)
cov(Ret, use="complete.obs")

cor(Ret, use="complete.obs")

