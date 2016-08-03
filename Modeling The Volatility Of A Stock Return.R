install.packages("fImport")
install.packages("fBasics")
install.packages("TTR")
install.packages("timeSeries")
install.packages("rugarch")
require(fImport)
require(fBasics)
require(TTR)
require(timeSeries)
require(rugarch)

aph = yahooSeries("APH", from="1990-01-01", to="2016-05-18", frequency="daily")
aphadjclse = aph[,6]
aphRet = diff(aphadjclse)/aphadjclse
colnames(aphRet) = "APH.Adj.Ret"

basicStats(aphRet)

# Notably the mean daily return is -.10% and the median daily return is 0%. Daily variance is about .06%.

t.test(aphRet, na.action="Exclude")

# The T-Test's high p-value for the observed mean is evidence that the return is effectively 0.

plot(aphRet)
abline(h=-0.001041)
plot(abs(aphRet))
abline(h=0.001041)

# Using [-1,] to remove the NA observation from the data.
acf(aphRet[-1,])

aphRetSq = aphRet[-1,]^2
Aphma25Plot = SMA(aphRetSq, n=25)
Aphma100Plot = SMA(aphRetSq, n=100)
AphSigma25 = rollMean(aphRetSq, 25, align="right")

plot(Aphma25Plot^0.5) # Black
lines(Aphma100Plot^0.5, col="2") # Red
lines(AphSigma25^0.5, col="3", lty=2) # Green

# They're close, but it's obvious that the 100 day moving average is more persistently holding onto the effects of shocks.

Aphema94Plot = EMA(aphRetSq, ratio=.06)
plot(Aphma25Plot^0.5)
lines(Aphema94Plot^0.5, col="2")

plot(window(abs(aphRet), start="2008-01-01", end="2009-12-31"))
lines(window(Aphema94Plot^0.5, start="2008-01-01", end="2009-12-31"), col="2", lwd="3")
lines(window(Aphma25Plot^0.5, start="2008-01-01", end="2009-12-31"), col="3", lwd="4")

Aphema94Plot = sort(Aphema94Plot, decreasing = TRUE) # Changing the order of the values to match up with the return values.
Filtered.Aphema94Plot = na.omit(Aphema94Plot) # Removing NAs so the volatility estimate can be used in denominator of the standardized returns formula.
Win.aphRet = window(aphRet, start="1991-12-24", end="2016-05-17")
Win.Aphema94Plot = window(Filtered.Aphema94Plot, start="1991-12-24", end="2016-05-17")
St.aphRet = Win.aphRet/Win.Aphema94Plot # Standardized APH returns.

# The standardized returns are not standard normally distributed based on the Q-Q plot below.
qqnorm(St.aphRet)

# GARCH modeling begins here
spec = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,0)))
Aphfitgarch = ugarchfit(spec=spec, data=aphRet[-1,])
round(coef(Aphfitgarch), 3)
# Low omega suggests that the conditional voltaility is more persistent and reverts back to its long run mean slower.

# Testing to see if the omega remains low even during the post-Recession recovery.
Win = window(aphRet, start="2010-01-01", end="2016-05-18")
AphfitgarchWin = ugarchfit(spec=spec, data=Win[-1,])
round(coef(AphfitgarchWin), 3)

# The beta value is reduced by .027 while the ar1 coefficient drops dramatically.

SigmaAphgarch = sigma(Aphfitgarch)

plot(abs(aphRet))
lines(Aphema94Plot^0.5, col="2", lwd="3")
lines(SigmaAphgarch, col="3", lwd="4")

plot(window(abs(aphRet), start="2008-01-01", end="2009-12-31"))
lines(window(Aphema94Plot^0.5, start="2008-01-01", end="2009-12-31"), col="2", lwd="3")
lines(window(SigmaAphgarch, start="2008-01-01", end="2009-12-31"), col="3", lwd="4")

# Trying GJR-GARCH

gjrspec = ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,0)))
Aphfitgjrgarch = ugarchfit(spec=gjrspec, data=aphRet[-1,])
round(coef(Aphfitgjrgarch), 3)
# The negative gamma coefficient may suggest that negative shocks cause disproportionately less volatility in returns.
# Notably though the coefficient is very close to 0.

signbias(Aphfitgjrgarch)
# Sign bias indicates that shocks to both signs have significant impacts on volatility.

FitInfo = cbind(infocriteria(Aphfitgarch), infocriteria(Aphfitgjrgarch))
colnames(FitInfo) = c("GARCH", "GJR")
FitInfo

# GJR provides the best fit on multiple information criteria.

newsgarch = newsimpact(Aphfitgarch)
newsgjrgarch = newsimpact(Aphfitgjrgarch)
plot(newsgarch$zx, newsgarch$zy, xlab=newsgarch$xper, ylab=newsgarch$yexpr, lwd=2)
plot(newsgjrgarch$zx, newsgjrgarch$zy, xlab=newsgjrgarch$xper, ylab=newsgjrgarch$yexpr, lwd=2)

plot(ugarchforecast(Aphfitgarch, n.ahead=250))
plot(ugarchforecast(Aphfitgjrgarch, n.ahead=250))

