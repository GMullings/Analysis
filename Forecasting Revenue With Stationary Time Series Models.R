library(zoo)
library(dyn)
library(urca)

temp = tempfile()
download.file("http://faculty.baruch.cuny.edu/smanzan/eco9723/files/EPS_REV_FALL2015.csv",temp)
data = read.csv(temp)
unlink(temp)

mytick = "SBUX" # Starbucks
index = which(data[,"tic"] == mytick)
mydata = data[index,]

startdate = mydata[2, "datacqtr"]
rev = zooreg(mydata[, "revtq"], start=as.yearqtr(startdate), frequency=4)

# Creating the Trend and Dummy Variables
trend = zooreg(1:length(rev), start=as.yearqtr(startdate), frequency=4)
trendsq = trend^2
trendcub = trend^3
Q1 = zooreg(as.numeric(cycle(rev) ==1), start=start(rev), frequency=4)
Q2 = zooreg(as.numeric(cycle(rev) ==2), start=start(rev), frequency=4)
Q3 = zooreg(as.numeric(cycle(rev) ==3), start=start(rev), frequency=4)
Q4 = zooreg(as.numeric(cycle(rev) ==4), start=start(rev), frequency=4)

# Determining whether to use the log of SBUX's revenue. 
par(mfrow=c(2,2))
plot(rev,xlab="")
plot(log(rev), xlab="")
plot(diff(rev), xlab="")
plot(diff(log(rev)), xlab="")

# Since the plotted difference in revenue doesn't vary by tremendous amounts, 
# I'll use the logarithmic revenue values.

lrev = log(rev)
drev = diff(rev)
dlrev = diff(lrev)
adffit = dyn$lm(dlrev ~ lag(rev, -1) + lag(dlrev, -1:-4) + trend + Q2 + Q3 + Q4)
summary(adffit)

# Using an Augmented Dickey-Fuller Test to test the null hypothesis that the 
# logarithmic revenue values are non-stationary with a trend.
# Fourth lag seems statistically significant to predicting revenue, so the ADF 
# test will be run with that many lags.

sum(is.na(lrev))
# NA in lrev. The ADF test won't work with this.
head(lrev)
# NA present in the first observation. Removing it and running the ADF test.
NaFreelrev = lrev[-1,]
adf = ur.df(NaFreelrev, type="trend", lags=4)
summary(adf)

# Test statistic -7.38 is far greater than the critical value -3.45, rejecting the null hypothesis that the logarithmic revenue values 
# are non-stationary.

par(mfrow=c(1,1))
fitlin  = dyn$lm(lrev ~ trend)
fitquad = dyn$lm(lrev ~ trend + trendsq)
fitcub = dyn$lm(lrev ~ trend + trendsq + trendcub)
plot(lrev, xlab="", col="gray50")
lines(fitted(fitlin),col=2,lwd=2,lty=2)
lines(fitted(fitquad),col=4,lwd=2,lty=2)
lines(fitted(fitcub),col=6,lwd=2,lty=2)

# The cubic trend seems to provide the best fit visually. Since the model is stationary, we can safely assess the signifcance of the
# fit using t-test statistics and p values.

round(summary(fitlin)$coefficients, 4)
round(summary(fitquad)$coefficients, 4)
round(summary(fitcub)$coefficients, 4)

# All three models seem statistically significant, although the standard trend is the most significant while the quadratic one seems more so appropriate visually. An Akaike Information Criterion (AIC) Test 
# would help determine whether either model is systematically leaving out data.

# Running an AIC test to determine which model fits the historical data the 
# best.

AIC(fitlin)
AIC(fitquad)
AIC(fitcub)

# The cubic model seems best at capturing data points. Checking the autocorrelation of residuals for the cubed trend.

acf(residuals(fitcub), lag=12, xlab="")

# Few Residuals are significantly correlated and they become a lot less so as time goes on. Testing if the logarithmic
# revenue values are seasonal.

fitlins = dyn$lm(lrev ~ trendcub + Q2 + Q3 + Q4)
summary(fitlins)

# None of the seasonal dummies are statistically significant. Plotting out the residuals and a reference diagram.

par(mfrow=c(1,3))
plot(lrev, xlab="")
plot(residuals(fitlins), xlab="")
acf(residuals(fitlins), lag=12, xlab="")

# Residuals are highly and persistently autocorrelated when seasonal dummies are inclded. The data is not seasonal.

# An auto-regressive component will be built into the model. The AR factor should capture many of the residuals.

resid = residuals(fitcub)
fitresid = ar(resid, aic=TRUE, order.max=8, demean=FALSE, method="ols")
ord = 1:fitresid$order # Optimal order
fitlinsar = dyn$lm(lrev ~ lag(lrev, -ord) + trendcub)
summary(fitlinsar)

# As should be expected the lags are much more statistically significant than the cubic trend.

AIC(fitlinsar)
par(mfrow=c(1,1))
plot(residuals(fitlinsar), xlab="")

# The AIC value has almost tripled, and the residuals for this model are more tightly around 0
# and far less predictable.

# Forecast function is a dynamic modification of one provided by CUNY Professor Sebastiano Manzan.
myforecast <- function(y,  ord=1, n.ahead=5, trend=1, seasonal="yes", obs=4)
{
        # y        = time series to be forecast
        # ord      = lags to be included
        # n.ahead  = number of forecasts
        # trend    = exponential change of the trend
        # seasonal = "yes" or "no"
        # obs = number of observations per unit of time.
        
        require(dyn)
        
        ypred   <- window(as.ts(y), end=(end(y) + n.ahead/obs), extend=TRUE) # Window is a 
        # subsetting function, Ypred subsets the time series up to n.ahead/obs observations. 
        # Extend needs to be true to do this.
        ypred   <- zooreg(c(ypred), start=start(y), frequency=obs) # c(ypred) used to turn 
        # ypred into a vector for zooreg
        
        if (!is.null(trend) && trend > 1){
                for (e in 2:trend){
                        X.pred = cbind((zooreg(1:length(ypred), start=start(y), frequency=obs))^e)
                }}
        
        if (!is.null(trend) && trend == 1){
                t   <- zooreg(1:length(ypred), start=start(y), frequency=obs)
                X.pred = cbind(t) } # Predicting the time component of the trend to the distance of 
        # ypred, as done during the analysis. 
        
        unit <- cycle(ypred) # Making unit a variable of the cycles of ypred. Should have a 
        # way to specify the number of cycles in the function.
        if (seasonal == "yes"){
                if (!is.null(obs) && obs > 1) {
                        for (u in 2:obs) { # Loops through the obs count and creates seasonal dummy 
                                # variables for each unit as a new column in X.pred.
                                X.pred  = cbind(X.pred, zooreg(as.numeric(quarter ==u), start=start(y), frequency=obs))
                        }}
        }
        if (!exists("X.pred")){ # If there is no trend, function assumes lags will predict 
                # future observations.
                fit <- dyn$lm(ypred ~ lag(ypred, -ord)) 
                for (i in 1:n.ahead) ypred[length(y)+i] <- window(predict(fit, ypred), start=end(y)+i/obs, end=end(y)+i/obs)
        }
        if (exists("X.pred")){
                fit <- dyn$lm(ypred ~ lag(ypred, -ord) + X.pred)
                for (i in 1:n.ahead) ypred[length(y)+i] <- window(predict(fit, cbind(ypred,X.pred)), start=end(y)+i/obs, end=end(y)+i/obs)
        }  
        forecasts <- window(ypred, start=(end(y)+(1/obs)))  
        return(forecasts)
}

myf = myforecast(lrev, ord=ord, n.ahead = 6, trend = 3, seasonal="No")
myf
myf = myforecast(rev, ord=ord, n.ahead = 6, trend = 3, seasonal="No")
round(myf, 2)