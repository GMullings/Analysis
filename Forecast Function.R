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