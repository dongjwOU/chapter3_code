## Trend.R

#Function to extract time series parameters from univariate time series object
# based on the TrendAAT from the GreenBrown R package 

#Packages required: Bfast, Strucchage, plyr, kendall
#Extra functions requires: AllEqual, TrendUncertainty, TrendSampleme 

#Inputs:
# Yt          A univariate time-series object of class TS
#
# mosum.pval  Maximum p-value for the OLS-MOSUM test in order to search 
#             for breakpoints. If p = 0.05, breakpoints will be only searched 
#             in the time series trend component if the OLS-MOSUM test indicates a significant 
#             structural change in the time series. If p = 1 breakpoints will be always searched 
#             regardless if there is a significant structural change in the time series or not. (use 0.05/0.1)

#h            minimal segment size either given as fraction relative to the sample size or as an 
#           integer giving the minimal number of observations in each segment. (use 0.15)


### maximal number of breaks to be calculated (integer number). By default the maximal number allowed by h is used. See \code{\link[strucchange]{breakpoints}} for details.


# sample.method = c("sample", "all", "none"),
### Sampling method for combinations of start and end dates to compute uncertainties in trends. 
#If "sample" (default), trend statistics are computed for a sample of combinations of start and 
#end dates according to . If "all", trend statistics are computed for all combinations of start 
#and end dates longer than .  
#If "none", trend statistics will be only computed for the entire time series 
#(i.e. no sampling of different start and end dates). 




Trend <- function(Yt, mosum.pval,h,breaks, sample.method){
  
  
#Prepare time series for anlaysis, using bfastpp
#requires the AllEqual.R function 
time <- time(Yt)  
d <- bfastpp(Yt)
if (nrow(d) < 2 | AllEqual(d$response)) return(NoTrend(Yt))  

#Test the time series for breakpoints
test <- sctest(response ~ trend, data=d, type="OLS-MOSUM", h=h)
if (is.na(test$p.value)) test$p.value <- 9999
if (test$p.value <= mosum.pval) calc.breaks <- TRUE

#estimate breakpoints
bp_est <- breakpoints(response ~ trend, h=h, breaks=breaks, data=d)
d$seg <- breakfactor(bp_est)
m <- lm(response ~ seg / trend, data = d)
  
m <- lm(response ~ trend, data = d)

m.sum <- summary(m)
  
#Estimate trend component 
trend_est <- rep(NA, length(Yt))
trend_est[d$trend] <- predict(m, d) 
trend_est <- approx((1:length(Yt)), trend_est, xout=1:length(Yt), method="linear", rule=c(1,1))$y
trend_est <- ts(trend_est, start=start(Yt), frequency=frequency(Yt))
trend_est <- (trend_est - mean(trend_est, na.rm=TRUE)) + mean(Yt, na.rm=TRUE)  

#results: estimate p-value, slope and uncertanties
# based on the TrendUncertainty.R function, also requires the TrendSample.R
trd.unc <- TrendUncertainty(Yt, bp_est, sample.method = sample.method, sample.min.length=sample.min.length, sample.size=sample.size)

# Return results as trend list 
result <- list(
  series = Yt,
  trend = trend_est,
  time = as.vector(time),
  bp = bp_est,
  slope = unlist(llply(trd.unc, function(x) x$slope)), 
  slope_unc = ldply(trd.unc, function(x) x$slope_unc),
  pval = unlist(llply(trd.unc, function(x) x$pval)),  
  pval_unc = ldply(trd.unc, function(x) x$pval_unc),
  tau = unlist(llply(trd.unc, function(x) x$tau)),
  tau_unc = ldply(trd.unc, function(x) x$tau_unc),
  bptest = test,
  method = "AAT")
class(result) <- "Trend"
return(result)
  
}