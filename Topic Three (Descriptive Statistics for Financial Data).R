##### Descriptive Statistics for Financial Data #####
 #### Descriptive Statistics for Financial Data ####

  ### Example Datasets Using Quantmod ###
install.packages("quantmod")
library(quantmod)

  ### Download Microsoft daily prices January 2 1998-May 31 2012 ###
msft_d<-getSymbols("MSFT", from='1998-01-02',to="2012-12-31",warnings= FALSE,auto.assign = FALSE)
head(msft_d)


  ### Download daily S&P 500 index January 2 1998-May 31 2012 ###
sp500_d<-getSymbols("ˆGSPC", from='1998-01-02',
                    to="2012-12-31",
                    warnings= FALSE,
                    auto.assign = FALSE)

head(sp500_d)

#length(sp500_d)
# pick the adjusted price-day
msftd=msft_d$MSFT.Adjusted
sp500d=sp500_d$GSPC.Adjusted
#compute daily returns


  ### End-of-month prices are extracted from the daily prices using the xts function to.monthly(): ###
# pick the adjusted price-month
  msft_m=to.monthly(msft_d$MSFT.Adjusted, OHLC=FALSE)
sp500_m=to.monthly(sp500_d$GSPC.Adjusted, OHLC=FALSE)
head(msft_m,3)
head(sp500_m,3)

     #It will also be convenient to create a merged xts object containing both the Microsoft and S&P 500 index prices:
  msft_sp_m<-merge(msft_m,sp500_m)
colnames(msft_sp_m)=c("MSFT","SP500")
head(msft_sp_m,3)

     #We create xts objects containing simple returns using the PerformanceAnalytics function Return.calculate() (and remove the first NA value with na.omit()):
     #We need the PerformanceAnalytics()
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

msft_ret_d<-na.omit(Return.calculate(msftd,
                                     method="simple"))
sp500_ret_d<-na.omit(Return.calculate(sp500d,
                                      method="simple"))
msft_sp_ret_d<-merge(msft_ret_d,sp500_ret_d)
colnames(msft_sp_ret_d)<-c("MSFT","SP500")
head(msft_sp_ret_d,3)

#Monthly returns -S&P 500
msft_ret_m<-na.omit(Return.calculate(msft_m,
                                     method = "simple"))
head(msft_ret_m,3)

#Monthly returns -S&P 500
sp500_ret_m<-na.omit(Return.calculate(sp500_m,
                                      method = "simple"))
head(sp500_ret_m,3)

#Merge the monthly returns for stock and index
msft_sp_ret_m<-merge(msft_ret_m,sp500_ret_m)
colnames(msft_sp_ret_m)=c("MSFT","SP500")

     #We also create xts objects containing monthly continuously compounded (cc) returns:
  sp500_ret_mC<-log(1+sp500_ret_m)
msft_sp_ret_mC<-log(1+msft_sp_ret_m)


  ### Time Plots: A time plot for the monthly simple returns ###
plot(msft_sp_ret_m, multi.panel=TRUE, yaxis.same=FALSE,
     main="", lwd=2, col="blue")


  ### Plotting returns on the same scale ###
plot(msft_sp_ret_m, main="", multi.panel=FALSE, lwd=2,
     col=c("red", "blue"), lty=c("dashed", "solid"),
     legend.loc = "topright")


  ### Are Microsoft returns normally distributed? A first look ###
     #The shape of the histogram for Microsoft returns suggests that a normal distribution might be a good candidate for the unknown distribution of Microsoft returns.
     #To investigate this conjecture, we simulate random returns from a normal distribution with mean and standard deviation calibrated to the Microsoft daily and monthly returns using:
  
#set seed to replicate the results
  set.seed(123)
# Draw from a random number, using n=length of msft
# mean = mean(msft), sd= sd(msft)
msft_randomd<-rnorm(length(msft_ret_d),
                    mean=mean(msft_ret_d),sd=sd(msft_ret_d))
## Create an xts function to correspond to msft data
msft_random_d<-xts(msft_randomd,index(msft_ret_d))

#set seed to replicate the results
set.seed(123)
# Draw from a random number, using n=length of msft
# mean = mean(msft), sd= sd(msft)
msft_randomm<-rnorm(length(msft_ret_m),
                    mean=mean(msft_ret_m),sd=sd(msft_ret_m))
## Create an xts function to correspond to msft data
msft_random_m<-xts(msft_randomm,index(msft_ret_m))


  ### MSFT Monthly Returns vs Simulated ###
par(mfrow=c(2,2))
plot.zoo(msft_ret_m, main="Monthly Returns on MSFT",
         lwd=2, col="blue", ylim=c(-0.4, 0.4), ylab="Returns")
abline(h=0)
plot.zoo(msft_random_m, main="Simulated Normal Returns",
         lwd=2, col="blue", ylim=c(-0.4, 0.4), ylab="Returns")
abline(h=0)

par(mfrow=c(2,2))
hist(msft_ret_m, main="MSFT Monthly Returns", col="cornflowerblue",
     xlab="returns")
hist(msft_random_m, main="MSFT MonthlySimulated Returns",
     col="cornflowerblue",
     xlab="returns", breaks=msftHist$breaks)

     #The simulated normal returns shares many of the same features as the Microsoft returns: both fluctuate randomly about zero. However, there are some important differences.
     #In particular, the volatility of Microsoft returns appears to change over time (large before 2003, small between 2003 and 2008, and large again after 2008) whereas the simulated returns has constant volatility. - Additionally, the distribution of Microsoft returns has fatter tails (more extreme large and small returns) than the simulated normal returns. Apart from these features, the simulated normal returns look remarkably like the Microsoft monthly returns

par(mfrow=c(2,2))
msftDailyHist = hist(msft_ret_d, plot=FALSE, breaks=15)
par(mfrow=c(2,2))
plot.zoo(msft_ret_d, main="Monthly Returns on MSFT",
         lwd=2, col="blue", ylim=c(-0.15, 0.15), ylab="Returns")
abline(h=0)
plot.zoo(msft_randomd, main="Simulated Normal Returns",
         lwd=2, col="blue", ylim=c(-0.15, 0.15), ylab="Returns")
abline(h=0)

     #The daily returns- Here the constant volatility of simulated data does not match the volatility patterns of the Microsoft daily returns, and the tails of the histogram for the Microsoft returns are “fatter” than the tails of the simulated histogram.


  ### Empirical CDF ###
#Example:Empirical CDF for monthly returns on MSFT -Computing the empirical CDF of Microsoft monthly returns at a given point, say R = 0, in R is:
  Fhat.0<-sum(msft_ret_m<=0)/nrow(msft_ret_m)
Fhat.0

     #The R function ecdf() can be used to compute the empirical CDF for Microsoft monthly returns.
plot(ecdf(coredata(msft_ret_m)), main="Empirical CDF
of Microsoft Monthly Stock Returns", col="blue")


  ###Empirical quantiles of the Microsoft and S&P 500 monthly returns ###
apply(msft_sp_ret_m, 2, quantile)
     #In the above code, the R function apply() is used to compute the empirical quantiles on each column of the xts object msft_sp_ret_m.

#To compute quantiles for a specified α use the probs argument. For example, to compute the 1% and 5% quantiles of the monthly returns use:
  apply(msft_sp_ret_m, 2, quantile, probs=c(0.01,0.05))
  
  
  ### Example: Student’s t QQ-plot for Microsoft returns ###
install.packages("car")  
library(car)
  
  qqPlot(coredata(msft_ret_m), distribution="t",
         df=5, ylab="MSFT quantiles", envelope=FALSE)
  
  
  ### Time Series Descriptive Statistics ###
   ## SACF for the Microsoft and S&P 500 returns ##
acf(coredata(msft_ret_m), lag.max = 5,plot=FALSE)
acf(coredata(msft_ret_d), lag.max = 5,plot=FALSE)


  ### Pair-wise scatterplots for multiple series ###
#For more than two data series, the R function pairs() plots all pair-wise scatterplots in a single plot. For example, to plot all pair-wise scatterplots for the GWN, Microsoft returns and S&P 500 returns use:
  dataToPlot = merge(msft_random_m,msft_ret_m,sp500_ret_m)
pairs(coredata(dataToPlot), col="blue", pch=16, cex=1.25, c

      
  ### Sample covariance and correlation between Microsoft and S&P 500 returns ###
#The scatterplots of Microsoft and S&P 500 returns suggest positive linear relationships in the data. We can confirm this by computing the sample covariance and correlation using the R functions cov() and cor(). For the monthly returns, we have
cov(msft_ret_m,sp500_ret_m)
cor(msft_ret_m,sp500_ret_m)


  ### Example: Rolling estimates of µ for Microsoft, S&P 500 Index, and simulated GWN ###
#Consider computing rolling estimates of µ for Microsoft, Starbucks and the S&P 500 index returns. You can easily do this using the zoo function rollapply(), which applies a user-specified function to a time series over rolling windows of fixed size.
#The argumements to rollapply() are
args(zoo:::rollapply.zoo)

#For example, to compute 24-month rolling estimates of µ for Microsoft, S&P 500 index, and simulated GWN data (that matches the mean and volatility of Microsoft returns), incremented by one month with the time index aligned to the end of the window use
roll.data = merge(msft_ret_m,sp500_ret_m,msft_random_m)
colnames(roll.data) = c("MSFT", "SP500", "GWN")
roll.muhat = rollapply(roll.data, width=24, by=1,
                       by.column=TRUE, FUN=mean, align="right")
class(roll.muhat)

#The object roll.muhat is an “xts” object with three columns giving the rolling mean estimates for the GWN data, Microsoft, and the S&P 500 index. The first 23 months of data are NA as we need at least 24 months of data to compute the first rolling estimates:
  head(roll.muhat,3)
  
plot(roll.muhat, main="", multi.panel=FALSE, lwd=2,
       col=c("black", "red", "green"), lty=c("solid",
                                             "solid", "solid"), major.ticks="years",
       grid.ticks.on="years",legend.loc = "topright")


  ### Rolling estimates of standard deviation of MSFT, S&P 500 ###
roll.sigmahat = rollapply(roll.data,width=24,
                          by=1, by.column=TRUE, FUN=sd, align="right")
head(roll.sigmahat, 5)

#The rolling volatilities for Microsoft and the S&P 500 index show a similar pattern of time variation over the sample but the Microsoft volatilities are considerably more variable:
  ans = apply(na.omit(roll.sigmahat),2,range)
rownames(ans) = c("Min", "Max")
ans

#Example: Rolling estimates of ρij for Microsoft, GWN, and the S&P 500 index
rhohat=function(x){
  corhat=cor(x)
  corhat.vals=corhat[lower.tri(corhat)]
  names(corhat.vals)=c("MSFT.SP00","MSFT.GWN",
                       "SP500.GWN")
  corhat.vals
}
roll.rhohat = rollapply(roll.data, width=24, FUN=rhohat,
                        by=1, by.column=FALSE, align="right")
head(na.omit(roll.rhohat),n=3)