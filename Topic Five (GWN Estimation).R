##### GWN Estimation #####
  ### Estimators for the Parameters of the GWN Model ###
   ## Estimating the GWN model parameters for Microsoft, Starbucks and the S&P500 index ##
library(quantmod)
library(PerformanceAnalytics)

msft<-getSymbols("MSFT", from='1998-01-02',
                 to="2012-12-31",
                 warnings= FALSE,
                 auto.assign = FALSE)
stbux<-getSymbols("SBUX", from='1998-01-02',
                  to="2012-12-31",
                  warnings= FALSE,
                  auto.assign = FALSE)
sp500<-getSymbols("ˆGSPC", from='1998-01-02',
                  to="2012-12-31",
                  warnings= FALSE,
                  auto.assign = FALSE)
msft_d<-msft$MSFT.Adjusted
stbux_d<-stbux$SBUX.Adjusted
sp500_d<-sp500$GSPC.Adjusted

head(msft_d,3)
head(stbux_d,3)
head(sp500_d,3)

    # Convert Daily Prices to Monthly Prices #
msftPrices = to.monthly(msft_d, OHLC=FALSE)
sp500Prices = to.monthly(sp500_d, OHLC=FALSE)
sbuxPrices = to.monthly(stbux_d, OHLC=FALSE)

    # Calculate Returns #
msftRetS = na.omit(Return.calculate(msftPrices,
                                    method="simple"))
sp500RetS = na.omit(Return.calculate(sp500Prices,
                                     method="simple"))
sbuxRetS = na.omit(Return.calculate(sbuxPrices,
                                    method="simple"))
msftRetC = log(1 + msftRetS)
sp500RetC = log(1 + sp500RetS)
sbuxRetC = log(1 + sbuxRetS)

    # Merge the dataset #
gwnRetS = merge(msftRetS, sbuxRetS, sp500RetS)
gwnRetC = merge(msftRetC, sbuxRetC, sp500RetC)
colnames(gwnRetS) = colnames(gwnRetC) = c("MSFT", "SBUX", "SP500")

    # Plot the Monthly Simple Return #
install.packages("ggplot2")
library(ggplot2)
install.packages("GGally")
library(GGally)

ggpairs(gwnRetC, columns=2:4)

#The estimates of µi (i = masft,stbux, sp500) can be computed using the R functions apply() and mean()
muhat=apply(gwnRetC,2,mean)
muhat
  ##Starbucks has the highest average monthly return at 1.5% and the S&P 500 index has the lowest at 0.2%

#The estimates of the parameters σ^2i, σi can be computed using apply() var() and sd
sigma2hat=apply(gwnRetC,2,var)
sigmahat=apply(gwnRetC,2,sd)
rbind(sigma2hat,sigmahat)
  ##Starbucks has the most variable monthly returns at 11%, and the S&P 500 index has the smallest at 5%. -The covariance and correlation matrix estimates

    # Correlation matrix #
cormat = cor(gwnRetC)
cormat

#To extract the unique pairwise values of σij and ρij from the matrix objects covmat and cormat use
covhat = covmat[lower.tri(covmat)]
rhohat = cormat[lower.tri(cormat)]
names(covhat) <- names(rhohat) <-
  c("msft,sbux","msft,sp500","sbux,sp500")
covhat

rhohat