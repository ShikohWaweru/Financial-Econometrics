#### The Time Value of Money ####
  ## Example 1.1 ##
   # Consider putting $1000 in an interest checking account that pays a simple annual percentage rate of 3%. The future value after n = 1,5 and 10 years is:
V=1000
R=0.03
FV1=V*(1+R)
FV5=V*(1+R)^5
FV10=V*(1+R)^10

 ### Multiple Compounding Periods ###
  ## Example 1.2 : FV with different compounding frequencies ##
   # For a simple annual percentage rate of 10%, the value of 1000 at the end of one year (n = 1) for different values of m is:
V1.2 = 1000
R1.2 = 0.1
m = c(1,2,4,356,10000)
FV1.2 = V1.2*(1+R1.2/m)^(m)
print(cbind(m, FV1.2),digits=7)
  # The result with continuous compounding is:
print(V1.2*exp(R1.2), digits = 7)

 ### Asset Return Calculations ###
  ## Example 1.3 :  Simple Returns ##
   # Consider a one - month investment in M stock. Supppose you buy the stock in month t-1 (eg end of Feb) at Pt-1 = $85 and sell the stock the next month (eg end of Mar) for Pt = $90. Further assume that M does not pay dividend between months t-1 and t. The one - month simple net and gross returns (over the month of Mar) are then:
P0 = 90
Pm1 = 85
R0 = (P0 -Pm1)/Pm1
c(R0,1+R0)

 ### Multiple Returns ###
   # Calculations for multiple returns can be vectorized in R as follows:
Pm2 = 80
 # create vector of prices
P = c(Pm2, Pm1, P0)
 # calculate vector of 1-month returns from vector of prices
R= (P[2:3] - P[1:2])/P[1:2]
R
 # calculate 2-month return from 2-1 month returns
(1 + R[1])*(1 + R[2]) - 1
 #same calculation vectorized using R function cumprod()
cumprod(1+R) - 1

 ### Portfolio Return ###
  ## Example 1.4 : Portfolio Return ##
   # Consider a portfolio of Microsoft and Starbucks stock in which you initially purchase 10 sharesof each stock at the end of month t-1 at the prices Pm,t-1 = $85 and Ps,t-1 = $30 respectively,. The intial value of the portfolio is Vt-1 = 10*$85 + 10*$30 = $1150. The portfolio shares are Xm = 850/1150 = 0.7391 and Xs = 300/1150 = 0.2609. Suppose at the end of month t, Pm = $90 and Ps = $28. Assuming that Microsoft and Starbucks dont pay dividend between periods t-1 and t, then the one period return of the 2 stocks are:
P.msft = c(85,90)
P.stbx = c(30,28)
V1.4 = P.msft[1]*10 + P.stbx[1]*10
X.msft = 10*P.msft[1]/V1.4
X.stbx = 10*P.stbx[1]/V1.4
R.msft = (P.msft[2] - P.msft[1])/P.msft[1]
R.stbx = (P.stbx[2] - P.stbx[1])/P.stbx[1]
R.p = X.msft*R.msft + X.stbx*R.stbx
V1.4.1 = V1.4*(1+R.p)
c(R.p, X.msft*R.msft, X.stbx*R.stbx, V1.4.1)

  ### Multiperiod portfolio returns and rebalancing ###
   ## Scenario 1: No Rebalancing (prices and shares framework)
    # We illustrate the buy-and-hold scenario 1 using a simple portfolio consisting of two assets A and B invested for three months starting at time t-3 in a framework where we know the asset prices each period and the initial amounts invested in each asset.
    # In particular we assume that the initial portfolio consists of Sa=100 shares of asset A and Sb=50 shares of asset B.
    # The end of month prices and values (Vit = SiPit, i=A,B) of the assets as well as the end of month values of the portfolio are given by:
 
 # price of asset A in t-3, t-2, t-1, t
P.A <- c(5,7,6,7)
 # value of asset A in t-3, t-2, t-1, t
V.A <- c(500,700,600,700)
 #price of asset B in t-3, t-2, t-1,t 
P.B <- c(10,11,12,8)
 #value of asset B in t-3, t-2, t-1,t
V.B <- c(500,550,600,400)
 #value of portfolio
V.p= V.A+V.B
 #create a table by binding the vectors (cols)
table.vals<-cbind(P.A,V.A,P.B,V.B,V.p)
 #create rows (row vector)
rownames(table.vals)<-c("t-3","t-2","t-1","t")
 #print the table
table.vals

   #In the above table the time index t represents the end of month t. Slightly abusing notation, we can also say that the time index t − 1 represents the beginning of month t. This is important because for the portfolio calculations the end-of-month values VA,t−1, VB,t−1 and Vp,t−1 are used to calculate the portfolio weights to be applied in month t.
   #The portfolio return Rp,t, is still a share weighted average of the individual asset returns but with time varying portfolio weights xA,t−1 and xB,t−1

R.A = (V.A[2:4] - V.A[1:3])/V.A[1:3]
R.B = (V.B[2:4] - V.B[1:3])/V.B[1:3]
R.p = (V.p[2:4] - V.p[1:3])/V.p[1:3]
table.returns = cbind(R.A, R.B, R.p)
rownames(table.returns) = c("t-2", "t-1", "t")
table.returns


   ### Returns and Weights Framework ###
  #The above calculations are based on knowing asset prices and initial shares purchased for each asset. In many situations, we only have return information and initial portfolio weights on each asset and the initial value of the portfolio.
  #Using the above example, the initial portfolio weights are xA,t−3 = xB,t−3 = 0.5 and the initial value of the portfolio is Vp,t−3 = 1000.

#create vectors to store the results
V.A=V.B=V.p=rep(0,4)
# Let us see the vectors
print(cbind(V.A,V.B,V.p))
#Set the first element of portfolio values V.p =10000,
#the initial portfolio value
V.p[1]=1000
V.p
# Given the initial weights (0.5,0.5),
#deduce the initial values of asset A and B
V.A[1]=V.B[1]=0.5*V.p[1]
print(cbind(V.A,V.B))
# We now compute the other asset values cumulatively
V.A[2:4]=cumprod(1+R.A)*V.A[1]
V.A
V.B[2:4]=cumprod(1+R.B)*V.B[1]
V.B
# Obtain the portfolio values as sum of asset values
V.p[2:4]=V.A[2:4]+V.B[2:4]
V.p
## Create a table to hold asset and portfolio values
table.vals=cbind(V.A,V.B,V.p)
## assign row names to table
rownames(table.vals)=c("t-3","t-2","t-1","t")
table.vals

   #The portfolio weights at the end-of-month t are computed as and the R calculations are:

x.A=V.A/V.p
x.B=V.B/V.p
table.weights=cbind(x.A,x.B)
rownames(table.weights)=c("t-3","t-2","t-1","t")
table.weights

   #The R calculations for returns based on values are
diff(table.vals[,"V.p"])/table.vals[1:3,"V.p"]

   #The calculation for returns based on weights is
x.A[1:3]*R.A + x.B[1:3]*R.B


   ###Constant Portfolio Weights ###
  #If the initial portfolio weights are to be held constant over time then the portfolio typically needs to be rebalanced at each time period to adjust the portfolio weights back to the initial weights
  #Illustration: The initial portfolio is an equally weighted portfolio, xA,t−3 = xB,t−3 = 0.5. To have constant weights the portfolio will need to be rebalanced

# Create a matrix of zeros to hold the values
value.table=matrix(0,4,3)
value.table
# Label the rows and columns
rownames(value.table)=c("t-3", "t-2", "t-1", "t")
# Label column names
colnames(value.table)=c("V.A", "V.B", "V.p")
## replace the 't-3' row with initial values- all cols
value.table["t-3", ] = c(V.A[1], V.B[1], V.p[1])
value.table
# we now create a loop to rebalance the portfolios
for (t in 2:4) {
  value.table[t, "V.A"] = value.table[t-1, "V.A"]*
    (1 + R.A[t-1])
  value.table[t, "V.B"] = value.table[t-1, "V.B"]*
    (1 + R.B[t-1])
  value.table[t, "V.p"] = value.table[t, "V.A"] +
    value.table[t, "V.B"]
  RB = max(value.table[t, "V.A"], value.table[t, "V.B"]) -
    value.table[t, "V.p"]/2
  if (value.table[t, "V.A"] > value.table[t, "V.B"]) {
    value.table[t, "V.A"] = value.table[t, "V.A"] - RB
    value.table[t, "V.B"] = value.table[t, "V.B"] + RB
  }
  else{
    value.table[t, "V.A"] = value.table[t, "V.A"] + RB
    value.table[t, "V.B"] = value.table[t, "V.B"] - RB
  }
  value.table[t, "V.p"] = value.table[t, "V.A"] +
    value.table[t, "V.B"]
}
value.table
  ##Notice that the rebalanced portfolio slightly outperforms the buy-and-hold portfolio

   ###Return Calculations with Data in R- Representing time series using xts objects ###
  #We use the Do Jones stock daily data over the period -we first load the necessary packages which contains the data
install.packages("xts")
install.packages("zoo")
install.packages("qrmdata")
install.packages("qrmtools")
library(zoo)
library(xts)
library(qrmdata)
library(qrmtools)

## Dow Jones stock price data
data("DJ_const")
## We extract a time period and take the first 5 stocks
DJdata <- DJ_const['2006-12-29/2015-12-31', 1:5]
head(DJdata)
tail(DJdata)

  #There are many ways of representing a time series in R.
  #For financial time series xts (extensible time series) objects from the xts package are especially convenient and useful
  #An xts object consists of two pieces of information: (1) a matrix of numerical data with different time series in the columns, (2) an R object representing the common time indexes associated with rows of the data.
  #xts objects extend and enhance the zoo class of time series objects from the zoo package written by Achim Zeileis (zoo stands for Z’s ordered observations)
  #The matrix of time series data can be extracted from the xts object using the xts function coredata

class(DJdata)
matrix.data=coredata(DJdata)
head(matrix.data,3)

  #The resulting matrix of data does not have any date information.
  #The date index can be extracted using the xts function index()
  #The object date.index is of class Date

date.index=index(DJdata)
class(date.index)

  #Another advantage of xts objects is that you can easily extract observations at or between specific dates. - For example, to extract AAPL’s data on 2007-01-09
DJdata["2007-01-09","AAPL"]

  #Or to extract all of the prices of AAPL for January 2007 use
DJdata["2007-01","AAPL"]

  #Transform prices to log prices
stock.data.logs<-log(coredata(DJdata))
head(stock.data.logs,5)

  ### Convert the Frequency ###
  #End-of-month prices can be extracted from the daily prices using the xts function to.monthly()
stock.data.logs.month<-to.monthly(DJdata)
head(stock.data.logs.month)

  #Note that several columns now appear- open. high, low, and close prices- we can suppress all these and work with adjusted close- by setting OHLC = FALSE
stock.data.month<-to.monthly(DJdata, OHLC=FALSE)
head(stock.data.month)

  #To preserve the Date class of the time index and show the end-of-month date, use the optional argument indexAt="lastof
stock.data.month<-to.monthly(DJdata, OHLC=FALSE,
indexAt = "lastof")
head(stock.data.month)

   ### Convert to weekly ###
stock.data.wk<-to.weekly(DJdata, OHLC=FALSE,
                         indexAt = "lastof")
head(stock.data.wk)

   ### Plotting xts objects ###
stock.data.month<-to.monthly(DJdata, OHLC=FALSE,)
plot(stock.data.month, main="Monthly Closing Prices",
     legend.loc="topleft")

  #Because the xts class inherits from the zoo class, the zoo method function plot.zoo() can also be used to plot xts objects
plot.zoo(stock.data.month, main="Monthly Closing Prices",plot.type="single", lwd=2, ylab="Price")
grid()
legend(x="topleft", legend=colnames(stock.data.month),
       lwd=2)

  #Using ggplot2
library(ggplot2)
autoplot(stock.data.month,facets=Series~.)+
  ggtitle("Monthly Closing Prices") +
  ylab("Closing Price Per Share") +
  xlab("Year")

  ## Calculating retirns ##
 #Brute force return calculations
returns.m<-diff(stock.data.month)/lag(stock.data.month)
head(returns.m)
 

  ##Calculating returns using Return.calculate
  #Simple and continuously compounded returns can be computed using the performanceAnalytics function Return.calculate()
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

legend
