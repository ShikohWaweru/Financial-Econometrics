##### Volatility Models  ######
 #### The ARCH(1) Model ####
  ### Simulate an ARCH(1) model using rugarch ###
   ## RUGARCH Package ##
install.packages("rugarch")
library(rugarch)
library(PerformanceAnalytics)

   ## Specification- ugarchspec() ##
# The ugarchspec function is the entry point for most of the modelling done in the rugarch package. 
# This is where the model for the conditional mean, variance and distribution is defined, in addition to allowing the user to pass any starting or fixed parameters, the naming of which is described in the documentation
arch1.spec = ugarchspec(variance.model =
                          list(garchOrder=c(1,0)),
                        mean.model = list(armaOrder=c(0,0)),
                        fixed.pars=list(mu = 0, omega=0.5, alpha1=0.5))

  ##The argument fixed.pars is a list whose components give the ARCH(1) model parameters. The names of the list components match the parameters from the ARCH(1) model: mu is µ, omega is ω, and alpha1 is α1

   ## Simulation: ugarchpath() ##
# Simulated values of Rt and σt are produced using the ugarchpath() function taking as input the “uGARCHspec” object arch1.spec and the number of simulations, n.sim=1000, to produce:
  set.seed(123)
arch1.sim=ugarchpath(arch1.spec,n.sim=1000)
class(arch1.sim)
slotNames(arch1.sim)
names(arch1.sim@path)

#The path slot is alist which contains the simulated values of σt, Rt and t = Rt + −µt as matrix objects.
# Invoking the plot() method produces a menu of plot choices
par(mfrow=c(2,1))
plot(arch1.sim, which=2) #simulated series Rt
plot(arch1.sim, which=1) #sigma


 #### ARCH(p) process ####
  ### Example: ARCH TEST ###
rm(list=ls()) #Removes all items in Environment!
install.packages("FinTS")
install.packages("tseries")
install.packages("dynlm")
install.packages("vars")
install.packages("nlWaldTest")
install.packages("broom")
install.packages("devtools")
install.packages("car")
install.packages("sandwich")
install.packages("knitr")
install.packages("forecast")

library(FinTS) #for function `ArchTest()`
library(rugarch) #for GARCH models
library(tseries) # for `adf.test()`
library(dynlm) #for function `dynlm()`
library(vars) # for function `VAR()`
library(nlWaldTest) # for the `nlWaldtest()` function
library(lmtest) #for `coeftest()` and `bptest()`.
library(broom) #for `glance(`) and `tidy()`
library(devtools)
library(car) #for `hccm()` robust standard errors
## Loading required package: carData
library(sandwich)
library(knitr) #for `kable()`
library(forecast)
install_git("https://github.com/ccolonescu/PoEdata")
library(PoEdata)

# The following example uses the dataset byd, which contains 500 generated observations on the returns to shares in BrightenYourDay Lighting. Here is the time series plot of the data and histogram.
par(mfrow=c(1,2))
data("byd", package="PoEdata")
rTS <- ts(byd$r)
plot.ts(rTS)
hist(rTS, main="", breaks=20, freq=FALSE, col="grey")

   ## Step 1: Run regression (GWN) ##
byd.mean<-dynlm(rTS~1)
summary(byd.mean)

    # Obtain residuals squared #
# Obtain residuals squared
ehatsq <- ts(resid(byd.mean)^2)
byd.ARCH <- dynlm(ehatsq~L(ehatsq))
summary(byd.ARCH)

   ## Step 2-4: Auxilliary regression and hypothesis testing ##
T <- nobs(byd.mean) 
q <- length(coef(byd.ARCH))-1
Rsq <- glance(byd.ARCH)[[1]]

LM <- (T-q)*Rsq
alpha <- 0.05
Chicr <- qchisq(1-alpha, q)
print(c(LM,Chicr))

   ## The result is the LM statistic, equal to 62.16, which is to be compared to the critical chi-squared value with α = 0.05 and q = 1 degrees of freedom; this value is χ2(0.95,1) = 3.84; this indicates that the null hypothesis is rejected, concluding that the series has ARCH effects

#The same conclusion can be reached if, instead of the step-by-step procedure we use one of R’s ARCH test capabilities, the ArchTest() function in package FinTS.
bydArchTest <- ArchTest(byd, lags=1, demean=TRUE)
bydArchTest


 #### Generalized ARCH (GARCH) Models ####
  ### (Simulate GARCH(1,1) model using rugarch) ###
garch11.spec=ugarchspec(variance.model =
                          list(garchOrder=c(1,1)),
                        mean.model = list(armaOrder=c(0,0)),
                        fixed.pars = list(mu=0,omega=0.1,
                                          alpha1=0.1,beta1=0.8))

   ## Simulation ##
set.seed(123)
garch11.sim=ugarchpath(garch11.spec,n.sim = 1000)
par(mfrow=c(2,1))
plot(garch11.sim, which=2)
plot(garch11.sim, which=1)


  ### Fitting ARMA + GARCH Models ###
  ## Example: AR(1) GARCH(1,1) model fit to daily BMW stock log returns #
# This example uses the daily BMW stock log returns. The ugarchfit() function from R’s rugarch package is used for fit an AR(1)+GARCH(1,1) model to this series
# Although ugarchfit() allows the white noise to have a nonGaussian distribution, we begin this example using the Gaussian white noise (the default)

install.packages("evir")
library(evir) # extreme value theory data
data(bmw, package='evir')
arma.garch.norm=ugarchspec(mean.model = list(armaOrder=c(1,0)),
                           variance.model = list(garchOrder=c(1,1)))
bmw.garch.norm=ugarchfit(data=bmw,spec=arma.garch.norm)

  ##In the output, the AR(1) term is statistically significant, implying that there is a small amount of positive autocorrelation
  ##Both alpha1 and beta1 are highly significant which implies rather persistent volatiltity clustering

   ## Assume the white noise follows a t-distribution ##
arma.garch.t=ugarchspec(mean.model = list(armaOrder=c(1,0)),
                        variance.model = list(garchOrder=c(1,1)),
                        distribution.model = "std")
bmw.garch.t=ugarchfit(data=bmw,spec=arma.garch.t)


 #### The GJR Model ####
  ### Example: GJR Japanese Yen series ###
install.packages("haven")
library(haven)
dtafile <- file.path("/Volumes/GoogleDrive/My Drive/Strathmore_Economics/Financial Econometrics/Brooks/Dataset/currencies.dta")
data<-read_dta(dtafile)
data<-na.omit(data)
head(data)

   ## Standard GARCH (1,1) ##
garch11.spec = ugarchspec(mean.model=list(armaOrder=c(0,0)),
                          variance.model=list(garchOrder=c(1,1),model="sGARCH"))
garch11.fit = ugarchfit(garch11.spec,data=data$rjpy)
garch11.fit

   ## GJR estimation ##
gjrgarch11.spec = ugarchspec(mean.model=list(armaOrder=c(0,0)),
                             variance.model=list(garchOrder=c(1,1),model="gjrGARCH"))
gjrgarch11.fit = ugarchfit(gjrgarch11.spec,data=data$rjpy)
gjrgarch11.fit


