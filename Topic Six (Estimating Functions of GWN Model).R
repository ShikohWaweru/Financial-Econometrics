##### Estimating Functions of GWN Model Parameters #####
  ### Functions of GWN Model Parameters ###
   ## Example: Microsoft ##
library(quantmod)
library(PerformanceAnalytics)

msft<-getSymbols("MSFT", from='1998-01-02',
                 to="2012-12-31",
                 warnings= FALSE,
                 auto.assign = FALSE)
msftPrices = to.monthly(msft, OHLC=FALSE)
msftRetS = na.omit(Return.calculate(msftPrices$MSFT.Adjusted, method="discrete"))
msftRetC = log(1 + msftRetS)
head(msftRetS,3)
head(msftRetC,3)

   ## The GWN model estimates for µmsft and σmsft ##
n.obs = nrow(msftRetC)
muhatC = mean(msftRetC)
sigmahatC = sd(msftRetC)
muhatS = mean(msftRetS)
sigmahatS = sd(msftRetS)
estimates = c(muhatC, sigmahatC, muhatS, sigmahatS)
se.muhatC = sigmahatC/sqrt(n.obs)
se.muhatS = sigmahatS/sqrt(n.obs)
se.sigmahatC = sigmahatC/sqrt(2*n.obs)
se.sigmahatS = sigmahatS/sqrt(2*n.obs)
stdErrors = c(se.muhatC, se.sigmahatC, se.muhatS, se.sigmahatS)
ans = rbind(estimates, stdErrors)
colnames(ans) = c("mu.cc", "sigma.cc", "mu.simple", "sigma.simple")
ans

   ## Plug-in estimates of example functions for example data ##
# Let W0 = $100, 000 and rf = 0.03/12 = 0.0025. Using the plug-in estimates of the GWN model parameters, the plug-in estimates of the functions (8.1)-(8.4) are;
W0 = 100000
r.f = 0.03/12
f1.hat = muhatS + sigmahatS*qnorm(0.05)
f2.hat = -W0*f1.hat
f3.hat = -W0*(exp(muhatC + sigmahatC*qnorm(0.05)) - 1)
f4.hat = (muhatS-r.f)/sigmahatS
fhat.vals = cbind(f1.hat, f2.hat, f3.hat, f4.hat)
colnames(fhat.vals) = c("f1", "f2", "f3", "f4")
rownames(fhat.vals) = "Estimate"
fhat.vals


  ### The Jacknife ###
   ## Jackknife standard error for µˆ for Microsoft ##
# We can compute the leave-one-out estimates {µˆ−t}T t=1 using for loop as follows:
  n.obs=length(msftRetS)
muhat.loo=rep(0,n.obs) #container to store the estimates
for (i in 1:n.obs) {
  muhat.loo[i]=mean(msftRetS[-i]) #for all obs of msftretS except i
}
mean(muhat.loo)

   ## Histogram of leave-one out estimator of µ for Microsoft ##
#The jackknife estimated standard error is the scaled standard deviation of the Tµˆ−t values
se.jack.muhat = sqrt(((n.obs - 1)^2) / n.obs) * sd(muhat.loo)
se.jack.muhat

#By default, jackknife() takes as input a vector of data and the name of an R function and returns a list with components related to the jackknife procedure. The component jack.values contains the leave-one-out estimators, and the component jack.se gives you the jackknife estimated standard error:
  muhat.jack$jack.se
  

  ### Performing the Nonparametric Bootstrap in R ###
# The nonparametric bootstrap procedure is easy to perform in R. Youcan implement the procedure by “brute force” in very much the same way as you perform a Monte Carlo experiment. 
# In this approach you program all parts of the bootstrapping procedure.
# Alternatively, you can use the R package boot which contains functions for automating certain parts of the bootstrapping procedure  

   ## Brute force nonparametric bootstrap of GWN model in R ##
# Consider using the nonparametric bootstrap to compute estimates of the bias and standard error for muˆ in the GWN model for Microsoft. The R code for the brute force “for loop” to implement the nonparametric bootstrap is:
    B=1000
  muhat.boot=rep(0,B)
  n.obs=nrow(msftRetS)
  set.seed(123)
  for (i in 1:B) {
    boot.data<-sample(msftRetS,n.obs,replace=TRUE)
    muhat.boot[i]=mean(boot.data)
  }

#The distribution of the bootstrap estimates looks normal. As a result, the bootstrap 95% confidence interval for µ has the form:
    se.boot = sd(muhat.boot)
  lower = muhatS + se.boot
  upper = muhatS - se.boot
  ans = cbind(muhatS, se.boot, lower, upper)
  colnames(ans)=c("Estimate", "Std Error", "2.5%", "97.5%")
  rownames(ans) = "mu"
  ans    
  
  
  ### R package boot ###  
# The R package boot implements a variety of bootstrapping techniques including the basic non-parametric bootstrap described above. 
# The boot package was written to accompany the textbook Bootstrap Methods and Their Application by (Davison and Hinkley 1997). 
# The two main functions in boot are boot() and boot.ci(), respectively. 
# The boot() function implements the bootstrap for a statistic computed from a user-supplied function. 
# The boot.ci() function computes bootstrap confidence intervals given the output from the boot() function.

   ## Nonparametric bootstrap of GWN model using the R package boot ##
# To use the boot() function to implement the bootstrap for The function must have two arguments: x and idx. Here, x represents the original data and idx represents the random integer index (created internally by boot()) to subset x for each bootstrap sample. 
# For example, a function to be passed to boot() for µˆ is, a function must be specified to compute µˆ for each bootstrap sample.
# The function must have two arguments: x and idx. Here, x represents the original data and idx represents the random integer index (created internally by boot()) to subset x for each bootstrap sample. 
# For example, a function to be passed to boot() for µ is
  
  mean.boot = function(x, idx) {
    # arguments:
    # x data to be resampled
    # idx vector of scrambled indices created by boot() function
    # value:
    # ans mean value computed using resampled data
    ans = mean(x[idx])
    ans
  }

# To implement the nonparametric bootstrap for muˆ with 999 samples use
  set.seed(123)
  muhat.boot = boot(msftRetS, statistic = mean.boot, R=999)
  class(muhat.boot)  
  names(muhat.boot)
  
  
  ### Histogram and Normal QQ-Plot ###
# Normal, percentile, and bias-adjusted (bca) percentile 95% confidence intervals can be computed using
  boot.ci(muhat.boot, conf=0.95,
          type=c("norm", "perc", "bca")) 
  
# The GWN model estimate σˆ can be “bootstrapped” in a similar fashion. First, we write a function to compute σˆ for each bootstrap sample
  sd.boot = function(x, idx) {
    ans = sd(x[idx])
    ans
  }
  set.seed(123)
  sigmahat.boot = boot(msftRetS, statistic = sd.boot, R=999)
  sigmahat.boot
