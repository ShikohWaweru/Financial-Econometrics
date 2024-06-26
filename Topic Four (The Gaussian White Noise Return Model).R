##### The Gaussian White Noise Return Model #####
  ### Monte Carlo Simulation of the GWN Model ###
   ## Example: Microsoft data to calibrate univariate Monte Carlo simulation of GWN model

library(quantmod)
library(PerformanceAnalytics)

msft_d<-getSymbols("MSFT", from='1998-01-02',
                   to="2012-12-31",
                   warnings= FALSE,
                   auto.assign = FALSE)

# Covert to monthly
msft_m=to.monthly(msft_d$MSFT.Adjusted, OHLC=FALSE)
colnames(msft_m)<-"MSFT"

## Compute Monthly cc returns
msft_ret_m<-na.omit(Return.calculate(msft_m,
                                     method = "log"))
length(msft_ret_m)

head(msft_ret_m)

#Compute mean and sd of Microsoft monthly returns
mu.msftRetC=mean(msft_ret_m)
sigma.msftRetC=sd(msft_ret_m)
c(mu.msftRetC,sigma.msftRetC)


  ### Simulating observations from the GWN model ###
mu = 0.004
sd.e = 0.09
n.obs = length(msft_ret_m)
set.seed(111)
sim.e = rnorm(n.obs, mean=0, sd=sd.e)
sim.ret = mu + sim.e
sim.ret = xts(sim.ret, index(msft_ret_m))
plot(sim.ret, main="Simulated (from GWN) Monthly Returns",
     lwd=2, col="blue")

     #The simulated returns fluctuate around µ = 0.004 , and the typical fluctuation is approximately equal to σ = 0.10.
     #The simulated return data look somewhat like the actual monthly return data for microsoft


  ### Creating many simulations from the GWN model ###
#To create ten simulated samples of size T = 132 from the GWN model for Microsoft cc returns use
sim.e=matrix(0,n.obs,10)
head(sim.e)
set.seed(111)
for (i in 1:10){
  sim.e[,i]=rnorm(n.obs,mean=0, sd=sd.e)
}
head(sim.e)

     #Each panel shows an alternative reality for returns over the period February 1998 through May 2012. Each one of these alternative realities is equally likely to happen, and the typical behavior of the simulated returns is similar to actual returns