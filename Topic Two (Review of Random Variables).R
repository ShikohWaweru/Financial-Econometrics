##### REVIEW OF RANDOM VARIABLES ####
  ### Random Variables ###

   ## Discrete Random Variables ##
# Let X denote the annual return on Safaricom stock over the next year. We might hypothesize that the annual return will be influenced by the general state of the economy. Consider five possible states of the economy: depression, recession, normal, mild boom and major boom.
  #create a vector of returns
r.safcom<-c(-0.3,0,0.1,0.2,0.5)
  # Vector of probs
prob.vals<- c(0.05,0.20,0.50,0.20,0.05)
  # plot a histogram
plot(r.safcom,prob.vals,type="h",lwd=4,
     xlab = "annual return",ylab = "probability", xaxt="n")
points(r.safcom,prob.vals, pch=16, cex=2, col="blue")
axis(1,at=r.safcom)


   ## Quantiles of the distribution of a random variable ##  
#Find Pr(X ≥ 2). 
  #By symmetry of the normal distribution, Pr(X ≥ 2) = Pr(X ≤ −2) = Φ(−2)
pnorm(-2)
  #Next, consider finding Pr(−1 ≤ X ≤ 2): Pr(−1 ≤ X ≤ 2) = Pr(X ≤ 2) − Pr(X ≤ −1)
pnorm(2)-pnorm(-1)


  ## Plotting the standard normal curve in R ##
#When working with a probability distribution, it is a good idea to make plots of the pdf or cdf to reveal important characteristics. The following examples illustrate plotting distributions using R.
  #Plot pdf
x.vals = seq(-4, 4, length=150)
plot(x.vals, dnorm(x.vals), type="l", lwd=2, col="blue",
     xlab="x", ylab="pdf")

  #plot cdf
plot(x.vals, pnorm(x.vals), type="l", lwd=2, col="blue",
     xlab="x", ylab="CDF")


  ## The general normal distribution ##
#Example Let R denote the monthly return on an investment in Safaricom stock, and assume that it is normally distributed with mean µR = 0.01 and standard deviation σR = 0.10. That is, R ∼ N(0.01,(0.10)^2). In R
mu.r=0.01
sd.r=0.1
x.vals=seq(-4,4,length=150)*sd.r+mu.r
plot(x.vals,dnorm(x.vals,mean=mu.r,sd=sd.r),
     type="l",lwd=2,col="blue",xlab="x",ylab="pdf")


  ## Risk-return trade-off ##
#Consider the following investment problem. We can invest in two non-dividend paying stocks, Amazon and Boeing, over the next month. Let RA denote the monthly return on Amazon and RB denote the monthly return on Boeing. Assume that RA ∼ N(0.02,(0.10)^2) and RB ∼ N(0.01,(0.5)^2)

#For example, the probability of losing more than 10% in one month for Amazon is
pnorm(-0.10,0.02,0.1)
#and the probability of losing more than 10% in one month forBoeing is
pnorm(-0.5,0.01,0.1)


  ## Computing tail probabilities and quantiles from the Student’s t distribution ##
#The R functions p(t) and q(t) can be used to conpute the cdf and quantiles of a student’s t random variable.

#For ν = 1, 2, 5, 10, 60, 100 and ∞ the 1% quantiles can be computes using:
  v=c(1,2,10,60,100,Inf)
qt(0.01,df=v)


  ## Value-at-Risk: An introduction ##
#Example: Consider an investment of $10,000 in Microsoft stock over the next month. Let R denote the monthly simple return on microsoft stock and assume that R ∼ N(0.05,(0.10)^2). Let W0 denote the investment value at the beginning of the month and W1 denote the investment value at the end of the month. In this example, W0 = $10, 000. Consider the following questions:
  # a) What is the probability that end-of-month wealth is less than $9, 000, and what must the return on Microsoft be for this to happen?
    #we use the above normal distribution for W1 to get: Pr(W1 < $9, 000) is:
pnorm(9000,mean=10500,sd=1000)

  # b) What is the loss in dollars that would occur if the return on Microsoft stock is equal to its 5% quantile, q.05? That is, what is the monthly 5% VaR on the $10,000 investment in Microsoft?
qnorm(0.05,mean=0.05,sd=0.10)
  #This means that, with 5% probability the return on Microsoft stock is -11.4% or less