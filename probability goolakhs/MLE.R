minLog = 0.0000001
data <- rpois(100, 12)
data <- rnorm(100, 10, 10)
data <- runif(100, 5, 10)
data <- rgamma(100, 5, 5)
write(data, file = "normData",
      ncolumns = 100,
      append = FALSE, sep = " ")


uniform.lik <- function(theta,y){
  a<-theta[1]
  b<-theta[2]
  n<-length(y)
  if(a>=b || a>min(y) || b<max(y))
    return(0)
  return(-n*log(1/(b-a)))
}

poisson.lik<-function(mu,y)
{
  n<-length(y)
  if(mu<minLog)
    mu=minLog
  logl<-log(mu)*sum(y)-n*mu
  return(-logl)
}

normal.lik<-function(theta,y)
{
  mu<-theta[1]
  sigma2<-theta[2]
  n<-length(y)
  if(sigma2<minLog)
    sigma2=minLog
  logl<- -.5*n*log(2*pi) -.5*n*log(sigma2) - (1/(2*sigma2))*sum((y-mu)**2)
  return(-logl)
}

#gamma.lik<-function(theta,y)
#{
#  alpha<-theta[1]
#  betha<-theta[2]
#  n<-length(y)
#  logl<- (alpha-1)*sum(log(y)) -(1/betha)*sum(y) - n*alpha*log(betha) - n*log(gamma(alpha))
#  return(-logl)
#}

gamma.lik<-function(theta,y)
{
  alpha<-theta[1]
  betha<-theta[2]
  n<-length(y)
  logl<- sum(log(dgamma(y,alpha, betha)))
  return(-logl)
}

estimate <- function(filename, likelihoodFunction, start){
  observations <- as.numeric(read.table(filename)[1,])
  if(identical(likelihoodFunction ,uniform.lik)){
    estimateReturn <- optim(start,likelihoodFunction,y=observations,method="L-BFGS-B", lower=c(a=-Inf, b=max(observations)), upper=c(a=min(observations), b=Inf))
  }
  else{
    estimateReturn <- optim(start,likelihoodFunction,y=observations,method="L-BFGS-B")
  }
  return(estimateReturn$par)
}

estimate("poisData", poisson.lik, 1)
estimate("normData", normal.lik, c(0,1))
estimate("unifData", uniform.lik, c(0,1))
estimate("gammaData", gamma.lik, c(1,1))
