rm(list=ls())
set.seed(81196)

KK = 2
w.true = 0.6 # True weights associated with the components
mu.true = rep(0, KK) 
mu.true[1] = 0 # True mean for 1st component
mu.true[2] = 5
sigma.true = 1
n = 120
cc = sample(1:KK, n, replace=T, prob=c(w.true, 1-w.true))
x = rep(0, n)
for(i in 1:n){
  x[i] = rnorm(1, mu.true[cc[i]], sigma.true)
}

par(mfrow=c(1, 1))
xx.true = seq(-8, 11, length=200)
yy.true = w.true*dnorm(xx.true, mu.true[1], sigma.true)+(1 - w.true)*dnorm(xx.true, mu.true[2], sigma.true)
plot(xx.true, yy.true, type='l', xlab='x', ylab='True density', lwd=2)
points(x, rep(0, n), col=cc)

# For EM initialize the parameters
w = 0.5
mu = rnorm(KK, mean(x), sd(x))
sigma = sd(x)

xx = seq(-8, 11, length=200)
yy = w*dnorm(xx, mu[1], sigma)+(1 - w)*dnorm(xx, mu[2], sigma)
plot(xx, yy, type='l', xlab='x', ylab='True density', lwd=2)
points(x, rep(0, n), col=cc)

s = 0
sw = FALSE
QQ = -Inf
QQ.out = NULL
epsilon = 10^(-5)

while(!sw){
  # E step
  v = array(0, dim=c(n, KK))
  v[,1] = log(w) + dnorm(x, mu[1], sigma, log=TRUE) # compute the log of the weights
  v[,2] = log(1-w) + dnorm(x, mu[2], sigma, log=TRUE) # compute the log of the weights
  for (i in i:n){
    v[i,] = exp(v[i,] - max(v[i,]))/sum(exp(v[i,] - max(v[i,])))
  }
  
  #M step
  ## weights
  w = mean(v[,1])
  mu = rep(0, KK)
  for(k in 1:KK){
    for(i in 1:n){
      mu[k] = mu[k] + v[i, k] * x[i]
    }
    mu[k] = mu[k] / sum(v[,k])
  }
  # standard deviations
  sigma = 0
  for(i in i:n){
    for(k in 1:KK){
      sigma = sigma + v[i, k] * (x[i] - mu[k])^2
    }
  }
  sigma = sqrt(sigma/sum(v))
  
  #Check convergence
  QQn = 0
  for(i in 1:n){
    QQn = QQn + v[i, 1]*(log(w) + dnorm(x[i], mu[1], sigma, log=TRUE)) +
                v[i, 2]*(log(1-w) + dnorm(x[i], mu[2], sigma, log=TRUE))
  }
  if(abs(QQn-QQ) / abs(QQn) < epsilon){
    sw=TRUE
  }
  QQ = QQn
  QQ.out = c(QQ.out, QQ)
  s = s + 1
  print(paste(s, QQn))
  
}