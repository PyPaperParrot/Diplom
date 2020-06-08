library(EMCluster, quietly = TRUE)
library(fitdistrplus)
set.seed(1234)
##x1 <- da1$da

KK = 2
w.true = 0.6 # True weights associated with the components
mu.true = rep(0, KK) 
mu.true[1] = 0 # True mean for 1st component
mu.true[2] = 5
sigma.true = 1
n = 200
cc = sample(1:KK, n, replace=T, prob=c(w.true, 1-w.true))
x = rep(0, n)
for(i in 1:n){
  x[i] = rnorm(1, mu.true[cc[i]], sigma.true)
}


emobj = simple.init(df, nclass = 5)
emobj <- shortemcluster(df, emobj)
summary(emobj)
ret <- emcluster(df, emobj, assign.class = TRUE)
summary(ret)


xx.true = seq(-0.05, 0.05, length=500)
dgausmixt = 0.62245*dnorm(xx.true, mu.true[1], sigma.true)+(1 - w.true)*dnorm(xx.true, mu.true[2], sigma.true)



x1 <- da1$da
ret.1 <- starts.via.svd(df, nclass = 2, method = "em")
summary(ret.1)

stripchart(df)



library(mclust)
library(readr)

df = read_csv(file='/home/dimitry/studies/4 курс/8 семестр/Диплом/Исследование/DATA/EURUSD_Day_RETURN_05.05.2003-29.04.2020.csv')

model <- densityMclust(df[[1]], G=2) 
summary(model, parameters = TRUE) 
plot(model, what='BIC')
plot(model, what='density', data=df[[1]])

df_hist <- hist(df[[1]], nclass=120, probability=TRUE)
#dist
plot(model, what='density', col='blue')
lines(df_hist$mids, df_hist$density, type='p', lwd=1)
xx=seq(-0.04, 0.04, length=20)
curve(dnorm(x, norm_para[1], norm_para[2]), type="l", lwd = 1, lty = 2, add=TRUE)

#log dist
plot(model, what='density', col='blue')
df_hist <- hist(df[[1]], nclass=120, probability=TRUE)
model_hist <- hist(model$data, nclass=120, probability=TRUE)

plot(df_hist$mids, log(df_hist$density), type='p', lwd=1)
lines(model$data, log(model$density), type='p')

lines(model_hist$mids, log(model_hist$density), type='l')

#bilog dist
df_hist <- hist(df[[1]], nclass=120, probability=TRUE)
model_hist <- hist(model$data, nclass=120, probability=TRUE)

plot(df_hist$mids, log(df_hist$density), log='x', type='p', lwd=1)
lines(model_hist$mids, log(model_hist$density), type='l')

lines(model$data, log(model$density), type='p')
plot(model, what='density', col='blue')
xx=seq(-0.04, 0.04, length=20)
curve(dnorm(x, norm_para[1], norm_para[2]), log='x', type="l", lwd = 1, lty = 2, add=TRUE)



lines(df_hist$counts)
plot(df[[1]],df_hist, type='p', log='y')


## gaussian mixture
KK = 2
w.true = 0.6494518 # True weights associated with the components
mu.true = rep(0, KK) 
mu.true[1] = -8.206663e-05 # True mean for 1st component
mu.true[2] = 1.272163e-04 
mu.true = rep(0, KK)
sigma.true[1] = sqrt(5.198873e-05)
sigma.true[2] = sqrt(3.524890e-06)

n = 200
cc = sample(1:KK, n, replace=T, prob=c(w.true, 1-w.true))
x = rep(0, n)
for(i in 1:n){
  x[i] = rnorm(1, mu.true[cc[i]], sigma.true[cc[i]])
}

## GOOD RESults
####Day gmm
df = read_csv(file='/home/dimitry/studies/4 курс/8 семестр/Диплом/Исследование/DATA/EURUSD_Day_RETURN_05.05.2003-29.04.2020.csv')

fit <- fitdist(df[[1]], 'norm') #fitting norm on data
fit.coef <-coef(fit)

df_hist <- hist(df[[1]], nclass=50, probability=TRUE)
#dist
xx = seq(-0.028, 0.028, length=200)
yy = w.true*dnorm(xx, mu.true[1], sigma.true[1]) + (1-w.true)*dnorm(xx, mu.true[2], sigma.true[2])
hist(df[[1]], nclass=30, probability=TRUE, main='', xlab='data')
lines(xx, yy, type='l', col='blue')
#lines(df_hist$mids, df_hist$density, type='p')
lines(xx, dnorm(xx, fit.coef['mean'], fit.coef['sd']), lty=2)
legend('topright', c('Mixture of 5 gaussians', 'Gaussian'), 
       col=c('blue', 'black'), lty=c(1,2), cex=0.65, xjust = 1)


#log
df_hist <- hist(df[[1]], nclass=50, probability=TRUE)
xx = seq(-0.028, 0.028, length=200)
yy = w.true*dnorm(xx, mu.true[1], sigma.true[1]) + (1-w.true)*dnorm(xx, mu.true[2], sigma.true[2])
plot(xx, yy, type='l', log='y', col='blue', xlab='data', ylab='log(Density)')
lines(df_hist$mids, df_hist$density, type='p')
lines(xx, dnorm(xx, fit.coef['mean'], fit.coef['sd']), lty=2)
legend('bottom', c('Histogramm','Mixture of 5 gaussians', 'Gaussian'), 
       col=c('black','blue', 'black'), 
       lty=c(NaN, 1,2), pch=c(1, NaN, NaN),
       cex=0.65)

#bilog

xx = seq(-0.028, 0.028, length=200)
yy = w.true*dnorm(xx, mu.true[1], sigma.true[1]) + (1-w.true)*dnorm(xx, mu.true[2], sigma.true[2])
plot(xx, yy, type='l', log='xy', col='blue', xlab='data', ylab='log(Density)')
lines(df_hist$mids, df_hist$density, type='p')#, log='x', type='p', lwd=1)
lines(xx, dnorm(xx, fit.coef['mean'], fit.coef['sd']), lty=2)
legend('bottom', c('Histogramm','Mixture of 5 gaussians', 'Gaussian'), 
       col=c('black','blue', 'black'), 
       lty=c(NaN, 1,2), pch=c(1, NaN, NaN),
       cex=0.65)





## GOOD RESults
## HOUR gaussian mixture
KK = 2
w.true = 0.2712045 # True weights associated with the components
mu.true = rep(0, KK) 
mu.true[1] = -9.302649e-06 # True mean for 1st component
mu.true[2] = 3.020387e-06 
mu.true = rep(0, KK)
sigma.true[1] = sqrt(4.697537e-06)
sigma.true[2] = sqrt(2.451062e-07 )
####HOUR gmm
df_hour = read_csv(file='/home/dimitry/studies/4 курс/8 семестр/Диплом/Исследование/DATA/EURUSD_Hour_RETURN_05.05.2003-29.04.2020.csv')

fit_hour <- fitdist(df_hour[[1]], 'norm') #fitting norm on data
fit_hour.coef <-c(-9.302649e-06, 2.451062e-07)

model_hour <- densityMclust(df_hour[[1]]) 
summary(model_hour, parameters = TRUE) 
df_hist <- hist(df_hour[[1]], nclass=70, probability=TRUE)
#dist
xx = seq(-0.028, 0.028, length=500)
yy = w.true*dnorm(xx, mu.true[1], sigma.true[1]) + (1-w.true)*dnorm(xx, mu.true[2], sigma.true[2])
hist(df_hour[[1]], nclass=70, probability=TRUE, main='', xlab='data')
lines(xx, yy, type='l', col='blue')
lines(xx, dnorm(xx, 0, sqrt(2.051062e-06 )), lty=2)
legend('topright', c('Mixture of 8 gaussians', 'Gaussian'), 
       col=c('blue', 'black'), lty=c(1,2), cex=0.65, xjust = 1)
#dist
#xx = seq(-0.028, 0.028, length=200)
#yy = w.true*dnorm(xx, mu.true[1], sigma.true[1]) + (1-w.true)*dnorm(xx, mu.true[2], sigma.true[2])
#plot(xx, yy, type='l', col='blue')
#lines(df_hist$mids, df_hist$density, type='p')

#log
xx = seq(-0.02, 0.02, length=200)
xxxx = seq(-0.02, 0.02, length=200)
yy = w.true*dnorm(xx, mu.true[1], sigma.true[1]) + (1-w.true)*dnorm(xx, mu.true[2], sigma.true[2])
plot(df_hist$mids, df_hist$density, type='p', log='y', xlab='data', ylab='log(Density)')
lines(xx, yy, type='l', col='blue')
lines(xxxx, dnorm(xxxx, 0, sqrt(2.051062e-06 )), lty=2)
axis(side = 1, at = c(log(0), log(800)),labels = T)

legend('bottom', c('Histogramm','Mixture of 8 gaussians', 'Gaussian'), 
       col=c('black','blue', 'black'), 
       lty=c(NaN, 1,2), pch=c(1, NaN, NaN),
       cex=0.65)

#log
xx = seq(-0.028, 0.028, length=200)
yy = w.true*dnorm(xx, mu.true[1], sigma.true[1]) + (1-w.true)*dnorm(xx, mu.true[2], sigma.true[2])
plot(xx, yy, type='l', log='y', col='blue')
lines(df_hist$mids, df_hist$density, type='p')
#bilog
xx = seq(-0.028, 0.028, length=200)
yy = w.true*dnorm(xx, mu.true[1], sigma.true[1]) + (1-w.true)*dnorm(xx, mu.true[2], sigma.true[2])
plot(xx, yy, type='l', log='xy', col='blue')
lines(df_hist$mids, df_hist$density, type='p')#, log='x', type='p', lwd=1)



## GOOD RESults
####Minute gmm
df_minute = read_csv(file='/home/dimitry/studies/4 курс/8 семестр/Диплом/Исследование/DATA/EURUSD_Minute_RETURN_29.04.2019-29.04.2020.csv')

model_minute <- densityMclust(df_minute[[1]]) 
summary(model_minute, parameters = TRUE) 
df_hist <- hist(df_hour[[1]], nclass=50, probability=TRUE)
#dist
xx = seq(-0.028, 0.028, length=200)
yy = w.true*dnorm(xx, mu.true[1], sigma.true[1]) + (1-w.true)*dnorm(xx, mu.true[2], sigma.true[2])
plot(xx, yy, type='l', col='blue')
lines(df_hist$mids, df_hist$density, type='p')
#log
xx = seq(-0.028, 0.028, length=200)
yy = w.true*dnorm(xx, mu.true[1], sigma.true[1]) + (1-w.true)*dnorm(xx, mu.true[2], sigma.true[2])
plot(xx, yy, type='l', log='y', col='blue')
lines(df_hist$mids, df_hist$density, type='p')
#bilog
xx = seq(-0.028, 0.028, length=200)
yy = w.true*dnorm(xx, mu.true[1], sigma.true[1]) + (1-w.true)*dnorm(xx, mu.true[2], sigma.true[2])
plot(xx, yy, type='l', log='xy', col='blue')
lines(df_hist$mids, df_hist$density, type='p')#, log='x', type='p', lwd=1)

####kstest gmm day
library(AdaptGauss)

model <- densityMclust(df[[1]], G=2) 
summary(model, parameters = TRUE) 

KStestMixtures(df[[1]], Means = c(-8.210032e-05, 1.272194e-04),
               SDs = c(sqrt(5.199482e-05)), sqrt(3.527330e-06), 
               Weights = c(0.6493525, 0.3506475),
               IsLogDistribution=1)