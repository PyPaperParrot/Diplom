library(ghyp)
library(readr)
library(car)
set.seed(42)
df = read_csv(file='/home/dimitry/studies/4 курс/8 семестр/Диплом/Исследование/DATA/EURUSD_Day_RETURN_05.05.2003-29.04.2020.csv')

hyp_dist = fit.hypuv(df, silent = TRUE)
hist(hyp_dist, log=TRUE)

NIG_dist = fit.NIGuv(df, silent = TRUE)
hist(NIG_dist, log=TRUE)

t_dist = fit.tuv(df, silent=TRUE)
hist(t_dist, log=TRUE)

gauss_dist = fit.gaussuv(df, silent=TRUE)
hist(gauss_dist, log=TRUE)

qqghyp(NIG_dist, gaussian=FALSE)

n <- length(df[[1]])
x <- sort(df[[1]]); vals <- unique(df[[1]])
rval <- approxfun(vals, cumsum(tabulate(match(x, vals)))/n, 
                  method = "constant", yleft = 0, yright = 1, f = 0,
                  ties = "ordered")

xx = seq(-0.05, 0.05, length=500)
emp_dist = epdf(df[[1]])
yy = emp_dist(xx)
lines(density(df[[1]]), col = "red", lwd = 2)
plot()
hist(NIG_dist)
plot(xx, yy, type='l', xlab='x', ylab='True density', lwd=2)


## HYP day return
HYPday.returns <- fit.hypuv(data=df[[1]],silent=TRUE)
par(mar=c(4, 4, 1, 1) + 0.1)

hist(HYPday.returns,ghyp.col="blue",plot.legend=T, legend.cex=0.8, main='')
hist(HYPday.returns,log.hist=T,nclass=30,legend.cex=0.8,ghyp.col="blue")
hist(HYPday.returns, log='x',log.hist=T,nclass=70,legend.cex=0.8,ghyp.col="blue")
qqghyp(HYPday.returns,plot.legend=T,legend.cex=1, main='')
################################
## NIG day return
NIGday.returns <- fit.NIGuv(data=df[[1]],silent=TRUE)
par(mar=c(4, 4, 1, 1) + 0.1)
hist(NIGday.returns,ghyp.col="blue",plot.legend=T, legend.cex=0.8, main='')
hist(NIGday.returns,log.hist=T,nclass=30,legend.cex=0.8,ghyp.col="blue")
hist(NIGday.returns, log='x',log.hist=T,nclass=70,legend.cex=0.8,ghyp.col="blue")
qqghyp(NIGday.returns,plot.legend=T,legend.cex=0.8)
#################




## HOURs
hour_df = read_csv(file='/home/dimitry/studies/4 курс/8 семестр/Диплом/Исследование/DATA/EURUSD_Hour_RETURN_05.05.2003-29.04.2020.csv')

## HYP return
  HYPhour.returns <- fit.hypuv(data=hour_df[[1]],silent=TRUE)
  par(mar=c(4, 4, 1, 1) + 0.1)
  
  hist(HYPhour.returns,nclass=70,ghyp.col="blue",plot.legend=T, legend.cex=0.8, main='')
  hist(HYPhour.returns,log.hist=T,nclass=70,location='topright',legend.cex=0.8,ghyp.col="blue")
  hist(HYPhour.returns, log='x',log.hist=T,location='topright',nclass=70,legend.cex=0.8,ghyp.col="blue")
  qqghyp(HYPhour.returns,plot.legend=T,legend.cex=1, main='')
################################
## NIG hour return
NIGhours.returns <- fit.NIGuv(data=hour_df[[1]],silent=TRUE)
par(mar=c(4, 4, 1, 1) + 0.1)
hist(NIGhours.returns,nclass=70,ghyp.col="blue",plot.legend=T, legend.cex=0.8, main='')
hist(NIGhours.returns,log.hist=T,nclass=70,location='topright',legend.cex=0.8,ghyp.col="blue")
hist(NIGhours.returns, log='x',log.hist=T,nclass=70,legend.cex=0.8,ghyp.col="blue")
qqghyp(NIGhours.returns,plot.legend=T,legend.cex=0.8, main='')
#################

## Minutes
minutes_df = read_csv(file='/home/dimitry/studies/4 курс/8 семестр/Диплом/Исследование/DATA/EURUSD_Minute_RETURN_05.06.2019-04.09.2019.csv')

## HYP minute return
HYPminute.returns <- fit.hypuv(data=minutes_df[[1]],silent=TRUE)
par(mar=c(4, 4, 1, 1) + 0.1)

hist(HYPminute.returns,nclass=500,ghyp.col="blue",plot.legend=T, legend.cex=0.8, main='')
hist(HYPminute.returns,log.hist=T,nclass=70,location='topright',legend.cex=0.8,ghyp.col="blue")
hist(HYPminute.returns, log='x',log.hist=T,location='topright',nclass=70,legend.cex=0.8,ghyp.col="blue")
qqghyp(HYPminute.returns,plot.legend=T,legend.cex=1, main='')
################################
## NIG minute return
NIGminute.returns <- fit.NIGuv(data=minutes_df[[1]],silent=TRUE)
par(mar=c(4, 4, 1, 1) + 0.1)
hist(NIGminute.returns,nclass=500,ghyp.col="blue",plot.legend=T, legend.cex=0.8, main='')
hist(NIGminute.returns,log.hist=T,nclass=300,location='topright',legend.cex=0.8,ghyp.col="blue")
hist(NIGminute.returns, log='x',log.hist=T,nclass=300,location='topright',legend.cex=0.8,ghyp.col="blue")
qqghyp(NIGminute.returns,plot.legend=T,legend.cex=0.8, main='')
#################

##GH minute return
GHminute.returns <- fit.ghypuv(data=minutes_df[[1]],silent=TRUE)
par(mar=c(4, 4, 1, 1) + 0.1)
hist(GHminute.returns,nclass=500,ghyp.col="blue",plot.legend=T, legend.cex=0.8, main='')
hist(GHminute.returns,log.hist=T,nclass=300,location='topright',legend.cex=0.8,ghyp.col="blue")
hist(GHminute.returns, log='x',log.hist=T,nclass=300,location='topright',legend.cex=0.8,ghyp.col="blue")
qqghyp(GHminute.returns,plot.legend=T,legend.cex=0.8, main='')