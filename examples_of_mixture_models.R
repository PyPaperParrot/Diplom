## Bimodal
x = seq(-5, 12, length=100)
y = 0.6 * dnorm(x, mean=0, sd=1) + 0.4 * dnorm(x, mean=5, sd=2)
par(mar=c(4, 4, 1, 1) + 0.1)
plot(x, y, type='l', ylab='Density', las=1)

## Skewd
x = seq(-5, 12, length=100)
y = 0.55 * dnorm(x, 0, sqrt(2)) + 0.45 * dnorm(x, 3, 4)
par(mar=c(4,4,1,1) + 0.1)
plot(x, y, type='l', las=1)

## Heavy tailed
x = seq(-12, 12, length=100)
y = 0.4 * dnorm(x, 0, sqrt(2)) +
  0.4 * dnorm(x, 0, sqrt(16)) +
  0.2 * dnorm(x, 0, sqrt(20))
z = dnorm(x, 0, sqrt(0.4*2 + 0.4*16 + 0.2*20))
par(mar=c(4,4,1,1) + 0.1)
plot(x, y, type='l', las=1)
lines(x, z, lty=2)
legend(2, 0.16, c('Mixture', 'Gaussian'), lty=c(1,2), bty='n', cex=0.77)


