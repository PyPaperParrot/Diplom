# Generate n observations from a mixture of two Gaussians
n = 50
w = c(0.6, 0.4)
mu = c(0, 5)
sigma = c(1, 2)
cc = sample(1:2, n, replace=T, prob=w)
x = rnorm(n, mu[cc], sigma[cc])
# Plot G(x) (the marginal density x) along with the observations
# just sampled
xx = seq(-5, 12, length=200)
yy = w[1]*dnorm(xx, mu[1], sigma[1]) + w[2]*dnorm(xx, mu[2], sigma[2])
par(mar=c(4, 4, 1, 1) + 0.1)
plot(xx, yy, type='l', ylab='Density', las=1)
points(x, y=rep(0, n), pch=1)
points(x, y=rep(0, n), pch=1, col=cc)

n = 200
w = c(0.7, 0.2, 0.1)
lambda =c(1, 2, 6)
cc = sample(1:3, n , replace=T, prob=w)
x = rpois(n,  lambda[cc])

x_count = tapply(x, x, FUN=sum)
barplot(x_count)

