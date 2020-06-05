df = read_csv(file='/home/dimitry/studies/4 курс/8 семестр/Диплом/Исследование/DATA/EURUSD_Day_RETURN_05.05.2003-29.04.2020.csv')

# another univariate plot
plot(df[[1]], type="h")

# histogram plus density plot
par(mar=c(4, 4, 1, 1) + 0.1)

df_density <- density(df[[1]])
hist(df[[1]], breaks=40, probability=TRUE)
lines(df_density)
rug(df[[1]])

# display the "normal" theoretical reference distribution
z <- seq(-0.05,0.01,0.05)
pdf_z <- dnorm(z)   # get probability density function
plot(z, pdf_z)


# density plot
df_density <- density(df[[1]])
plot(df_density)