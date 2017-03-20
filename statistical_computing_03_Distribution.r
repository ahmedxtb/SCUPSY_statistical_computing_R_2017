
data = c(97, 82, 59, 64, 73,
         75, 80, 68, 87, 66,
         63, 93, 75, 70, 67,
         51, 84, 76, 80, 71,
        77, 90, 68, 82, 74)

quantile(data, c(.32, .40, .60), type = 6)

# mean = 65, sd = 8
# what percentage of scores is above 75?

pnorm(75, mean = 65, sd = 8, lower.tail = F)

curve(dnorm(x, 65, 8), xlim = c(0, 100), main = "Areas under the Normal distribution (M = 65, SD = 8)")

# what percentage of scores is between 43 and 73?
x=seq(0, 100, length=200)
y=dnorm(x, mean = 65, sd = 8)
plot(x,y,type="l", lwd=2, col="black")

x=seq(43, 73, length=200)
y=dnorm(x, mean = 65, sd = 8)
polygon(c(43,x,73),c(0,y,0),col="gray")
abline(0,0)

(prob.values = round(pnorm(c(43, 73), mean = 65, sd = 8), 4))
prob.values[2] - prob.values[1]

# For each of the following levels of significance, indicate the critical value.
# Use the normal curve as the underlying distribution
# CI levels: 0.05, 0.1, 0.01, 0.02, 0.025
sig.levels = c(0.05, 0.1, 0.01, 0.02, 0.025)
# one tailed
round(qnorm(sig.levels, mean = 0, sd = 1, lower.tail = T), 3)
round(qnorm(sig.levels, mean = 0, sd = 1, lower.tail = F), 3)

# two tailed
round(qnorm(sig.levels/2, mean = 0, sd = 1, lower.tail = T), 3)
round(qnorm(sig.levels/2, mean = 0, sd = 1, lower.tail = F), 3)

# plot one tailed and two tailed with Standard Normal Distribution
par(mfrow=c(2,3))

sig.level = 0.01

# the first figure, left tailed
x=seq(-4, 4, length=100)
y=dnorm(x, mean = 0, sd = 1)
plot(x,y,type="l", lwd=2, col="black", main = "one tailed/ left")

my_zscore = qnorm(sig.level, mean = 0, sd = 1, lower.tail = T)

x=seq(-4, my_zscore, length= 50)
y=dnorm(x, mean = 0, sd = 1)
polygon(c(-4,x,my_zscore),c(0,y,0),col="gray")
abline(0,0)


# the first figure, left tailed
x=seq(-4, 4, length=100)
y=dnorm(x, mean = 0, sd = 1)
plot(x,y,type="l", lwd=2, col="black", main = "one tailed/ right")

my_zscore = qnorm(sig.level, mean = 0, sd = 1, lower.tail = F)

x=seq(my_zscore, 4, length= 50)
y=dnorm(x, mean = 0, sd = 1)
polygon(c(my_zscore, x, 4),c(0,y,0),col="gray")
abline(0,0)

# the first figure, two tailed
x=seq(-4, 4, length=100)
y=dnorm(x, mean = 0, sd = 1)
plot(x,y,type="l", lwd=2, col="black", main = "one tailed/ left")

my_zscore = qnorm(sig.level/2, mean = 0, sd = 1, lower.tail = T)

x=seq(-4, my_zscore, length= 50)
y=dnorm(x, mean = 0, sd = 1)
polygon(c(-4,x,my_zscore),c(0,y,0),col="gray")

my_zscore = qnorm(sig.level/2, mean = 0, sd = 1, lower.tail = F)

x=seq(my_zscore, 4, length= 50)
y=dnorm(x, mean = 0, sd = 1)
polygon(c(my_zscore, x, 4),c(0,y,0),col="gray")
abline(0,0)

abline(0,0)

# the first figure, left tailed
x=seq(-4, 4, length=100)
y=dt(x, df = 1)
plot(x,y,type="l", lwd=2, lty = 1, col="black", ylim = c(0, 0.8))

x=seq(-4, 4, length=100)
y=dt(x, df = 6)
lines(x,y,type="l", lwd=2, lty = 2, col="red")

x=seq(-4, 4, length=100)
y=dt(x, df = 12)
lines(x,y,type="l", lwd=2, lty = 2, col="red")

x=seq(-4, 4, length=100)
y=dt(x, df = 30)
lines(x,y,type="l", lwd=2, lty = 2, col="red")

x=seq(-4, 4, length=100)
y=dnorm(x, mean = 0, sd = 1)
lines(x,y,type="l", lwd=2, col="blue", main = "one tailed/ right")

# Using the t dis. as the underlying distribution,
# state the critical values for each of the following
# CI levels: 0.05, 0.05, 0.01, 0.01, 0.1
# d.f.: 20, 200, 27, 270, 10
sig.levels = c(0.05, 0.05, 0.01, 0.01, 0.1)
my.df = c(20, 200, 27, 270, 10)

# one tailed
round(qt(sig.levels, my.df, lower.tail = T), 3)
round(qt(sig.levels, my.df, lower.tail = F), 3)

# two tailed
round(qt(sig.levels/2, my.df, lower.tail = T), 3)
round(qt(sig.levels/2, my.df, lower.tail = F), 3)




