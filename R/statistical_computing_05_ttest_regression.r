
library(RCurl)

my.url <- getURL("https://raw.githubusercontent.com/deltaphase/SCUPSY_statistical_computing_R_2017/master/R/data/onewayANOVA.csv")

df <-read.csv(textConnection(my.url))

# print data
df

# column-wise means
#apply(df, 2, mean)

# column-wise s.d.
#apply(df, 2, sd)

df.summary = cbind(apply(df, 2, mean), 
                  apply(df, 2, sd),
                  apply(df, 2, length))

df.summary <- as.data.frame(df.summary)
colnames(df.summary) <- c("Mean", "S.D.", "N")

df.summary
df.summary$SE <- df.summary$S.D / sqrt(df.summary$N)
df.summary

## t test, step by step
diff_ = mean(df$X3) - mean(df$X4)
s2 =(var(df$X3)*5 + var(df$X4)*5)/(6 + 6 -2)
t.stat = diff_ / sqrt(s2 * (1/6 + 1/6))


# p-value
# two-tail (A ≠ B)
pval.2tail = min(pt(t.stat, 10, lower.tail = T),  pt(t.stat, 10, lower.tail = F)) * 2

# one-tail, left (A - B < 0; A < B)
pval.left = pt(t.stat, 10, lower.tail = T)

# one-tail, right (A - B < 0; A < B)
pval.right = pt(t.stat, 10, lower.tail = F)


print("two-tail t-test, p-value=")
round(pval.2tail, 3)

print("one-tail (left) t-test, p-value=")
round(pval.left, 3)

print("one-tail (right) t-test, p-value=")
round(pval.right, 3)



# two-tail (A ≠ B)
t.test(df$X3, df$X4, var.equal = T, alternative = c("two.sided"))

# one-tail, left (A - B < 0; A < B)
t.test(df$X3, df$X4, var.equal = T, alternative = c("less"))


# one-tail, right (A - B < 0; A < B)
t.test(df$X3, df$X4, var.equal = T, alternative = c("greater"))

df.summary
## compare s.d.
9.3/  5.00
pf(1.86, 5, 5, lower.tail = F)
##

#install.packages(lawstat)
library(tidyr)
library(lawstat)

data_long <- gather(df,
                    key = treatments,
                    value = scores,
                    X1,X2,
                    factor_key=TRUE)
data_long

levene.test(data_long$scores, data_long$treatments)

# regression
library(RCurl)

#my.url <- getURL("https://raw.githubusercontent.com/deltaphase/SCUPSY_statistical_computing_R_2017/master/R/data/BodyFat.txt")
#df <-read.table(textConnection(my.url), header = T)
df<- read.table("BodyFat.txt", header = T)
df
df <- df[order(df$X1), ]

lm.model.X1 <- lm(Y ~ X1, data = df)
summary(lm.model.X1)


plot(df$X1, df$Y, type = "p")
abline(lm.model.X1)

conf.bands <- data.frame(predict(lm.model.X1, interval = c("confidence")))
conf.bands

lm.model.X1$coefficients[1] + lm.model.X1$coefficients[2]*df$X1

plot(df$X1, df$Y, type = "p", ylim = c(0, 35), ylab = "Y", xlab = "X1")
abline(lm.model.X1)
#lines(df$X1, conf.bands$lwr, col = 'blue', lty = 2, lwd = 2)
#lines(df$X1, conf.bands$upr, col = 'blue', lty = 2, lwd = 2)

pred.bands <- data.frame(predict(lm.model.X1, interval = c("prediction")))

plot(df$X1, df$Y, type = "p", ylim = c(0, 35), ylab = "Y", xlab = "X1")
abline(lm.model.X1)

lines(df$X1, conf.bands$lwr, col = 'blue', lty = 2, lwd = 2)
lines(df$X1, conf.bands$upr, col = 'blue', lty = 2, lwd = 2)

lines(df$X1, pred.bands$lwr, col = 'red', lty = 2, lwd = 2)
lines(df$X1, pred.bands$upr, col = 'red', lty = 2, lwd = 2)

summary(lm(Y ~ X1, df))

anova((lm(Y ~ X1, df)))

summary(lm(Y ~ X2, df))
anova((lm(Y ~ X2, df)))

summary(lm(Y ~ X1 + X2, df))
anova(lm(Y ~ X1 + X2, df))



fit <- lm(Y ~ X1 + X2 + X3, df)
summary(fit)
anova(fit)

fit.aov <- anova(fit)
fit.aov

tab <- as.table(cbind(
  SS = c("SSR(x1, x2, x3)" = sum(fit.aov[1:3, 2]),
         "SSR(x1)"           = fit.aov[1, 2],
         "SSR(x2|x1)"        = fit.aov[2, 2],
         "SSR(x3|x1, x2)"    = fit.aov[3, 2],
         "SSE"               = fit.aov[4, 2],
         "Total"             = sum(fit.aov[, 2])),

  Df = c(                    sum(fit.aov[1:3, 1]),
                               fit.aov[1, 1],
                               fit.aov[2, 1],
                               fit.aov[3, 1],
                               fit.aov[4, 1],
                               sum(fit.aov$Df)),

  MS = c(                    sum(fit.aov[1:3, 2]) / sum(fit.aov[1:3, 1]),
                               fit.aov[1, 3],
                               fit.aov[2, 3],
                               fit.aov[3, 3],
                               fit.aov[4, 3],
                               NA)
))

round(tab, 2)

  
SS = c("SSR(x1, x2, x3)" = sum(fit.aov[1:3, 2]),
         "SSR(x1)"           = fit.aov[1, 2],
         "SSR(x2|x1)"        = fit.aov[2, 2],
         "SSR(x3|x1, x2)"    = fit.aov[3, 2],
         "SSE"               = fit.aov[4, 2],
         "Total"             = sum(fit.aov[, 2]))

SS


fit.re <- update(fit, . ~ . - X3)  # the "." indicates "old model terms"
summary(fit.re)
summary(fit)

anova(fit.re, fit)


