
# One Way Anova
#install.packages("RCurl")
#install.packages("ez")
library(ez)
library(RCurl)

my.url <- getURL("https://raw.githubusercontent.com/deltaphase/SCUPSY_statistical_computing_R_2017/master/R/data/onewayANOVA.csv")

df <-read.csv(textConnection(my.url))

df

names(df)

library(tidyr)

# The arguments to gather():
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
# - factor_key: Treat the new key column as a factor (instead of character vector)
data_long <- gather(df,
                    key = treatments,
                    value = scores,
                    X1:X4,
                    factor_key=TRUE)
data_long
dim(data_long)


plot(data_long$treatments, data_long$scores)

# https://www.r-bloggers.com/about-boxplot/
#
# “… the bottom and top of the box are always the 25th and 75th percentile 
# (the lower and upper quartiles, respectively), and the band near the middle 
# of the box is always the 50th percentile (the median). But the ends of the 
# whiskers can represent several possible alternative values…”
# In R’s default boxplot{graphics} code,

# upper whisker = min(max(x), Q_3 + 1.5 * IQR) 
# lower whisker = max(min(x), Q_1 – 1.5 * IQR)

# where IQR = Q_3 – Q_1, the box length.
# So the upper whisker is located at the *smaller* of the maximum x value and Q_3 + 1.5 IQR, 
# whereas the lower whisker is located at the *larger* of the smallest x value and Q_1 – 1.5 IQR.

summary(aov(scores ~ treatments, data = data_long))

data_long$Ss <- paste("S", seq(1,24,1))

rt_anova = ezANOVA(
    data = data_long
    , dv = scores
    , wid = Ss
    , between = treatments
    , detailed = T
    , return_aov = T
)
rt_anova

my.url <- getURL("https://raw.githubusercontent.com/deltaphase/SCUPSY_statistical_computing_R_2017/master/R/data/TwoWayANOVA.csv")

df <-read.csv(textConnection(my.url))
str(df)

par(mfrow = c(1,3))
plot(df$treat.A, df$dv)
plot(df$treat.B, df$dv)
interaction.plot(df$treat.A, df$treat.B, df$dv,
                xlab = "",
                ylab = "duration (minutes)")

summary(aov(dv ~ treat.A + treat.B + treat.A:treat.B, data = df))

df$Ss <- paste("S", seq(1,80,1))

rt_anova = ezANOVA(
    data = df
    , dv = dv
    , wid = Ss
    , between = .(treat.A, treat.B)
    , detailed = T
    , return_aov = T
)
rt_anova

rt_anova$ANOVA

# repeated measure
my.url <- getURL("https://raw.githubusercontent.com/deltaphase/SCUPSY_statistical_computing_R_2017/master/R/data/twoWay_repeatedmeasure.csv")

df <-read.csv(textConnection(my.url))


df

par(mfrow = c(1,3))
plot(df$iv1, df$dv)
plot(df$iv2, df$dv)
interaction.plot(df$iv2, df$iv1, df$dv,
                xlab = "",
                ylab = "")

#install.packages("ez")
library(ez)

rt_anova = ezANOVA(
    data = df
    , dv = dv
    , wid = Ss
    , within = .(iv1,iv2)
    , detailed = T
    , return_aov = T
)

rt_anova

summary((rt_anova$aov))


