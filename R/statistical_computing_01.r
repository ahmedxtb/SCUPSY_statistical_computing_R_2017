
# set path
setwd("/Users/kevinhsu/Downloads/SCUPSY_statistical_computing_R_2017-master/R")


rm(list = ls())
myIris = iris

dim(myIris)
# [1] 150   5
# 150 observations, 5 variables (1-4 num, 5th as factor)
myIris$obs.id = paste(rep('obsv', 150), seq(1,150,1), sep = '.')

head(myIris)

#install.packages("tidyr")
library(tidyr)

# The arguments to gather():
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
# - factor_key: Treat the new key column as a factor (instead of character vector)
data_long <- gather(myIris,
                    key = iris.features,
                    value = iris.meas,
                    Sepal.Length, Sepal.Width, Petal.Length, Petal.Width,
                    factor_key=TRUE)
head(data_long)
dim(data_long)

# The arguments to spread():
# - data: Data object
# - key: Name of column containing the new column names
# - value: Name of column containing values
data_wide <- spread(data_long,
                    key = iris.features,
                    value = iris.meas)
head(data_wide)



write.csv(myIris, file = "myIris.csv", row.names = F, fileEncoding = "utf-8")
write.csv(data_long, file = "myIris_long.csv", row.names = F, fileEncoding = "utf-8")
write.csv(data_wide, file = "myIris_wide.csv", row.names = F, fileEncoding = "utf-8")


rm(list = ls())
ls()

data_long <- read.table("myIris_long.csv", header = T, sep = ",")
(data_long)

rm(list = ls())
myIris = iris


data_long <- read.table("myIris_long.csv", header = T, sep = ",")
par(mfrow=c(1,1))
plot(seq(1,150,1),myIris$Sepal.Length)
plot(seq(1,150,1),myIris$Sepal.Length, xlab = "index", ylab = "Sepal Length")


plot(myIris$Sepal.Width,myIris$Sepal.Length, xlab = "Sepal Width", ylab = "Sepal Length")
plot(myIris[,1],myIris[,2], xlab = names(myIris)[1], ylab = names(myIris)[2])


par(mfrow=c(1,2))
plot(myIris[,1],myIris[,2], xlab = names(myIris)[1], ylab = names(myIris)[2])
plot(myIris[,3],myIris[,4], xlab = names(myIris)[3], ylab = names(myIris)[4])
par(mfrow=c(1,1))

head(myIris)
plot(myIris[,3],myIris[,4], xlab = names(myIris)[3], ylab = names(myIris)[4], type = "n")

my.label <- c(rep("a", 50), rep("b", 50), rep("c", 50))

my.color <- c(rep("red", 50), rep("blue", 50), rep("green", 50))

#text(myIris[,3],myIris[,4], my.label, cex=0.7, col=my.color)

text(myIris[1,3],myIris[1,4], my.label[1], cex=0.7, col=my.color[1])
text(myIris[60,3],myIris[60,4], my.label[60], cex=0.7, col=my.color[60])

#legend("bottomright", "(x,y)", legend=c("setosa","versicolor", "virginica"),
#       pch = "abc", col=c("red","blue","green"))

#pairs(myIris[,1:4])
pairs(myIris[,1:4], col=as.integer(myIris[,5])+1)


attach(data_long)
(aggregate(iris.meas ~ Species + iris.features, FUN = mean))
(aggregate(iris.meas ~ Species + iris.features, FUN = sd))
(aggregate(iris.meas ~ Species + iris.features, FUN = length))
(aggregate(iris.meas ~ Species + iris.features, FUN = function(x) c(N = length(x), Means = mean(x), SD = sd(x))))
detach(data_long)

aggregate(iris.meas ~ Species + iris.features, data = data_long, FUN = function(x) c(N = length(x), Means = mean(x), SD = sd(x)))


print(aggregate(iris.meas ~ Species + iris.features, data = data_long, FUN = function(x) c(N = length(x), Means = mean(x), SD = sd(x))))

# t-test
head(myIris)

idx.setosa = myIris$Species == "setosa"
setosa.data = myIris[idx.setosa, 1:4]

idx.versi = myIris$Species == "versicolor"
versi.data = myIris[idx.versi, 1:4]

idx.virg = myIris$Species == "virginica"
virginica.data = myIris[idx.virg, 1:4]

t.test(setosa.data$Petal.Length, virginica.data$Petal.Length)

t.test(setosa.data$Petal.Length, virginica.data$Petal.Length, var.equal = T)

sd(setosa.data$Petal.Length)
# 0.173664
sd(virginica.data$Petal.Length)
# 0.5518947
sd(virginica.data$Petal.Length) / sd(setosa.data$Petal.Length)
# 3.177945
?pf
pf(3.1779, 49, 49, lower.tail = F)
# p value = 4.385304e-05 < .05

t.test(setosa.data$Petal.Length, versi.data$Petal.Length)
t.test(virginica.data$Petal.Length, versi.data$Petal.Length)

