# try mxnet
setwd("/Users/kevinhsu/Documents/D/course/my2017_scu_statisitcal_computing/mxnet_example")
#

install.packages("drat", repos="https://cran.rstudio.com")
drat:::addRepo("dmlc")
install.packages("mxnet")

library("mxnet")

## MNIST example
devtools::install_github("jlmelville/mnist")
library(mnist)
mnist <- download_mnist()


data(iris)
# 奇数⾏を学習⽤データ、偶数⾏を検証⽤データとする
train.ind <- seq(1, nrow(iris), 2)
train.x <- data.matrix(iris[train.ind, 1:4])
train.y <- as.numeric(iris[train.ind, 5]) - 1
test.x <- data.matrix(iris[-train.ind, 1:4])
test.y <- as.numeric(iris[-train.ind, 5]) - 1
table(train.y)

# mxnetでランダムプロセスをコントロールするための関数
mx.set.seed(0)

# 学習および検証時の誤差を後で使うためのリファレンスクラス
logger <- mx.metric.logger$new()

model <- mx.mlp(
  train.x, train.y,
  eval.data = list(data = test.x, label = test.y),
  hidden_node = 6,
  out_node = 3,
  activation = "relu",
  out_activation = "softmax",
  num.round = 200,
  array.batch.size = 25,
  learning.rate = 0.07,
  momentum = 0.9,
  eval.metric = mx.metric.accuracy,
  epoch.end.callback = mx.callback.log.train.metric(5, logger))

# 学習毎の精度をプロット
tmp <- data.frame("epoc" = seq(1, 200, 1),
           "train" = logger$train, "test" = logger$eval)
plot(tmp$epoc, tmp$train, type = "l", col = c(1))
lines(tmp$epoc, tmp$test, type = "l", col = c(3))

legend(130, 0.6, c("train", "test"), col=c(1, 3), lty = c(1,1))

# 検証⽤データに当てはめ
preds <- predict(model, test.x)
head(t(preds))

# 最も確率が⾼い列を予測した品種とする
pred.label <- max.col(t(preds)) - 1
# 混合⾏列をプロット
library(caret)
confusionMatrix(pred.label, test.y)

graph.viz(model$symbol)

## 2 layer
model <- mx.mlp(
  train.x, train.y,
  eval.data = list(data = test.x, label = test.y),
  hidden_node = c(6, 6),
  out_node = 3,
  activation = "relu",
  out_activation = "softmax",
  num.round = 200,
  array.batch.size = 25,
  learning.rate = 0.07,
  momentum = 0.9,
  eval.metric = mx.metric.accuracy,
  epoch.end.callback = mx.callback.log.train.metric(5, logger))

# 学習毎の精度をプロット
tmp <- data.frame("epoc" = seq(1, 200, 1),
                  "train" = logger$train[201:400], "test" = logger$eval[201:400])
plot(tmp$epoc, tmp$train, type = "l", col = c(1))
lines(tmp$epoc, tmp$test, type = "l", col = c(3))

legend(130, 0.6, c("train", "test"), col=c(1, 3), lty = c(1,1))

graph.viz(model$symbol)

preds <- predict(model, test.x)
head(t(preds))
pred.label <- max.col(t(preds)) - 1
confusionMatrix(pred.label, test.y)


## MNIST example
rm(list = ls())
devtools::install_github("jlmelville/mnist")
library(mnist)

# fetch the data set from the MNIST website
# mnist <- download_mnist()
load("~/Documents/D/course/my2017_scu_statisitcal_computing/mxnet_example/mnist_datasets.RData")

# view the fifth digit
show_digit(mnist, 30)
mnist$Label[30]

# mx.symbol.Convolutionで処理できる4次元配列に変換
train.x <- t(mnist[1:60000, 1:784])
train.y = mnist[1:60000, 785]
dim(train.x) <- c(28, 28, 1, 60000)

test.x <- t(mnist[60001:70000, 1:784])
test.y =  mnist[60001:70000, 785]
dim(test.x) <- c(28, 28, 1, 10000)
 
# CNN
input <- mx.symbol.Variable('data')

# C1
c1 <- mx.symbol.Convolution(
  data = input, kernel = c(5, 5), num_filter = 6)
a1 <- mx.symbol.Activation(data = c1, act_type = "tanh")

# S2
s2 <- mx.symbol.Pooling(
  data = a1, pool_type = "max", kernel = c(2, 2))

# C3
c3 <- mx.symbol.Convolution(
  data = s2, kernel = c(5, 5), num_filter = 16)
a2 <- mx.symbol.Activation(data = c3, act_type = "tanh")
# S4
s4 <- mx.symbol.Pooling(
  data = a2, pool_type = "max", kernel = c(2, 2))
# C5
c5 <- mx.symbol.Flatten(data = s4)

# F6
f6 <- mx.symbol.FullyConnected(data = c5, num_hidden = 84)
a3 <- mx.symbol.Activation(data = f6, act_type = "tanh")

# Output
output <- mx.symbol.FullyConnected(data = a3, num_hidden = 10)

# loss
lenet <- mx.symbol.SoftmaxOutput(data = output)

logger <- mx.metric.logger$new()

t1 = proc.time()
model <- mx.model.FeedForward.create(
  lenet,
  X = train.x,
  y = train.y,
  num.round = 20,
  array.batch.size = 1000,
  learning.rate = 0.05,
  momentum = 0.9,
  wd = 0.00001,
  eval.metric = mx.metric.accuracy,
  epoch.end.callback = mx.callback.log.train.metric(100)
  )

t2 = proc.time()
t2 - t1

data.frame(epoc = 1:20,
  train = logger$train,
  test = logger$eval)

dim(logger$train)
tmp <- data.frame("epoc" = seq(1, 200, 1),
                  "train" = logger$train, "test" = logger$eval)
plot(tmp$epoc, tmp$train, type = "l", col = c(1))
lines(tmp$epoc, tmp$test, type = "l", col = c(3))

legend(130, 0.6, c("train", "test"), col=c(1, 3), lty = c(1,1))

graph.viz(model$symbol)

preds <- predict(model, test.x)
head(t(preds))
pred.label <- max.col(t(preds)) -2
confusionMatrix(pred.label, test.y)
mx.model.save(model, "cnn_", 20)
