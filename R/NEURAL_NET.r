
# https://rpubs.com/skydome20/R-Note8-ANN
# 必須安裝的套件
library(neuralnet) # for neuralnet(), nn model
library(nnet)      # for class.ind()
library(caret)     # for train(), tune parameters


# input nodes: Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
# output node: Species
# 必須將Species 轉變成dummy variables
data <- iris
head(class.ind(data$Species))

# 將 dummy variables 與原來的資料併在一起
data <- cbind(data, class.ind(data$Species))
head(data)

# 訓練 NN 模型, backpropagation algorithms
formula.bpn <- setosa + versicolor + virginica ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
# class(formula.bpn)
# [1] "formula"

bpn <- neuralnet(formula = formula.bpn, 
                  data = data,
                  hidden = c(2),       # 一個隱藏層：2個node
                  learningrate = 0.01, # learning rate
                  threshold = 0.01,    # partial derivatives of the error function, a stopping criteria
                  stepmax = 5e5        # 最大的ieration數 = 500000(5*10^5)

                  )

# 畫出模型
plot(bpn)

# 用 nrow()擷取資料筆數，乘上0.8後，表示train set 要有 80% 的資料(data size)
smp.size <- floor(0.8*nrow(data)) 
# 這裡規定好亂數表，讓每次抽樣的結果一樣
set.seed(131)                     
# 從原始資料裡面，抽出train set所需要的資料筆數(data size)
train.ind <- sample(seq_len(nrow(data)), smp.size)
# 分成train/test
train <- data[train.ind, ]
test <- data[-train.ind, ]

# tune parameters
model <- train(form=formula.bpn,     # formula
               data=train,           # 資料
               method="neuralnet",   # 類神經網路(bpn)
               
               # 最重要的步驟：觀察不同排列組合(第一層1~4個nodes ; 第二層0~4個nodes)
               # 看何種排列組合(多少隱藏層、每層多少個node)，會有最小的RMSE
               tuneGrid = expand.grid(.layer1=c(1:4), .layer2=c(0:4), .layer3=c(0)),               
               
               # 以下的參數設定，和上面的neuralnet內一樣
               learningrate = 0.01,  # learning rate
               threshold = 0.01,     # partial derivatives of the error function, a stopping criteria
               stepmax = 5e5         # 最大的ieration數 = 500000(5*10^5)
               )

# 會告訴你最佳的參數組合是什麼：第一隱藏層1個node，第二隱藏層2個node
model

plot(model)

bpn <- neuralnet(formula = formula.bpn, 
                  data = train,
                  hidden = c(1,2),     # 第一隱藏層1個node，第二隱藏層2個nodes
                  learningrate = 0.01, # learning rate
                  threshold = 0.01,    # partial derivatives of the error function, a stopping criteria
                  stepmax = 5e5        # 最大的ieration數 = 500000(5*10^5)

                  )

# 最佳化的bpn模型
plot(bpn)

# 用訓練好的模型(bpn)預測test set：
# 需要注意的是，輸入的test資料只能包含input node的值
# 所以取前四個欄位，丟入模型進行預測
pred <- compute(bpn, test[, 1:4])  

# 預測結果
pred$net.result

# 四捨五入後，變成0/1的狀態
pred.result <- round(pred$net.result)
pred.result

# 把結果轉成data frame的型態
pred.result <- as.data.frame(pred.result)

# 把預測結果轉回Species的型態： 建立一個新欄位，叫做Species
pred.result$Species <- ""

# 把預測結果轉回Species的型態
for(i in 1:nrow(pred.result)){
  if(pred.result[i, 1]==1){ pred.result[i, "Species"] <- "setosa"}
  if(pred.result[i, 2]==1){ pred.result[i, "Species"] <- "versicolor"}
  if(pred.result[i, 3]==1){ pred.result[i, "Species"] <- "virginica"}
}

pred.result

# 混淆矩陣 (預測率有96.67%)
table(real    = test$Species, 
      predict = pred.result$Species)


