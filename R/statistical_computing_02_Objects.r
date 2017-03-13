
# R有很多種資料形式。較常使用的有：vectors, matrices, lists, and data frames
# 列出記憶體裡面暫存的資料：
# > ls()
# 辨別資料類型的指令：
# > str()
# > class()

rm(list = ls())
# R objects: vectors

ls()

a = rnorm(100, mean = 50, sd = 5) # 採用標準常態分配產生隨機亂數，mean = 50, sd = 5
str(a)

head(a) ### 列出前面六項
tail(a) ### 列出最後六項
summary(a)


b = c(1,2,3,4,5,6) #### 使用 c() 建立 vector，包含數字項
# b = seq(1,6,1) # 從1 ~ 6, 每隔 1 取一個數值
str(b)
c = c("1","2","3","4","5","6") #### 使用 c() 建立 vector，由字串構成
# c = as.character(seq(1,6,1)) # 如果變數 b 已經存在，可以用： c = as.character(b)
str(c)
summary(b)
summary(c)


c = as.character(seq(1,6,1))
b = seq(1,6,1)
c = as.character(b)
print(c)

# 下列哪一向可以執行 averaging??
mean(b)
mean(c)


class(b)
class(c)



d = as.numeric(c)   ### help(as)
class(d)
mean(d)



colors <- c("red", "orange", "yellow", "green", "blue", "indigo", "violet")

length(colors)  ### 計算 vector 長度
colors[7] ### 回傳第七項




colors[7] <- "purple" ### 把第七項換成 "purple"
colors

x = rnorm(10, 0, 1) # 採用標準常態分配產生隨機亂數，mean = 0, sd = 1
idx = x > 1
idx # 列出 "x 大於 1" 的 比對結果
str(idx)


x.select = x[idx]
x.select

sample(x, 4) # randomly picks four states
sample(x) # randomly permute the entire vector of state names
sample(x, replace=TRUE) # selection with replacement


colors
sample(colors, 4) # randomly picks four states
sample(colors) # randomly permute the entire vector of state names
sample(colors, 50, replace=TRUE) # selection with replacement


colors.random <- sample(colors, 50, replace=TRUE) # selection with replacement
class(colors.random)
summary(colors.random)
colors.random.fc <- as.factor(colors.random)
summary(colors.random.fc)
plot(colors.random.fc)

USPersonalExpenditure
# Personal Expenditure Data

# This data set consists of United States personal expenditures (in billions of dollars) in the categories;
# food and tobacco, household operation, medical and health, personal care, and private education
# for the years 1940, 1945, 1950, 1955 and 1960

USPersonalExpenditure[1, 3]
USPersonalExpenditure["Food and Tobacco", "1950"]
USPersonalExpenditure[1, "1950"]

USPersonalExpenditure[1, c(5, 3, 1)]
USPersonalExpenditure["Food and Tobacco", c("1960", "1950", "1940")]


# apply(matrix, dimension, function, function_arguments)
USPersonalExpenditure
apply(USPersonalExpenditure, 2, FUN = sum)
#apply(USPersonalExpenditure, 2, FUN = function(x) (sum(x^2)))
    
apply(USPersonalExpenditure, 1, sum)
apply(USPersonalExpenditure, 2, mean)



x = round(rnorm(5, 0, 1), 2) # 採用標準常態分配產生隨機亂數，mean = 0, sd = 1
print(x)
rev(x) # reverses the vector
sum(x) # sums all the elements in a numeric or logical vector
cumsum(x) # returns a vector of cumulative sums (or a running total)
diff(x) # returns a vector of differences between adjacent elements
max(x) # returns the largest element
min(x) # returns the smallest element
range(x) # returns a vector of the smallest and largest elements
mean(x) # returns the arithmetic mean


# factors
# state.abb
# state.area
# state.center
# state.division
# state.name
# state.region
# state.x77

str(state.name)
str(state.x77)

class(state.x77)
head(state.x77)

attributes(state.x77)
#state.x77[5,8] # California 的 Area
state.x77["California", "Area"]

#my.US.state <- cbind(state.x77, as.character(state.division))
#head(my.US.state)

state.division

class(state.division)

summary(state.division)

summary(as.factor(colors))

# 把 state.x77 以及 state.division 結合為一個包含數字像、文字像的複合物件，稱作 data.frame
# 1. 先把 state.x77 的橫列名稱 (rownames)獨立成為 character 物件
#(obj_colnames <- colnames(state.x77))
(obj_rownames <- rownames(state.x77))
state.x77.data = state.x77

ls()

attributes(state.x77.data)
#colnames(state.x77.data) <- NULL
#rownames(state.x77.data) <- NULL
head(state.x77.data)

state.df <- data.frame(obj_rownames, state.division, state.x77.data)
head(state.df)

str(state.df)

state.df["New York", ]


