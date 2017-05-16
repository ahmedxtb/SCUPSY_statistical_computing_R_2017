rm(list = ls())

# 一所大學的新生入學時接受英文能力測驗，假設資料符合常態分佈。
# 平均值為 65分，標準差 8
x.mean = 65
x.sd = 8


# Q1. 求出得分49, 55, 65, 72的Z分數
# Z = (x - x.mean) / x.sd

(49 - x.mean)/x.sd

get.zscore <- function(x = NULL, x.mean = 65, x.sd = 8){
  my.zscore = (x - x.mean)/x.sd
  print(my.zscore)
}


# Q2. 求出Z分數2, -1, -0.25, 1.5的原始得分
# x = Z * sd + mean

2 * x.sd + x.mean

get.xscore <- function(x.zscore = NULL, x.mean = 65, x.sd = 8){
  my.xscore = x.zscore*x.sd + x.mean
  print(my.xscore)
}

get.xscore(2)
get.xscore(c(2, -1, -0.25, 1.5))

# Q3. what percentage of scores are above 75?
# 得分大於75 的人數大約佔多少比例？
# (這邊的解法還沒換算成百分比，在解題時請注意問題的單位)
z75 = get.zscore(75)
1 - pnorm(z75, lower.tail = T)
pnorm(z75, lower.tail = F)

# Q4. what percentage of scores are between 43-73?
# 得分介於 43-73 的人數大約佔多少比例？
# (這邊的解法還沒換算成百分比，在解題時請注意問題的單位)
z43 = get.zscore(43)
z73 = get.zscore(73)
pnorm(1) - pnorm(-2.75)

# 補充練習，Z分數 vs t分數
pnorm(1)
pt(1, df = 6)
pt(1, df = 30)
pt(1, df = 60)
pt(1, df = 500)

## Q5. what is the 67th percentile?
# 百分等級 67% 對應的原始得分
qnorm(0.67)
get.xscore(qnorm(0.67))

## Q6. what is the precentile of 51?
#  得分51的百分等級
# (這邊的解法還沒換算成百分比，在解題時請注意問題的單位)
get.zscore(51)
pnorm(get.zscore(51))
