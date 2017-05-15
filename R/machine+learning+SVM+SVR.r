
library("e1071")


head(iris,5)
pairs(iris[,1:4], col=as.integer(iris[,5])+1)
#str(iris)
#unique(iris$Species)
#summary(iris)

attach(iris)
detach(iris)

x <- subset(iris, select=-Species)
y <- Species

#svm_model <- svm(Species ~ ., data=iris)
svm_mode1 <- svm(x,y, cost = 1, gamma = 0.25)
summary(svm_mode1)

pred <- predict(svm_mode1,x)

table(pred,y)

svm_mode1 <- svm(x,y, cost = 10, gamma = 2)
pred <- predict(svm_mode1,x)
table(pred,y)

# Visualizing SVM model (1)
svm_mode1 <- svm(Species ~ ., data=iris, cost = 1, gamma = 0.25)
plot(svm_mode1, iris, Petal.Width ~ Petal.Length,
slice = list(Sepal.Width = 3, Sepal.Length = 4),color.palette = terrain.colors)


# Visualizing SVM model (2)
plot(svm_mode1, iris, Sepal.Width ~ Petal.Width,
slice = list(Sepal.Length = 3, Petal.Length = 4),color.palette = terrain.colors)


svm_tune <- tune(svm, train.x=x, train.y=y, 
              kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

print(svm_tune)

svm_model_after_tune <- svm(Species ~ ., data=iris, kernel="radial",
                            cost=1, gamma=0.5)
summary(svm_model_after_tune)

pred <- predict(svm_model_after_tune,x)
table(pred,y)

# SVR 
# https://github.com/alexandrekow/svmtutorial/tree/master/SupportVectorRegressionInR

#### Step 1, regression
# Load the data from the csv file
#dataDirectory <- "D:/DropBox/Dropbox/@Alex_Tutoriaux/"  
dataDirectory <- "/Users/kevinhsu/Downloads/"
data <- read.csv(paste(dataDirectory, 'regression.csv', sep=""), header = TRUE)

# Plot the data 
plot(data, pch=16)

# Create a linear regression model
model <- lm(Y ~ X, data)

# Add the fitted line
abline(model)

#### Step 2, regression, RMSE
plot(data, pch=16)
model <- lm(Y ~ X , data)

# make a prediction for each X 
predictedY <- predict(model, data)

# display the predictions
points(data$X, predictedY, col = "blue", pch=4) 

# This function will compute the RMSE
rmse <- function(error)
{
  sqrt(mean(error^2))
}

error <- model$residuals  # same as data$Y - predictedY
predictionRMSE <- rmse(error)   # 5.703778 


#### Step 3, regression vs SVR
rmse <- function(error)
{
  sqrt(mean(error^2))
}


plot(data, pch=16)
 

# linear model ==============================================
model <- lm(Y ~ X , data)
  
predictedY <- predict(model, data) 
points(data$X, predictedY, col = "blue", pch=4)   


error <- model$residuals  # same as data$Y - predictedY
predictionRMSE <- rmse(error)   # 5.703778 
# end of linear model =======================================


plot(data, pch=16)

# svr model ==============================================
if(require(e1071)){ 
  model <- svm(Y ~ X , data)
  
  predictedY <- predict(model, data)
   
  points(data$X, predictedY, col = "red", pch=4)
  
  # /!\ this time  svrModel$residuals  is not the same as data$Y - predictedY
  # so we compute the error like this
  error <- data$Y - predictedY  
  svrPredictionRMSE <- rmse(error)  # 3.157061 
} 


# end of svr model =======================================

#### Step 4, tuning SVR
rmse <- function(error)
{
  sqrt(mean(error^2))
}


plot(data)


# linear model ==============================================
model <- lm(Y ~ X , data)

predictedY <- predict(model, data) 
points(data$X, predictedY, col = "blue", pch=4)   


error <- model$residuals  # same as data$Y - predictedY
predictionRMSE <- rmse(error)   # 5.703778 
# end of linear model =======================================


plot(data)

# svr model ==============================================
if(require(e1071)){
  
  
  model <- svm(Y ~ X , data)
  
  predictedY <- predict(model, data)
  
  points(data$X, predictedY, col = "red", pch=17)
  
  
  error <- data$Y - predictedY  # /!\ this time  svrModel$residuals  is not the same as data$Y - predictedY
  svrPredictionRMSE <- rmse(error)  # 3.157061 
  
    
  tuneResult <- tune(svm, Y ~ X,  data = data, 
                ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
  ) 
  print(tuneResult) # best performance: MSE = 8.371412, RMSE = 2.89  epsilon  1e-04   cost 4
   
  # Draw the first tuning graph 
  plot(tuneResult) 
    
  # On the first tuning graph, we can see that the graph is darker on the leftside when epsilon is small,
  # so we adjust the tuning to go in this direction 
  
  # Draw the second tuning graph
  tuneResult <- tune(svm, Y ~ X,  data = data, 
                     ranges = list(epsilon = seq(0,0.2,0.01),
                                   cost = 2^(2:9))
  ) 
  
  print(tuneResult) 
  plot(tuneResult)
  
  plot(data, pch=16)
  tunedModel <- tuneResult$best.model
  tunedModelY <- predict(tunedModel, data) 
  
  points(data$X, predictedY, col = "red", pch=4)
  lines(data$X, predictedY, col = "red", pch=4)
  
  points(data$X, tunedModelY, col = "blue", pch=4)
  lines(data$X, tunedModelY, col = "blue", pch=4)
  
  error <- data$Y - tunedModelY  
  
  # this value can  be different because the best model is determined by cross-validation over randomly shuffled data 
  tunedModelRMSE <- rmse(error)  # 2.219642 
} 


# end of svr model =======================================



tuneResult$best.model


