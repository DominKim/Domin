#####################################################################
if(!require("xgboost")) install.packages("xgboost"); library(xgboost)
if(!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)
if(!require("dplyr")) install.packages("dplyr"); library(dplyr)
#####################################################################

# 주가 데이터 Input
stock <- read.csv("nor_stock.csv", stringsAsFactors = F, encoding = "euc-kr")
str(stock)
dim(data)
data <- stock[-c(1:4)]

# XGboost
dmatrix <- xgb.DMatrix(data = data.matrix(train[,-1]), label = train[,1])
xgb_model <- xgboost(dmatrix, max_dept = 2, nthread = 2, nrounds = 3, eta = 0.5,
                     verbose = 0, objective = "reg:squarederror")
xgb_model$
  summary(xgb_model)

# 중요변수확인
xgb_import <- xgb.importance(colnames(train[,-1]), xgb_model)
xgb_import
xgb.plot.importance(xgb_import)


# 4. 예측
xgb_pred <- predict(xgb_model, as.matrix(test[,-1]))
xgb_pred

# 5. 정확도 평가
err <- test[,1] - xgb_pred
(err^2)
mse <- mean(err^2)
mse # 0.004318746

cor(test[,1], xgb_pred) # 0.9032194
