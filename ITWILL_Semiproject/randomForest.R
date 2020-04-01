#####################################################################################
if(!require("randomForest")) install.packages("randomForest"); library(randomForest)
if(!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)
if(!require("dplyr")) install.packages("dplyr"); library(dplyr)
#####################################################################################

# 주가 데이터 Input
stock <- read.csv("nor_stock.csv", stringsAsFactors = F, encoding = "euc-kr")
stock <- read.csv("f_re_stock.csv", stringsAsFactors = F, encoding = "euc-kr")
str(stock)
dim(data)
data <- stock[-c(1:4)]

# 랜덤 포레스트
random_model <- randomForest(주가 ~ ., data, mtree = 500, mtry = 4, importance = T)
random_model
# 설명력 : 약 82

random_model$importance
# BPS, PBR, 매출액, 자본총계 변수들은 다른 변수들에 비해 다소 높은 중요도를 보인다.
# 그러므로 위의 변수들을 가지고 알고리즘을 다시 한번 돌려볼 필요가 있다.

varImpPlot(random_model)
cor(random_model$y, random_model$predicted) # 0.9091815
# mse : 0.0009224629
# 대표적인 앙상블모형 randomForest와 XGboost를 통해 예측을 한결과 랜덤포레스트 알고리즘이
# 다소 높은 상관성과 다소 낮은 mse(평균제곱오차)를 보인다.
# 그러므로 주가데이터를 예측 하기위해서는 랜덤 포레스트 알고리즘을 활용하는게 적합하다.

# 중요 변수들 포함한 데이터 추출
re_stock <- stock %>% select(주가, BPS, PBR, 매출액, 자본총계)
dim(re_stock)

# re 모델링
re_random_model <- randomForest(주가 ~ ., re_stock, mtree = 500, mtry = 1, importance = T)
re_random_model # 설명력 약 86

varImpPlot(re_random_model)
cor(re_random_model$y, re_random_model$predicted) # 0.9328136

# BPS, PBR, 매출액, 자본총계 4개 변수들을 가지고 예측을 하였을 시에 더 높은 정확도를 보였다.
# 그러므로 주가를 예측할 때에는 위의 변수들을 가지고 예측을 했을시에 약 86%의 높은 예측율을 보인다.
