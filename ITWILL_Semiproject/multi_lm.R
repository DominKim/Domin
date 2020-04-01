######################################################################
if(!require("cvTools")) install.packages("cvTools"); library(cvTools)
if(!require("car")) install.packages("car"); library(car)
if(!require("corrplot")) install.packages("corrplot"); library(corrplot)
if(!require("dplyr")) install.packages("dplyr"); library(dplyr)
#######################################################################

# 주가 데이터 Input
stock <- read.csv("nor_stock.csv", stringsAsFactors = F, encoding = "euc-kr")
str(stock)
dim(data)
data <- stock[-c(1:4)]

# train / test set 설정
idx <- sample(nrow(data), nrow(data)*0.7)
train <- data[idx,]
test <- data[-idx,]


# 다중회귀분석 + 교차검정 필요
multi_model <- lm(주가 ~ ., train)
summary(multi_model)
# Multiple R-squared:  0.2196,	Adjusted R-squared:  0.2149 

# 모델평가
multi_pred <- predict(multi_model, test)
cor(test$주가, multi_pred) # 0.5578614
mse <- test$주가 - multi_pred
mean(mse^2) # 0.002975705


# 유의도 > 0.05 변수 추출
novalue <- character()
novalue_num <- numeric()
cnt <- 1
for (i in 1:length(summary(multi_model)$coefficients[,4])) {
  if(summary(multi_model)$coefficients[,4][i] > 0.05){
    novalue[cnt] <- names(summary(multi_model)$coefficients[,4][i])
    novalue_num[cnt] <- summary(multi_model)$coefficients[,4][i]
    cnt <- cnt + 1
  }
}
paste(novalue, collapse = ",-")
# "(Intercept) 당기순익 영업흐름 BPS 매출증가 지배ROE" 변수들은 주가를 예측하는데 유의하지 못한 변수들이다.
# 그러므로 이러한 변수들을 제거하고 다중회귀분석을 다시 돌려볼 필요가 있다.

# 변수 제거
names(data)
re_data <- data %>% select(-당기순익,-영업흐름,-BPS,-매출증가,-지배ROE)
str(re_data)

# train /test set 재설정
re_train <- re_data[idx,]
re_test <- re_data[-idx,]

# 다중회귀분석
re_multi_model <- lm(주가 ~ ., re_train)
summary(re_multi_model)
# Multiple R-squared:  0.2169,	Adjusted R-squared:  0.2141


# 모델평가
re_multi_pred <- predict(re_multi_model, re_test)
cor(re_test$주가, re_multi_pred) # 0.551524
mse <- re_test$주가 - re_multi_pred
mean(mse^2) # 0.003018186
# 오히려 설명력은 떨어지고 예측력은 거의 비슷하다. > 공선성을 확인해서 변수제거를 시도해 볼 필요가 있다.

# 다중공선성(Multicolinearity)확인
novar <- character()
cnt <- 1
for (i in 1:length(names(vif(multi_model)))) {
  if (sqrt(vif(multi_model))[i] > 2){
    novar[cnt] <- names(vif(multi_model))[i]
    cnt <- cnt + 1
  }
}
paste(novar, collapse = " ,-")
# "자본총계 매출액 영업이익 당기순익 영익률 매출이익" 변수들이 실제로 높은 공선성을 보인다.

# 상관분석을 통해 상관성이 높은 변수 둘 중 중요도가 낮은 변수 제거
corrplot(cor(data[-1]), method = "number", type = "upper", diag = F)

# 상관분석을 통해 서로 높은 상관도를 보이는 변수들은 영업이익 당기순익(0.98) 자본총계 매출액(0.92)
# 이다. 그리고, 자본총계 같은 경우에는 공선성이 높은 변수들 중 4개랑 높은 양의 상관성을 보이므로
# 필히 제거가 필요하다. 


# 공선성 높은 변수제거 자본총계
re_data <- data %>% select(-자본총계)

# train /test set 재설정
re_train <- re_data[idx,]
re_test <- re_data[-idx,]

# 다중회귀분석
re_multi_model <- lm(주가 ~ ., re_train)
summary(re_multi_model)
# Multiple R-squared:  0.2117,	Adjusted R-squared:  0.2073 


# 모델평가
re_multi_pred <- predict(re_multi_model, re_test)
cor(re_test$주가, re_multi_pred) # 0.5406467
mse <- re_test$주가 - re_multi_pred
mean(mse^2) # 0.003044309
# 자본총계만 제거시에는 오히려 낮은 예측력과 설명력을 보임

# 공선성 높은 변수제거 자본총계, 매출액
re_data <- data %>% select(-자본총계, -매출액)

# train /test set 재설정
re_train <- re_data[idx,]
re_test <- re_data[-idx,]

# 다중회귀분석
re_multi_model <- lm(주가 ~ ., re_train)
summary(re_multi_model)
# Multiple R-squared:  0.2046,	Adjusted R-squared:  0.2006 


# 모델평가
re_multi_pred <- predict(re_multi_model, re_test)
cor(re_test$주가, re_multi_pred) # 0.5557501
mse <- re_test$주가 - re_multi_pred
mean(mse^2) # 0.002937004
# 위와 마찬가지로 오히려 예측력과 설명력이 떨어짐


# 공선성 높은 변수제거 자본총계, 매출액, 영업이익
re_data <- data %>% select(-자본총계, -매출액, -영업이익)

# train /test set 재설정
re_train <- re_data[idx,]
re_test <- re_data[-idx,]

# 다중회귀분석
re_multi_model <- lm(주가 ~ ., re_train)
summary(re_multi_model)
# Multiple R-squared:  0.2006,	Adjusted R-squared:  0.197 


# 모델평가
re_multi_pred <- predict(re_multi_model, re_test)
cor(re_test$주가, re_multi_pred) # 0.5525958
mse <- re_test$주가 - re_multi_pred
mean(mse^2) # 0.002928888


# 공선성 높은 변수제거 자본총계, 매출액, 영업이익, 당기순익
re_data <- data %>% select(-자본총계, -매출액, -영업이익, -당기순익)

# train /test set 재설정
re_train <- re_data[idx,]
re_test <- re_data[-idx,]

# 다중회귀분석
re_multi_model <- lm(주가 ~ ., re_train)
summary(re_multi_model)
# Multiple R-squared:  0.1636,	Adjusted R-squared:  0.1603 


# 모델평가
re_multi_pred <- predict(re_multi_model, re_test)
cor(re_test$주가, re_multi_pred) # 0.4672997
mse <- re_test$주가 - re_multi_pred
mean(mse^2) # 0.00338315
# 공선성이 높은 변수들을 제거 했음에도 불구하고 오히려 설명력과 예측력이 떨어지므로
# 다른 알고리즘을 사용하는게 주가 예측모델에 더 적합하다.

# 다중회귀분석 교차검정
# k = 3 : d1 =50, d2 = 50, d3 = 50
cross <- cvFolds(n = nrow(data), K = 3, R = 2, type = "random")
cross #  Fold : dataset   Index : rownum
str(cross)

K <- 1:3 # k겹
R <- 1:2 # set
ACC <- numeric(); ACC2 <- numeric(); ACC3 <- numeric()
cnt <- 1
for (r in R) { # set = 열 index(2회)
  cat("R = ", r, "\n")
  for (k in K) { # k겹 = 행 index(3회)
    idx <- cross$subsets[cross$which == k, r]
    # cat("K = ", k, "\n")
    # print(idx)
    kmulti_test <- data[idx, ]   # 검정용(50) 
    kmulti_train <- data[-idx, ] # 훈련용(100)
    kmulti_model <- lm(주가 ~ ., kmulti_train)
    kmulti_pred <- predict(kmulti_model, kmulti_test)
    ACC[cnt] <- cor(kmulti_test$주가, kmulti_pred)
    ACC2[cnt] <- mean((kmulti_test$주가 - kmulti_pred)^2)
    ACC3[cnt] <- summary(kmulti_model)$adj.r.squared
    cnt <- cnt + 1 # 카운터
  }
}
mean(ACC) # 0.385987 상관성
mean(ACC2) # 0.007918064 mse
mean(ACC3) # 0.2883298 설명력
# 다중회귀 모델을 교차검정한 결과도 상관성과 설명력은 0.4 미만이므로 다른 알고리즘을 채택해서 예측을
# 해볼필요가 있다.






