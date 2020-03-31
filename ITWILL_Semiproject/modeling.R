# (3) 모델 평가
# 1) 산업명 정리
industry_name <- names(table(re_stock$industry))
industry_name


# 2) 모델
ind_df <- vector(mode = "list", length = 31)
ind_model_sm <- vector(mode = "list", length = 31)
ind_model <- vector(mode = "list", length = 31)
cnt <- 1
coefficient <- numeric()
변수 <- character()
산업 <- character()
설명력 <- numeric()
pred <- vector(mode = "list", length = 31)
cor_re <- vector(mode = "list", length = 31)
mse <- vector(mode = "list", length = 31)

for (i in 1:length(industry_name)) {
  
  # 산업별 분류
  ind_df[[i]] <- re_stock %>% filter(industry == industry_name[i])
  
  # train / test set
  idx <- sample(nrow(ind_df[[i]]), 0.7*nrow(ind_df[[i]]))
  train <- ind_df[[i]][idx, -c(1:5)]
  test <- ind_df[[i]][-idx,-c(1:5) ]
  
  # 모델 생성
  ind_model[[i]] <- lm(주가 ~ ., train)
  ind_model_sm[[i]] <- summary(lm(주가 ~ ., train))
  for (l in 1:length(ind_model_sm[[i]]$coefficients[,4])) {
    if (ind_model_sm[[i]]$coefficients[,4][l] < 0.05){
      coefficient[cnt] <- ind_model_sm[[i]]$coefficients[,4][l]
      변수[cnt] <- names(ind_model_sm[[i]]$coefficients[,4][l])
      산업[cnt] <- industry_name[i]
      설명력[cnt] <- ind_model_sm[[i]]$adj.r.squared
      cnt <- cnt + 1
    }
  }
  pred[[i]] <- predict(ind_model[[i]], test)
  cor_re[[i]] <- cor(test$"주가", pred[[i]])
  mse[[i]] <- mean((test$"주가" - pred[[i]])^2)
}
# 모델 summary 계수
result_multi_lm <- data.frame(coefficient, 변수, 산업,설명력)
result_multi_lm

# 산업별 설명력
a_r_squared <- result_multi_lm %>% group_by(산업) %>% summarise(설명력 = mean(설명력))
a_r_squared

# 모델 모든 결과값 결합
re_cor <- unlist(cor_re)
re_mse <- unlist(cor_re)
ind_cor_mse <- data.frame(산업 = industry_name, correlatin = re_cor, mse = re_mse)
ind_cor_mse
final_result <- left_join(a_r_squared, ind_cor_mse, by = "산업")
final_result

# 설명력 top3
final_result %>% arrange(desc(설명력)) %>% head(3)
View(final_result)
#   산업   설명력 correlatin   mse
# 1 통신사  0.985      0.753 0.753
# 2 생보    0.982      0.979 0.979
# 3 기계    0.973      0.936 0.936

real_result <- left_join(result_multi_lm %>% filter(산업 %in% c("벤처캐피탈", "생보", "통신사")), final_result %>% arrange(desc(설명력)) %>% head(3))
real_result <- real_result %>% arrange(desc(설명력))

# 최종 결과값
real_result

# 공선성 확인
library(car)
vif_sum <- vector(mode = "list", length = 31)
a <- vif_sum[[1]]
a <- ifelse(a == 1, 0, 0)
for (i in 1:31) {
  vif_sum[[i]] <- ifelse(sqrt(vif(ind_model[[i]])) > 2, 0, 1)
  for (r in 1:16) {
    a[r] <- a[r] + vif_sum[[i]][r]
  }
}
sort(a, decreasing = T)

# 
par(family="NanumGothic")
corrplot(cor(re_stock[6:22]), method = "number", type = "upper", diag = F)

#########################################
### 교차검정
#########################################

# 단계1 : K겹 교차검정을 위한 샘플링
if(!require("cvTools")) install.packages("cvTools");library(cvTools)
?cvFolds
# cvFolds(n, K = 5, R = 1,
#         type = c("random", "consecutive", "interleaved"))
cross <- cvFolds(n = nrow(re_stock), K = 10, R = 2, type = "random")
cross
str(cross)
str(stock_price)
# 단계2 : 검정 실시
K <- 1:10 # K겹
R <- 1:2  # Set 횟수
cnt <- 1
ACC <- numeric()
ACC2 <- numeric()
for (r in R) {
  cat("R = ", r, "번째 Set", "\n")
  for(k in K) {
    idx <- cross$subsets[cross$which == k, r]
    train <- re_stock[idx, ]
    test  <- re_stock[-idx, ]
    model <- lm(주가 ~ 매출액, train)
    pred <- predict(model, test)
    ACC[cnt] <- summary(model)["adj.r.squared"]
    ACC2[cnt] <- cor(test$주가, pred)
    cnt <- cnt + 1
  }
}
mean(unlist(ACC))
mean(unlist(ACC2))
unlist
