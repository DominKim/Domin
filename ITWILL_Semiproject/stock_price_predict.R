if(!require("readxl"))install.packages("readxl");library(readxl)
library(data.table) # rbindlist

# multiple csv file read and join
temp = list.files(pattern="*.csv")
aa <- vector(mode = "list", length = 93)
for (i in 1:length(temp)) {
  aa[[i]] <- read.csv(temp[i], fileEncoding = "euc-kr", stringsAsFactors = F, ) %>% mutate(id = temp[i])
}
bb <- rbindlist(aa)
write.xlsx(bb, "total_raw_data.xlsx", sheetName = "rawdata", row.names = F)
stock[6:22] <- ifelse(is.na(stock[6:22]), NA, as.numeric(unlist(stock[6:22])))

# 데이터 불러오기 
stock <- read.csv("stock_price.csv", stringsAsFactors = F)
str(stock)
dim(stock)
summary(stock)
stock %>% filter(is.na(매출액))
stock3 <- na.omit(stock)
dim(stock3)
# 단순 회귀분석 
# stock_price 변수 설명
# $ id      : chr  
# $ 결산년도: chr  
# $ 주가    : num  
# $ 자본총계: num  
# $ 매출액  : num  
# $ 영업이익: num  
# $ 당기순익: num  
# $ BPS     : num  
# $ PER     : num  
# $ EPS     : num  
# $ 부채율  : num  
# $ 영익률  : num  

# (1) 데이터 전처리
stock_price <- tbl[,c(-1,-2)]
stock_price <- na.omit(stock_price)
str(stock_price)
# (2) 이상치 확인!
boxplot(stock_price)

stock_price <- scale(stock_price)


# 변수들 정규성 검증
check_standard <- function(x) {
  for (i in 1:10) {
    tryCatch({
      if(shapiro.test(x[,i])[[2]] >= 0.05){
        print(colnames(df)[i])
      } else {
        print("정규성 x")
      }
    }, error = function(e){cat("Error :", colnames(df)[i],conditionMessage(e), "\n")})
  }
}
check_standard(stock_price)


# (3) 모델 평가
name <- colnames(stock_price)[-1]
name
stock_price <- data.frame(stock_price)
cnt <- 0
mods <- vector(mode = "list", length = 9)
pred <- vector(mode = "list", length = 9)
cor_re <- vector(mode = "list", length = 9)
mse <- vector(mode = "list", length = 9)
for (i in name) {
  cnt <- cnt + 1
  cat("주가 와", i, "의 상관계수는")
  mods[[cnt]] <- lm(주가 ~ stock_price[,i], data = stock_price)
  pred[[cnt]] <- predict(mods[[cnt]], stock_price)
  cor_re[[cnt]] <- cor(stock_price$"주가", pred[[cnt]])
  mse[[cnt]] <- mean((stock_price$"주가" - pred[[cnt]])^2)
  cat(cor_re[[cnt]], "이다.", "\n")
}



#########################################
### 교차검정
#########################################

# 단계1 : K겹 교차검정을 위한 샘플링
if(!require("cvTools")) install.packages("cvTools");library(cvTools)
?cvFolds
# cvFolds(n, K = 5, R = 1,
#         type = c("random", "consecutive", "interleaved"))
cross <- cvFolds(n = nrow(stock_price), K = 10, R = 2, type = "random")
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
    train <- stock_price[idx, ]
    test  <- stock_price[-idx, ]
    model <- lm(주가 ~ 매출액, train)
    pred <- predict(model, test)
    ACC[cnt] <- summary(model)["adj.r.squared"]
    ACC2[cnt] <- cor(test$주가, pred)
    cnt <- cnt + 1
  }
}
mean(unlist(ACC))
mean(unlist(ACC2))
