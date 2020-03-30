# multiple excel file read and join

if(!require("readxl"))install.packages("readxl");library(readxl)
files <- list.files( pattern = "*.xlsx")
files
tbl <- sapply(files, read_xlsx, simplify = F) %>% bind_rows(.id = "id")
tbl <- data.frame(tbl)


str(tbl)


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
mse
