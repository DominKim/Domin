# (1) 데이터 전처리
#  - 결측치 제가
re_stock <- na.omit(stock)
str(re_stock)

# - 이상치 확인!
boxplot(re_stock[6:22])

# - 정규화
#   - 변수들 정규성 검증
check_standard <- function(x) {
  for (i in 1:10) {
    tryCatch({
      if(shapiro.test(x[,i])[[2]] >= 0.05){
        print(colnames(df)[i])
      } else {
      }
    }, error = function(e){cat()})
  }
}
check_standard(re_stock)

#   - 정규화(0 ~ 1)
nor <- function(x) {
  re <- (x - min(x)) /( max(x) - min(x))
  return(re)
}
a <- apply(re_stock[6:22], 2, nor)
re_stock[6:22] <- a
View(re_stock)