# (1) 데이터 전처리
#  - 결측치 제가
stock <- read.csv("stock12.csv", stringsAsFactors = F, fileEncoding = "euc-kr")
str(stock)
# 제거 해야 되는 변수 PER, EPS, 영익증가, year, 지배EPS
re_stock <- stock %>% select(-c(PER, EPS, 영익증가, 지배EPS))
re_stock$rownum <- row.names(re_stock)
str(re_stock)
summary(re_stock)

# 결측치 변수 확인
str(re_stock)
a <- summary(re_stock)[7,]
a <- na.omit(a)

# "   영업이익" "   당기순익" "   영업흐름" "     PBR"   "    부채율"  "   매출증가" "   지배ROE" 

re_stock1 <- re_stock
# 결측치 처리
re_stock %>% filter(is.na(지배ROE)) %>% select(c(company, year, rownum))
mean(re_stock[1282:1285, "지배ROE"])
x <- re_stock %>% filter(company == "현대상사" & year == 12)
x
# 결측치를 가지고 있는 변수명
na_var_names <- character()
cnt <- 1
for (i in 1:19) {
  if(!is.na(summary(x)[7,][i])){
   na_var_names[cnt] <- names(summary(x)[7,][i])
   cnt <- cnt + 1
  }
}

na_var_names <- unlist(str_remove_all(na_var_names, " "))

for (i in 1:length(na_var_names)) {
  x[,na_var_names[i]] <- ifelse(is.na(x[,na_var_names[i]]), mean(x[,na_var_names[i]], na.rm = T), x[,na_var_names[i]])
}

##########################
re_stock[2662:2665,] <- x
re_stock[2112:2114, "매출증가"] <- 139.3

# 결측치 처리 데이터 저장
write.csv(re_stock3, "f_re_stock.csv", row.names = F, quote = F, fileEncoding = "euc-kr")

stock <- read.csv("f_re_stock.csv", stringsAsFactors = F, encoding = "euc-kr")
str(stock)
stock[5:17] <- as.numeric(unlist(stock[5:17]))
str(stock)
re_stock <- stock
# - 이상치 확인!
boxplot(stock[5:17])

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
a <- apply(re_stock[5:17], 2, nor)
re_stock[5:17] <- a
View(re_stock)
write.csv(re_stock, "nor_stock.csv", row.names = F, quote = F, fileEncoding = "euc-kr")
