# (1) 데이터 전처리
#  - 결측치 제가
stock <- read.csv("stock12.csv", stringsAsFactors = F, fileEncoding = "euc-kr")
str(stock)
# raw data의 변수중 (PER, EPS, 영익증가, year, 지배EPS) 분석을 할때 필요없는 변수들 제거
re_stock <- stock %>% select(-c(PER, EPS, 영익증가, 지배EPS))
re_stock$rownum <- row.names(re_stock)
str(re_stock)
summary(re_stock)

# 결측치 변수 확인
str(re_stock)
a <- summary(re_stock)[7,]
a <- na.omit(a)
a
# 정제 후에 결측치를 가진 변수를 아래와 같이 추출하였다.
# "   영업이익" "   당기순익" "   영업흐름" "     PBR"   "    부채율"  "   매출증가" "   지배ROE" 
# 회의를 통해 주가의 특성을 고려해서 결측치를 가지는 변수들은 각 회사들의 연도별 평균을 가지고 
# 처리하기로 정하였다.

re_stock1 <- re_stock
# 결측치 처리
## 결측치를 가지고 있는 변수들의 행 출력
re_stock %>% filter(is.na(지배ROE)) %>% select(c(company))
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
na_var_names
# 결측치 평균으로 대체
for (i in 1:length(na_var_names)) {
  x[,na_var_names[i]] <- ifelse(is.na(x[,na_var_names[i]]), mean(x[,na_var_names[i]], na.rm = T), x[,na_var_names[i]])
}


re_stock[2662:2665,] <- x


# 결측치 처리 데이터 저장
write.csv(re_stock3, "f_re_stock.csv", row.names = F, quote = F, fileEncoding = "euc-kr")

stock <- read.csv("f_re_stock.csv", stringsAsFactors = F, encoding = "euc-kr")
str(stock)
stock[5:17] <- as.numeric(unlist(stock[5:17]))
str(stock)
re_stock <- stock


# 주가를 예측하기 위한 변수들은 비율로 되어 있는 변수들과 실수로 되어 있고 크기도 서로 다르기 때문에
# 단위를 통일 할 필요가 있다.
# 단위 통일을 위해 우선 정규성 검정을 실시하였다.
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
# 종속변수인 주가를 제외한 모든변수들은 정규성을 보이지 않으므로 scale 함수를 쓰는 것보다
# 0 ~ 1 사이 단위로 정규화를 하는게 더낫다고 판단하였다.

#   - 정규화(0 ~ 1)
nor <- function(x) {
  re <- (x - min(x)) /( max(x) - min(x))
  return(re)
}
a <- apply(re_stock[5:17], 2, nor)
re_stock[5:17] <- a
View(re_stock)
write.csv(re_stock, "nor_stock.csv", row.names = F, quote = F, fileEncoding = "euc-kr")
# 모든 변수들을 0 ~ 1사이로 정규화 한뒤에 무거운 엑셀 파일 보다는 가벼운 csv파일로 저장