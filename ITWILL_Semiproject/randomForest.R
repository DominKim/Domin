#####################################################################################
if(!require("randomForest")) install.packages("randomForest"); library(randomForest)
if(!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)
if(!require("dplyr")) install.packages("dplyr"); library(dplyr)
if(!require("corrplot")) install.packages("corrplot"); library(corrplot)
if(!require("reshape2")) install.packages("reshape2"); library(reshape2)
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
importance(random_model)
# BPS, PBR, 매출액, 자본총계 변수들은 다른 변수들에 비해 다소 높은 중요도를 보인다.
# 그러므로 위의 변수들을 가지고 알고리즘을 다시 한번 돌려볼 필요가 있다.

varImpPlot(random_model)

cor(random_model$y, random_model$predicted) # 0.9091815
# mse : 0.0009224629
# 대표적인 앙상블모형 randomForest와 XGboost를 통해 예측을 한결과 랜덤포레스트 알고리즘이
# 다소 높은 상관성과 다소 낮은 mse(평균제곱오차)를 보인다.
# 그러므로 주가데이터를 예측 하기위해서는 랜덤 포레스트 알고리즘을 활용하는게 적합하다.

# 중요 변수들 포함한 데이터 추출
str(data)
colnames(data)
re_stock <- data %>% select(주가, 자본총계, PBR, BPS, 매출액)
dim(re_stock)
View(re_stock)
# re 모델링
re_random_model <- randomForest(주가 ~ ., re_stock, mtree = 500, mtry = 1, importance = T)
re_random_model # 설명력 약 86

varImpPlot(re_random_model)
str(re_random_model)
cor(re_random_model$y, re_random_model$predicted) # 0.9328136
plot(re_random_model$y, re_random_model$predicted)
# BPS, PBR, 매출액, 자본총계 4개 변수들을 가지고 예측을 하였을 시에 더 높은 정확도를 보였다.
# 그러므로 주가를 예측할 때에는 위의 변수들을 가지고 예측을 했을시에 약 86%의 높은 예측율을 보인다.
corrplot(cor(re_stock), method = "number", type = "upper", diag = F)
str(data)
str(re_stock["주가"])


ra_imp.df <- data.frame(varImpPlot(random_model)[,])
ra_imp.df$var <- row.names(ra_imp.df)
sort_ra_imp.df <- ra_imp.df %>% arrange(desc(X.IncMSE))
row.names(sort_ra_imp.df) <- sort_ra_imp.df$var
sort_ra_imp.df

ggplot(sort_ra_imp.df, aes(x = reorder(var, X.IncMSE), y = X.IncMSE)) + geom_col() + coord_flip() + labs(x = "중요변수", y = "X.IncMSE") + theme_minimal() + theme(axis.title = element_text(face = "bold", size = 13, color = "darkblue")) + theme(axis.text.y = element_text(face = "bold", size = 11, color = "black"))  + ggtitle("Important Variables plot") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue")) + annotate("rect", xmin = 8.5, xmax = 12.5, ymin = 0, ymax = 41, alpha = .2, fill="red")
ggsave("X.IncMSE 중요변수.png", dpi = 300)

ggplot(sort_ra_imp.df, aes(x = reorder(var, IncNodePurity), y = IncNodePurity)) + geom_col() + coord_flip() + labs(x = "중요변수", y = "IncNodePurity") + theme_minimal() + theme(axis.title = element_text(face = "bold", size = 13, color = "darkblue")) + theme(axis.text.y = element_text(face = "bold", size = 11, color = "black")) + ggtitle("Important Variables plot") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue")) + annotate("rect", xmin = 8.5, xmax = 12.5, ymin = 0, ymax = 2.3, alpha = .2, fill="red")
ggsave("IncNodePurity 중요변수.png", dpi = 300)





# 시각화 작업
# 주가 ~ 각 변수 산점도 및 추세선
dta2 <- melt(re_stock, id.vars = c("주가"))
head(dta2)

ggplot(dta2, aes(x = 주가, y = value)) + 
  geom_point(alpha = .4) + geom_rug(data = dta2) + labs(y="", x="주가") +
  scale_color_brewer(palette=2) + facet_wrap(~variable, scales="free_y", ncol=4) + theme_minimal() +
  stat_smooth() + theme(strip.background = element_rect(fill="gray75"),strip.text.y = element_text(size=14, angle=-90, face="bold"))
ggsave("중요변수 산점도.png", dpi = 300)

# 주가 예측치와, 실제 주가 산점도
# ggplot 사용을 위해 data frame 생성
rf_df <- data.frame(주가 = re_random_model$y, 예측치 = re_random_model$predicted)

ggplot(data= rf_df, aes(x = 예측치, y = 주가)) + geom_point(color = "gray60") +
  stat_smooth(method = "lm", color = "red") + 
  labs(x = "Predictive Value", y = "Stock Price") + geom_rug() + theme_minimal() +
  geom_text(x = 0.8, y= 0.5, label = "설명력 : 약86%") +
  geom_text(x = 0.8, y= 0.43, label = "mse : 0.0007") + 
  geom_text(x = 0.8, y= 0.36, label = "상관도 : 약 0.93") +
  ggtitle("Random Forest Stock Price ~ Predictive Value corrplot") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue")) + theme(axis.title = element_text(face = "bold", size = 15, color = "darkblue"))
ggsave("랜덤포레스트결과값 상관도.png", dpi = 300)




