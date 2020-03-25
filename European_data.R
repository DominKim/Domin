# import package
Sys.setenv("JAVA_HOME" = "/Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home")
if(!require("rJava")) install.packages("rJava");library(rJava)
if(!require("dplyr")) install.packages("dplyr"); library(dplyr)
if(!require("DBI")) install.packages("DBI"); library(DBI)
if(!require("RJDBC")) install.packages("RJDBC"); library(RJDBC)
if(!require("stringr")) install.packages("stringr"); library(stringr)
if(!require("xlsx")) install.packages("xlsx"); library(xlsx)
if(!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)
if(!require("lattice")) install.packages("lattice"); library(lattice)
if(!require("corrplot")) install.packages("corrplot"); library(corrplot)
if(!require("car")) install.packages("car"); library(car)
# 공분산성(vif)
if(!require("MASS")) install.packages("MASS"); library(MASS)
if(!require("nnet")) install.packages("nnet"); library(nnet)
if(!require("fmsb")) install.packages("fmsb"); library(fmsb)
if(!require("cvTools"))install.packages("cvTools"); library(cvTools)
if(!require("ROCR")) install.packages("ROCR"); library(ROCR)
# 분류정확도 그래프 

library(help = MASS)
# 학위논문 데이터 분석

# db 연결
drv <- JDBC("oracle.jdbc.OracleDriver", "/Users/mac/Downloads/ojdbc6.jar")
con <- dbConnect(drv, "jdbc:oracle:thin:@//localhost:32769/xe", "scott", "tiger")
d1 <- dbGetQuery(con, "select * from european order by ROWN")
str(d1)

# 구단명, 시즌 분리
team <- unlist(str_replace_all(d1$TEAM, "\\([0-9]{2}_[0-9]{2}\\)", ""))
season <- unlist(str_extract(d1$TEAM, "[0-9]{2}_[0-9]{2}"))
d1$TEAM <- team
d1$SEASON <- season
View(d1)
dim(d1) # 522*40

remove(df)
# 변수 위치 조정
df <- d1[2:3]
df[3] <- d1[40]
df[4 :39] <- d1[4:39]
View(df)
str(df) # 522 39
dim(df)

# 데이터 표준화 / 정규화
# scale() = 표준화
# 정규화 nor = (x - min) / max - min
# 정규분포 확인 shapiro.test
# 정규성 검증 확인 사용자 정의 함수
check_standard <- function(x) {
  for (i in 1:38) {
    tryCatch({
      if(shapiro.test(x[,i])[[2]] >= 0.05){
        print(colnames(df)[i])
      }
    }, error = function(e){cat("Error :", colnames(df)[i],conditionMessage(e), "\n")})
  }
}
check_standard(df) # "SHOTS_CONCEDED"
# [해설] 독립변수들이 "SHOTS_CONCEDED"를 제외하고 정규성을 띄지 않기 때문에
#        정규화를 통해 값의 범위를 0 ~ 1 사이로 일치

# 정규화 사용자정의 함수
nor <- function(x) {
  re <- (x - min(x)) / max(x) - min(x)
  return(re)
}

df_f <- nor(df, "RANK")
View(df_f)
View(df)
class(df[, i]) == "numeric"
df[4:39] <- nor(df[4:39])
summary(df)
View(df)

# 데이터 분류
# 1. 국가별 변수 만들기
# 522개 행 만들기
df$LEAGUE <- as.vector(runif(522, 1, 1))
# 빈 열 만들기
df$LEAGUE <- NA
df$LEAGUE[1:162] <- 1
df$LEAGUE[163:342] <- 2
df$LEAGUE[343:522] <- 3
df$LEAGUE <- ifelse(df$LEAGUE == 1, "Germany",
                    ifelse(df$LEAGUE == 2, "Spain", "England"))

# 변수 정렬
View(df)
dim(df)
f_df <- df[1:3]
f_df[4] <- df[40]
f_df[5:40] <- df[4:39]
View(f_df)

# 2. 순위 별
f_df <- f_df %>% mutate(RANK_C = ifelse(RANK <= 6, "Upper", "Lower"))
View(f_df)

# 변수 정렬
df <- f_df[1:4]
df[5] <- f_df[41]
df[6:41] <- f_df[5:40]
View(f_df)

# 데이터 확인
dim(df) # 522  41
str(df)

# 이상치 제거
# 연속형 변수 저장
df_num <- df[,6:41]

# min, max 추출
boxplot(df_num)$stats
length(df_num)

# a, b 변수 생셩
a <- NA;b <- NA
for (i in 1:length(df_num)) { 
  a[i] <- boxplot(df_num)$stats[,i][1]
  b[i] <- boxplot(df_num)$stats[,i][5]
  ab <- data.frame(a,b)
}

# 이상치 제거
df_f <- df
for (i in 1:36) {
  df_f <- df_f %>% filter(df_f[i+ 5] >= ab[i,1] & df_f[i + 5] <= ab[i,2])
}
table(df$RANK_C) # 360, 162
table(df_f$RANK_C) # 204, 51
boxplot(df_f[6:41])
write.xlsx(df, "scale_indiv_project.xlsx", sheetName = "Second", row.names = F)
str(df_f)
# 비연속형 변수
# RANK, TEAM, SEASON, LEAGUE, RANK_C
# 연속형 변수
# LEAGUE, RANK_C, GOALS, SHOTS, YELLOW, RED, POSSESSION, PASS, AERIALWON, 
# SHOTS_CONCEDED, TACKLES, INTERCEPTIONS, FOULS, OFFSIDES, SHOTS_ON_TARGET, 
# DRIBBLES, FOULDED, OPEN_PLAY, COUNTER_ATTACK, SET_PIECE, PENALTY, OWN_GOAL, 
# CROSS, THROUGH_BALL, LONG_BALLS, SHORT_PASSES, LEFT_SIDE, MIDDLE_SIDE, 
# RIGHT_SIDE, SHOT_LEFT, SHOT_MIDDLE, SHOT_RIGHT, IN_6_YARD_BOX, IN_18_YARD_BOX
# OUTSIDE_OF_BOX, OWN_THIRD

# 변수 간의 관계분석
# 1) 범주형 vs 범주형
tab1 <- table(df_f$RANK_C, df_f$LEAGUE)
tab1
#         England Germany Spain
# Lower      94      81    42
# Upper      17      21    15
barplot(tab1, beside = F, horiz = F, col = rainbow(2), legend = row.names(tab1), ylim = c(0, 100))
mosaicplot(tab1, color = rainbow(3))
ggplot(df_f, aes(x = LEAGUE, fill = RANK_C)) + geom_bar()
# [해석] Lower : England > Germany > Spain : 평준화 정도로 해석 가능
#        Upper : Germany > England > Spain : 평준화 정도로 해석 가능
# Why ? 이상치를 제거해서 엄청 잘하거나 못하는 기록을 가진 팀 제거!

# 시즌별 순위 변동 확인
dotplot(-RANK ~ SEASON, data = df_f, groups = TEAM, type= "o")


# 상관행렬
df_f_cor <- cor(df_f[,6:41])
round(df_f_cor, 2)
corrplot(df_f_cor, 
         method = "shade",  # 숫자로 표현
         type = "upper",     # 색상 200개 선정
         order = "hclust",   # 왼쪽 아래 행렬만 표시
         addCoef.col = "black",  # 상관계수 색깔
         diag = F,
         number.cex = 0.5, 
         tl.cex = 0.5,
         addcolorlabel="no")





##############################################################################
################################# 추론 통계 ##################################
##############################################################################
# 다중 회귀분석을 위해 범주형 변수 -> 숫자 변환
df_f <- df_f %>% mutate(RANK_C2 = ifelse(RANK_C == "Upper", 1, 2))
df_f <- df_f %>% mutate(LEAGUE2 = ifelse(LEAGUE == "Germany", 1,
                                         ifelse(LEAGUE == "Spain", 2, 3)))

# 변수명 추출
aa <- colnames(df_f)
str(aa)
aa2 <- aa[4:39]
dim(aa2)
paste(aa2, collapse = "+")
paste(aa2, collapse = ", ")
str(df)

# plot.lm : 다중 회귀모델 lm(y ~ x) x : . (모든데이터)

df_f2 <- df_f[c(aa[1], aa[6:39])]
str(df_f2)

# [분산팽창지수를 구하고 VIF 10 이상인 변수 중 가장 큰 값을 순차적으로 제거하는
# R 사용자 정의함수]

# Multi-collinearity check and remove the highly correlated variables step by step

# UDF of stepwise VIF function with preallocated vectors

# code source: https://beckmw.wordpress.com/2013/02/05/collinearity-and-stepwise-vif-selection/
vif_func <- function(in_frame,thresh=10, trace=F,...){
  
  require(fmsb)
  
  if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  
  vif_init <- vector('list', length = ncol(in_frame))
  
  names(vif_init) <- names(in_frame)
  
  var_names <- names(in_frame)
  
  for(val in var_names){
    
    regressors <- var_names[-which(var_names == val)]
    
    form <- paste(regressors, collapse = '+')
    
    form_in <- formula(paste(val,' ~ .'))
    
    vif_init[[val]] <- VIF(lm(form_in,data=in_frame,...))
    
  }
  
  vif_max<-max(unlist(vif_init))
  
  if(vif_max < thresh){
    
    if(trace==T){ #print output of each iteration
      
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('', times = nrow(vif_init) ),quote=F)
      
      cat('\n')
      
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
      
    }
    
    return(names(in_frame))
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    
    while(vif_max >= thresh){
      
      vif_vals <- vector('list', length = ncol(in_dat))
      
      names(vif_vals) <- names(in_dat)
      
      var_names <- names(in_dat)
      
      for(val in var_names){
        
        regressors <- var_names[-which(var_names == val)]
        
        form <- paste(regressors, collapse = '+')
        
        form_in <- formula(paste(val,' ~ .'))
        
        vif_add <- VIF(lm(form_in,data=in_dat,...))
        
        vif_vals[[val]] <- vif_add
        
      }
      
      max_row <- which.max(vif_vals)
      
      #max_row <- which( as.vector(vif_vals) == max(as.vector(vif_vals)) )
      
      vif_max<-vif_vals[max_row]
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        
        vif_vals <- do.call('rbind', vif_vals)
        
        vif_vals
        
        prmatrix(vif_vals,collab='vif',rowlab=row.names(vif_vals),quote=F)
        
        cat('\n')
        
        cat('removed: ', names(vif_max),unlist(vif_max),'\n\n')
        
        flush.console()
      }
      in_dat<-in_dat[,!names(in_dat) %in% names(vif_max)]
    }
    return(names(in_dat))
  }
}

X_independent <- vif_func(df_f2, thresh=10, trace=T)
paste(X_independent, collapse = "+")
length(X_independent) # 30
length(df_f2)         # 35
# 제거된 함수  = SHORT_PASSES 25.24992, RIGHT_SIDE 53.54048, SHOT_MIDDLE 93.99055, OUTSIDE_OF_BOX 95.47376, GOALS 174.562 




# 순위 예측
a1 <- read.xlsx("clustering.xlsx", sheetName = "군집분석")
model <- lm(RANK ~ SHOTS+YELLOW+RED+POSSESSION+PASS+AERIALWON+SHOTS_CONCEDED+TACKLES+INTERCEPTIONS+FOULS+OFFSIDES+SHOTS_ON_TARGET+DRIBBLES+FOULDED+OPEN_PLAY+COUNTER_ATTACK+SET_PIECE+PENALTY+OWN_GOAL+CROSS+THROUGH_BALL+LONG_BALLS+LEFT_SIDE+MIDDLE_SIDE+SHOT_LEFT+SHOT_RIGHT+IN_6_YARD_BOX+IN_18_YARD_BOX+OWN_THIRD, data = df_f2)
summary(model)
stepAIC(model)
plot(model)
sqrt(vif(model)) > 2
# [결과식] 
# Adjusted R-squared:  0.6759(67%) , p-value: < 2.2e-16(유의한 회귀식)
summary(lm(formula = RANK ~ YELLOW + RED + PASS + AERIALWON + SHOTS_CONCEDED + 
     FOULS + OPEN_PLAY + COUNTER_ATTACK + SET_PIECE + PENALTY + 
     OWN_GOAL + CROSS + THROUGH_BALL + IN_6_YARD_BOX, data = df_f2))


# 상위권 하위권 예측
idx <- sample(nrow(df_f), nrow(df_f)*0.7)
train <- df_f[idx,]
test <- df_f[-idx,]
df_f$RANK_C2 <- ifelse(df_f$RANK_C2 == 1, 0, 1)
table(train$RANK_C2 )
model2 <- glm(RANK_C2 ~ SHOTS+YELLOW+RED+POSSESSION+PASS+AERIALWON+SHOTS_CONCEDED+TACKLES+INTERCEPTIONS+FOULS+OFFSIDES+SHOTS_ON_TARGET+DRIBBLES+FOULDED+OPEN_PLAY+COUNTER_ATTACK+SET_PIECE+PENALTY+OWN_GOAL+CROSS+THROUGH_BALL+LONG_BALLS+LEFT_SIDE+MIDDLE_SIDE+SHOT_LEFT+SHOT_RIGHT+IN_6_YARD_BOX+IN_18_YARD_BOX+OWN_THIRD, data = train, family = "binomial")
summary(model2)
plot(model2)

# 로지스틱 회귀모델 예측치생성 : 검정 데이터
# newdata = test : 새로운 데이터 셋, type = "response" : 0 ~ 1확률값으로 예측
pred <- predict(model2, newdata = test, type = "response")
pred

# cut off = 0.5
cpred <- round(pred)
table(cpred)

y_true <- test$RANK_C2

# 교차분할표
table(y_true, cpred)
acc <- (9 + 58) / nrow(test)
acc # 0.8701299
upper <- 9 / 15
upper # 0.6
lower <- 58 / 62
lower # 0.9354839

#############################################################################
# 최종 수정 data.frame DB 전송
dbWriteTable(con, name = "f_eruopean", value = df_f)

# DB 연결 종료 
dbDisconnect(con) 