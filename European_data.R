# 학위논문 데이터 분석
Sys.setenv("JAVA_HOME" = "/Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home")
# oracle db 연결 패키지 in memory
library(DBI)
library(RJDBC)

# db 연결
drv <- JDBC("oracle.jdbc.OracleDriver", "/Users/mac/Downloads/ojdbc6.jar")
con <- dbConnect(drv, "jdbc:oracle:thin:@//localhost:32769/xe", "scott", "tiger")
dbDisconnect(con) # 연결 종료
d1 <- dbGetQuery(con, "select * from european order by ROWN")
dbSendQuery(con, "")
str(d1)

# 구단명, 시즌 분리
team <- unlist(str_replace_all(d1$TEAM, "\\([0-9]{2}_[0-9]{2}\\)", ""))
season <- unlist(str_extract(d1$TEAM, "[0-9]{2}_[0-9]{2}"))
d1$TEAM <- team
d1$SEASON <- season
View(d1)
dim(d1)
# 522*38
# 질문 엑셀 파일 임포트 했는데 행의 순서가 바뀜?

# 빈 data.frame 만들기
df <- data.frame(matrix(nrow = 522, ncol = 1))
df

# 변수 위치 조정
df[1] <- d1[2]
colnames(df) <- "RANK"
df[2] <- d1[3]
df[3] <- d1[40]
df[4 :39] <- d1[4:39]
View(df)
str(df)
dim(df)
View(df)
# 데이터 프레임 저장
library(xlsx)
write.xlsx(df, "indiv_project.xlsx", sheetName = "First", row.names = F)


# 데이터 정규화
# scale() = 표준화
# 정규화 nor = (x - min) / max - min
# 정규분포 확인 shapiro.test
shapiro.test(df[,5]) # p-value = 2.457e-12  < 0.05 : 정규성 X
# 정규분포 함수 
nor <- function(x) {
  re <- (x - min(x)) / max(x) - min(x)
  return(re)
}
df[4:39] <- nor(df[4:39])
summary(df)

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
View(df)

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

# 정규화 및 정렬 데이터 저장

# 이상치 제거
df_f <- df
for (i in 1:36) {
  df_f <- df_f %>% filter(df_f[i+ 5] >= ab[i,1] & df_f[i + 5] <= ab[i,2])
}
table(df$RANK_C) # 360, 162
table(df_f$RANK_C) # 217, 53
boxplot(df_f[6:41])
write.xlsx(df, "scale_indiv_project.xlsx", sheetName = "Second", row.names = F)

# 변수 간의 관계분석
# 1) 범주형 vs 범주형
tab1 <- table(df_f$RANK_C, df_f$LEAGUE)
tab1
#         England Germany Spain
# Lower      94      81    42
# Upper      17      21    15
barplot(tab1, beside = T, horiz = F, col = rainbow(2), legend = row.names(tab1), ylim = c(0, 100))
mosaicplot(tab1, color = rainbow(3))
ggplot(df_f, aes(x = REAGUE)) + geom_bar()














######################################################

# 변수명 추출
aa <- colnames(df)
aa2 <- aa[4:39]
dim(aa2)
paste(aa2, collapse = "+")
str(df)

# plot.lm : 다중 회귀모델 lm(y ~ x) x : . (모든데이터)
a1 <- read.xlsx("clustering.xlsx", sheetName = "군집분석")
model <- lm(RANK ~ GOALS+SHOTS+YELLOW+RED+POSSESSION+PASS+AERIALWON+SHOTS_CONCEDED+TACKLES+INTERCEPTIONS+FOULS+OFFSIDES+SHOTS_ON_TARGET+DRIBBLES+FOULDED+OPEN_PLAY+COUNTER_ATTACK+SET_PIECE+PENALTY+OWN_GOAL+CROSS+THROUGH_BALL+LONG_BALLS+SHORT_PASSES+LEFT_SIDE+MIDDLE_SIDE+RIGHT_SIDE+SHOT_LEFT+SHOT_MIDDLE+SHOT_RIGHT+IN_6_YARD_BOX+IN_18_YARD_BOX+OUTSIDE_OF_BOX+OWN_THIRD+MIDDLE_THIRD+OPPOSITION_THIRD, data = df_f)
summary(model)
plot(model)