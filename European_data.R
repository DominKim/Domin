# 학위논문 데이터 분석

# oracle db 연결 패키지 in memory
library(DBI)
library(RJDBC)

# db 연결
drv <- JDBC("oracle.jdbc.OracleDriver", "/Users/mac/Downloads/ojdbc6.jar")
con <- dbConnect(drv, "jdbc:oracle:thin:@//localhost:32769/xe", "scott", "tiger")
d1 <- dbGetQuery(con, "select * from european")
dbDisconnect(con) # 연결 종료
str(d1)

# 구단명, 시즌 분리
team <- unlist(str_replace_all(d1$TEAM, "\\([0-9]{2}_[0-9]{2}\\)", ""))
season <- unlist(str_extract(d1$TEAM, "[0-9]{2}_[0-9]{2}"))
d1$TEAM <- team
d1$SEASON <- season
View(d1)
dim(d1)
522*38
# 질문 엑셀 파일 임포트 했는데 행의 순서가 바뀜?

# 빈 data.frame 만들기
df <- data.frame(matrix(nrow = 522, ncol = 1))
df

# 변수 위치 조정
df[1] <- d1[1]
colnames(df[1]) <- c("r")
df[2] <- d1[2]
df[3] <- d1[3]
df[4] <- d1[40]
df[5:40] <- d1[4:39]
View(df)
df <- df %>% select(-matrix.nrow...522..ncol...1.)

# 데이터 프레임 저장
library(xlsx)
write.xlsx(df, "indiv_project.xlsx", sheetName = "First", row.names = F)


# 데이터 정규화
hist(df$RED) # 정규분포 확인
df[4:39] <- scale(df[4:39])
View(df)

# 정규화 데이터 저장
write.xlsx(df, "scale_indiv_project.xlsx", sheetName = "Second", row.names = F)

# 데이터 분류
# 1. 국가별
df$REAGUE <- as.vector(runif(522, 1, 1))
df$REAGUE[163:344] <- 2
df$REAGUE[345:522] <- 3
df$REAGUE <- ifelse(df$REAGUE == 1, "Germany",
                    ifelse(df$REAGUE == 2, "Spain", "England"))
View(df$REAGUE)

# 2. 순위 별
df <- df %>% mutate(RANK_C = ifelse(RANK <= 6, "Upper", "Lower"))
View(df)

# 데이터 확인
str(df)
dim(df) # 522  41

# 연속형 변수 저장
df_num <- df[,4:39]

# 
boxplot(df_num)$stats
name <- colnames(df_num)
df_num$name[1]
boxplot(df_num$GOALS)$stats

for (i in 1:36) {
  df_num$GOALS <- ifelse(df_num$GOALS >= boxplot(df_num)$stats[,i][1] &
                     df_num$GOALS <= boxplot(df_num)$stats[,i][5], df_num$GOALS, NA)
}

class(boxplot(df_num)$stats[1][1])
View(df_num)
table(is.na(df_num$GOALS))

a <- function(x) {
  print(df_num$x)
}
a("GOALS")

df_num$GOALS <- ifelse(df_num$GOALS >= boxplot(df_num)$stats[,i][1] &
                         df_num$GOALS <= boxplot(df_num)$stats[,i][5], df_num$GOALS, NA)
for (i in 1) {
  a <-unlist(str_extract_all(name[i], "[A-Z]{3,}"))
  print(a)
  print(df_num$a)
}

df_
colnames(df_num)

# 변수명 추출
aa <- colnames(df)
aa2 <- aa[4:39]
dim(aa2)
paste(aa2, collapse = "+")
str(df)

# plot.lm : 다중 회귀모델 lm(y ~ x) x : . (모든데이터)
model <- lm(RANK ~ GOALS+SHOTS+YELLOW+RED+POSSESSION+PASS+AERIALWON+SHOTS_CONCEDED+TACKLES+INTERCEPTIONS+FOULS+OFFSIDES+SHOTS_ON_TARGET+DRIBBLES+FOULDED+OPEN_PLAY+COUNTER_ATTACK+SET_PIECE+PENALTY+OWN_GOAL+CROSS+THROUGH_BALL+LONG_BALLS+SHORT_PASSES+LEFT_SIDE+MIDDLE_SIDE+RIGHT_SIDE+SHOT_LEFT+SHOT_MIDDLE+SHOT_RIGHT+IN_6_YARD_BOX+IN_18_YARD_BOX+OUTSIDE_OF_BOX+OWN_THIRD+MIDDLE_THIRD+OPPOSITION_THIRD, data = df)
summary(model)
plot(model)

