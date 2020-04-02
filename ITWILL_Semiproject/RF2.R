################## Load packages ##################
library("ggplot2") # Graphics engine
library("RColorBrewer") # Nice color palettes
if(!require("plot3D"))install.packages("plot3D");library(plot3D)
library("dplyr") # Better data manipulations
library("parallel") # mclapply for multicore processing
# Analysis packages.
if(!require("randomForestSRC"))install.packages("randomForestSRC");library(randomForestSRC)
 # random forests for survival, regression and
# classification
if(!require("ggRandomForests"))install.packages("ggRandomForests");library(ggRandomForests)
################ Default Settings ##################


stock <- read.csv("nor_stock.csv", stringsAsFactors = F, encoding = "euc-kr")
stock <- read.csv("f_re_stock.csv", stringsAsFactors = F, encoding = "euc-kr")
str(stock)
dim(data)
head(stock)
data <- stock[-c(1:4)]



dta2 <- melt(data, id.vars = c("주가"))
head(dta2)
ggplot(dta2, aes(x = 주가, y = value)) + 
  geom_point(alpha = .4) + geom_rug(data = dta2) + labs(y="", x="주가") +
  scale_color_brewer(palette=2) + facet_wrap(~variable, scales="free_y", ncol=3) + theme_light() +
  stat_smooth()


ggplot(data= df, aes(x = y, y = x)) + geom_point(color = "gray75") +
  stat_smooth(method = "lm", color = "red") + 
  labs(x = "주가", y = "예측치", title = "Random Forest 주가 ~ 예측치 상관 그래프") +
  geom_rug() + theme_light() + geom_text(x = 0.8, y= 0.5, label = "")
