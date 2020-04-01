# raw_data 병합
if(!require("readxl"))install.packages("readxl");library(readxl)
library(xlsx)
library(data.table) # rbindlist
if(!require("stringr"))install.packages("stringr");library(stringr)

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
stock <- read.csv("f_stock_price.csv", stringsAsFactors = F)
str(stock)
View(stock)
str(stock)


# 데이터 분류
library(dplyr)
stock %>% group_by(결산년도) %>% summarise(n = n()) %>% arrange(n)
year <- unlist(str_remove_all(stock$결산년도, "\\([0-9]{1}Q\\)"))
year2 <- unlist(str_remove_all(year, "[0-9]{2}월"))
year3 <- unlist(str_remove_all(year2, "년"))
View(year)
summary(stock$결산년도)
year_num <- unlist(str_extract_all(year, "[0-9]{2}"))
stock$year <- year3
View(stock)
re_stock
write.csv(re_stock, "f_stock_price.csv", row.names = F, quote = F, fileEncoding = "euc-kr")

