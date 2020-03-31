# raw_data 병합
if(!require("readxl"))install.packages("readxl");library(readxl)
library(data.table) # rbindlist

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
stock <- read.xlsx("f_data.xlsx", stringsAsFactors = F, sheetName = "a", encoding = "UTF-8")
str(stock)
dim(stock)
summary(stock)