# multiple excel file read and join

if(!require("readxl"))install.packages("readxl");library(readxl)
files <- list.files(path = "C:/~" ,pattern = "*.xlsx", full.names = T)
files
tbl <- sapply(files, read_xlsx, simplify = F) %>% bind_rows(.id = "id")
tbl <- data.frame(tbl)

# lm 
cnt <- 0
mods <- vector(mode = "list", length = 3)
pred <- vector(mode = "list", length = 3)
cor_re <- vector(mode = "list", length = 3)
for (i in names) {
  cnt <- cnt + 1
  cat("y 와", i, "의 상관계수는")
  mods[[cnt]] <- lm(y ~ iris2[,i], data = iris2)
  pred[[cnt]] <- predict(mods[[cnt]], iris2)
  cor_re[[cnt]] <- cor(iris2$y, iris2[,i])
  cat(cor_re[[cnt]], "이다.", "\n")
}
