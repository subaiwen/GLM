# HW06
invisible(lapply(c("tidyverse","MASS","nnet"),library, character.only=TRUE))
# 2.
df2 <- read.table("/Users/liubeixi/Desktop/GLM/Data/GLM_sample_txt/CH14TA13.txt")[,-1]
colnames(df2) <- c("preg", "preg1", "preg2","preg3","nutri","age1","age3","alcohol","smoking")
reg2 <- multinom(cbind(preg3,preg2,preg1) ~ .-preg, data = df2)
summary(reg2)

# 3.
df3 <- read.table("/Users/liubeixi/Desktop/GLM/Data/GLM_sample_txt/TA_8_5_Housing.txt",header = T)
attach(df3)
## a.
tb1 <- xtabs( frequency ~ satisfaction + contact)
tb2 <- xtabs( frequency ~ satisfaction + type)
tb3 <- xtabs( frequency ~ contact + type)
proptb1 <- prop.table(tb1)
proptb2 <- prop.table(tb2)
proptb3 <- prop.table(tb3)
## b.
reg3 <- multinom(satisfaction ~ contact + type, weights = frequency, data = df3)
summary(reg3)
detach(df3)
