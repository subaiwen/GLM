# HW07
invisible(lapply(c("tidyverse","MASS","nnet"),library, character.only=TRUE))
# 1.
df1 <- read.table("/Users/liubeixi/Desktop/GLM/Data/GLM_sample_txt/CH14TA13.txt")[,-1]
colnames(df1) <- c("preg", "preg1", "preg2","preg3","nutri","age1","age3","alcohol","smoking")
df1$OrderedRes <- ordered(df1$preg, c("1", "2", "3"))
polr_reg <- polr(OrderedRes ~ .-preg-preg1-preg2-preg3, data=df1 , Hess=T)
summary(polr_reg)

#2.
df2 <- read.table("/Users/liubeixi/Desktop/GLM/Data/GLM_sample_txt/TA_8_5_Housing.txt",header = T)
df2$OrderedRes <- ordered(df2$satisfaction2, c("1", "2", "3"))
polr_reg_sat <- polr(OrderedRes ~ contact + type, weights = frequency, data = df2)
summary(polr_reg_sat)

# Pearson Goodness-of-Fit test
df2 %>%
  arrange(type, contact) -> df2 # group the covariate pattern
observed<-matrix(df2$frequency, byrow=T, ncol=3)
observed
df3 <- df2 %>% # get n for covariate pattern
  group_by(type, contact) %>%
  summarise(n = sum(frequency))
df3 <- df3[rep(row.names(df3), each = 3),]
yhat<-predict(polr_reg_sat, type="probs")*df3$n
yhat
expected<-yhat[c(1:6)*3, ]
expected
rsp<-(observed-expected)/sqrt(expected)  # Standardized Pearson Residuals
rsp
c("PearsonChiSq"=sum(rsp^2), "df" = 12-5, "p-value"= 1-pchisq(sum(rsp^2), 12-5))
