# hw09
invisible(lapply(c("knitr","kableExtra","tidyverse"),library, character.only=TRUE))
#------------------------------------------------------------------------------------------
# 1.
df <- readxl::read_xls("/Users/liubeixi/Desktop/GLM/Data/GLM_data/Table 10.4 Leukemia data.xls", skip = 2)
# survival times: in weeks, for leukemia patients
# WBC: white blood cell count (WBC)
# AG: the results of a test (AG positive and AG negative)

# a.
# Even though there is no censoring in this data, you need to create a survival object,
# with status=1 (1 for occurrence, 0 for censored) for all cases.
library(survival)
df$censor <- 1
KMfit <- survfit(Surv(`survival time`, censor)~AG, data=df)
# Empirical Survival function
summary(KMfit[1]) # AG = -
summary(KMfit[2]) # AG = +

# b.
par(mfrow=c(1,2), pty="s")
plot(KMfit[1]$time, -log(KMfit[1]$surv), main="Exponential", xlab="Time",
     ylab="H=-Log(S)")
points(KMfit[2]$time, -log(KMfit[2]$surv), pch=16, col=2)
legend(c(0,0.1), c(2.9,3), legend=c("Control", "Treatment"), col=c(1,2), pch=c(1,16), box.lty=0, cex = 0.5, pt.cex = 1.4) # straight line starts from 0: good

plot(log(KMfit[1]$time), log(-log(KMfit[1]$surv)), main="Weibull",
     xlab="Log(Time)", ylab="Log(H)=Log(-Log(S))")
points(log(KMfit[2]$time), log(-log(KMfit[2]$surv)),pch=16, col=2)
legend(c(0.5,1), c(1,1.3), legend=c("Control", "Treatment"), col=c(1,2), pch=c(1,16), box.lty=0, cex = 0.5, pt.cex = 1.4) # parallel

# c.
reg <- survreg(Surv(`survival time`, censor) ~ AG + log(WBC), dist='exponential', data=df)
summary(reg)
predict.1 <- predict(reg, type="response", newdata = df[df$AG=="+",])
predict.0 <- predict(reg, type="response", newdata = df[df$AG=="-",])
summary(predict.0) # predicted survival time of negative AG
summary(predict.1) # predicted survival time of positive AG

#------------------------------------------------------------------------------------------
# 2.
dt <- readxl::read_xls("/Users/liubeixi/Desktop/GLM/Data/GLM_data/Table 10.5 Hepatitis trial - corrected.xls", skip = 2)
dt$status <- ifelse(dt$censor == 'loss to follow-up', 0, 1)
# survival time: in months, of 44 patients with chronic active hepatitis.
# group: controlled trial of prednisolone compared with no treatment.

# a.
# a.1
kmfit <- survfit(Surv(`survival time`, status)~group, data=dt)
# Empirical Survival function
summary(kmfit[1]) # group=no treatment
summary(kmfit[2]) # group=prednisolone

# a.2
par(mfrow=c(1,2), pty="s")
plot(kmfit[1]$time, -log(kmfit[1]$surv), main="Exponential", xlab="Time",
     ylab="H=-Log(S)")
points(kmfit[2]$time, -log(kmfit[2]$surv), pch=16, col=2)
legend(c(0,0.1), c(3,3.3), legend=c("Control", "Treatment"), col=c(1,2), pch=c(1,16), box.lty=0, cex = 0.5, pt.cex = 1.4)
# not straight line

plot(log(kmfit[1]$time), log(-log(kmfit[1]$surv)), main="Weibull",
     xlab="Log(Time)", ylab="Log(H)=Log(-Log(S))")
points(log(kmfit[2]$time), log(-log(kmfit[2]$surv)),pch=16, col=2)
legend(c(0.5,1), c(1,1.4), legend=c("Control", "Treatment"), col=c(1,2), pch=c(1,16), box.lty=0, cex = 0.5, pt.cex = 1.4)
# not parallel

# a.3
fit <- survreg(Surv(`survival time`, status) ~ group, dist='weibull', data=dt)
summary(fit)
