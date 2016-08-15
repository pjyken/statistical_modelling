library(dplyr)
library(ggplot2)
library(GGally)
library(hglm)


fdta = read.table("Data/Exam/BankDefaultData.txt", header = TRUE)
fdtb = read.table("Data/Exam/MedicalStudy.txt", header = TRUE)
set.seed(417674)

dfa = fdta[sample(1:nrow(fdta), size = 1000),]
dfb = fdtb[fdtb$Patient %in% sample(unique(fdtb$Patient), 80, replace = FALSE),]


str(dfb)

#fitb1 = hglm(fixed = y ~ a1 + a2, random = ~1 | subject, data = dfb, calc.lik = T, 
# family = [fill], maxit = 500)
#fitb1$likelihood$cAIC

# ggpairs(dfb)


f1 = hglm(fixed = Enzyme ~ Bloodpress + Heartperf, random = ~ 1 | Patient + Day,
          data = dfb, calc.like = TRUE, family = binomial(link = "logit"), maxit = 500)
summary(f1)
exp(f1$fixef)


f2 = hglm(fixed = Enzyme ~ Hunthess + Heartperf + Bloodpress + Heartfail +
            Heartrate + Age + Gender + Hypertn + Smoke + Diabetes + Highchol, 
          random = ~ 1 | Patient + Day,
          data = dfb, calc.like = TRUE, family = binomial(link = "logit"), maxit = 500)
summary(f2)
exp(f2$fixef)


f3 = hglm(fixed = Enzyme ~ Heartperf + Bloodpress + Gender + Age + Hypertn +
            Diabetes, #+ Heartfail +
            #Heartrate +  + Hypertn + Smoke + Diabetes + Highchol, 
          random = ~ 1 | Patient + Day,
          data = dfb, calc.like = TRUE, family = binomial(link = "logit"), maxit = 500)
summary(f3)
plot(f3)
exp(f2$fixef)