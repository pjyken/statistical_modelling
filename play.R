library(dplyr)
library(ggplot2)
library(GGally)

library(leaps)

fdta = read.table("Data/Exam/BankDefaultData.txt", header = TRUE)
fdtb = read.table("Data/Exam/MedicalStudy.txt", header = TRUE)
set.seed(417674)

dfa = fdta[sample(1:nrow(fdta), size = 1000),]
dfb = fdtb[fdtb$Patient %in% sample(unique(fdtb$Patient), 80, replace = FALSE),]

# rm(fdta, fdtb)

levels(dfa$Customer) = c("Existing", "New")
levels(dfa$Gender) = c("Female", "Male")


df = dfa %>%
  select(Loan, Income, Gender, Customer, Age) 

# df %>% ggpairs()


df = df %>% mutate(Loan = log(Loan), Income = log(Income))

# ggpairs(df)

df = dfa[c("Loan", "Income", "Gender", "Customer", "Age")]

library(MASS)

f0 = lm(Loan ~ Income, data = df)
summary(f0)

f1 = lm(Loan ~ Income * Gender * Customer * Age, data = df)
summary(f1)

stepback = stepAIC(f1, direction="backward")

f2 = lm(Loan ~ Income + Gender + Age + Income:Gender + Income:Age, data = df)
summary(f2)


f3 = lm(Loan ~ Income + Gender + Customer + Age, data = df)
summary(f3)

stepfor = stepAIC(f3, direction = "forward", scope = list(upper = ~.^3, lower = ~1))

f4 = lm(Loan ~ Income + Gender + Customer + Age + Income:Gender + Income:Age, data = df)
summary(f4)

f5 = lm(Loan ~ Income + Gender + Income:Gender, data = df)
summary(f5)


f6 = lm(Loan ~ Income + I(Income^2) + I(Income^3) + I(Income^4) + I(Income^5), data = df)
summary(f6)

stepfor = stepAIC(f6, direction = "forward", scope = list(upper = ~.^3, lower = ~1))


f6 = lm(Loan ~ Income + I(Income^2) + I(Income^3) + I(Income^4) + I(Income^5), data = df)
summary(f6)


f7 = lm(Loan ~ Gender * Age * (Income + I(Income^2) + I(Income^3) + I(Income^4) + I(Income^5) +
          I(Income^6) + I(Income^7) + I(Income^8) + I(Income^9) + I(Income^10)), data = df)

summary(f7)

df %>%
  ggplot(aes(y = Loan, x = Income)) + 
  geom_point(aes(y = Loan, x = Income, color = Age, shape = Gender)) + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 4), se = FALSE) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 10), linetype = "twodash", color = "red", se = FALSE) 


df = df %>%
  mutate(s_Income = scale(Income), s_Loan = scale(Loan))


f8 = lm(s_Loan ~ s_Income + I(s_Income^2) + I(s_Income^3), data = df )
summary(f8)


stepfor = stepAIC(f8, direction = "forward", scope = list(upper = ~.^5, lower = ~1))

# predict(f5, )

dff = cbind(df, fv = f7$fitted.values)


## IC Outputs

AIC(f0)
AIC(f1)
AIC(f2) 
AIC(f3)
AIC(f4)
AIC(f5)
AIC(f6) 
AIC(f7) # Best
AIC(f8)


AIC(f0, k = log(nrow(df)))
AIC(f1, k = log(nrow(df)))
AIC(f2, k = log(nrow(df))) 
AIC(f3, k = log(nrow(df)))
AIC(f4, k = log(nrow(df)))
AIC(f5, k = log(nrow(df)))
AIC(f6, k = log(nrow(df))) 
AIC(f7, k = log(nrow(df))) # Best
AIC(f8, k = log(nrow(df)))


f10 = lm(Loan ~ Income + pmax((df$Income - 37.5), 0) + pmax((df$Income - 40), 0), data = df)
summary(f10)
plot(f10)

dff = cbind(df, fv = f10$fitted.values)
  
dff %>%
  ggplot(aes(x = Income, y = Loan)) + geom_point() + 
  geom_point(aes(x = Income, y = fv), color = "red", alpha = 0.5) 


f11 = lm(Loan ~ (Income + pmax((df$Income - 37.5), 0) + pmax((df$Income - 40), 0)) + Gender, data = df)
summary(f11)
plot(f11)


AIC(f10, k = log(nrow(df)))
AIC(f10)
AIC(f11, k = log(nrow(df)))

dff = cbind(df, fv = f11$fitted.values)


f12 = lm(Loan ~ (Income + pmax((df$Income - 38), 0) + pmax((df$Income - 40.5), 0)), data = df)

summary(f12)
plot(f12)

dff = cbind(df, fv = f12$fitted.values)

dff %>%
  ggplot(aes(x = Income, y = Loan)) + geom_point() + 
  geom_point(aes(x = Income, y = fv), color = "red", alpha = 0.5)


f13 = lm(Loan ~ Income + I(Income^2) + I(Income^3) + I(pmax((df$Income -37.5),0)^3) +
                   I(pmax((df$Income -40),0)^3) , data = df)
summary(f13)
# plot(f13)

dff = cbind(df, fv = f13$fitted.values)

dff %>%
  ggplot(aes(x = Income, y = Loan)) + geom_point() + 
  geom_point(aes(x = Income, y = fv), color = "red", alpha = 0.5)



library(SemiPar)

f14 = spm(df$Loan ~ f(df$Income, basis = "trunc.poly", degree = 3) , spar.method = "ML")
summary(f14)

dff = cbind(df, fv= f14$fit$fitted)



#AIC
2*(f14$fit$logLik-2*sum(f14$aux$df))



