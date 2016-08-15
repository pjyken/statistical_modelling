library(dplyr)
library(ggplot2)
library(GGally)

library(MASS)
library(glmnet)

fdta = read.table("Data/Exam/BankDefaultData.txt", header = TRUE)
fdtb = read.table("Data/Exam/MedicalStudy.txt", header = TRUE)
set.seed(417674)

dfa = fdta[sample(1:nrow(fdta), size = 1000),]
dfb = fdtb[fdtb$Patient %in% sample(unique(fdtb$Patient), 80, replace = FALSE),]

dfa$Education = as.character(dfa$Education)
dfa$Education = factor(dfa$Education, levels = c("N","P","M","S","H"))




head(dfa)


f1 = glm(Default ~ ., data = dfa, family = binomial)
summary(f1)
plot(f1)

stepfor = stepAIC(f1, direction = "forward", scope = list(upper = ~.^5, lower = ~1))


f2 = glm(Default ~ Income + Loan + Age + Gender + Education + Children + 
           Employment + Adress + Phone + Customer + Term + Limit + Income:Term + 
           Employment:Customer + Adress:Limit + Employment:Phone + Income:Phone + 
           Loan:Gender + Customer:Term + Employment:Adress, data = dfa, family = binomial)
summary(f2)

stepboth = stepAIC(f1, direction = "both", scope = list(upper = ~.^5, lower = ~1))


f3 = glm(Default ~ Income + Loan + Gender + Education + Employment + Adress + 
           Phone + Customer + Term + Limit + Income:Term + Employment:Customer + 
           Adress:Limit + Employment:Phone + Income:Phone + Loan:Gender + 
           Customer:Term + Employment:Adress, data = dfa, family = binomial)
summary(f3)


stepback = stepAIC(f3, direction = "backward", scope = list(upper = ~.^5, lower = ~1))

f4 = glm(Default ~ Income + Education + Employment + Phone + Customer + 
           Term + Limit,
         data = dfa, family = binomial)
summary(f4)

# Now using BIC

stepfor = stepAIC(f1, direction = "forward", scope = list(upper = ~.^5, lower = ~1), k = log(1000))

f8 = glm(Default ~ Income + Loan + Age + Gender + Education + Children + 
  Employment + Adress + Phone + Customer + Term + Limit + Income:Term + 
  Employment:Customer, data = dfa, family = binomial)

summary(f8)

stepboth = stepAIC(f1, direction = "both", scope = list(upper = ~.^5, lower = ~1), k = log(1000))

f9 = glm(Default ~ Income + Employment + Phone + Term + Income:Term, data = dfa, family = binomial)
summary(f9)

stepback = stepAIC(f3, direction = "backward", scope = list(upper = ~.^5, lower = ~1), k = log(1000))

f10 = glm(Default ~ Income + Employment + Phone + Customer + Term + Income:Term + 
            Employment:Customer, data = dfa, family = binomial)

summary(f10)



y = unlist(dfa["Default"])

x = dfa %>%
  dplyr::select(-Default) %>%
  mutate(Gender = as.integer(Gender), Education = as.integer(Education),
         Phone = as.integer(Phone), Customer = as.integer(Customer)) %>%
  as.matrix()

# x = dfa[c(1:2, 4:ncol(dfa))]

f5 = glmnet(x,y, family = "binomial")
print(f5)

plot(f5)
plot(f5, xvar = "lambda")
coef(f5, s=c(0.01, 0.05))



f6 = cv.glmnet(x,y, family = "binomial", nfold= 3)
f6$lambda.min
# f6 = cv.glmnet(x,y, family = "binomial", alpha = 1)
# f6$lambda.min
coef(f6, s=f6$lambda.min)
plot(f6)

f7 = glm(Default ~ Income + Education + Children + Employment + Customer + Phone, data = dfa)
summary(f7)

library(mgcv)

f11 = gam(Default ~ s(Income) + Employment + Phone + Term + Income:Term, 
          data = dfa, family = binomial, method = "GCV.Cp")
summary(f11)



AIC(f1)
AIC(f2)
AIC(f3)
AIC(f4)
AIC(f7)
AIC(f8)
AIC(f9)
AIC(f10)
AIC(f11)

AIC(f1, k = log(nrow(dfa)))
AIC(f2, k = log(nrow(dfa)))
AIC(f3, k = log(nrow(dfa)))
AIC(f4, k = log(nrow(dfa)))
AIC(f7, k = log(nrow(dfa)))
AIC(f8, k = log(nrow(dfa)))
AIC(f9, k = log(nrow(dfa)))
AIC(f10, k = log(nrow(dfa)))
AIC(f11, k = log(nrow(dfa)))


dfa %>% ggplot(aes(x = Default, y = Income)) + stat_sum(aes(size = ..n.., group = 1)) +
  scale_size_area(max_size = 10)

library(reshape2)

dfa %>%
  select(Default, Income , Employment , Phone , Term) %>%
  melt() %>%
  melt(id.vars = "Default") %>%
  ggplot(aes(factor(Default), y = value, ))

melt()
