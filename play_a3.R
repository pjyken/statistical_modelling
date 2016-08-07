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


y = unlist(dfa["Default"])

x = dfa %>%
  dplyr::select(-Default) %>%
  mutate(Gender = as.integer(Gender), Education = as.integer(Education),
         Phone = as.integer(Phone), Customer = as.integer(Customer)) %>%
  as.matrix()

x = dfa[c(1:2, 4:ncol(dfa))]

f5 = glmnet(x,y, family = "binomial")
print(f5)

plot(f5)
plot(f5, xvar = "lambda")
coef(f5, s=c(0.01, 0.1))


f6 = cv.glmnet(x,y, family = "binomial", nfold= 3)
f6$lambda.min
coef(f5, s=f6$lambda.min)


library(lasso2)
