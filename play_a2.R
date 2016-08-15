library(dplyr)
library(ggplot2)
library(GGally)


fdta = read.table("Data/Exam/BankDefaultData.txt", header = TRUE)
fdtb = read.table("Data/Exam/MedicalStudy.txt", header = TRUE)
set.seed(417674)

dfa = fdta[sample(1:nrow(fdta), size = 1000),]
dfb = fdtb[fdtb$Patient %in% sample(unique(fdtb$Patient), 80, replace = FALSE),]


df = dfa %>%
  select(Loan, Age)

df %>%
  ggplot(aes(y = Loan, x = Age)) + geom_point() + geom_smooth(method = "gam")


















