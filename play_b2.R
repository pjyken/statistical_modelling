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

library(lmmLasso)
#x_matrix = as.matrix(cbind(1, dfb[,c(specify columns)]))
#z = matrix(rep(1,nrow(dfb)), ncol=1)
#colnames(z) = "Intercept"
#grp = dfb$Patient

#fitb2 = lmmlasso(y = y, x = x_matrix, z = z, grp = grp, lambda = mylamba, pdMat = "pdIndent)
#fitb2$aic



