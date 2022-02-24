suicide = read.delim("Lacourse et al. (2001) Females.dat", header = TRUE)
library(car)
library(mlogit)
 
head(suicide)

suicide_model = glm(formula = Suicide_Risk~
                      Age + Marital_Status + Mother_Negligence +
                      Father_Negligence + Self_Estrangement +
                      Isolation + Normlessness + Meaninglessness +
                      Drug_Use + Metal + Worshipping + Vicarious, 
                    data = suicide , family = binomial(), tail)
summary(suicide_model)

# model fit 

modelChi = suicide_model$null.deviance - suicide_model$deviance
modelDf = suicide_model$df.null - suicide_model$df.residual

model_prob = 1- pchisq(modelChi, modelDf)
modelChi; modelDf ; model_prob

R2 = modelChi/suicide_model$null.deviance
R2 

exp(suicide_model$coefficients)# odds ratio
exp(confint(suicide_model)) # should not cross
