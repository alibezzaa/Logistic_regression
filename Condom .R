# used packages
library(car)

CondomData = read.delim("condom.dat", header = TRUE)
CondomData$gender = relevel(CondomData$gender, "Male")
CondomData$use = relevel(CondomData$use, 2)
CondomData$previous = relevel(CondomData$previous, 3)
head(CondomData)

# first model 

model1 = glm(formula = use~ gender + safety + perceive, data = CondomData, 
             family = binomial())
model2 = glm(formula = use~ gender + safety + perceive + selfcon + previous + sexexp
             , data = CondomData, 
             family = binomial())
summary(model1)
summary(model2)
model1Chi = model1$null.deviance - model1$deviance
model1df = model1$df.null - model1$df.residual
model1Prob = 1- pchisq(model1Chi, model1df)
model1Chi ; model1df ; model1Prob

# model 2
model2Chi = model2$null.deviance - model2$deviance
model2df = model2$df.null - model2$df.residual
model2Prob = 1- pchisq(model2Chi, model2df)
model2Chi ; model2df ; model2Prob

logisticPseudoR2s <- function(LogModel){
  dev <- LogModel$deviance
  nullDev <- LogModel$null.deviance
  modelN <- length(LogModel$fitted.values)
  R.l <- 1 - dev/ nullDev
  R.cs <- 1 - exp(-(nullDev - dev) /modelN)
  R.n <- R.cs / (1 - (exp(-(nullDev/modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2 ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2       ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2          ", round(R.n, 3), "\n")
}

logisticPseudoR2s(model1)
logisticPseudoR2s(model2)


# odds ratio and CI

exp(model1$coefficients)# odds ratio
exp(confint(model1)) # should not cross 1
exp(model2$coefficients)
exp(confint(model2))
# comparing models

difChi = model1$deviance - model2$deviance
difdf = model1$df.residual - model2$df.residual
diff.prob = 1 - pchisq(difChi, difdf )
difChi ; difdf ; diff.prob

anova(model1 , model2)

# Reliability

vif(model2)
1/(vif(model2)) # higher than 0.1 (Menard, 1995)

# Test for linearity of the logit

CondomData$logSafety = log(CondomData$safety + 1)
CondomData$logSexexp = log(CondomData$sexexp + 1)
CondomData$logSelfcon = log(CondomData$selfcon + 1)
CondomData$logPerceive = log(CondomData$perceive + 1)

modelTest = glm(use~ safety + sexexp + selfcon + perceive 
                + safety:logSafety + sexexp:logSexexp + selfcon:logSelfcon + perceive:logPerceive,
                data = CondomData, family = binomial())
summary(modelTest)

# casewise diagnostics

CondomData$predicted.probabilities<-fitted(model2)
CondomData$standardized.residuals<-rstandard(model2)
CondomData$studentized.residuals<-rstudent(model2)
CondomData$dfbeta<-dfbeta(model2)
CondomData$dffit<-dffits(model2)
CondomData$leverage = hatvalues(model2)
CondomData[, c("leverage", "studentized.residuals", "dfbeta")]
hist(CondomData$studentized.residuals)
# Predicted probabilities 

(CondomData[c(12,53,75), c("use", "safety", "sexexp","selfcon", "perceive",
                           "previous", "gender", "predicted.probabilities")])

# predicting the probability for a female 

summary(model2)
z = (-4.959739 +0.002656*1 -0.482460*2 + 0.949088*6 + 0.347626*2 + 1.087196*1 + 2*0.180423)
1/(1 + exp(-z))
