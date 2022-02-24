library(car)

displayData = read.delim("Display.dat", header = TRUE)

model1 = glm(formula = display~ fb , data = displayData,
             family = binomial())
model2 = update(model1, .~. + age + fb:age)
summary(model1)
summary(model2)
# Model probability
modelChi = model1$null.deviance - model1$deviance
modelDf = model1$df.null - model1$df.residual
modelProb = 1 - pchisq(modelChi, modelDf)
modelChi;  modelDf; modelProb

# Function to compute R^2's
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

# odds ratio and CI

exp(model1$coefficients)# odds ratio
exp(confint(model1)) # should not cross 1

# comparing models

difChi = model1$deviance - model2$deviance
difdf = model1$df.residual - model2$df.residual
diff.prob = 1 - pchisq(difChi, difdf )
difChi ; difdf ; diff.prob

anova(model1 , model2)

logisticPseudoR2s(model2)
exp(model2$coefficients)
exp(confint(model2))
displayData$predicted.probabilites = model1$fitted.values
head(displayData, n=15L)

# Residuals diagnostics

displayData$leverage = hatvalues(model1)
displayData$studentized.residuals = rstudent(model1)
displayData$dfbeta = dfbeta(model1)

displayData[, c("leverage", "studentized.residuals", "dfbeta")]
